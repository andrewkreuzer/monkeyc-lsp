open Import
module Version = Version
module Diagnostics = Diagnostics
module Diff = Diff
open Fiber.O

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Jsonrpc.Response.Error.raise
    (make_error ~code:InternalError ~message:"Request not supported yet!" ())

let view_metrics_command_name = "monkeyclsp/view-metrics"

let view_metrics server =
  let* json = Metrics.dump () in
  let uri, chan =
    Filename.open_temp_file (sprintf "lsp-metrics.%d" (Unix.getpid ())) ".json"
  in
  output_string chan json;
  close_out_noerr chan;
  let req =
    let uri = Uri.of_path uri in
    Server_request.ShowDocumentRequest
      (ShowDocumentParams.create ~uri ~takeFocus:true ())
  in
  let+ { ShowDocumentResult.success = _ } = Server.request server req in
  `Null

let initialize_info (client_capabilities : ClientCapabilities.t) :
    InitializeResult.t =
  (* let codeActionProvider = *)
  (*   match client_capabilities.textDocument with *)
  (*   | Some { codeAction = Some { codeActionLiteralSupport = Some _; _ }; _ } -> *)
  (*     let codeActionKinds = *)
  (*       Action_inferred_intf.kind :: Action_destruct.kind *)
  (*       :: List.map *)
  (*            ~f:(fun (c : Code_action.t) -> c.kind) *)
  (*            [ Action_type_annotate.t *)
  (*            ; Action_remove_type_annotation.t *)
  (*            ; Action_construct.t *)
  (*            ; Action_refactor_open.unqualify *)
  (*            ; Action_refactor_open.qualify *)
  (*            ; Action_add_rec.t *)
  (*            ; Action_inline.t *)
  (*            ] *)
  (*       |> List.sort_uniq ~compare:Poly.compare *)
  (*     in *)
  (*     `CodeActionOptions (CodeActionOptions.create ~codeActionKinds ()) *)
  (*   | _ -> `Bool true *)
  (* in *)
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create
         ~openClose:true
         ~change:TextDocumentSyncKind.Incremental
         ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false
         ())
  in
  let codeLensProvider = CodeLensOptions.create ~resolveProvider:false () in
  let completionProvider =
    CompletionOptions.create
      ~triggerCharacters:[ "."; "#" ]
      ~resolveProvider:true
      ()
  in
  let signatureHelpProvider =
    SignatureHelpOptions.create
      ~triggerCharacters:[ " "; "~"; "?"; ":"; "(" ]
      ()
  in
  let renameProvider =
    `RenameOptions (RenameOptions.create ~prepareProvider:true ())
  in
  let workspace =
    let workspaceFolders =
      WorkspaceFoldersServerCapabilities.create
        ~supported:true
        ~changeNotifications:(`Bool true)
        ()
    in
    ServerCapabilities.create_workspace ~workspaceFolders ()
  in
  let capabilities =
    (* let experimental = *)
    (*   `Assoc *)
    (*     [ ( "ocamllsp" *)
    (*       , `Assoc *)
    (*           [ ("interfaceSpecificLangId", `Bool true) *)
    (*           ; Req_switch_impl_intf.capability *)
    (*           ; Req_infer_intf.capability *)
    (*           ; Req_typed_holes.capability *)
    (*           ; Req_wrapping_ast_node.capability *)
    (*           ; Req_hover_extended.capability *)
    (*           ] ) *)
    (*     ] *)
    (* in *)
    (* let executeCommandProvider = *)
    (*   let commands = *)
    (*     if *)
    (*       Action_open_related.available *)
    (*         (let open Option.O in *)
    (*         let* window = client_capabilities.window in *)
    (*         window.showDocument) *)
    (*     then *)
    (*       view_metrics_command_name :: Action_open_related.command_name *)
    (*       :: Document_text_command.command_name *)
    (*       :: Merlin_config_command.command_name :: Dune.commands *)
    (*     else Dune.commands *)
    (*   in *)
    (*   ExecuteCommandOptions.create ~commands () *)
    (* in *)
    (* let semanticTokensProvider = *)
    (*   let full = `Full (SemanticTokensOptions.create_full ~delta:true ()) in *)
    (*   `SemanticTokensOptions *)
    (*     (SemanticTokensOptions.create *)
    (*        ~legend:Semantic_highlighting.legend *)
    (*        ~full *)
    (*        ()) *)
    (* in *)
    let positionEncoding =
      let open Option.O in
      let* general = client_capabilities.general in
      let* options = general.positionEncodings in
      List.find_map
        ([ UTF8; UTF16 ] : PositionEncodingKind.t list)
        ~f:(fun encoding ->
          Option.some_if (List.mem options ~equal:Poly.equal encoding) encoding)
    in
    ServerCapabilities.create
      ~textDocumentSync
      ~hoverProvider:(`Bool true)
      ~declarationProvider:(`Bool true)
      ~definitionProvider:(`Bool true)
      ~typeDefinitionProvider:(`Bool true)
      ~completionProvider
      ~signatureHelpProvider
      ~codeLensProvider
      ~referencesProvider:(`Bool true)
      ~documentHighlightProvider:(`Bool true)
      ~documentFormattingProvider:(`Bool true)
      ~selectionRangeProvider:(`Bool true)
      ~documentSymbolProvider:(`Bool true)
      ~workspaceSymbolProvider:(`Bool true)
      ~foldingRangeProvider:(`Bool true)
      ~renameProvider
      ~workspace
      ?positionEncoding
      ()
  in
  let serverInfo =
    let version = Version.get in
    InitializeResult.create_serverInfo ~name:"monkeyclsp" ~version ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let on_initialize server (ip : InitializeParams.t) =
  let state : State.t = Server.state server in
  let workspaces = Workspaces.create ip in
  let diagnostics =
    Diagnostics.create
      (let open Option.O in
      let* td = ip.capabilities.textDocument in
      td.publishDiagnostics)
      (function
        | [] -> Fiber.return ()
        | diagnostics ->
          let state = Server.state server in
          task_if_running state.detached ~f:(fun () ->
              let batch = Server.Batch.create server in
              List.iter diagnostics ~f:(fun d ->
                  Server.Batch.notification batch (PublishDiagnostics d));
              Server.Batch.submit batch))
  in
  let+ monkeyc =
    let progress =
      Progress.create
        ip.capabilities
        ~report_progress:(fun progress ->
          Server.notification
            server
            (Server_notification.WorkDoneProgress progress))
        ~create_task:(fun task ->
          Server.request server (Server_request.WorkDoneProgressCreate task))
    in
    let monkeyc =
      Monkeyc.create
        workspaces
        ip.capabilities
        diagnostics
        progress
        state.store
        ~log:(State.log_msg server)
    in
    let+ () = Fiber.Pool.task state.detached ~f:(fun () -> Monkeyc.run monkeyc) in
    monkeyc
  in
  let initialize_info = initialize_info ip.capabilities in
  let state =
    let position_encoding =
      match initialize_info.capabilities.positionEncoding with
      | None | Some UTF16 -> `UTF16
      | Some UTF8 -> `UTF8
      | Some UTF32 | Some (Other _) -> assert false
    in
    State.initialize state ~position_encoding ip workspaces monkeyc diagnostics
  in
  let state =
    match ip.trace with
    | None -> state
    | Some trace -> { state with trace }
  in
  let resp =
    match ip.capabilities.textDocument with
    | Some
        { TextDocumentClientCapabilities.synchronization =
            Some
              { TextDocumentSyncClientCapabilities.dynamicRegistration =
                  Some true
              ; _
              }
        ; _
        } ->
      Reply.later (fun send ->
          let* () = send initialize_info in
          let register =
            RegistrationParams.create
              ~registrations:
                (let make method_ =
                   let id = "ocamllsp-cram-dune-files/" ^ method_ in
                   (* TODO not nice to copy paste *)
                   let registerOptions =
                     let documentSelector =
                       [ "cram"; "dune"; "dune-project"; "dune-workspace" ]
                       |> List.map ~f:(fun language ->
                              `TextDocumentFilter
                                (TextDocumentFilter.create ~language ()))
                     in
                     TextDocumentRegistrationOptions.create ~documentSelector ()
                     |> TextDocumentRegistrationOptions.yojson_of_t
                   in
                   Registration.create ~id ~method_ ~registerOptions ()
                 in
                 [ make "textDocument/didOpen"; make "textDocument/didClose" ])
          in
          Server.request
            server
            (Server_request.ClientRegisterCapability register))
    | _ -> Reply.now initialize_info
  in
  (resp, state)

let set_diagnostics _detached _diagnostics doc =
    match Document.syntax doc with
    | Monkeyc | Manifest | Jungle -> Fiber.return ()

let on_request :
    type resp.
       State.t Server.t
    -> resp Client_request.t
    -> (resp Reply.t * State.t) Fiber.t =
 fun server req ->
  let rpc = server in
  let state : State.t = Server.state server in
  let now res = Fiber.return (Reply.now res, state) in
  let later f req =
    Fiber.return
      ( Reply.later (fun k ->
            let* resp = f state req in
            k resp)
      , state )
  in
  match req with
  | Client_request.UnknownRequest { meth; params } ->
    let _ = meth in
    let _ = params in
    not_supported ()
  | Initialize ip ->
    let+ res, state = on_initialize server ip in
    (res, state)
  | DebugTextDocumentGet { textDocument = { uri }; position = _ } ->
    let _ = uri in
    not_supported ()
  | DebugEcho params -> now params
  | Shutdown -> Fiber.return (Reply.now (), state)
  | WorkspaceSymbol _req -> not_supported ()
  | CodeActionResolve ca -> now ca
  | ExecuteCommand _command -> not_supported ()
  | CompletionItemResolve _ci -> not_supported ()
  | CodeAction _params -> not_supported ()
  | InlayHint _ -> now None
  | TextDocumentColor _ -> now []
  | TextDocumentColorPresentation _ -> now []
  | TextDocumentHover req ->
    let mode =
      match state.configuration.data.extended_hover with
      | Some { enable = true } -> Hover_req.Extended_variable
      | Some _ | None -> Hover_req.Default
    in
    later (fun (_ : State.t) () -> Hover_req.handle rpc req mode) ()
  | TextDocumentReferences _req ->not_supported ()
  | TextDocumentCodeLensResolve codeLens -> now codeLens
  | TextDocumentCodeLens _req -> not_supported ()
  | TextDocumentHighlight _req -> not_supported ()
  | DocumentSymbol { textDocument = { uri }; _ } ->
    let _ = uri in
    not_supported ()
  | TextDocumentDeclaration { textDocument = { uri }; position } ->
    let _ = position in
    let _ = uri in
    not_supported ()
  | TextDocumentDefinition { textDocument = { uri }; position; _ } ->
    let _ = position in
    let _ = uri in
    not_supported ()
  | TextDocumentTypeDefinition { textDocument = { uri }; position; _ } ->
    let _ = position in
    let _ = uri in
    not_supported ()
  | TextDocumentCompletion _params -> not_supported ()
  | TextDocumentPrepareRename _ -> not_supported ()
  | TextDocumentRename _req -> not_supported ()
  | TextDocumentFoldingRange _req -> not_supported ()
  | SignatureHelp _req -> not_supported ()
  | TextDocumentLinkResolve l -> now l
  | TextDocumentLink _ -> now None
  | WillSaveWaitUntilTextDocument _ -> now None
  | TextDocumentFormatting { textDocument = { uri }; options = _; _ } ->
    let _ = uri in
    not_supported ()
  | TextDocumentOnTypeFormatting _ -> now None
  | SelectionRange _req -> not_supported ()
  | TextDocumentImplementation _ -> not_supported ()
  | SemanticTokensFull _p -> not_supported ()
  | SemanticTokensDelta _p -> not_supported ()
  | TextDocumentMoniker _ -> not_supported ()
  | TextDocumentPrepareCallHierarchy _ -> not_supported ()
  | TextDocumentRangeFormatting _ -> not_supported ()
  | CallHierarchyIncomingCalls _ -> not_supported ()
  | CallHierarchyOutgoingCalls _ -> not_supported ()
  | SemanticTokensRange _ -> not_supported ()
  | LinkedEditingRange _ -> not_supported ()
  | WillCreateFiles _ -> not_supported ()
  | WillRenameFiles _ -> not_supported ()
  | WillDeleteFiles _ -> not_supported ()

let on_notification server (notification : Client_notification.t) :
    State.t Fiber.t =
  let state : State.t = Server.state server in
  let store = state.store in
  match notification with
  | TextDocumentDidOpen params ->
    let* doc =
      let position_encoding = State.position_encoding state in
      Document.make
        ~position_encoding
        (State.wheel state)
        params
    in
    let* () = Document_store.open_document store doc in
    let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
    state
  | TextDocumentDidClose { textDocument = { uri } } ->
    let+ () =
      let* () = Document_store.close_document store uri in
      task_if_running state.detached ~f:(fun () ->
          Diagnostics.send (State.diagnostics state))
    in
    state
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } ->
    let doc =
      Document_store.change_document store uri ~f:(fun prev_doc ->
          Document.update_text ~version prev_doc contentChanges)
    in
    let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
    state
  | CancelRequest _ -> Fiber.return state
  | ChangeConfiguration req ->
    let+ configuration = Configuration.update state.configuration req in
    { state with configuration }
  | DidSaveTextDocument { textDocument = { uri }; _ } -> (
    let state = Server.state server in
    match Document_store.get_opt state.store uri with
    | None ->
      ( Log.log ~section:"on receive DidSaveTextDocument" @@ fun () ->
        Log.msg "saved document is not in the store" [] );
      Fiber.return state
    | Some doc ->
      let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
      state)
  | ChangeWorkspaceFolders change ->
    let state =
      State.modify_workspaces state ~f:(fun ws ->
          Workspaces.on_change ws change)
    in
    Fiber.return state
  | DidChangeWatchedFiles _
  | DidCreateFiles _
  | DidDeleteFiles _
  | DidRenameFiles _
  | WillSaveTextDocument _
  | Initialized
  | WorkDoneProgressCancel _
  | WorkDoneProgress _
  | Exit -> Fiber.return state
  | SetTrace { value } -> Fiber.return { state with trace = value }
  | UnknownNotification req ->
    let+ () =
      State.log_msg
        server
        ~type_:Error
        ~message:("Unknown notication " ^ req.method_)
    in
    state

let start stream =
  let detached = Fiber.Pool.create () in
  let server = Fdecl.create Dyn.opaque in
  let store = Document_store.make server detached in
  let handler =
    let on_request = { Server.Handler.on_request } in
    Server.Handler.make ~on_request ~on_notification ()
  in
  let* configuration = Configuration.default () in
  let wheel = Configuration.wheel configuration in
  let server =
    let symbols_thread = Lazy_fiber.create Lev_fiber.Thread.create in
    Fdecl.set
      server
      (Server.make
        handler
        stream
        (State.create
          ~store
          ~detached
          ~configuration
          ~wheel
          ~symbols_thread));
    Fdecl.get server
  in
  let with_log_errors what f =
    let+ (_ : (unit, unit) result) =
      Fiber.map_reduce_errors
        (module Monoid.Unit)
        f
        ~on_error:(fun exn ->
          Format.eprintf "%s: %a@." what Exn_with_backtrace.pp_uncaught exn;
          Fiber.return ())
    in
    ()
  in
  let run () =
    Fiber.all_concurrently_unit
      [ with_log_errors "detached" (fun () -> Fiber.Pool.run detached)
        ; Lev_fiber.Timer.Wheel.run wheel
        ; (let* () = Server.start server in
          let finalize =
            [ Document_store.close_all store
              ; Fiber.Pool.stop detached
              ; Lev_fiber.Timer.Wheel.stop wheel
              ; Fiber.of_thunk (fun () ->
                Fiber.return ())
            ]
          in
          let finalize =
            match (Server.state server).init with
            | Uninitialized -> finalize
            | Initialized _init -> finalize
          in
          Fiber.all_concurrently_unit finalize)
      ]
  in
  let metrics = Metrics.create () in
  Metrics.with_metrics metrics run

let socket sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let fd =
    Lev_fiber.Fd.create
      (Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0)
      (`Non_blocking false)
  in
  let* () = Lev_fiber.Socket.connect fd sockaddr in
  Lev_fiber.Io.create_rw fd

let stream_of_channel : Lsp.Cli.Channel.t -> _ = function
  | Stdio ->
    let* stdin = Lev_fiber.Io.stdin in
    let+ stdout = Lev_fiber.Io.stdout in
    (stdin, stdout)
  | Pipe path ->
    if Sys.win32 then (
      Format.eprintf "windows pipes are not supported";
      exit 1)
    else
      let sockaddr = Unix.ADDR_UNIX path in
      socket sockaddr
  | Socket port ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    socket sockaddr

let run channel () =
  Lev_fiber.run ~sigpipe:`Ignore (fun () ->
      let* input, output = stream_of_channel channel in
      start (Lsp_fiber.Fiber_io.make input output))
  |> Lev_fiber.Error.ok_exn
