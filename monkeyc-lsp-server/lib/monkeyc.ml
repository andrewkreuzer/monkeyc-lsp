open! Import
open Fiber.O

let view_promotion_capability = ("diagnostic_promotions", `Bool true)

type config =
  { diagnostics : Diagnostics.t
  ; document_store : Document_store.t
  ; include_promotions : bool
  ; progress : Progress.t
  ; log : type_:MessageType.t -> message:string -> unit Fiber.t
  }

type active =
  { mutable workspaces : Workspaces.t
  ; config : config
  ; pool : Fiber.Pool.t
  }

let poll active last_error =
  let _ = last_error in
  let workspaces = active.workspaces in
  let _workspace_folders = Workspaces.workspace_folders workspaces in
  let+ () = Fiber.return () in
      `No_error

type state =
  | Closed
  | Active of active

type t = state ref

let create workspaces (client_capabilities : ClientCapabilities.t) diagnostics
    progress document_store ~log =
  let config =
    let include_promotions =
      (let open Option.O in
      let* td = client_capabilities.textDocument in
      let* diagnostics = td.publishDiagnostics in
      diagnostics.dataSupport)
      |> Option.value ~default:false
      &&
      match client_capabilities.experimental with
      | Some (`Assoc xs) -> (
        match List.assoc xs (fst view_promotion_capability) with
        | Some (`Bool b) -> b
        | _ -> false)
      | _ -> false
    in
    { document_store; diagnostics; progress; include_promotions; log }
  in
  ref
    (Active
       { pool = Fiber.Pool.create ()
       ; config
       ; workspaces
       })

let create workspaces (client_capabilities : ClientCapabilities.t) diagnostics
  progress document_store ~log =
  create
    workspaces
    client_capabilities
    diagnostics
    progress
    document_store
    ~log

let run_loop t =
  Fiber.repeat_while ~init:`No_error ~f:(fun state ->
      match !t with
      | Closed -> Fiber.return None
      | Active active ->
        let* state = poll active state in
        (* TODO make this a bit more dynamic. if poll completes fast, wait more,
           if it's slow, then wait less *)
        let+ () = Lev_fiber.Timer.sleepf 0.25 in
        Some state)

let run t : unit Fiber.t =
  Fiber.of_thunk (fun () ->
      match !t with
      | Closed -> Fiber.return ()
      | Active active ->
        Fiber.fork_and_join_unit
          (fun () -> run_loop t)
          (fun () -> Fiber.Pool.run active.pool))

let update_workspaces t workspaces =
  match !t with
  | Closed -> Code_error.raise "dune is already closed" []
  | Active active -> active.workspaces <- workspaces
