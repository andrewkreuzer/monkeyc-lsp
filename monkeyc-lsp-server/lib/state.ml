open Import

type init =
  | Uninitialized
  | Initialized of
      { params : InitializeParams.t
      ; workspaces : Workspaces.t
      ; monkeyc : Monkeyc.t
      ; diagnostics : Diagnostics.t
      ; exp_client_caps : Client.Experimental_capabilities.t
      ; position_encoding : [ `UTF16 | `UTF8 ]
      }

type hover_extended = { mutable history : (Uri.t * Position.t * int) option }

type t =
  { store : Document_store.t
  ; init : init
  ; configuration : Configuration.t
  ; detached : Fiber.Pool.t
  ; trace : TraceValue.t
  ; symbols_thread : Lev_fiber.Thread.t Lazy_fiber.t
  ; wheel : Lev_fiber.Timer.Wheel.t
  ; hover_extended : hover_extended
  }

let create ~store  ~detached ~configuration ~symbols_thread ~wheel =
  { init = Uninitialized
  ; store
  ; configuration
  ; detached
  ; trace = Off
  ; symbols_thread
  ; wheel
  ; hover_extended = { history = None }
  }

let wheel t = t.wheel

let initialize_params (state : t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized init -> init.params

let workspaces (state : t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized init -> init.workspaces

let workspace_root t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> (
    match init.params.rootUri with
    | None -> assert false
    | Some uri -> uri)

let position_encoding t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.position_encoding

let diagnostics t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.diagnostics

let initialize t ~position_encoding (params : InitializeParams.t) workspaces
  monkeyc diagnostics =
  assert (t.init = Uninitialized);
  { t with
    init =
      Initialized
        { params
          ; workspaces
          ; diagnostics
          ; monkeyc
          ; position_encoding
          ; exp_client_caps =
            Client.Experimental_capabilities.of_opt_json
              params.capabilities.experimental
        }
  }

let modify_workspaces t ~f =
  let init =
    match t.init with
    | Uninitialized -> assert false
    | Initialized init ->
      Initialized { init with workspaces = f init.workspaces }
  in
  { t with init }

let client_capabilities t = (initialize_params t).capabilities

let experimental_client_capabilities t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized { exp_client_caps; _ } -> exp_client_caps

let log_msg server ~type_ ~message =
  let state = Server.state server in
  task_if_running state.detached ~f:(fun () ->
      let log = LogMessageParams.create ~type_ ~message in
      Server.notification server (Server_notification.LogMessage log))
