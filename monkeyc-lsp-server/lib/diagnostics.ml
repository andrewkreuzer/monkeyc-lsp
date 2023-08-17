open Import
open Fiber.O

let ocamllsp_source = "ocamllsp"

let dune_source = "dune"

module Uri = struct
  include Uri

  let compare x y = Ordering.of_int (Uri.compare x y)
end

module Uri_c = Comparable.Make (Uri)
module Uri_set = Uri_c.Set

type t =
  { send : PublishDiagnosticsParams.t list -> unit Fiber.t
  ; mutable dirty_uris : Uri_set.t
  ; related_information : bool
  ; tags : DiagnosticTag.t list
  }

let create (capabilities : PublishDiagnosticsClientCapabilities.t option) send =
  let related_information, tags =
    match capabilities with
    | None -> (false, [])
    | Some c -> (
      ( Option.value ~default:false c.relatedInformation
      , match c.tagSupport with
        | None -> []
        | Some { valueSet } -> valueSet ))
  in
  { dirty_uris = Uri_set.empty
  ; send
  ; related_information
  ; tags
  }

let send =
  fun _ ->
    Fiber.of_thunk (fun () ->
        let+ () = Fiber.return () in
        ())
