open! Import
open Fiber.O

module Kind = struct
  type t =
    | Intf
    | Impl

  let of_fname p =
    match Filename.extension p with
    | ".mc" -> Impl
    (* | ".mli" | ".eliomi" | ".rei" -> Intf *)
    | ext ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InvalidRequest
           ~message:"unsupported file extension"
           ~data:(`Assoc [ ("extension", `String ext) ])
           ())
end

module Syntax = struct
  type t =
    | Monkeyc
    | Manifest
    | Jungle

  let human_name = function
    | Monkeyc -> "Monkey C"
    | Manifest -> "Manifest"
    | Jungle -> "Jungle"

  let all = [ ("monkey-c", Monkeyc) ]

  let of_fname =
    let of_fname_res = function
      | "manifest" -> Ok Manifest
      | s -> (
        match Filename.extension s with
        | ".mc" -> Ok Monkeyc
        | ".jungle" -> Ok Jungle
        | ext -> Error ext)
    in
    fun s ->
    match of_fname_res s with
    | Ok x -> x
    | Error ext ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
          ~code:InvalidRequest
          ~message:(Printf.sprintf "unsupported file extension")
          ~data:(`Assoc [ ("extension", `String ext) ])
          ())

  let to_language_id x =
    List.find_map all ~f:(fun (k, v) -> Option.some_if (v = x) k)
    |> Option.value_exn

  let of_text_document (td : Text_document.t) =
    match List.assoc all (Text_document.languageId td) with
    | Some s -> s
    | None -> Text_document.documentUri td |> Uri.to_path |> of_fname
end

let await task =
  let* cancel_token = Server.cancel_token () in
  let f () = Lev_fiber.Thread.await task in
  let without_cancellation res =
    match res with
    | Ok s -> Ok s
    | Error (`Exn exn) -> Error exn
    | Error `Cancelled ->
      let exn = Code_error.E (Code_error.create "unexpected cancellation" []) in
      let backtrace = Printexc.get_callstack 10 in
      Error { Exn_with_backtrace.exn; backtrace }
  in
  match cancel_token with
  | None -> f () |> Fiber.map ~f:without_cancellation
  | Some t -> (
    let+ res, outcome =
      Fiber.Cancel.with_handler t f ~on_cancel:(fun () ->
          Lev_fiber.Thread.cancel task)
    in
    match outcome with
    | Not_cancelled -> without_cancellation res
    | Cancelled () ->
      let e =
        Jsonrpc.Response.Error.make
          ~code:RequestCancelled
          ~message:"cancelled"
          ()
      in
      raise (Jsonrpc.Response.Error.E e))

type t =
  | Other of
  { tdoc : Text_document.t
    ; syntax : Syntax.t
  }

let tdoc = function
  | Other d -> d.tdoc

let uri t = Text_document.documentUri (tdoc t)

let syntax = function
  | Other t -> t.syntax

let text t = Text_document.text (tdoc t)

let version t = Text_document.version (tdoc t)

let make _wheel (doc : DidOpenTextDocumentParams.t)
    ~position_encoding =
  Fiber.of_thunk (fun () ->
      let tdoc = Text_document.make ~position_encoding doc in
      let syntax = Syntax.of_text_document tdoc in
      match syntax with
      | Monkeyc | Manifest | Jungle -> Fiber.return (Other { tdoc; syntax }))

let update_text ?version t changes =
  match Text_document.apply_content_changes ?version (tdoc t) changes with
  | exception Text_document.Invalid_utf error ->
    Log.log ~section:"warning" (fun () ->
        let error =
          match error with
          | Malformed input ->
            [ ("message", `String "malformed input"); ("input", `String input) ]
          | Insufficient_input -> [ ("message", `String "insufficient input") ]
        in
        Log.msg
          "dropping update due to invalid utf8"
          (( "changes"
           , Json.yojson_of_list
               TextDocumentContentChangeEvent.yojson_of_t
               changes )
          :: error));
    t
  | tdoc -> (
    match t with
    | Other o -> Other { o with tdoc })

let close t =
  match t with
  | Other _ -> Fiber.return ()
