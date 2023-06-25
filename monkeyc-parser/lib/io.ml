open Core
let r file = In_channel.read_all file

type t =
  { file: string
    ; input : char list
    ; pos : int
    ; line : int
    ; col : int
    ; mutable current: string
  }

module Input_stream : sig
  val make : string -> t
  val next : t -> t * char
  val peek : t -> char option
  val double_peek : t -> char option
  val eof : t -> bool
  val size : t -> int
end = struct
  let make file =
    {
      file = file;
      input = String.to_list (r file);
      pos = 0;
      line = 1;
      col = 0;
      current = "";
    }

  let next t =
    match t.input with
    | [] -> raise End_of_file
    | h :: tail ->
      match h with
      | '\n' -> { t with line = t.line + 1; col = 0; input = tail}, h
      | _ -> { t with col = t.col + 1; input = tail}, h

  let peek t =
    List.hd t.input

  let double_peek t =
    match List.nth_exn t.input 1 with
    | c -> Some c
    | exception _ -> None

  let eof t =
    peek t |> Option.is_none

  let size t =
    List.length t.input
end
