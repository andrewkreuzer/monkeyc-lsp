open Core
let r file = In_channel.read_all file

(* class ['string] input_stream file = *)
(*   object (self) *)
(*     val mutable input = String.to_list (r file) *)
(*     val pos = ref 0 *)
(*     val line = ref 1 *)
(*     val col = ref 0 *)
(*     method next = *)
(*       match input with *)
(*       | [] -> raise End_of_file *)
(*       | h :: t -> *)
(*         (1* I don't knwo why '=' doesn't work here *1) *)
(*         if phys_equal h '\n' then line := !line + 1 *)
(*         else col := !col + 1; *)
(*         input <- t; h *)
(*     method peek = *)
(*       List.hd input *)
(*     method eof = *)
(*       self#peek |> Option.is_none *)
(*     method size = *)
(*       List.length input *)
(*   end;; *)

type inputStream =
  {
    file: string;
    input : char list;
    pos : int;
    line : int;
    col : int;
    current: string;
  }

module InputStream : sig
  val make : string -> inputStream
  val next : inputStream -> inputStream * char
  val peek : inputStream -> char option
  val eof : inputStream -> bool
  val size : inputStream -> int
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

  let eof t =
    peek t |> Option.is_none

  let size t =
    List.length t.input
end
