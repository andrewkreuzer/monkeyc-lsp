open Import

type mode =
  | Default
  | Extended_fixed of int
  | Extended_variable

let format_contents ~syntax ~markdown ~typ ~doc =
  `MarkupContent
    (if markdown then
     let value =
       let markdown_name = Document.Syntax.human_name syntax in
       match doc with
       | None -> sprintf "```%s\n%s\n```" markdown_name typ
       | Some d ->
         let doc = sprintf "(** %s *)" d
         in
         sprintf "```%s\n%s\n```\n---\n%s" markdown_name typ doc
     in
     { MarkupContent.value; kind = MarkupKind.Markdown }
    else
      let value =
        match doc with
        | None -> sprintf "%s" typ
        | Some d -> sprintf "%s\n%s" typ d
      in
      { MarkupContent.value; kind = MarkupKind.PlainText })

let handle server { HoverParams.textDocument = { uri }; position; _ } _mode =
  Fiber.of_thunk (fun () ->

    let _ = uri in
    let _ = position in

    let state : State.t = Server.state server in
    let syntax = Document.Syntax.Monkeyc in
    let contents =
      let markdown =
        let client_capabilities = State.client_capabilities state in
        ClientCapabilities.markdown_support
          client_capabilities
          ~field:(fun td ->
            Option.map td.hover ~f:(fun h -> h.contentFormat))
      in
      format_contents ~syntax ~markdown ~typ:"Test" ~doc:(Some ("a test hover"))
    in
    Fiber.return (Some (Hover.create ~contents ())))
