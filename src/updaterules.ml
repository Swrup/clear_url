open Lwt.Syntax

let url = "https://rules2.clearurls.xyz/data.minify.json"

let program =
  let* _, body = Cohttp_lwt_unix.Client.get (url |> Uri.of_string) in
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt_io.write_line Lwt_io.stdout body

let () = Lwt_main.run program
