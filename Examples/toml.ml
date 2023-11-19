type key = string
type value = string
type property = key * value
type section = { title : string; properties : property list }
type root = Property of property | Section of section

let print_root (v : root) =
  match v with
  | Property (k, v) -> print_endline (k ^ " = " ^ v)
  | Section { title; properties } ->
      print_endline ("[" ^ title ^ "]");
      List.iter (fun (k, v) -> print_endline (k ^ " = " ^ v)) properties

let toml_parser : root Morsec.parser =
  failwith "TODO: initialize Morsec not IMPLEMENTED"

let read_file (file_path : string) : string =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

(* let () = *)
(*   let result = "test.toml" |> read_file in *)
(*   print_endline result *)

let () =
  let result =
    "./test.toml" |> read_file |> Morsec.transform |> toml_parser.parse
  in
  match result with
  | Ok (_, v) -> v |> print_root
  | Error _ -> print_endline "ERROR"
