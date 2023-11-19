type morsec_str = { text : string; position : int }
type error = { message : string; position : int }

let transform text = { text; position = 0 }

let morsec_str_sub start len (s : morsec_str) : morsec_str =
  { text = String.sub s.text start len; position = s.position + start }

type 'a parser = { parse : morsec_str -> (morsec_str * 'a, error) result }

let wrap x = { parse = (fun input -> Ok (input, x)) }

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  {
    parse =
      (fun input ->
        match p.parse input with
        | Ok (input', x) -> Ok (input', f x)
        | Error err -> Error err);
  }

let bind (f : 'a -> 'b parser) (p : 'a parser) : 'b parser =
  {
    parse =
      (fun input ->
        match p.parse input with
        | Ok (input', x) -> (f x).parse input'
        | Error error -> Error error);
  }

let prefix (prefix' : string) : string parser =
  {
    parse =
      (fun input ->
        let n = String.length prefix' in
        let m = String.length input.text in
        let prefix = input |> morsec_str_sub 0 n in
        if prefix.text == prefix' then
          let rest = input |> morsec_str_sub n (m - n) in
          Ok (rest, prefix')
        else
          Error
            {
              position = input.position;
              message = Printf.sprintf "Expected %S" prefix';
            });
  }
