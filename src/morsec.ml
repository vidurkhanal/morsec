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

let while_parse (predicate : char -> bool) : string parser =
  {
    parse =
      (fun input ->
        let n = String.length input.text in
        let i = ref 0 in
        while String.get input.text !i |> predicate && !i < n do
          incr i
        done;
        Ok (morsec_str_sub !i (n - !i) input, String.sub input.text 0 !i));
  }

let prefix (prefix' : string) : string parser =
  {
    parse =
      (fun input ->
        try
          let prefix_len = String.length prefix' in
          let input_len = String.length input.text in
          let prefix_input = input |> morsec_str_sub 0 prefix_len in
          if String.equal prefix_input.text prefix' then
            let rest =
              input |> morsec_str_sub prefix_len (input_len - prefix_len)
            in
            Ok (rest, prefix')
          else
            Error
              {
                position = input.position;
                message = Printf.sprintf "Expected %S" prefix';
              }
        with Invalid_argument _ ->
          Error
            {
              position = input.position;
              message = Printf.sprintf "Expected %S" prefix';
            });
  }

let ( *> ) (p : 'a parser) (q : 'b parser) : 'b parser =
  {
    parse =
      (fun input ->
        match p.parse input with
        | Ok (input', _) -> q.parse input'
        | Error e -> Error e);
  }

let ( <* ) (p : 'a parser) (q : 'b parser) : 'a parser =
  {
    parse =
      (fun input ->
        match p.parse input with
        | Ok (input', x) ->
            q.parse input' |> Result.map (fun (input, _) -> (input, x))
        | Error e -> Error e);
  }

let ( <*> ) (p : 'a parser) (q : 'b parser) : ('a * 'b) parser =
  {
    parse =
      (fun input ->
        input |> p.parse
        |> Result.map (fun (input', x) ->
               input' |> q.parse
               |> Result.map (fun (input, y) -> (input, (x, y))))
        |> Result.join);
  }

let ( <|> ) (p : 'a parser) (q : 'a parser) : 'a parser =
  {
    parse =
      (fun input ->
        match input |> p.parse with
        | Ok (input', x) -> Ok (input', x)
        | Error e ->
            input |> q.parse
            |> Result.map_error (fun e' ->
                   {
                     position = e.position;
                     message =
                       Printf.sprintf "Something went wrong. Either %s or %s"
                         e.message e'.message;
                   }));
  }

let optional (_p : 'a parser) : 'a option parser =
  failwith "TODO: OPTIONAL PARSER YET TO BE IMPLEMENTED."
