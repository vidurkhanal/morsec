type morsec_str = { text : string; position : int }
type error = { message : string; position : int }

let transform text = { text; position = 0 }

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
