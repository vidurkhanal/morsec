type morsec_str = { text : string; position : int }
type error = { message : string; position : int }

let transform text = { text; position = 0 }

type 'a parser = { parse : morsec_str -> (morsec_str * 'a, error) result }
