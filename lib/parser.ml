open Tokenizer

type markdownData =
  | Paragraph of string
  | Header of int * string
  | LineBreak

let rec parse = function
  | HeaderToken level :: TextToken t :: rest -> Header (level, t) :: parse rest
  (* TODO: Deal with empty headers *)
  | TextToken t :: rest -> Paragraph t :: parse rest
  | NewlineToken :: rest -> LineBreak :: parse rest
  | [] -> []
  | _ -> failwith "Error: Cannot Parse"
