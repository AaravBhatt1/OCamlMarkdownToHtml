type token =
  | Newline
  | Header of int
  | Text of string

let countHeaderVal =
  let rec countHeaderValHelper count = function
    | '#' :: rest -> countHeaderValHelper (count + 1) rest
    | _ -> count
  in countHeaderValHelper 1

let rec combineText = function
  | Text a :: Text b :: rest -> combineText (Text (a ^ b) :: rest)
  | x :: xn -> x :: combineText xn
  | [] -> []

let rec drop n lst =
  match n, lst with
  | 0, _ -> lst  (* If n is 0, return the list as is *)
  | _, [] -> []  (* If the list is empty, return an empty list *)
  | n, _ :: t -> drop (n - 1) t

let tokenizer mdText =
  (*
    I am converting the string to a list.
    Writing code with lists is nicer syntaxtically, but lists are eager not lazy like Sequences.
  *)
  let listText = mdText |> String.to_seq |> List.of_seq
  in let rec tokenizerHelper ?(newline = false)= function
    | [] -> []
    (* TODO: Fix detecting header vs # in text - header should only be possible at start of a line*)
    | '#' :: rest when newline ->
        let level = countHeaderVal rest
        in Header level :: tokenizerHelper (drop (level - 1) rest)
    | '\n' :: rest -> Newline :: tokenizerHelper rest ~newline:true
    | char :: rest -> Text (Char.escaped char) :: tokenizerHelper rest
  in tokenizerHelper listText ~newline:true |> combineText
