type token =
  | NewlineToken
  | HeaderToken of int
  | TextToken of string

let countHeaderVal =
  let rec countHeaderValHelper count = function
    | '#' :: rest -> countHeaderValHelper (count + 1) rest
    | _ -> count
  in countHeaderValHelper 1

let rec combineText = function
  | TextToken a :: TextToken b :: rest -> combineText (TextToken (a ^ b) :: rest)
  | x :: xn -> x :: combineText xn
  | [] -> []

let rec drop n lst =
  match n, lst with
  | 0, _ -> lst  (* If n is 0, return the list as is *)
  | _, [] -> []  (* If the list is empty, return an empty list *)
  | n, _ :: t -> drop (n - 1) t

let tokenize mdText =
  (*
    I am converting the string to a list.
    Writing code with lists is nicer syntaxtically, but lists are eager not lazy like Sequences.
  *)
  let listText = mdText |> String.to_seq |> List.of_seq
  in let rec tokenizerHelper ?(newline = false)= function
    | [] -> []
    | '#' :: rest when newline ->
        let level = countHeaderVal rest
        in HeaderToken level :: tokenizerHelper (drop (level - 1) rest)
    | '\n' :: rest -> NewlineToken :: tokenizerHelper rest ~newline:true
    | char :: rest -> TextToken (Char.escaped char) :: tokenizerHelper rest
  in tokenizerHelper listText ~newline:true |> combineText
