open OCamlHtmlToMarkdown.Tokenizer

(* TODO: Create a proper test suite *)

let () =
  let output = tokenizer "#Header\nnewline starts # here\n##"
    in let expectedOutput = [Header 1; Text "Header" ; Newline; Text "newline starts # here"; Newline; Header 2]
    in
      assert (output = expectedOutput);
