open MarkdownToHtml.Tokenizer
open MarkdownToHtml.Parser

(* TODO: Create a proper test suite *)

let () =
  let output1 = tokenize "#Header\nnewline starts # here\n"
  in let expectedOutput1 = [HeaderToken 1; TextToken "Header" ; NewlineToken; TextToken "newline starts # here"; NewlineToken]
  in let output2 = parse output1
  in let expectedOutput2 = [Header (1, "Header"); LineBreak ; Paragraph "newline starts # here" ; LineBreak]
  in
    assert (output1 = expectedOutput1);
    assert (output2 = expectedOutput2);
