module WinHeader.FindFunction

open FSharp.Text.Lexing
open WinHeader.C

let run ctagsMap funcName =
  match ExtractFunction.run funcName ctagsMap with
  | None -> None
  | Some funcStr ->
    let lexBuf = LexBuffer<char>.FromString funcStr
    let funcDecl = CParser.funcdecl CLexer.tokenize lexBuf
    Some funcDecl
