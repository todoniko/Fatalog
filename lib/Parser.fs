namespace Fatalog

open FParsec

module Parser =
    (* Terminal symbols *)
    let private comma = spaces >>. pchar ',' .>> spaces
    let private horn = spaces >>. pstring ":-" .>> spaces
    let private quote = pchar '\"'
    let private lbracket = pchar '('
    let private rbracket = pchar ')'
    let private clausend = pchar '.' <|> pchar '?'

    (*  Entities *)
    let private identifier = spaces >>. many1Chars asciiLetter .>> spaces

    (*  DSL types *)
    let private var = identifier |>> DSL.Term.Var

    let private sym =
        quote >>. manyCharsTill anyChar quote
        |>> DSL.Term.Sym

    // p(t1,...,tn)
    let private literal =
        let terms = between lbracket rbracket (sepBy (sym <|> var) comma)
        pipe2 identifier terms DSL.Make.literal

    // l0 :- l1,...,ln
    let private clause =
        let body = horn >>. (sepBy literal comma)
        pipe2 literal (body <|>% []) DSL.Make.clause

    // c1,...,cn
    let program: Parser<_, unit> = sepEndBy1 (clause .>> clausend) spaces
