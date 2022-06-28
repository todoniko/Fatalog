namespace Fatalog.DSL

(*
    P ∈ Program = c1,...,cn
    c ∈ Clause  = l0 <- l1,...,ln
    l ∈ Literal = p(t1,...,tn)
    t ∈ Term    = k|x

    Where
    p = finite set of predicate symbols.
    k = finite set of variable symbols.
    x = finite set of constants.
*)

type Term =
    | Var of string
    | Sym of string

type Predicate = string
type Literal = Predicate * Term list
type Clause = { Head: Literal; Body: Literal list }
type Program = Clause list
type Knowledge = Literal list

module Literal =
    let make (p: Predicate) (terms: Term list) : Literal = (p, terms)

    let inline extractVars ((_, terms): Literal) =
        terms
        |> List.filter (function
            | Var _ -> true
            | _ -> false)
        |> List.distinct

    let inline map (f: Term -> Term) ((predicate, terms): Literal) : Literal = (predicate, List.map f terms)

module Clause =
    let inline isRangeRestricted { Head = head; Body = body } =
        let bodyVars = List.map Literal.extractVars body |> List.concat in

        Literal.extractVars head
        |> List.forall (fun headVar -> List.contains headVar bodyVars)

    let make (head: Literal) (body: Literal list) : Clause =
        let candidate = { Head = head; Body = body } in

        if isRangeRestricted candidate then
            candidate
        else
            failwith $"Variables in {head} do not appear in {body}"

module Knowledge =
    let empty: Knowledge = []

    let inline infer (eval: Clause -> Knowledge) clauses facts : Knowledge =
        List.map eval clauses
        |> List.concat
        |> List.append facts
        |> List.distinct
