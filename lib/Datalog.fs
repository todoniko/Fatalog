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

module Make =
    let literal (p: Predicate) (terms: Term list) : Literal = (p, terms)
    let clause (head: Literal) (body: Literal list) : Clause = { Head = head; Body = body }
