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

module Term =
    let inline isVar term =
        match term with
        | Var _ -> true
        | _ -> false

module Literal =
    let make (p: Predicate) (terms: Term list) : Literal = (p, terms)

    let inline vars ((_, terms): Literal) =
        terms |> List.filter Term.isVar |> List.distinct

    let inline ground (f: Term -> Term) ((predicate, terms): Literal) : Literal =
        (predicate,
         List.map
             (function
             | Sym _ as sym -> sym
             | v -> f v)
             terms)

    let inline tryMatch f l1 l2 =
        match l1, l2 with
        | ((predicate, terms), (predicate', terms')) when predicate = predicate' -> f terms terms'
        | _ -> None

module Clause =
    let inline private isRangeRestricted { Head = head; Body = body } =
        let bodyVars = List.map Literal.vars body |> List.concat in

        Literal.vars head
        |> List.forall (fun headVar -> List.contains headVar bodyVars)

    let make (head: Literal) (body: Literal list) : Clause =
        let candidate = { Head = head; Body = body } in

        if isRangeRestricted candidate then
            candidate
        else
            failwith $"Variables in {head} do not appear in {body}"

    let inline head { Head = head; Body = _ } : Literal = head
    let inline body { Head = _; Body = body } : Literal list = body

module Program =
    let inline eval (f: Literal -> list<Literal> -> Knowledge) (p: Program) : Knowledge =
        List.map (fun x -> f (Clause.head x) (Clause.body x)) p
        |> List.concat

module Knowledge =
    let empty: Knowledge = []

    let inline merge (known: Knowledge) (inferred: Knowledge) : Knowledge =
        inferred |> List.append known |> List.distinct
