namespace Fatalog.Interpreter

open Fatalog.DSL

module S =
    type T = (Term * Term) list

    let empty: T = []

    let inline private find term substitution =
        List.tryFind (fst >> ((=) term)) substitution

    let inline substitute literal (substitution: T) : Literal =
        let tryfix term =
            match find term substitution with
            | None -> term
            | Some (_, sym) -> sym in

        let replace term =
            match term with
            | Sym _ as sym -> sym
            | v -> tryfix v in

        Literal.map replace literal

    let private zip (l1: Literal) (l2: Literal) : T option =
        let dedup (var, sym) lst : T option =
            match find var lst with
            | Some (_, s) when s <> sym -> None
            | _ -> Some((var, sym) :: lst) in

        let rec loop substitution : T option =
            match substitution with
            | (_, Var _) :: _ -> failwith $"Invalis substitution {substitution} - Var is not allowed as snd"
            | [] -> Some empty
            | (Sym a, Sym b) :: rest -> if a = b then loop rest else None
            | (var, sym) as term :: rest -> Option.bind (dedup term) (loop rest) in

        match l1, l2 with
        | ((predicate, terms), (predicate', terms')) when predicate = predicate' -> List.zip terms terms' |> loop
        | _ -> None

    let inline expand body facts : T list =
        let eval literal subs : T list =
            subs
            |> List.map (fun s ->
                let ground = substitute literal s in

                List.choose (zip ground) facts
                |> List.map (fun ext -> List.append s ext))
            |> List.concat in

        List.foldBack eval body [ empty ]

module Naive =
    let infer clauses facts : Knowledge =
        let eval clause : Knowledge =
            List.map (S.substitute clause.Head) (S.expand clause.Body facts) in

        Knowledge.infer eval clauses facts

    let solve (prog: Program) =
        let rec loop facts : Knowledge =
            let inferred = infer prog facts in

            if inferred = facts then
                facts
            else
                loop inferred in

        if List.forall Clause.isRangeRestricted prog then
            loop Knowledge.empty
        else
            failwith "This program is not range restricted"
