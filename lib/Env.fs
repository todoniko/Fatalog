namespace Fatalog.Interpreter

open Fatalog.DSL

type Env = (Term * Term) list

module Env =
    let empty: Env = []

    let inline find term env : (Term * Term) option = List.tryFind (fst >> ((=) term)) env

    let inline substitute literal env : Literal =
        Literal.ground
            (fun v ->
                match find v env with
                | None -> v
                | Some (_, sym) -> sym)
            literal

    let inline make unknown fact : Env option =
        let dedup (var, sym) lst =
            match find var lst with
            | Some (_, s) when s <> sym -> None
            | _ -> Some((var, sym) :: lst) in

        let rec loop env =
            match env with
            | (_, Var _) :: _ -> failwith $"Invalis substitution {env} - Var is not allowed as snd"
            | [] -> Some empty
            | (Sym a, Sym b) :: rest -> if a = b then loop rest else None
            | (var, sym) as term :: rest -> Option.bind (dedup term) (loop rest) in

        Literal.tryMatch (fun t1 t2 -> List.zip t1 t2 |> loop) unknown fact
