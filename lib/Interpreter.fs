namespace Fatalog.Interpreter

open Fatalog.DSL

module Naive =
    let expand body kb : Env list =
        let gen literal s =
            List.choose (Env.make (Env.substitute literal s)) kb
            |> List.map (fun e -> List.append s e)

        let eval literal subs =
            List.concat <| List.map (gen literal) subs in

        List.foldBack eval body [ Env.empty ]

    let apply head : (Env list -> Knowledge) = List.map (Env.substitute head)

    let infer program kb : Knowledge =
        program
        |> Program.eval (fun head body -> expand body kb |> apply head)
        |> Knowledge.merge kb

    let solve program : Knowledge =
        let rec loop kb =
            let inferred = infer program kb in

            if inferred = kb then
                kb
            else
                loop inferred in

        loop Knowledge.empty
