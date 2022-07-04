namespace Fatalog.Test

open Fatalog.DSL
open Fatalog.Interpreter
open Xunit

[<AutoOpen>]
module Helpers =
    let edge = Literal.make "edge"

module TransitiveClosure =
    let path = Literal.make "path"

    let graph =
        [ edge [ Sym "a"; Sym "b" ]
          edge [ Sym "b"; Sym "c" ]
          edge [ Sym "c"; Sym "d" ] ]
        |> List.map (fun lit -> Clause.make lit [])

    let result_kb =
        [ ("edge", [ Sym "a"; Sym "b" ])
          ("edge", [ Sym "b"; Sym "c" ])
          ("edge", [ Sym "c"; Sym "d" ])
          ("path", [ Sym "a"; Sym "b" ])
          ("path", [ Sym "b"; Sym "c" ])
          ("path", [ Sym "c"; Sym "d" ])
          ("path", [ Sym "a"; Sym "c" ])
          ("path", [ Sym "b"; Sym "d" ])
          ("path", [ Sym "a"; Sym "d" ]) ]

    let ``edge is path`` =
        let x, y = (Var "x", Var "y") in Clause.make (path [ x; y ]) [ edge [ x; y ] ]


    let non_linear =
        let x, y, z = (Var "x", Var "y", Var "z") in

        [ ``edge is path``
          Clause.make (path [ x; z ]) [ edge [ x; y ]; path [ y; z ] ] ]

    [<Fact>]
    let ``transitive closure of a--b--c--d left linear computation`` () =
        let left_linear =
            let x, y, z = (Var "x", Var "y", Var "z") in

            [ ``edge is path``
              Clause.make (path [ x; z ]) [ edge [ x; y ]; path [ y; z ] ] ] in

        Assert.Equal<Knowledge>(Naive.solve (left_linear @ graph), result_kb)

    [<Fact>]
    let ``transitive closure of a--b--c--d right linear computation`` () =
        let right_linear =
            let x, y, z = (Var "x", Var "y", Var "z") in

            [ ``edge is path``
              Clause.make (path [ x; z ]) [ path [ x; y ]; edge [ y; z ] ] ] in

        Assert.Equal<Knowledge>(Naive.solve (right_linear @ graph), result_kb)

    [<Fact>]
    let ``transitive closure of a--b--c--d non linear computation`` () =
        let non_linear =
            let x, y, z = (Var "x", Var "y", Var "z") in

            [ ``edge is path``
              Clause.make (path [ x; z ]) [ path [ x; y ]; path [ y; z ] ] ] in

        Assert.Equal<Knowledge>(Naive.solve (non_linear @ graph), result_kb)

    [<Fact>]
    let ``even or odd path in a--b--c--d`` () =
        let mutial_recursion =
            let x, y, z = (Var "x", Var "y", Var "z") in
            let odd = Literal.make "odd" in
            let even = Literal.make "even" in
            let edge = Literal.make "edge" in

            [ Clause.make (odd [ x; y ]) [ edge [ x; y ] ]
              Clause.make (even [ x; z ]) [ edge [ x; y ]; odd [ y; z ] ]
              Clause.make (odd [ x; z ]) [ edge [ x; y ]; even [ y; z ] ] ] in

        Assert.Equal<Knowledge>(
            Naive.solve (mutial_recursion @ graph),
            [ ("edge", [ Sym "a"; Sym "b" ])
              ("edge", [ Sym "b"; Sym "c" ])
              ("edge", [ Sym "c"; Sym "d" ])
              ("odd", [ Sym "a"; Sym "b" ])
              ("odd", [ Sym "b"; Sym "c" ])
              ("odd", [ Sym "c"; Sym "d" ])
              ("even", [ Sym "a"; Sym "c" ])
              ("even", [ Sym "b"; Sym "d" ])
              ("odd", [ Sym "a"; Sym "d" ]) ]
        )

module Clique =
    let tri_clique = Literal.make "tri_clique"
    let quad_clique = Literal.make "quad_clique"

    let seven_node =
        [ edge [ Sym "a"; Sym "b" ]
          edge [ Sym "a"; Sym "g" ]
          edge [ Sym "a"; Sym "e" ]
          edge [ Sym "a"; Sym "c" ]
          edge [ Sym "b"; Sym "g" ]
          edge [ Sym "b"; Sym "f" ]
          edge [ Sym "b"; Sym "d" ]
          edge [ Sym "g"; Sym "e" ]
          edge [ Sym "g"; Sym "f" ]
          edge [ Sym "e"; Sym "f" ]
          edge [ Sym "e"; Sym "d" ]
          edge [ Sym "e"; Sym "c" ]
          edge [ Sym "f"; Sym "c" ]
          edge [ Sym "f"; Sym "d" ]
          edge [ Sym "c"; Sym "d" ] ]
        |> List.map (fun lit -> Clause.make lit [])

    let rules =
        let w, x, y, z = (Var "w", Var "x", Var "y", Var "z") in

        [ Clause.make
              (quad_clique [ w; x; y; z ])
              [ edge [ w; x ]
                edge [ w; y ]
                edge [ w; z ]
                edge [ x; y ]
                edge [ x; z ]
                edge [ y; z ] ]
          Clause.make
              (tri_clique [ x; y; z ])
              [ edge [ x; y ]
                edge [ x; z ]
                edge [ y; z ] ] ]

    let direction_invariant =
        let x, y = (Var "x", Var "y") in [ Clause.make (edge [ x; y ]) [ edge [ y; x ] ] ]

    [<Fact>]
    let ``7 node graph with 10 3cliques and 1 4clique `` () =
        Assert.Equal<Knowledge>(
            Naive.solve (rules @ seven_node),
            [ ("edge", [ Sym "a"; Sym "b" ])
              ("edge", [ Sym "a"; Sym "g" ])
              ("edge", [ Sym "a"; Sym "e" ])
              ("edge", [ Sym "a"; Sym "c" ])
              ("edge", [ Sym "b"; Sym "g" ])
              ("edge", [ Sym "b"; Sym "f" ])
              ("edge", [ Sym "b"; Sym "d" ])
              ("edge", [ Sym "g"; Sym "e" ])
              ("edge", [ Sym "g"; Sym "f" ])
              ("edge", [ Sym "e"; Sym "f" ])
              ("edge", [ Sym "e"; Sym "d" ])
              ("edge", [ Sym "e"; Sym "c" ])
              ("edge", [ Sym "f"; Sym "c" ])
              ("edge", [ Sym "f"; Sym "d" ])
              ("edge", [ Sym "c"; Sym "d" ])
              ("quad_clique", [ Sym "e"; Sym "f"; Sym "c"; Sym "d" ])
              ("tri_clique", [ Sym "a"; Sym "b"; Sym "g" ])
              ("tri_clique", [ Sym "a"; Sym "g"; Sym "e" ])
              ("tri_clique", [ Sym "b"; Sym "g"; Sym "f" ])
              ("tri_clique", [ Sym "g"; Sym "e"; Sym "f" ])
              ("tri_clique", [ Sym "a"; Sym "e"; Sym "c" ])
              ("tri_clique", [ Sym "e"; Sym "f"; Sym "c" ])
              ("tri_clique", [ Sym "b"; Sym "f"; Sym "d" ])
              ("tri_clique", [ Sym "e"; Sym "f"; Sym "d" ])
              ("tri_clique", [ Sym "e"; Sym "c"; Sym "d" ])
              ("tri_clique", [ Sym "f"; Sym "c"; Sym "d" ]) ]
        )

    [<Fact>]
    let ``7 node graph with invariant direction of edges has 60 3cliques`` () =
        let result =
            Naive.solve (direction_invariant @ rules @ seven_node)
            |> List.filter (function
                | ("tri_clique", _) -> true
                | _ -> false) in

        Assert.Equal(List.length result, 60)

    [<Fact>]
    let ``7 node graph with invariant direction of edges has 24 4cliques`` () =
        let result =
            Naive.solve (direction_invariant @ rules @ seven_node)
            |> List.filter (function
                | ("quad_clique", _) -> true
                | _ -> false) in

        Assert.Equal(List.length result, 24)
