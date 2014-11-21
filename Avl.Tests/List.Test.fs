module Avl.List.Test

open Avl
open NUnit.Framework
open Swensen.Unquote

[<Test>]
let ``Given a list, create gives the correct result``() =
    let expected = [| 1..10 |]
    test <@ seq { 1..10 } |> AvlList.create |> AvlList.asArray = expected @>

[<Test>]
let ``Given a list, length gives the correct result``() =
    test <@ seq { 1..10 } |> AvlList.create |> AvlList.length = 10 @>

[<Test>]
let ``Given a list, add gives the correct result``() =
    let expected = [| 1..11 |]
    test <@ seq { 1..10 } |> AvlList.create |> AvlList.add 11 |> AvlList.asArray = expected @>

[<Test>]
let ``Given a list, addRange gives the correct result``() =
    let expected = [| 1..20 |]
    test <@ AvlList.empty |> AvlList.addRange (seq { 1..10 }) |> AvlList.addRange (seq { 11..20 }) |> AvlList.asArray = expected @>

[<Test>]
let ``Given a list, height gives the correct result``() =
    test <@ AvlList.empty |> AvlList.height = -1 @>
    test <@ seq { 1..1 } |> AvlList.create |> AvlList.height = 0 @>
    test <@ seq { 1..3 } |> AvlList.create |> AvlList.height = 1 @>
    test <@ seq { 1..4 } |> AvlList.create |> AvlList.height = 2 @>
    test <@ seq { 1..7 } |> AvlList.create |> AvlList.height = 2 @>
    test <@ seq { 1..8 } |> AvlList.create |> AvlList.height = 3 @>

let rec isBalanced = function
    | AvlList.Tree.Leaf -> true
    | AvlList.Tree.Node (_, _, left, right) ->
        if left.Height - right.Height |> abs > 1 then false
        else
            isBalanced left && isBalanced right

[<Test>]
let ``Given a list, verify that create returns a balanced list``() =
    test <@ AvlList.empty |> isBalanced @>
    test <@ seq { 1..1 } |> AvlList.create |> isBalanced @>
    test <@ seq { 1..3 } |> AvlList.create |> isBalanced @>
    test <@ seq { 1..4 } |> AvlList.create |> isBalanced @>
    test <@ seq { 1..7 } |> AvlList.create |> isBalanced @>
    test <@ seq { 1..8 } |> AvlList.create |> isBalanced @>

[<Test>]
let ``Given a list, verify that add returns a list with the correct height``() =
    test <@ AvlList.empty |> AvlList.add 1 |> AvlList.height = 0 @>
    test <@ AvlList.empty |> AvlList.add 1 |> AvlList.add 2 |> AvlList.height = 1 @>
    test <@ AvlList.empty |> AvlList.add 1 |> AvlList.add 2 |> AvlList.add 3 |> AvlList.height = 1 @>

[<Test>]
let ``Given a list, verify that add returns a balanced list``() =
    test <@ AvlList.empty |> AvlList.add 1 |> isBalanced @>
    test <@ AvlList.empty |> AvlList.add 1 |> AvlList.add 2 |> isBalanced @>
    test <@ AvlList.empty |> AvlList.add 1 |> AvlList.add 2 |> AvlList.add 3 |> isBalanced @>

