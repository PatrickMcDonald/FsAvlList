#load "List.fs"
open Avl

let ``1 .. 7`` = AvlList.create [| 1 .. 7 |]

let describe l =
    l
    |> AvlList.asSeq
    |> Seq.iter (printf "%d ")

    printfn ""
    printfn "length: %d" <| AvlList.length l
    printfn ""

AvlList.create [| 1 .. 3 |] |> describe
AvlList.create [| 1 .. 7 |] |> describe

AvlList.create [| 1 .. 3 |]
|> AvlList.add 4
|> describe

AvlList.create [| 1 .. 7 |]
|> AvlList.add 8
|> describe


AvlList.empty |> AvlList.insert 0 3 |> AvlList.insert 0 2 |> AvlList.insert 0 1
