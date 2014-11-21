module Avl.AvlList

type Tree<'T> =
    | Leaf
    | Node of 'T * int * Tree<'T> * Tree<'T>

    member this.Length =
        match this with
        | Leaf -> 0
        | Node (_, _, left, right) -> left.Length + right.Length + 1

    member this.Height =
        match this with
        | Leaf -> -1
        | Node (_, h, _, _) -> h

    member this.Value =
        match this with
        | Leaf -> failwith "Invalid operation"
        | Node (x, _, _, _) -> x

    member this.Left =
        match this with
        | Leaf -> failwith "Invalid operation"
        | Node (_, _, left, _) -> left

    member this.Right =
        match this with
        | Leaf -> failwith "Invalid operation"
        | Node (_, _, _, right) -> right

    member this.Add x =

        let balance node =
            match node with
            | Leaf -> 0
            | Node (x, h, left, right) -> right.Height - left.Height

        let rotateRight = function
            | Leaf -> Leaf
            | Node (x, _, left, right) ->
                let newRight = Node (x, 0, left.Right, right)
                Node (left.Value, 0, left.Left, newRight)

        let rotateLeft = function
            | Leaf -> Leaf
            | Node (x, _, left, right) ->
                let newLeft = Node (x, 0, left, right.Left)
                let h = 1 + max newLeft.Height right.Right.Height
                Node (right.Value, h, newLeft, right.Right)

        let balanceNode node =
            match balance node with
            | -1 | 0 | 1 -> node
            | x when x > 1 ->
                match node with
                | Leaf -> failwith "Should not happen"
                | Node (x, _, left, right) when balance right >= 0 -> rotateLeft node
                | _ -> node
            | _ -> rotateRight node

        let rec add l =
            match l with
            | Leaf -> Node (x, 0, Leaf, Leaf)
            | Node (x, _, left, right) ->
                let right = add right
                let h = 1 + max left.Height right.Height
                Node (x, h, left, right)
        add this |> balanceNode

    member this.AddRange collection =
        let mutable result = this
        for item in collection do
            result <- result.Add item
        result

let empty = Leaf

let rec length (tree: Tree<_>) =
    tree.Length

let height (tree: Tree<_>) =
    tree.Height

let create seq =
    let a = Array.ofSeq seq
    let rec init low high =
        if low > high then Leaf else
        let mid = low + (high - low) / 2
        let left = init low (mid - 1)
        let right = init (mid + 1) high
        Node (a.[mid], 1 + max left.Height right.Height, left, right)
    init 0 (a.Length - 1)

let rec asSeq tree =
    seq {
        match tree with
        | Leaf -> ()
        | Node (x, _, left, right) ->
            yield! asSeq left
            yield x
            yield! asSeq right
    }

let asArray tree =
    tree |> asSeq |> Array.ofSeq

let add x (l: Tree<_>) =
    l.Add(x)

let addRange collection tree =
    let mutable result = tree
    for item in collection do
        result <- result |> add item
    result
