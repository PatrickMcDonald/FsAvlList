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

    static member private Balance tree =
        match tree with
        | Leaf -> 0
        | Node (x, h, left, right) -> right.Height - left.Height

    static member internal createNode x (left: Tree<_>) (right: Tree<_>) =
        Node (x, 1 + max left.Height right.Height, left, right)

    member private this.RotateRight() =
        match this with
        | Leaf -> Leaf
        | Node (x, _, left, right) ->
            let newRight = Tree<_>.createNode x left.Right right
            Tree<_>.createNode left.Value left.Left newRight

    member private this.RotateLeft() =
        match this with
        | Leaf -> Leaf
        | Node (x, _, left, right) ->
            let newLeft = Tree<_>.createNode x left right.Left
            Tree<_>.createNode right.Value newLeft right.Right

    static member private RotateDoubleRight tree =
        match tree with
        | Leaf -> Leaf
        | Node (x, _, left, right) ->
            (Tree<_>.createNode x (left.RotateLeft()) right).RotateRight()

    static member private RotateDoubleLeft tree =
        match tree with
        | Leaf -> Leaf
        | Node (x, _, left, right) ->
            (Tree<_>.createNode x left (right.RotateRight())).RotateLeft()

    static member private BalanceNode(tree) =
        match Tree<_>.Balance tree with
        | -1 | 0 | 1 -> tree
        | x when x > 1 ->
            match tree with
            | Leaf -> failwith "Should not happen"
            | Node (x, _, left, right) when Tree<_>.Balance right < 0 -> tree |> Tree<_>.RotateDoubleLeft
            | _ -> tree.RotateLeft()
        | x when x < -1 ->
            match tree with
            | Leaf -> failwith "Should not happen"
            | Node (x, _, left, right) when Tree<_>.Balance left > 0 -> tree |> Tree<_>.RotateDoubleRight
            | _ -> tree.RotateRight()
        | _ -> failwith "Should not happen"

    member this.Add(x) =
        let rec add l =
            match l with
            | Leaf -> Tree<_>.createNode x Leaf Leaf
            | Node (x, _, left, right) ->
                let right = add right
                Tree<_>.createNode x left right
        add this |> Tree<_>.BalanceNode

    member this.Insert(i, x) =
        let rec insert i node x =
            match node with
            | Leaf ->
                if i = 0 then
                    Tree<_>.createNode x Leaf Leaf
                else
                    failwith "Index out of bounds"
            | Node (y, _, left, right) ->
                if i <= left.Length then
                    let newLeft = insert i left x
                    Tree<_>.createNode y newLeft right |> Tree<_>.BalanceNode
                else
                    let newRight = insert (i - left.Length - 1) right x
                    let h = 1 + max left.Height newRight.Height
                    Tree<_>.createNode y left newRight |> Tree<_>.BalanceNode
        insert i this x

    member this.AddRange(collection) =
        let mutable result = this
        for item in collection do
            result <- result.Add item
        result

    member this.InsertRange(i, collection) =
        let mutable i' = i
        let mutable result = this
        for item in collection do
            result <- result.Insert(i', item)
            i' <- i' + 1
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
        Tree<_>.createNode a.[mid] left right
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

let addRange collection (l: Tree<_>) =
    l.AddRange collection

let insert i x (l: Tree<_>) =
    l.Insert(i, x)

let insertRange i collection (l: Tree<_>) =
    l.InsertRange(i, collection)
