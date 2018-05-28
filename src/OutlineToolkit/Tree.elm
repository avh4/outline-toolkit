module OutlineToolkit.Tree exposing (Path, Tree, empty, fold, indexedFold, set, setAt)

import Array exposing (Array)


type Tree a
    = Tree a (Array (Tree a))


type Path
    = Path (List Int)


empty : Array (Tree a)
empty =
    Array.empty


setAt : List Int -> a -> Array (Tree a) -> Array (Tree a)
setAt path =
    set (Path path)


set : Path -> a -> Array (Tree a) -> Array (Tree a)
set (Path path) newValue trees =
    case path of
        [] ->
            trees

        [ i ] ->
            createOrSet i (Tree newValue empty) trees

        next :: rest ->
            case Array.get next trees of
                Nothing ->
                    -- The path doesn't exist, but we're not a the leaf node,
                    -- so we don't have an `a` value for the missing parent,
                    -- so we can't create it.
                    -- Just return unmodified
                    trees

                Just (Tree a children) ->
                    createOrSet next (Tree a (set (Path rest) newValue children)) trees


createOrSet : Int -> a -> Array a -> Array a
createOrSet path newValue array =
    let
        neededItems =
            (path + 1) - Array.length array
    in
    if neededItems <= 0 then
        Array.set path newValue array

    else
        Array.append array (Array.repeat neededItems newValue)


fold : (a -> List b -> b) -> Tree a -> b
fold f tree =
    indexedFold (always f) tree


indexedFold : (List Int -> a -> List b -> b) -> Tree a -> b
indexedFold f tree =
    fold_ f [] tree


fold_ : (List Int -> a -> List b -> b) -> List Int -> Tree a -> b
fold_ f path (Tree init trees) =
    if Array.isEmpty trees then
        f path init []

    else
        f
            path
            init
            (List.indexedMap
                (\i -> fold_ f (i :: path))
                (Array.toList trees)
            )
