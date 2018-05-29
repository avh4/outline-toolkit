module OutlineToolkit.Tree exposing (Path, Tree, empty, findOrCreate, fold, indent, indexedFold, insert, set)

import Array.Hamt as Array exposing (Array)


type Tree a
    = Tree a (Array (Tree a))


type Path
    = Path (List Int)


empty : Array (Tree a)
empty =
    Array.empty


set : List Int -> a -> Array (Tree a) -> Array (Tree a)
set path newValue =
    update (\_ children -> Tree newValue children) (Path path)


setNode : List Int -> Tree a -> Array (Tree a) -> Array (Tree a)
setNode path newNode =
    update (\_ _ -> newNode) (Path path)


update : (Maybe a -> Array (Tree a) -> Tree a) -> Path -> Array (Tree a) -> Array (Tree a)
update f (Path path) trees =
    case path of
        [] ->
            trees

        [ i ] ->
            case Array.get i trees of
                Nothing ->
                    createOrSet i (f Nothing Array.empty) trees

                Just (Tree a children) ->
                    Array.set i (f (Just a) children) trees

        next :: rest ->
            case Array.get next trees of
                Nothing ->
                    -- The path doesn't exist, but we're not a the leaf node,
                    -- so we don't have an `a` value for the missing parent,
                    -- so we can't create it.
                    -- Just return unmodified
                    trees

                Just (Tree a children) ->
                    Array.set next (Tree a (update f (Path rest) children)) trees


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


findOrCreate : List Int -> a -> Array (Tree a) -> Array (Tree a)
findOrCreate path defaultValue =
    let
        f a children =
            Tree (a |> Maybe.withDefault defaultValue) children
    in
    update f (Path path)


remove : List Int -> Array (Tree a) -> Array (Tree a)
remove path trees =
    case path of
        [] ->
            -- can't remove the implied root node
            trees

        [ i ] ->
            Array.append
                (Array.slice 0 i trees)
                (Array.slice (i + 1) (Array.length trees) trees)

        next :: rest ->
            case Array.get next trees of
                Nothing ->
                    -- can't remove a path that doesn't exist
                    trees

                Just (Tree a children) ->
                    Array.set next (Tree a (remove rest children)) trees


indent : List Int -> Array (Tree a) -> Array (Tree a)
indent path trees =
    case path of
        [] ->
            -- can't indent the root node
            trees

        [ 0 ] ->
            -- can't indent the first child
            trees

        [ i ] ->
            case Array.get i trees of
                Nothing ->
                    -- can't indent a path that doesn't exist
                    trees

                Just movedNode ->
                    -- remove child `i` and add it as the last grandchild of child `i-1`
                    trees
                        |> remove path
                        |> setNode
                            [ i - 1
                            , Array.get (i - 1) trees
                                |> Maybe.map (\(Tree _ grandchildren) -> Array.length grandchildren)
                                |> Maybe.withDefault 0
                            ]
                            movedNode

        next :: rest ->
            case Array.get next trees of
                Nothing ->
                    -- can't indent a path that doesn't exist
                    trees

                Just (Tree a children) ->
                    Array.set next (Tree a (indent rest children)) trees


{-| Inserts the new value after the given node, shifting all siblings down
-}
insert : List Int -> a -> Array (Tree a) -> Array (Tree a)
insert path newValue trees =
    case path of
        [] ->
            -- can't insert a new root node
            trees

        [ i ] ->
            let
                n =
                    Array.length trees
            in
            Array.append
                (Array.slice 0 (i + 1) trees |> Array.push (Tree newValue Array.empty))
                (Array.slice (i + 1) n trees)

        next :: rest ->
            case Array.get next trees of
                Nothing ->
                    -- can't insert below a path that doesn't exist
                    trees

                Just (Tree a children) ->
                    Array.set next (Tree a (insert rest newValue children)) trees


fold : (a -> List b -> b) -> Tree a -> b
fold f tree =
    indexedFold (always f) tree


indexedFold : (List Int -> a -> List b -> b) -> Tree a -> b
indexedFold f tree =
    fold_ f [] tree


fold_ : (List Int -> a -> List b -> b) -> List Int -> Tree a -> b
fold_ f path (Tree init trees) =
    if Array.isEmpty trees then
        f (List.reverse path) init []

    else
        f
            (List.reverse path)
            init
            (List.indexedMap
                (\i -> fold_ f (i :: path))
                (Array.toList trees)
            )
