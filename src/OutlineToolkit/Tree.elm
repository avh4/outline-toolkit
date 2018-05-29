module OutlineToolkit.Tree
    exposing
        ( Path
        , Tree
        , children
        , empty
        , findOrCreate
        , fold
        , get
        , indexedFold
        , insert
        , insertNode
        , remove
        , set
        , value
        )

import Array.Hamt as Array exposing (Array)


type Tree a
    = Tree a (Array (Tree a))


type Path
    = Path (List Int)



-- CREATING


empty : Array (Tree a)
empty =
    Array.empty



-- INSPECTING


value : Tree a -> a
value (Tree a _) =
    a


children : Tree a -> Array (Tree a)
children (Tree _ children) =
    children


get : List Int -> Array (Tree a) -> Maybe (Tree a)
get path trees =
    case path of
        [] ->
            -- There is no node value for the root
            Nothing

        [ i ] ->
            Array.get i trees

        next :: rest ->
            Array.get next trees
                |> Maybe.andThen ((\(Tree a children) -> children) >> get rest)



-- MODIFYING


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


{-| Inserts the new value after the given node, shifting all siblings down
-}
insert : List Int -> a -> Array (Tree a) -> Array (Tree a)
insert path newValue =
    insertNode path (Tree newValue Array.empty)


{-| Inserts the new node after the given node, shifting all siblings down
-}
insertNode : List Int -> Tree a -> Array (Tree a) -> Array (Tree a)
insertNode path newNode trees =
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
                (Array.slice 0 (i + 1) trees |> Array.push newNode)
                (Array.slice (i + 1) n trees)

        next :: rest ->
            case Array.get next trees of
                Nothing ->
                    -- can't insert below a path that doesn't exist
                    trees

                Just (Tree a children) ->
                    Array.set next (Tree a (insertNode rest newNode children)) trees


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
