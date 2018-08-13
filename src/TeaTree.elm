module TeaTree
    exposing
        ( Tree
        , Zipper
        , Path
          -- Tree operations
        , singleton
        , zipper
        , toTree
        , map
          -- Zipper operations
        , goToChild
        , goToRightMostChild
        , goUp
        , goLeft
        , goRight
        , goToRoot
        , goToNext
        , goToPrevious
        , goTo
        , updateFocusDatum
        , datum
        , depth
        , insertChild
        , appendChild
          --, updateChildren
        , getPath
          -- Path operations
        , goToPath
        , updateDatum
          -- Sorting
        , sortBy
        )

{-| Todo:
It will be a multiway Tree implementation, not a binary tree.

Will save this for an optimized version:

    type alias NodeArray a =
        Array Int a

Need to add API for simpler read-only walking of the tree. Zippers will churn
the heap, but a read only pass for rendering the view does not need them so
can be made more efficient.


## Types

@docs Tree, Zipper, Path


## Tree operations

@docs singleton, zipper, map


## Zipper operations

@docs goToChild, goToRightMostChild, goUp, goLeft, goRight, goToRoot, goToNext, goToPrevious, goTo, updateFocusDatum, datum, insertChild, appendChild, getPath


## Path operations

@docs goToPath, updateDatum


## Sorting

@docs sortBy

-}


type alias Id =
    Int


{-| -}
type Tree a
    = Tree
        { nextId : Id
        , innerTree : InnerTree a
        }


{-| -}
type Zipper a
    = Zipper
        { nextId : Id
        , currentPath : Path
        , innerTree : InnerTree a
        , crumbs : Breadcrumbs a
        , depth : Int
        }


{-| -}
type Path
    = Path Id (List Int)


type InnerTree a
    = InnerTree
        { id : Id
        , datum : a
        , children : Forest a
        }


type alias Forest a =
    List (InnerTree a)


type alias Context a =
    { id : Id
    , datum : a
    , before : Forest a
    , after : Forest a
    }


type alias Breadcrumbs a =
    List (Context a)



-- Id operations


getNextId : Id -> Id
getNextId id =
    id + 1



-- Tree operations


{-| -}
singleton : a -> Zipper a
singleton datum =
    Tree
        { nextId = 1
        , innerTree =
            InnerTree
                { id = 0
                , datum = datum
                , children = []
                }
        }
        |> zipper


{-| -}
zipper : Tree a -> Zipper a
zipper (Tree tree) =
    Zipper
        { nextId = tree.nextId
        , currentPath =
            case tree.innerTree of
                InnerTree inner ->
                    Path inner.id []
        , innerTree = tree.innerTree
        , crumbs = []
        , depth = 0
        }


{-| -}
toTree : Zipper a -> Tree a
toTree (Zipper zipper) =
    Tree
        { nextId = zipper.nextId
        , innerTree = zipper.innerTree
        }


mapInner : (a -> b) -> InnerTree a -> InnerTree b
mapInner fn (InnerTree tree) =
    let
        mappedDatum =
            fn tree.datum

        mappedChildren =
            List.map (\child -> mapInner fn child) tree.children
    in
        (InnerTree
            { id = tree.id
            , datum = mappedDatum
            , children = mappedChildren
            }
        )


{-| -}
map : (a -> b) -> Tree a -> Tree b
map fn (Tree tree) =
    let
        mappedInner =
            mapInner fn tree.innerTree
    in
        (Tree
            { nextId = tree.nextId
            , innerTree = mappedInner
            }
        )


mkNode datum id =
    InnerTree
        { id = id
        , datum = datum
        , children = []
        }


insertNodeToTree : a -> Id -> InnerTree a -> InnerTree a
insertNodeToTree childDatum childId (InnerTree { id, datum, children }) =
    InnerTree { id = id, datum = datum, children = (mkNode childDatum childId) :: children }


appendNodeToTree : a -> Id -> InnerTree a -> InnerTree a
appendNodeToTree childDatum childId (InnerTree { id, datum, children }) =
    InnerTree { id = id, datum = datum, children = children ++ [ (mkNode childDatum childId) ] }


{-| This operation may be faster than `map` when the type of the tree does not change.
It should be preferred to `map` in that case.
-}
update : (a -> a) -> Tree a -> Tree a
update fn tree =
    map fn tree



-- Zipper operations


splitOnIndex : Int -> List (InnerTree a) -> Maybe ( Forest a, InnerTree a, Forest a )
splitOnIndex n xs =
    let
        before =
            List.take n xs

        focus =
            List.drop n xs |> List.head

        after =
            List.drop (n + 1) xs

        -- The above seems inneficient unless the compiler is very smart,
        -- better to write our own loop to iterate the list just once.
    in
        case focus of
            Nothing ->
                Nothing

            Just f ->
                Just ( before, f, after )


{-| Walking the zipper context back to the root will produce a Tree with any
updates made as the zipper was walked over the tree, folded back in to the
new Tree.
-}
goToRoot : Zipper a -> Zipper a
goToRoot (Zipper zipper) =
    case zipper.crumbs of
        [] ->
            Zipper zipper

        otherwise ->
            goUp (Zipper zipper)
                |> Maybe.map goToRoot
                |> Maybe.withDefault (Zipper zipper)


{-| -}
goToChild : Int -> Zipper a -> Maybe (Zipper a)
goToChild n (Zipper zipper) =
    let
        (InnerTree inner) =
            zipper.innerTree

        maybeSplit =
            splitOnIndex n inner.children
    in
        case maybeSplit of
            Nothing ->
                Nothing

            Just ( before, focus, after ) ->
                let
                    (InnerTree innerFocus) =
                        focus
                in
                    Just
                        (Zipper
                            { nextId = zipper.nextId
                            , currentPath =
                                case zipper.currentPath of
                                    Path _ ps ->
                                        Path innerFocus.id (n :: ps)
                            , innerTree = focus
                            , crumbs =
                                { id = inner.id
                                , datum = inner.datum
                                , before = before
                                , after = after
                                }
                                    :: zipper.crumbs
                            , depth = zipper.depth + 1
                            }
                        )


{-| -}
goUp : Zipper a -> Maybe (Zipper a)
goUp (Zipper zipper) =
    case zipper.crumbs of
        { id, datum, before, after } :: bs ->
            Just
                (Zipper
                    { nextId = zipper.nextId
                    , currentPath =
                        case zipper.currentPath of
                            Path _ (_ :: ps) ->
                                Path id ps

                            _ ->
                                -- This branch should never happen.
                                Path -1 []
                    , innerTree =
                        InnerTree
                            { id = id
                            , datum = datum
                            , children = (before ++ [ zipper.innerTree ] ++ after)
                            }
                    , crumbs = bs
                    , depth = zipper.depth - 1
                    }
                )

        [] ->
            Nothing


{-| -}
goLeft : Zipper a -> Maybe (Zipper a)
goLeft (Zipper zipper) =
    case zipper.crumbs of
        { id, datum, before, after } :: bs ->
            case List.reverse before of
                [] ->
                    Nothing

                (InnerTree inner) :: rest ->
                    Just
                        (Zipper
                            { nextId = zipper.nextId
                            , currentPath =
                                case zipper.currentPath of
                                    Path _ (p :: ps) ->
                                        Path inner.id (p - 1 :: ps)

                                    _ ->
                                        -- This branch should never happen.
                                        Path -1 []
                            , innerTree = InnerTree inner
                            , crumbs =
                                { id = id
                                , datum = datum
                                , before = List.reverse rest
                                , after = zipper.innerTree :: after
                                }
                                    :: bs
                            , depth = zipper.depth
                            }
                        )

        [] ->
            Nothing


{-| -}
goRight : Zipper a -> Maybe (Zipper a)
goRight (Zipper zipper) =
    case zipper.crumbs of
        { id, datum, before, after } :: bs ->
            case after of
                [] ->
                    Nothing

                (InnerTree inner) :: rest ->
                    Just
                        (Zipper
                            { nextId = zipper.nextId
                            , currentPath =
                                case zipper.currentPath of
                                    Path _ (p :: ps) ->
                                        Path inner.id (p + 1 :: ps)

                                    _ ->
                                        -- This branch should never happen.
                                        Path -1 []
                            , innerTree = InnerTree inner
                            , crumbs =
                                { id = id
                                , datum = datum
                                , before = before ++ [ zipper.innerTree ]
                                , after = rest
                                }
                                    :: bs
                            , depth = zipper.depth
                            }
                        )

        [] ->
            Nothing


{-| -}
goToNext : Zipper a -> Maybe (Zipper a)
goToNext zipper =
    let
        upAndOver zipper =
            case goUp zipper of
                Nothing ->
                    Nothing

                Just zipper_ ->
                    case goRight zipper_ of
                        Nothing ->
                            upAndOver zipper_

                        zipper__ ->
                            zipper__
    in
        case goToChild 0 zipper of
            Just zipper_ ->
                Just zipper_

            Nothing ->
                case goRight zipper of
                    Just zipper_ ->
                        Just zipper_

                    Nothing ->
                        case upAndOver zipper of
                            Nothing ->
                                Nothing

                            zipper_ ->
                                zipper_


{-| -}
goToPrevious : Zipper a -> Maybe (Zipper a)
goToPrevious zipper =
    let
        recurseDownAndRight zipper_ =
            case goToRightMostChild zipper_ of
                Just zipper__ ->
                    recurseDownAndRight zipper__

                Nothing ->
                    Just zipper_
    in
        case goLeft zipper of
            Just zipper_ ->
                recurseDownAndRight zipper_

            Nothing ->
                goUp zipper


{-| -}
goToRightMostChild : Zipper a -> Maybe (Zipper a)
goToRightMostChild (Zipper zipper) =
    let
        (InnerTree inner) =
            zipper.innerTree
    in
        goToChild ((List.length inner.children) - 1) (Zipper zipper)


{-| -}
goTo : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
goTo predicate zipper =
    let
        goToElementOrNext zipper_ =
            if predicate (datum zipper) then
                Just zipper_
            else
                goToNext zipper_ |> Maybe.andThen goToElementOrNext
    in
        (goToRoot zipper) |> goToElementOrNext


{-| -}
datum : Zipper a -> a
datum (Zipper zipper) =
    let
        (InnerTree inner) =
            zipper.innerTree
    in
        inner.datum


{-| -}
depth : Zipper a -> Int
depth (Zipper zipper) =
    zipper.depth


{-| -}
updateFocusDatum : (a -> a) -> Zipper a -> Zipper a
updateFocusDatum fn (Zipper zipper) =
    let
        (InnerTree inner) =
            zipper.innerTree
    in
        Zipper
            { zipper
                | innerTree = InnerTree { inner | datum = (fn inner.datum) }
            }


{-| -}
insertChild : a -> Zipper a -> Zipper a
insertChild child (Zipper { nextId, currentPath, innerTree, crumbs, depth }) =
    let
        steppedId =
            nextId + 1
    in
        Zipper
            { nextId = steppedId
            , currentPath = currentPath
            , innerTree = insertNodeToTree child nextId innerTree
            , crumbs = crumbs
            , depth = depth
            }


{-| -}
appendChild : a -> Zipper a -> Zipper a
appendChild child (Zipper { nextId, currentPath, innerTree, crumbs, depth }) =
    let
        steppedId =
            nextId + 1
    in
        Zipper
            { nextId = steppedId
            , currentPath = currentPath
            , innerTree = appendNodeToTree child nextId innerTree
            , crumbs = crumbs
            , depth = depth
            }


{-| -}
getPath : Zipper a -> Path
getPath (Zipper zipper) =
    zipper.currentPath



-- Path operations


{-| The Path and Tree can be recombined to recover a previous position in the tree.
walkPath : Path -> Tree a -> Maybe (Zipper a)

Every node will be marked with a unique id, so that re-walking the tree from a Path
can be confirmed as correct. Walking a Path will produce a Maybe.

This allows events to be tagged with Paths which describe a return to a
previously visited position within a tree, without capturing any other data
associated with that node. This is to circumvent the stale data issue when
a user is interacting with a tree.

-}
goToPath : Path -> Tree a -> Maybe (Zipper a)
goToPath path tree =
    let
        (Path _ steps) =
            path

        walk : List Int -> Maybe (Zipper a) -> Maybe (Zipper a)
        walk steps zipper =
            case ( steps, zipper ) of
                ( [], Just zipper_ ) ->
                    zipper

                ( n :: ns, Just zipper_ ) ->
                    let
                        maybeChildZipper =
                            goToChild n zipper_
                    in
                        case maybeChildZipper of
                            Nothing ->
                                Nothing

                            Just zipper__ ->
                                walk ns (Just zipper__)

                ( _, Nothing ) ->
                    Nothing
    in
        walk steps (zipper tree |> Just)


{-| The contents of nodes in the tree will be held in an `Array Id a`. Ids will be assigned
sequentially. This will allow mapping by id without re-walking a Path possible. It will
only be necessary to re-walk paths when adding new nodes into the tree, as this is the only
situation when fresh ids will need to be generated.
-}
updateDatum : Path -> (a -> a) -> Tree a -> Tree a
updateDatum path fn tree =
    goToPath path tree
        |> Maybe.map (updateFocusDatum fn)
        |> Maybe.map goToRoot
        |> Maybe.map toTree
        -- Silent fail if the path is wrong.
        |> Maybe.withDefault tree


{-| -}
sortBy : (a -> comparable) -> Tree a -> Tree a
sortBy sortFn (Tree tree) =
    let
        innerSortBy sortFn (InnerTree innerTree) =
            InnerTree
                { id = innerTree.id
                , datum = innerTree.datum
                , children =
                    List.map (innerSortBy sortFn) innerTree.children
                        |> List.sortBy (\(InnerTree node) -> sortFn node.datum)
                }
    in
        Tree
            { nextId = tree.nextId
            , innerTree = innerSortBy sortFn tree.innerTree
            }



-- type InnerTree a
--     = InnerTree
--         { id : Id
--         , datum : a
--         , children : Forest a
--         }
--
-- type alias Forest a =
--    List (InnerTree a)
