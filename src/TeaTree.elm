module TeaTree exposing
    ( Tree, Zipper, Path
    , singleton, zipper, map
    , goToChild, goToRightMostChild, goUp, goLeft, goRight, goToRoot, goToNext, goToPrevious
    , goTo, updateFocusDatum, datum, insertChild, appendChild, getPath, toTree, depth
    , goToPath, updateDatum
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

@docs goToChild, goToRightMostChild, goUp, goLeft, goRight, goToRoot, goToNext, goToPrevious
@docs goTo, updateFocusDatum, datum, insertChild, appendChild, getPath, toTree, depth


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
singleton val =
    Tree
        { nextId = 1
        , innerTree =
            InnerTree
                { id = 0
                , datum = val
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
toTree (Zipper zip) =
    Tree
        { nextId = zip.nextId
        , innerTree = zip.innerTree
        }


mapInner : (a -> b) -> InnerTree a -> InnerTree b
mapInner fn (InnerTree tree) =
    let
        mappedDatum =
            fn tree.datum

        mappedChildren =
            List.map (\child -> mapInner fn child) tree.children
    in
    InnerTree
        { id = tree.id
        , datum = mappedDatum
        , children = mappedChildren
        }


{-| -}
map : (a -> b) -> Tree a -> Tree b
map fn (Tree tree) =
    let
        mappedInner =
            mapInner fn tree.innerTree
    in
    Tree
        { nextId = tree.nextId
        , innerTree = mappedInner
        }


mkNode val id =
    InnerTree
        { id = id
        , datum = val
        , children = []
        }


insertNodeToTree : a -> Id -> InnerTree a -> InnerTree a
insertNodeToTree childDatum childId (InnerTree inner) =
    InnerTree { id = inner.id, datum = inner.datum, children = mkNode childDatum childId :: inner.children }


appendNodeToTree : a -> Id -> InnerTree a -> InnerTree a
appendNodeToTree childDatum childId (InnerTree inner) =
    InnerTree { id = inner.id, datum = inner.datum, children = inner.children ++ [ mkNode childDatum childId ] }


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
goToRoot (Zipper zip) =
    case zip.crumbs of
        [] ->
            Zipper zip

        otherwise ->
            goUp (Zipper zip)
                |> Maybe.map goToRoot
                |> Maybe.withDefault (Zipper zip)


{-| -}
goToChild : Int -> Zipper a -> Maybe (Zipper a)
goToChild n (Zipper zip) =
    let
        (InnerTree inner) =
            zip.innerTree

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
                    { nextId = zip.nextId
                    , currentPath =
                        case zip.currentPath of
                            Path _ ps ->
                                Path innerFocus.id (n :: ps)
                    , innerTree = focus
                    , crumbs =
                        { id = inner.id
                        , datum = inner.datum
                        , before = before
                        , after = after
                        }
                            :: zip.crumbs
                    , depth = zip.depth + 1
                    }
                )


{-| -}
goUp : Zipper a -> Maybe (Zipper a)
goUp (Zipper zip) =
    case zip.crumbs of
        crumb :: bs ->
            Just
                (Zipper
                    { nextId = zip.nextId
                    , currentPath =
                        case zip.currentPath of
                            Path _ (_ :: ps) ->
                                Path crumb.id ps

                            _ ->
                                -- This branch should never happen.
                                Path -1 []
                    , innerTree =
                        InnerTree
                            { id = crumb.id
                            , datum = crumb.datum
                            , children = crumb.before ++ [ zip.innerTree ] ++ crumb.after
                            }
                    , crumbs = bs
                    , depth = zip.depth - 1
                    }
                )

        [] ->
            Nothing


{-| -}
goLeft : Zipper a -> Maybe (Zipper a)
goLeft (Zipper zip) =
    case zip.crumbs of
        crumb :: bs ->
            case List.reverse crumb.before of
                [] ->
                    Nothing

                (InnerTree inner) :: rest ->
                    Just
                        (Zipper
                            { nextId = zip.nextId
                            , currentPath =
                                case zip.currentPath of
                                    Path _ (p :: ps) ->
                                        Path inner.id (p - 1 :: ps)

                                    _ ->
                                        -- This branch should never happen.
                                        Path -1 []
                            , innerTree = InnerTree inner
                            , crumbs =
                                { id = crumb.id
                                , datum = crumb.datum
                                , before = List.reverse rest
                                , after = zip.innerTree :: crumb.after
                                }
                                    :: bs
                            , depth = zip.depth
                            }
                        )

        [] ->
            Nothing


{-| -}
goRight : Zipper a -> Maybe (Zipper a)
goRight (Zipper zip) =
    case zip.crumbs of
        crumb :: bs ->
            case crumb.after of
                [] ->
                    Nothing

                (InnerTree inner) :: rest ->
                    Just
                        (Zipper
                            { nextId = zip.nextId
                            , currentPath =
                                case zip.currentPath of
                                    Path _ (p :: ps) ->
                                        Path inner.id (p + 1 :: ps)

                                    _ ->
                                        -- This branch should never happen.
                                        Path -1 []
                            , innerTree = InnerTree inner
                            , crumbs =
                                { id = crumb.id
                                , datum = crumb.datum
                                , before = crumb.before ++ [ zip.innerTree ]
                                , after = rest
                                }
                                    :: bs
                            , depth = zip.depth
                            }
                        )

        [] ->
            Nothing


{-| -}
goToNext : Zipper a -> Maybe (Zipper a)
goToNext zip =
    let
        upAndOver uoZip =
            case goUp uoZip of
                Nothing ->
                    Nothing

                Just zip_ ->
                    case goRight zip_ of
                        Nothing ->
                            upAndOver zip_

                        zip__ ->
                            zip__
    in
    case goToChild 0 zip of
        Just zip_ ->
            Just zip_

        Nothing ->
            case goRight zip of
                Just zip_ ->
                    Just zip_

                Nothing ->
                    case upAndOver zip of
                        Nothing ->
                            Nothing

                        zip_ ->
                            zip_


{-| -}
goToPrevious : Zipper a -> Maybe (Zipper a)
goToPrevious zip =
    let
        recurseDownAndRight zip_ =
            case goToRightMostChild zip_ of
                Just zip__ ->
                    recurseDownAndRight zip__

                Nothing ->
                    Just zip_
    in
    case goLeft zip of
        Just zip_ ->
            recurseDownAndRight zip_

        Nothing ->
            goUp zip


{-| -}
goToRightMostChild : Zipper a -> Maybe (Zipper a)
goToRightMostChild (Zipper zip) =
    let
        (InnerTree inner) =
            zip.innerTree
    in
    goToChild (List.length inner.children - 1) (Zipper zip)


{-| -}
goTo : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
goTo predicate zip =
    let
        goToElementOrNext zip_ =
            if predicate (datum zip) then
                Just zip_

            else
                goToNext zip_ |> Maybe.andThen goToElementOrNext
    in
    goToRoot zip |> goToElementOrNext


{-| -}
datum : Zipper a -> a
datum (Zipper zip) =
    let
        (InnerTree inner) =
            zip.innerTree
    in
    inner.datum


{-| -}
depth : Zipper a -> Int
depth (Zipper zip) =
    zip.depth


{-| -}
updateFocusDatum : (a -> a) -> Zipper a -> Zipper a
updateFocusDatum fn (Zipper zip) =
    let
        (InnerTree inner) =
            zip.innerTree
    in
    Zipper
        { zip
            | innerTree = InnerTree { inner | datum = fn inner.datum }
        }


{-| -}
insertChild : a -> Zipper a -> Zipper a
insertChild child (Zipper zip) =
    let
        steppedId =
            zip.nextId + 1
    in
    Zipper
        { nextId = steppedId
        , currentPath = zip.currentPath
        , innerTree = insertNodeToTree child zip.nextId zip.innerTree
        , crumbs = zip.crumbs
        , depth = zip.depth
        }


{-| -}
appendChild : a -> Zipper a -> Zipper a
appendChild child (Zipper zip) =
    let
        steppedId =
            zip.nextId + 1
    in
    Zipper
        { nextId = steppedId
        , currentPath = zip.currentPath
        , innerTree = appendNodeToTree child zip.nextId zip.innerTree
        , crumbs = zip.crumbs
        , depth = zip.depth
        }


{-| -}
getPath : Zipper a -> Path
getPath (Zipper zip) =
    let
        (Path id steps) =
            zip.currentPath
    in
    Path id <| List.reverse steps



-- = Path Id (List Int)
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
        (Path _ pathSteps) =
            path

        walk : List Int -> Maybe (Zipper a) -> Maybe (Zipper a)
        walk steps zip =
            case ( steps, zip ) of
                ( [], Just zip_ ) ->
                    zip

                ( n :: ns, Just zip_ ) ->
                    let
                        maybeChildZipper =
                            goToChild n zip_
                    in
                    case maybeChildZipper of
                        Nothing ->
                            Nothing

                        Just zip__ ->
                            walk ns (Just zip__)

                ( _, Nothing ) ->
                    Nothing
    in
    walk pathSteps (zipper tree |> Just)


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
        innerSortBy (InnerTree innerTree) =
            InnerTree
                { id = innerTree.id
                , datum = innerTree.datum
                , children =
                    List.map innerSortBy innerTree.children
                        |> List.sortBy (\(InnerTree node) -> sortFn node.datum)
                }
    in
    Tree
        { nextId = tree.nextId
        , innerTree = innerSortBy tree.innerTree
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
