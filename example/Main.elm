module Main exposing (..)

import AnimationFrame
import Arc2d exposing (Arc2d)
import Color exposing (Color)
import Curve2d exposing (Curve2d)
import Ease
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Html.Lazy
import Http
import LineSegment2d exposing (LineSegment2d)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing ((|:), withDefault)
import Point2d exposing (Point2d)
import Ports.SVGTextPort exposing (textToSVG, textToSVGResponse)
import Task exposing (perform, Task)
import TeaTree exposing (Tree, Zipper)
import TextToSVG exposing (textAsPath, textAsText, TextAlignment(..), TextRenderFunc)
import TypedSvg exposing (svg, g, circle, rect, text_, tspan, line, path)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x, y, x1, y1, x2, y2, rx, ry, width, height, fontSize)
import TypedSvg.Attributes exposing (viewBox, shapeRendering, fill, fillOpacity, preserveAspectRatio, stroke, strokeDasharray, strokeLinecap, strokeLinejoin, fontFamily, textAnchor, textRendering, color, d, transform)
import TypedSvg.Core exposing (svgNamespace, text, Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (px, Align(..), Fill(..), Scale(..), MeetOrSlice(..), ShapeRendering(..), Opacity(..), AnchorAlignment(..), StrokeLinecap(..), StrokeLinejoin(..), TextRendering(..), Transform(..))
import Utils.GridMetrics exposing (Sized, Frame, rectToFrame, middle)
import Vector2d exposing (Vector2d)
import Window


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Model
    = LoadingModel
    | SizingText
    | SizingWindow SizingWindowModel
    | Ready ReadyModel


type alias SizingWindowModel =
    { tree : Tree Wedge
    }


type alias ReadyModel =
    { frame : Frame
    , tree : Tree Wedge
    }


type alias Wedge =
    { label : String
    , size : Float
    , fraction : Float
    , depth : Int
    , startAngle : Float
    , endAngle : Float
    , innerRadius : Float
    , outerRadius : Float
    , color : Color
    }


type Msg
    = LoadResult (Result Http.Error (Tree Wedge))
    | TextToSVGMsg TextToSVG.Msg
    | WindowSize Window.Size
    | HoverElement TeaTree.Path


init : ( Model, Cmd Msg )
init =
    ( LoadingModel
    , Task.attempt LoadResult fetchExample
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize


noop model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( model, action ) of
        ( LoadingModel, LoadResult result ) ->
            case result of
                Err _ ->
                    noop model

                Ok tree ->
                    ( SizingWindow { tree = tree }
                    , Task.perform WindowSize Window.size
                    )

        ( SizingWindow sizingWindowModel, WindowSize windowSize ) ->
            noop (Ready { frame = windowSizeToFrame windowSize, tree = sizingWindowModel.tree })

        ( Ready readyModel, WindowSize windowSize ) ->
            noop (Ready { frame = windowSizeToFrame windowSize, tree = readyModel.tree })

        ( Ready readyModel, HoverElement path ) ->
            noop
                (Ready
                    { readyModel
                        | tree =
                            -- TeaTree.goToPath path readyModel.tree
                            --     |> Maybe.map TeaTree.goToRoot
                            --     |> Maybe.map TeaTree.toTree
                            --     |> Maybe.withDefault readyModel.tree
                            TeaTree.updateDatum (Debug.log "path" path)
                                (\wedge ->
                                    { wedge
                                        | color =
                                            Color.hsl 0.5 1.0 1.0
                                    }
                                )
                                readyModel.tree
                    }
                )

        ( _, _ ) ->
            noop model


windowSizeToFrame : Window.Size -> Frame
windowSizeToFrame size =
    { x = 0.0, y = 0.0, w = toFloat size.width, h = toFloat size.height }
        |> rectToFrame



-- Rendering


black =
    Color.black


white =
    Color.white


offWhite =
    Color.rgb 242 235 238


midGray =
    Color.gray


strongPrintGray =
    Color.rgb 32 32 32


printGray =
    Color.rgb 48 48 48


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.Lazy.lazy fullView model
        ]


fullView : Model -> Html Msg
fullView model =
    case model of
        Ready ready ->
            diagram ready

        _ ->
            Html.div [] []


diagram : { frame : Frame, tree : Tree Wedge } -> Html Msg
diagram diag =
    let
        frame =
            diag.frame
    in
        svg
            [ preserveAspectRatio (Align ScaleMid ScaleMid) Meet
            , viewBox (round frame.x |> toFloat)
                (round frame.y |> toFloat)
                (round frame.w |> toFloat)
                (round frame.h |> toFloat)
            , svgNamespace
            , shapeRendering RenderGeometricPrecision
            ]
            [ background frame, wheel frame diag.tree ]


wheel : Frame -> Tree Wedge -> Svg Msg
wheel frame tree =
    let
        center =
            middle frame |> (\c -> Point2d.fromCoordinates ( c.x, c.y ))

        printFn zipper =
            case TeaTree.goToNext zipper of
                Just stepZipper ->
                    let
                        datum =
                            TeaTree.datum stepZipper
                    in
                        (wedge center (TeaTree.getPath stepZipper) datum)
                            :: printFn stepZipper

                Nothing ->
                    []

        startZipper =
            TeaTree.zipper tree
    in
        g []
            ((wedge center (TeaTree.getPath startZipper) (TeaTree.datum startZipper))
                :: (printFn startZipper)
            )


wedge : Point2d -> TeaTree.Path -> Wedge -> Svg Msg
wedge center path ({ label, size, startAngle, endAngle, innerRadius, outerRadius, color } as wedge) =
    let
        -- _ =
        --     Debug.log "wedge" wedge
        innerArc =
            Arc2d.with
                { centerPoint = center
                , radius = innerRadius
                , startAngle = startAngle
                , sweptAngle = endAngle - startAngle
                }

        outerArc =
            Arc2d.with
                { centerPoint = center
                , radius = outerRadius
                , startAngle = endAngle
                , sweptAngle = startAngle - endAngle
                }

        startLine =
            LineSegment2d.from
                (Point2d.fromPolarCoordinates ( outerRadius, startAngle ))
                (Point2d.fromPolarCoordinates ( innerRadius, startAngle ))
                |> LineSegment2d.translateBy (Vector2d.from Point2d.origin center)

        endLine =
            LineSegment2d.from
                (Point2d.fromPolarCoordinates ( innerRadius, endAngle ))
                (Point2d.fromPolarCoordinates ( outerRadius, endAngle ))
                |> LineSegment2d.translateBy (Vector2d.from Point2d.origin center)
    in
        Curve2d.fromArc outerArc
            |> Curve2d.addLineSegment endLine
            |> Curve2d.addArc innerArc
            |> Curve2d.addLineSegment startLine
            |> Curve2d.curve2d
                [ fill <| Fill color
                , strokeWidth 0.4
                , stroke white
                , TypedSvg.Events.onMouseOver <| HoverElement path
                ]


background : Sized a -> Svg msg
background size =
    let
        skirtScale =
            10
    in
        rect
            [ fill <| Fill offWhite
            , fillOpacity <| Opacity 0.8
            , strokeWidth 0
            , x -(skirtScale * size.w)
            , y -(skirtScale * size.h)
            , width <| (2 * skirtScale + 1) * size.w
            , height <| (2 * skirtScale + 1) * size.h
            ]
            []



-- REST Calls
-- These fetch and deocde the example .json file.


type Flare
    = Flare { name : String, children : List Flare, size : Maybe Int }


fetchExample : Task Http.Error (Tree Wedge)
fetchExample =
    Http.get "/flare.json" treeDecoder
        |> Http.toTask


treeDecoder : Decoder (Tree Wedge)
treeDecoder =
    (Decode.map
        (flareToWedgeTree
            >> (TeaTree.sortBy .size)
            >> initLayoutTree
        )
        flareDecoder
    )


flareDecoder : Decoder Flare
flareDecoder =
    (Decode.succeed
        (\name children size ->
            Flare
                { name = name
                , children = children
                , size = size
                }
        )
    )
        |: Decode.field "name" Decode.string
        |: Decode.map maybeEmptyList (Decode.maybe (Decode.field "children" (Decode.list (Decode.lazy (\_ -> flareDecoder)))))
        |: Decode.maybe (Decode.field "size" Decode.int)


initLayoutTree : Tree Wedge -> Tree Wedge
initLayoutTree tree =
    let
        size =
            TeaTree.zipper tree |> TeaTree.datum |> .size

        popN n l =
            case n of
                0 ->
                    ( List.head l |> Maybe.withDefault 0.0, List.tail l |> Maybe.withDefault [] )

                n ->
                    popN (n - 1) (List.tail l |> Maybe.withDefault [])

        layoutWedge accum fraction wedge =
            { wedge
                | fraction = fraction
                , startAngle = accum * 2 * pi
                , endAngle = (accum + fraction) * 2 * pi
                , innerRadius = (wedge.depth) * 90 |> toFloat
                , outerRadius = (wedge.depth + 1) * 90 |> toFloat
                , color =
                    Color.hsl
                        ((accum + fraction / 2) * pi * 2)
                        ((toFloat wedge.depth) * 0.05 + 0.7)
                        ((toFloat wedge.depth) * 0.02 + 0.7)
            }

        initFn accum starts depth zipper =
            let
                depth =
                    TeaTree.depth zipper

                wedge =
                    TeaTree.datum zipper

                fraction =
                    wedge.size / size

                wedgeZipper =
                    zipper
                        |> TeaTree.updateFocusDatum (layoutWedge accum fraction)

                -- _ =
                --     Debug.log "initFn"
                --         { depth = depth
                --         , fraction = fraction
                --         , accum = accum
                --         }
            in
                case TeaTree.goToNext wedgeZipper of
                    Just stepZipper ->
                        let
                            nextDepth =
                                TeaTree.depth stepZipper

                            ( nextAccum, nextStarts ) =
                                if nextDepth < depth then
                                    popN (depth - nextDepth - 1) starts
                                else if nextDepth == depth then
                                    ( accum + fraction, starts )
                                else
                                    ( accum, (accum + fraction) :: starts )
                        in
                            stepZipper
                                |> initFn nextAccum nextStarts nextDepth

                    Nothing ->
                        wedgeZipper
    in
        initFn 0 [] 0 (TeaTree.zipper tree)
            |> TeaTree.goToRoot
            |> TeaTree.toTree


flareToWedgeTree : Flare -> Tree Wedge
flareToWedgeTree (Flare flare) =
    let
        makeNode depth flare =
            { label = flare.name
            , size = flare.size |> Maybe.map toFloat |> Maybe.withDefault 0.0
            , fraction = 0.0
            , depth = depth
            , startAngle = 0.0
            , endAngle = 0.0
            , innerRadius = 0.0
            , outerRadius = 0.0
            , color = Color.rgba 200 120 80 0.6
            }

        setWedgeSize size wedge =
            { wedge | size = size }

        addChildren depth flares zipper =
            case flares of
                [] ->
                    ( zipper, TeaTree.datum zipper |> .size )

                (Flare f) :: fs ->
                    let
                        ( fsZipper, fsSize ) =
                            (addChildren depth fs zipper)

                        ( fZipper, fSize ) =
                            addChild depth f fsZipper

                        totalSize =
                            fSize + fsSize
                    in
                        ( fZipper |> TeaTree.updateFocusDatum (setWedgeSize totalSize), totalSize )

        addChild depth flare zipper =
            let
                node =
                    (makeNode (depth + 1) flare)

                emptyChild =
                    TeaTree.insertChild node zipper
                        |> TeaTree.goToChild 0
                        |> Maybe.withDefault zipper

                ( completeChild, childSize ) =
                    emptyChild
                        |> addChildren (depth + 1) flare.children
            in
                ( completeChild
                    |> TeaTree.goUp
                    |> Maybe.withDefault zipper
                    |> TeaTree.updateFocusDatum (setWedgeSize (node.size + childSize))
                , childSize
                )

        walk flare =
            let
                ( zipper, size ) =
                    addChildren 0 flare.children (TeaTree.singleton (makeNode 0 flare))
            in
                zipper |> TeaTree.goToRoot
    in
        walk flare
            |> TeaTree.toTree



-- Util code


maybeEmptyList : Maybe (List a) -> List a
maybeEmptyList maybeList =
    case maybeList of
        Nothing ->
            []

        Just xs ->
            xs
