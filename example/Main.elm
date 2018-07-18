module Main exposing (..)

import AnimationFrame
import Arc2d exposing (Arc2d)
import Color exposing (Color)
import Ease
import Geometry.Svg
import Html exposing (Html)
import Http
import LineSegment2d exposing (LineSegment2d)
import Json.Decode
import Point2d exposing (Point2d)
import Ports.SVGTextPort exposing (textToSVG, textToSVGResponse)
import Task exposing (perform)
import TeaTree exposing (Tree)
import TextToSVG exposing (textAsPath, textAsText, TextAlignment(..), TextRenderFunc)
import TypedSvg exposing (svg, g, circle, rect, text_, tspan, line, path)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x, y, x1, y1, x2, y2, rx, ry, width, height, fontSize)
import TypedSvg.Attributes exposing (viewBox, shapeRendering, fill, fillOpacity, preserveAspectRatio, stroke, strokeDasharray, strokeLinecap, strokeLinejoin, fontFamily, textAnchor, textRendering, color, d, transform)
import TypedSvg.Core exposing (svgNamespace, text, Svg)
import TypedSvg.Events exposing (onClick)
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
    | SizingWindow
    | Ready ReadyModel


type alias ReadyModel =
    { frame : Frame
    , tree : Tree Wedge
    }


type Msg
    = TextToSVGMsg TextToSVG.Msg
    | WindowSize Window.Size
    | ClickElement String


type alias Wedge =
    { label : String
    , size : Float
    , startAngle : Float
    , endAngle : Float
    , innerRadius : Float
    , outerRadius : Float
    , color : Color
    }


init : ( Model, Cmd Msg )
init =
    ( SizingWindow
    , Task.perform WindowSize Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize


noop model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( model, action ) of
        ( _, WindowSize windowSize ) ->
            noop (Ready { frame = windowSizeToFrame windowSize, tree = example })

        ( _, _ ) ->
            noop model


windowSizeToFrame : Window.Size -> Frame
windowSizeToFrame size =
    { x = 0.0, y = 0.0, w = toFloat size.width, h = toFloat size.height }
        |> rectToFrame


view : Model -> Html Msg
view model =
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


wheel : Frame -> Tree Wedge -> Svg msg
wheel frame tree =
    let
        center =
            middle frame |> (\c -> Point2d.fromCoordinates ( c.x, c.y ))
    in
        wedge center
            { label = "test"
            , size = 100
            , startAngle = 0
            , endAngle = pi / 3
            , innerRadius = 100
            , outerRadius = 150
            , color = black
            }


wedge : Point2d -> Wedge -> Svg msg
wedge center { label, size, startAngle, endAngle, innerRadius, outerRadius, color } =
    let
        innerArc =
            Arc2d.with
                { centerPoint = center
                , radius = innerRadius
                , startAngle = startAngle
                , sweptAngle = endAngle - startAngle
                }
                |> Geometry.Svg.arc2d [ fill FillNone, strokeWidth 2, stroke color ]

        outerArc =
            Arc2d.with
                { centerPoint = center
                , radius = outerRadius
                , startAngle = startAngle
                , sweptAngle = endAngle - startAngle
                }
                |> Geometry.Svg.arc2d [ fill FillNone, strokeWidth 2, stroke color ]

        startLine =
            LineSegment2d.from
                (Point2d.fromPolarCoordinates ( innerRadius, startAngle ))
                (Point2d.fromPolarCoordinates ( outerRadius, startAngle ))
                |> LineSegment2d.translateBy (Vector2d.from Point2d.origin center)
                |> Geometry.Svg.lineSegment2d [ fill FillNone, strokeWidth 2, stroke color ]

        endLine =
            LineSegment2d.from
                (Point2d.fromPolarCoordinates ( innerRadius, endAngle ))
                (Point2d.fromPolarCoordinates ( outerRadius, endAngle ))
                |> LineSegment2d.translateBy (Vector2d.from Point2d.origin center)
                |> Geometry.Svg.lineSegment2d [ fill FillNone, strokeWidth 2, stroke color ]
    in
        g [] [ innerArc, outerArc, startLine, endLine ]


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


black =
    Color.black


white =
    Color.white


offWhite =
    Color.rgb 247 245 248


midGray =
    Color.gray


strongPrintGray =
    Color.rgb 32 32 32


printGray =
    Color.rgb 48 48 48



-- REST Calls
--fetchExample : Task
--decoder : Decoder (Tree Wedge)
-- Example Data


example : Tree Wedge
example =
    TeaTree.singleton
        { label = "test"
        , size = 0.0
        , startAngle = 0.0
        , endAngle = pi
        , innerRadius = 50.0
        , outerRadius = 100.0
        , color = Color.rgb 0 0 0
        }
        |> TeaTree.toTree
