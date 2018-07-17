module Main exposing (..)

import AnimationFrame
import Arc2d
import Color exposing (Color)
import Ease
import Geometry.Svg
import Html exposing (Html)
import Http
import Json.Decode
import Ports.SVGTextPort exposing (textToSVG, textToSVGResponse)
import Task exposing (perform)
import TeaTree exposing (Tree)
import TextToSVG exposing (textAsPath, textAsText, TextAlignment(..), TextRenderFunc)
import Utils.GridMetrics exposing (Sized, Frame, rectToFrame)
import Window


--

import TypedSvg exposing (svg, g, circle, rect, text_, tspan, line, path)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x, y, x1, y1, x2, y2, rx, ry, width, height, fontSize)
import TypedSvg.Attributes exposing (viewBox, shapeRendering, fill, fillOpacity, preserveAspectRatio, stroke, strokeDasharray, strokeLinecap, strokeLinejoin, fontFamily, textAnchor, textRendering, color, d, transform)
import TypedSvg.Core exposing (svgNamespace, text, Svg)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (px, Align(..), Fill(..), Scale(..), MeetOrSlice(..), ShapeRendering(..), Opacity(..), AnchorAlignment(..), StrokeLinecap(..), StrokeLinejoin(..), TextRendering(..), Transform(..))


--import TypedSvg.Types exposing (ShapeRendering(..), Fill(..), Opacity(..), MeetOrSlice(..), Align(..), Scale(..))


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
    | Ready Frame


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
            noop (Ready <| windowSizeToFrame windowSize)

        ( _, _ ) ->
            noop model


windowSizeToFrame : Window.Size -> Frame
windowSizeToFrame size =
    { x = 0.0, y = 0.0, w = toFloat size.width, h = toFloat size.height }
        |> rectToFrame


view : Model -> Html Msg
view model =
    case model of
        Ready frame ->
            diagram frame

        _ ->
            Html.div [] []


diagram : Frame -> Html Msg
diagram current =
    svg
        [ preserveAspectRatio (Align ScaleMid ScaleMid) Meet
        , viewBox (round current.x |> toFloat)
            (round current.y |> toFloat)
            (round current.w |> toFloat)
            (round current.h |> toFloat)
        , svgNamespace
        , shapeRendering RenderGeometricPrecision
        ]
        [ background current ]


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
