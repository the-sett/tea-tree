module Main exposing (..)

import AnimationFrame
import Arc2d
import Color exposing (Color)
import Ease
import Geometry.Svg
import Http
import Json.Decode
import Ports.SVGTextPort exposing (textToSVG, textToSVGResponse)
import TeaTree exposing (Tree)
import TextToSVG exposing (textAsPath, textAsText, TextAlignment(..), TextRenderFunc)


type Model
    = LoadingModel
    | SizingText
    | Ready


type Msg
    = TextToSVGMsg TextToSVG.Msg
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



--decoder : Decoder (Tree Wedge)


example : Tree Wedge
example =
    TeaTree.singleton
        { label = ""
        , size = 0.0
        , startAngle = 0.0
        , endAngle = 0.0
        , innerRadius = 0.0
        , outerRadius = 0.0
        , color = Color.rgb 0 0 0
        }
        |> TeaTree.toTree
