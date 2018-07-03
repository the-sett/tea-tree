module Main exposing (..)

import AnimationFrame
import Arc2d
import Ease
import Geometry.Svg
import Ports.SVGTextPort exposing (textToSVG, textToSVGResponse)
import TeaTree
import TextToSVG exposing (textAsPath, textAsText, TextAlignment(..), TextRenderFunc)


type Model
    = SizingText
    | Ready


type Msg
    = TextToSVGMsg TextToSVG.Msg
    | ClickElement String


type alias Example =
    { label : String
    , size : Float
    }


example : Tree Example
example =
    TeaTree.singleton
        |> TeaTree.toTree
