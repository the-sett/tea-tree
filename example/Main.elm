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
import TeaTree exposing (Tree)
import TextToSVG exposing (textAsPath, textAsText, TextAlignment(..), TextRenderFunc)


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


init : ( Model, Cmd Msg )
init =
    ( LoadingModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.text "hello"



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
