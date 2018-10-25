port module Ports.SVGTextPort exposing (textToSVG, textToSVGResponse)

import TextToSVG exposing (TextToSVGPort, TextToSVGResponsePort)


{-| Requests that text is converted to SVG.
-}
port textToSVG : TextToSVGPort msg


{-| Creates a subscription to listen for responses to requests to convert text
to SVG
-}
port textToSVGResponse : TextToSVGResponsePort msg
