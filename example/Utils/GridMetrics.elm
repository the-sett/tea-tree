module Utils.GridMetrics
    exposing
        ( Sized
        , Positioned
        , Size
        , Position
        , Frame
        , rectToFrame
        , grid
        , rhythm
        , fontSizePx
        , fontGridFraction
        , plusGridFraction
        )


type alias Positioned a =
    { a | x : Float, y : Float }


type alias Sized a =
    { a | w : Float, h : Float }


type alias Position =
    { x : Float, y : Float }


type alias Size =
    { w : Float, h : Float }


type alias Frame =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , aspectRatio : Float
    }


rectToFrame : Sized (Positioned a) -> Frame
rectToFrame rect =
    { x = rect.x
    , y = rect.y
    , w = rect.w
    , h = rect.h
    , aspectRatio = rect.h / rect.w
    }



{--Grid size and rhythm. --}


grid : Int
grid =
    24


rhythm : Int -> Float
rhythm unit =
    toFloat (grid * unit)


gridFraction : (Int -> Int -> Int) -> Float -> Float
gridFraction op gridRatio =
    let
        fraction =
            (toFloat grid) * gridRatio + (toFloat (op grid grid) * (1.0 - gridRatio))
    in
        fraction |> round |> toFloat


plusGridFraction : Float -> Float
plusGridFraction =
    gridFraction (-)


minusGridFraction : Float -> Float
minusGridFraction =
    gridFraction (+)


fontSizePx : Float -> Float -> Float
fontSizePx lineSpacing gridUnits =
    (toFloat grid) * gridUnits / lineSpacing


fontGridFraction : Float -> Float -> Float
fontGridFraction lineSpacing gridUnits =
    ((toFloat grid) * gridUnits - (fontSizePx lineSpacing gridUnits)) * 0.7
