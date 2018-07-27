module Curve2d exposing (..)

import Arc2d exposing (Arc2d)
import LineSegment2d exposing (LineSegment2d)


type Curve2d
    = List Segment


type Segment
    = ArcSegment Arc2d
    | LineSegment LineSegment2d


fromArc : Arc2d -> Curve2d
fromArc arc =
    [ ArcSegment arc ]


addArc : Arc2d -> Curve2d -> Curve2d
addArc arc curve =
    (fromArc arc) :: curve


addArcs : List Arc2d -> Curve2d -> Curve2d
addArcs arcs curve =
    case arcs of
        [] ->
            curve

        a :: ass ->
            (fromArc a) :: (addArcs ass curve)


fromLineSegment : LineSegment2d -> Curve2d
fromLineSegment line =
    [ LineSegment line ]


addLineSegment : LineSegment2d -> Curve2d -> Curve2d
addLineSegment line curve =
    (fromLineSegment line) :: curve


addLineSegments : List LineSegment2d -> Curve2d -> Curve2d
addLineSegments lines curve =
    case lines of
        [] ->
            curve

        l :: ls ->
            (fromLine l) :: (addLineSegments ls curve)


curve2d : List (Attribute msg) -> Curve2d -> Svg msg
