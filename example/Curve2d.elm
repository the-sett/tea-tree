module Curve2d exposing (..)

import Arc2d exposing (Arc2d)
import LineSegment2d exposing (LineSegment2d)
import LowLevel.Command
import Path
import Point2d
import Segment
import SubPath
import Svg exposing (Svg, Attribute)
import Vector2


type alias Curve2d =
    List Segment


type Segment
    = ArcSegment Arc2d
    | LineSegment LineSegment2d


fromArc : Arc2d -> Curve2d
fromArc arc =
    [ ArcSegment arc ]


addArc : Arc2d -> Curve2d -> Curve2d
addArc arc curve =
    (ArcSegment arc) :: curve


addArcs : List Arc2d -> Curve2d -> Curve2d
addArcs arcs curve =
    case arcs of
        [] ->
            curve

        a :: ass ->
            (ArcSegment a) :: (addArcs ass curve)


fromLineSegment : LineSegment2d -> Curve2d
fromLineSegment line =
    [ LineSegment line ]


addLineSegment : LineSegment2d -> Curve2d -> Curve2d
addLineSegment line curve =
    (LineSegment line) :: curve


addLineSegments : List LineSegment2d -> Curve2d -> Curve2d
addLineSegments lines curve =
    case lines of
        [] ->
            curve

        l :: ls ->
            (LineSegment l) :: (addLineSegments ls curve)


pointToVec : Point2d.Point2d -> Vector2.Vec2 Float
pointToVec point =
    Point2d.coordinates point


curve2d : List (Attribute msg) -> Curve2d -> Svg msg
curve2d attributes curve =
    let
        convertSegment segment =
            case segment of
                ArcSegment arc ->
                    Segment.Arc
                        { arcFlag = LowLevel.Command.smallestArc
                        , direction = LowLevel.Command.clockwise
                        , start = Arc2d.startPoint arc |> pointToVec
                        , end = Arc2d.endPoint arc |> pointToVec
                        , radii = ( 50.0, 50.0 )
                        , xAxisRotate = Arc2d.sweptAngle arc
                        }

                LineSegment line ->
                    Segment.LineSegment (LineSegment2d.startPoint line |> pointToVec)
                        (LineSegment2d.endPoint line |> pointToVec)
    in
        (List.map convertSegment curve
            |> List.map Segment.toDrawTo
            |> SubPath.subpath (LowLevel.Command.moveTo ( 0.0, 0.0 ))
            |> SubPath.element
        )
            attributes