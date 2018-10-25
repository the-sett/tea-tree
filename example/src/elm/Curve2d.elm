module Curve2d exposing
    ( Curve2d
    , Segment(..)
    , addArc
    , addArcs
    , addLineSegment
    , addLineSegments
    , curve2d
    , fromArc
    , fromLineSegment
    , pointToVec
    )

import Arc2d exposing (Arc2d)
import LineSegment2d exposing (LineSegment2d)
import LowLevel.Command
import Path
import Point2d
import Segment
import SubPath
import Svg exposing (Attribute, Svg)
import Vector2d


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
    ArcSegment arc :: curve


addArcs : List Arc2d -> Curve2d -> Curve2d
addArcs arcs curve =
    case arcs of
        [] ->
            curve

        a :: ass ->
            ArcSegment a :: addArcs ass curve


fromLineSegment : LineSegment2d -> Curve2d
fromLineSegment line =
    [ LineSegment line ]


addLineSegment : LineSegment2d -> Curve2d -> Curve2d
addLineSegment line curve =
    LineSegment line :: curve


addLineSegments : List LineSegment2d -> Curve2d -> Curve2d
addLineSegments lines curve =
    case lines of
        [] ->
            curve

        l :: ls ->
            LineSegment l :: addLineSegments ls curve


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
                        { arcFlag =
                            if Arc2d.sweptAngle arc > pi || Arc2d.sweptAngle arc < -pi then
                                LowLevel.Command.largestArc

                            else
                                LowLevel.Command.smallestArc
                        , direction =
                            if Arc2d.sweptAngle arc < 0 then
                                LowLevel.Command.counterClockwise

                            else
                                LowLevel.Command.clockwise
                        , start = Arc2d.startPoint arc |> pointToVec
                        , end = Arc2d.endPoint arc |> pointToVec
                        , radii = ( Arc2d.radius arc, Arc2d.radius arc )
                        , xAxisRotate = 0.0
                        }

                LineSegment line ->
                    Segment.LineSegment (LineSegment2d.startPoint line |> pointToVec)
                        (LineSegment2d.endPoint line |> pointToVec)

        startPoint curve =
            case curve of
                [] ->
                    ( 0.0, 0.0 )

                c :: cs ->
                    case c of
                        ArcSegment arc ->
                            Arc2d.startPoint arc |> pointToVec

                        LineSegment line ->
                            LineSegment2d.startPoint line |> pointToVec
    in
    (curve
        |> List.map convertSegment
        |> List.map Segment.toDrawTo
        |> SubPath.subpath (LowLevel.Command.moveTo <| startPoint curve)
        |> SubPath.element
    )
        attributes
