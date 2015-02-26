import Color (..)
import Graphics.Collage as G
import Time
import Signal
import Graphics.Element as E
import Debug

type Fern = Fern { scale : Float, angle : Float, left : Maybe Fern, right : Maybe Fern, next : Maybe Fern }

fern scale isRight = if
    | scale <= 0.5 -> Nothing 
    | otherwise -> Just <|
        let rot = scale * scale in
        let rotation = if isRight then 0.05 / rot else -0.05 / rot in
        Fern { 
            scale = scale, 
            angle = rotation, 
            left = fern (scale * 0.80) False, 
            right = fern (scale * 0.80) True, 
            next = fern (scale * 0.95) isRight
        }

drawFern : Float -> Maybe Fern -> G.Form
drawFern growth fern = case fern of
    Nothing -> G.filled (rgba 100 200 100 0) <| G.circle 0
    Just (Fern { scale, angle, left, right, next }) ->
        G.rotate angle <| G.scale (growth * scale) <| G.group [
            G.traced (G.solid <| rgb 100 200 100) <| G.segment (0, 0) (0, 5),
            G.moveY 5 <| G.group [
                G.scale 0.8 <| G.rotate (-3.1416 * 0.5) <| drawFern growth left,
                G.scale 0.8 <| G.rotate (3.1416 * 0.5) <| drawFern growth right,
                drawFern growth next
            ]
        ]

main : Signal E.Element
main = 
    let fern1 = fern 1 False in
    Signal.map (\now ->
        let _ = Debug.watch "now" now in
        let growth = 0.1 + min 1 (sqrt now / 5) in
        G.collage 800 600 [G.moveY -300 <| G.scale 10 <| drawFern growth fern1]
    )
    frame

frame : Signal Float
frame = Signal.foldp (\dt theta -> theta + dt / 1000) 0 (Time.fps 25)

