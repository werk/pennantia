import Color (..)
import Graphics.Collage as G
import Graphics.Element (..)
import List as List

-- Types

type alias Progress = Float
type alias Position = (Float, Float)
type alias Stem = {positionProgress : Progress -> Position, widthProgress : Progress -> Float}

type Tree 
    = Flower {color : Color} 
    | Leaf {angle : Float}
    | Tree {stem : Stem, branches : List Tree}


-- Draw

main : Element
main = 
    let fullSize = 600
        halfSize = fullSize / 2
    in G.collage fullSize fullSize [
        G.move (0, -halfSize) (G.scale fullSize (drawTree (myTree3)))
    ]

drawTree : Tree -> G.Form
drawTree (Tree {stem, branches}) = G.group [G.group (drawStem 0.01 0 stem), G.group (List.map drawTree branches)]

drawStem : Float -> Float -> Stem -> List G.Form
drawStem step progress stem = 
    if progress > 1 then []
    else
        let p = stem.positionProgress progress
            width = stem.widthProgress progress
        in (G.move p (G.filled green (G.circle (width / 2)))) :: (drawStem step (progress + step) stem)

slope : (Float -> Position) -> Float -> Float
slope f x = 
    let epsilon = 0.01 in
    let (x1, y1) = f (x - epsilon) in
    let (x2, y2) = f (x + epsilon) in
    atan2 (y2 - y1) (x2 - x1) - pi/2

-- Generate 

boringPositionProgress h = (0, h)
funkyPositionProgress h = (sin (h*10 * (logBase 2 (h + 1))) / 10, h)

myTree : {stem : Stem, branches : List Tree}
myTree = {
        stem = {positionProgress = funkyPositionProgress, widthProgress = \h -> (1 - h) / 10},
        branches = []
    }

myTree2 = { myTree | branches <- [
        transformTree (slope myTree.stem.positionProgress 0.4 + pi/2) (myTree.stem.widthProgress 0.4 * 5) (myTree.stem.positionProgress 0.4) (Tree myTree),
        transformTree (-pi/2) (myTree.stem.widthProgress 0.8 * 5) (myTree.stem.positionProgress 0.8) (Tree myTree)
    ]}

myTree3 = buildTree myTree.stem 0

buildTree stem depth = if depth == 2 then Tree {stem = stem, branches = []} else
    let branchCount = 10 in
    let progresses = [1..branchCount] in
    let branch index = 
            let progress = (index - 0.5) / branchCount in
            let angle = slope myTree.stem.positionProgress progress + pi/2 * if round(index) % 2 == 0 then -1 else 1 in
            let width = myTree.stem.widthProgress progress * 5 in
            let position = myTree.stem.positionProgress progress in
            transformTree angle width position <| buildTree stem (depth + 1)
    in
    let branches = List.map branch progresses in
    Tree {
        stem = stem,
        branches = branches
    }

transformTree : Float -> Float -> Position -> Tree -> Tree
transformTree angle scale deltaPosition (Tree {stem, branches}) = 
    let
        {positionProgress, widthProgress} = stem
        positionProgress' progress = 
            let p = positionProgress progress
            in moveCoordinate deltaPosition (scaleCoordinate (scale, scale) (rotateCoordinate angle p))
        widthProgress' progress = widthProgress progress * scale
    in Tree {
            stem = {positionProgress = positionProgress', widthProgress = widthProgress'}, 
            branches = List.map (transformTree angle scale deltaPosition) branches
        }

scaleCoordinate : Position -> Position -> Position
scaleCoordinate (scaleX, scaleY) (x, y) =
    (x * scaleX, y * scaleY)

moveCoordinate : Position -> Position -> Position
moveCoordinate (dx, dy) (x, y) =
    (x + dx, y + dy)

rotateCoordinate : Float -> Position -> Position
rotateCoordinate angle (x, y) =
    (x * cos angle - y * sin angle, x * sin angle + y * cos angle)

