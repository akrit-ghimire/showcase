-- This program pathfinds in a maze!
-- Inspired by Don Sannella's Lecture on Best First Search Algorithm
-- Also Inspired by this video by Sebastian Lague (He's my fave coding YouTuber lol 🤣)
-- Link: https://www.youtube.com/watch?v=-L-WgKMFuhE&list=PLFt_AvWsXl0cq5Umv3pMC9SPnKjfp9eGW&index=1

-- Written by Akrit Ghimire - 08/11/23

module Pathfind where

import Data.Maybe ( fromJust, listToMaybe )

type Coordinate = (Int, Int)
type Grid       = [String]

data Path = Nil | Path {prev :: Path, pos :: Coordinate, gC :: Int, hC :: Int, fC :: Int} -- fC is fCost, same with others
    deriving (Eq, Show)

data Direction = Start | Up | Down | MoveLeft | MoveRight | TurnRight | TurnLeft | End
    deriving (Eq, Show)

createPath :: (Path, Coordinate, Int, Int, Int) -> Path
createPath (prev, pos, gC, hC, fC) = Path {prev = prev, pos = pos, gC = gC, hC = hC, fC = fC}

-- run 'testPathfinder' in GHCI to see how module works for demo maze
maze :: Grid
maze = [
        "################",
        "#    #      #  #",
        "#   G   #      #",
        "#############  #",
        "#  #     #     #",
        "#  #  #  # #####",
        "#  #  #  # #####",
        "#  #  #  # #####",
        "#  #  #  # #####",
        "#  #  #  # #####",
        "#  #  #  # #####",
        "#  #  #  # #####",
        "#     #  # #####",
        "#     #  # #####",
        "#   S #        #",
        "################"
    ]

getFullPath :: Path -> [Coordinate]
getFullPath Nil = []
getFullPath x   = pos x : getFullPath (prev x)

getPathDirections :: [Coordinate] -> [Direction]
getPathDirections [_]                  = [End]
getPathDirections ((x,y):(a,b):coords) | b < y = Up : getPathDirections ((a, b) : coords)
                                       | b > y = Down : getPathDirections ((a, b) : coords)
                                       | a > x = MoveRight : getPathDirections ((a, b) : coords)
                                       | a < x = MoveLeft : getPathDirections ((a, b) : coords)

addTurnPoints :: [Direction] -> [Direction]
addTurnPoints [] = []
addTurnPoints [a] = [a]
addTurnPoints (dir1:dir2:ds) =
  case (dir1, dir2) of
    (Up, MoveRight)   -> Up   : TurnRight : addTurnPoints (MoveRight : ds)
    (Up, MoveLeft)    -> Up   : TurnLeft  : addTurnPoints (MoveLeft : ds)
    (Down, MoveRight) -> Down : TurnLeft  : addTurnPoints (MoveRight : ds)
    (Down, MoveLeft)  -> Down : TurnRight : addTurnPoints (MoveLeft : ds)

    (MoveRight, Up)   -> MoveRight:TurnLeft: addTurnPoints (Up : ds)
    (MoveLeft, Up)    -> MoveLeft:TurnRight: addTurnPoints (Up : ds)
    (MoveRight, Down) -> MoveRight:TurnRight: addTurnPoints (Down : ds)
    (MoveLeft, Down)  -> MoveLeft:TurnLeft: addTurnPoints (Down : ds)
    _ -> dir1 : addTurnPoints (dir2 : ds)

findCoordOf :: Char -> Grid -> Maybe Coordinate
findCoordOf letter m = listToMaybe [ (x, y) | (row, y) <- zip m [0..], (x, l) <- zip [0..] row, l == letter]

testPathfinder :: IO () -- prints 'maze' with solution on the terminal
testPathfinder = do
    let value = algorithm maze [createPath(Nil, (4,14), 0, 0, 0)] [] (4,14) (4,2) 
    case value of
        Just x -> printPoints maze $ getFullPath x
        Nothing -> putStrLn "No value found"

testPathfinderDirections :: IO () -- prints list of english directions from start to end of maze
testPathfinderDirections = do
    let value = algorithm maze [createPath(Nil, (4,14), 0, 0, 0)] [] (4,14) (4,2) 
    case value of
        Just x -> print $ addTurnPoints $ getPathDirections $ reverse $ getFullPath x
        Nothing -> putStrLn "No value found"

-- Search Functions
getValidNeighbours :: Grid -> Coordinate -> [Coordinate]
getValidNeighbours map (x, y) = validate neighbourPoints
    where
        -- neighbourPoints = [ -- This would allow for diagonal movements which is not wanted for final program
        --         (x+1, y), (x+1, y+1), (x+1, y-1), -- three on right 
        --         (x, y+1), (x, y-1),               -- top bottom
        --         (x-1, y), (x-1, y+1), (x-1, y-1)  -- three on left
        --     ]
        neighbourPoints = [ -- This allows for adjacent movement (no diagonals)
                (x+1, y), -- right
                (x, y+1), -- top
                (x, y-1), -- bottom
                (x-1, y)  -- left 
            ]

        mapWidth  = length $ head map
        mapHeight = length map

        validate :: [Coordinate] -> [Coordinate]
        validate []             = []
        validate ((x,y):points) | x <= mapWidth && 
                                  x >= 0 &&
                                  y <= mapHeight &&
                                  y >= 0 &&
                                  isNotMapBorder(map !! y !! x) = (x,y) : validate points
                                | otherwise                     = validate points
        
isNotMapBorder :: Char -> Bool
isNotMapBorder x = x /= '#' -- can add other collision symbols here

algorithm :: Grid -> [Path] -> [Path] -> Coordinate -> Coordinate -> Maybe Path
algorithm _ [] _ _ _                                                   = Nothing
algorithm grid openPaths closedPaths start goal | pos bestPath == goal = Just bestPath 
                                                | otherwise            = algorithm grid (updatedOpenPaths ++ createNewPathsFrom grid bestPath closedPaths start goal) (bestPath : closedPaths) start goal
    where 
        (bestPath, updatedOpenPaths) = lowestFcost openPaths
        
createNewPathsFrom :: Grid -> Path -> [Path] -> Coordinate -> Coordinate -> [Path]
createNewPathsFrom grid bestPath closedPaths start goal = convertToPaths bestPath (getCoordsNotInPathsList (getValidNeighbours grid (pos bestPath)) closedPaths) start goal

getCoordsNotInPathsList :: [Coordinate] -> [Path] -> [Coordinate]
getCoordsNotInPathsList coords paths = [c | c <- coords, not (isCoordInPathsList c paths)]

isCoordInPathsList :: Coordinate -> [Path] -> Bool
isCoordInPathsList c paths = or [pos p == c | p <- paths]

convertToPaths :: Path -> [Coordinate] -> Coordinate -> Coordinate -> [Path]
convertToPaths bestPath points start goal = [
        createPath(bestPath, p, gCost, hCost, fCost) 
        | p <- points, 
        let gCost = distance p start -- distance from start
            hCost = distance p goal  -- distance from end
            fCost = gCost + hCost    -- the sum of the costs
    ]

distance :: Coordinate -> Coordinate -> Int
distance (x, y) (x', y') = round $ sqrt $ fromIntegral $ a^2 + b^2
    where 
        a = (x-x')*10 -- multiply by 10 to keep some data of the distance
        b = (y-y')*10

lowestFcost :: [Path] -> (Path, [Path])
lowestFcost paths = findAndSplit (findLowest paths defaultNode) paths
    where 
        findLowest [] lowF                                = lowF
        findLowest (p:paths) lowF | fC p < fC lowF        = findLowest paths p
                                  | otherwise             = findLowest paths lowF
        defaultNode = createPath (Nil, (0,0), 0, 0, 1000000) -- make default as worst case so it is not chosen

indexOf :: Eq a => a -> [a] -> Int
indexOf item list = head [idx | (idx, element) <- zip [0..] list, element == item]

findAndSplit :: Eq a => a -> [a] -> (a, [a])
findAndSplit item list = (element, newList)
    where 
        idx = indexOf item list
        element = list !! idx
        newList = take idx list ++ drop (idx+1) list


-- Grid Printing Functions
plotPoints :: Grid -> [Coordinate] -> Grid
plotPoints grid []             = grid
plotPoints grid ((x,y):points) = plotPoints newGrid points
    where 
        startChunk = take y grid 
        endChunk   = drop (y+1) grid
        desiredRow = grid !! y
        startRow   = take x desiredRow
        endRow     = drop (x+1) desiredRow
        newGrid    = startChunk ++ [startRow ++ "x" ++ endRow] ++ endChunk  

printPoints :: Grid -> [Coordinate] -> IO()
printPoints grid coords = printRows plottedGrid
    where 
        plottedGrid = plotPoints grid coords
        
        printRows :: Grid -> IO()
        printRows []       = putStrLn "Done"
        printRows (r:grid) = do 
            putStrLn r
            printRows grid