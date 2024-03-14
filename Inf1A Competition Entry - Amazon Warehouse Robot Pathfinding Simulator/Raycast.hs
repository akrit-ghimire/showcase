-- Inspired by 3DSage (YouTube)
-- Link: https://www.youtube.com/watch?v=gYRrGTC7GtA
-- Inspired by griffpatch (YouTube) 
-- Link: https://www.youtube.com/watch?v=M1c5TcdITVs&list=PLy4zsTUHwGJKolO9Ko_j6IStFIJnTYBul

-- Akrit Ghimire 16/11/2023 

module Raycast where

import Codec.Picture (pixelAt, PixelRGBA8, generateImage) -- JuicyPixels Library
import Picture (saveImage, isInsideRect, isOnLine, Background (img, width, height, Background), createColor)
import Pathfind (maze, Grid, isNotMapBorder)

mapWidth :: Int
mapWidth = 512
mapHeight :: Int
mapHeight = 256
mapSize :: Int
mapSize = 16
oneDegree :: Float
oneDegree = pi /180

data Player = Player {posx :: Float, posy :: Float, size :: Int, deltaX :: Float, deltaY :: Float, angle :: Float}

-- turtle commands
moveForward :: Int -> Player -> Player 
moveForward dist p = Player {posx = newX, posy = newY, size = size p, deltaX = deltaX p, deltaY = deltaY p, angle = angle p} 
    where 
        newX = fromIntegral dist + posx p + deltaX p
        newY = fromIntegral dist + posy p + deltaY p

moveBack :: Int -> Player -> Player 
moveBack dist p = Player {posx = newX, posy = newY, size = size p, deltaX = deltaX p, deltaY = deltaY p, angle = angle p} 
    where 
        newX = posx p -fromIntegral dist - deltaX p
        newY = posy p -fromIntegral dist - deltaY p

turnLeft :: Float -> Player -> Player 
turnLeft rad p = Player {posx = posx p, posy = posy p, size = size p, deltaX = newDeltaX, deltaY = newDeltaY, angle = newAngle} 
    where 
        preAngle  = angle p - rad
        newAngle  = if preAngle < 0 then preAngle + 2 * pi else preAngle 
        newDeltaX = 5* cos newAngle
        newDeltaY = 5* sin newAngle

turnRight :: Float -> Player -> Player 
turnRight rad p = Player {posx = posx p, posy = posy p, size = size p, deltaX = newDeltaX, deltaY = newDeltaY, angle = newAngle} 
    where 
        preAngle  = angle p + rad
        newAngle  = if preAngle > 2*pi then preAngle - 2 * pi else preAngle 
        newDeltaX = 5* cos newAngle
        newDeltaY = 5* sin newAngle

castRay :: Int -> Player -> Grid -> [((Int, Int), (Int, Float))]
castRay 0 _ _          = []
castRay rays p gameMap = ((rx, ry), (dist, dimFactor)) : castRay (rays-1) p gameMap 
    where 
        mapW = length $ head gameMap
        mapH = length gameMap
        dof  = 40 -- how far to look

        px = posx p + (fromIntegral (size p)/2)
        py = posy p + (fromIntegral (size p)/2)

        ra = calcAngle $ angle p

        calcAngle :: Float -> Float 
        calcAngle pa | rayAngle < 0    = rayAngle + 2*pi 
                     | rayAngle > 2*pi = rayAngle - 2*pi
                     | otherwise       = rayAngle 
            where 
                deg = -30 + fromIntegral rays/2
                rayAngle = pa - (oneDegree* deg)

        deltaX = 5 * cos ra 
        deltaY = 5 * sin ra

        (rx, ry) = findCollision (px, py) (deltaX, deltaY) gameMap dof
        dist = round $ distance (round px, round py) (rx, ry)
        dimFactor = if dist > 30 then 30 / fromIntegral dist else 1

        findCollision :: (Float, Float) -> (Float, Float) -> Grid -> Int -> (Int, Int)
        findCollision (x, y) (dX, dY) _ 0                                                                = (round x, round y)
        findCollision (x, y) (dX, dY) gameMap loop | check && not (isNotMapBorder (gameMap !! my !! mx)) = (round (x-dX), round (y-dY))
                                                        | otherwise                                       = findCollision (x+dX, y+dY) (dX,dY) gameMap (loop -1)
            where 
                my = round y `div` mapSize 
                mx = round x `div` mapSize 
                check = my < mapH && my >= 0 && mx < mapW && mx >= 0 

distance :: (Int, Int) -> (Int, Int) -> Float
distance (x, y) (x', y') = sqrt $ fromIntegral (x'-x)^2 + fromIntegral (y'-y)^2

createRaycastImage :: Player -> Grid -> Background
createRaycastImage p gameMap | isGridCorrectSize = Background {img = generateImage draw mapWidth mapHeight, width = mapWidth, height = mapHeight}
                             | otherwise            = error "Map must be a 16x16 grid" 
    where
        mapW = length $ head gameMap
        mapH = length gameMap

        midX           = round $ posx p + (fromIntegral (size p)/2)
        midY           = round $ posy p + (fromIntegral (size p)/2)
        playerMid = (midX, midY)

        isGridCorrectSize = mapH * mapSize == mapHeight && mapW * 2 * mapSize == mapWidth
        
        bgColor = createColor (29,29,29, 255)
        playerColor = createColor (255,0,0, 255)
        rayColor    = createColor (0, 255, 255, 255)
    
        rays = castRay 120 p gameMap
        rayBlockWidth = 2

        raysOptimised = [x | (x, i) <- zip rays (cycle [1..10]), i == 10] -- cast 120 rays but display only 20 on top down

        isOnRay :: (Int, Int) -> [((Int, Int), (Int, Float))] -> Bool
        isOnRay _ []                                                 = False
        isOnRay coord ((ray, _):rays) | isOnLine playerMid ray coord = True 
                                      | otherwise                    = isOnRay coord rays

        mapPieces = "#GS"
        mapPieceVals = [
                ('#', createColor (0, 0, 0, 255)),
                ('G', createColor (100, 0, 0, 255)),
                ('S', createColor (0, 100, 0, 255))
            ]

        isPlayer = undefined  
        isRay = undefined

        isMapPiece :: (Int, Int) -> Bool
        isMapPiece coord = getMapSqr coord gameMap `elem` mapPieces

        getMapPiece :: Char -> [(Char, PixelRGBA8)] -> PixelRGBA8
        getMapPiece sqr []                               = bgColor
        getMapPiece sqr ((char, color):xs) | char == sqr = color
                                           | otherwise   = getMapPiece sqr xs 

        checkInLeftHalf x = x < mapWidth `div` 2  

        getMapSqr :: (Int, Int) -> Grid -> Char
        getMapSqr (x,y) m = m !! row !! col
            where 
                col = x `div` mapSize
                row = y `div` mapSize
        
        draw :: Int -> Int -> PixelRGBA8
        draw x y | isInLeftHalf && isOnRay (x,y) raysOptimised          = rayColor
                 | isInLeftHalf && isInsideRect posTL posBR (x, y)      = playerColor
                 | isInLeftHalf && isMapPiece (x,y)                     = getMapPiece (getMapSqr (x,y) gameMap) mapPieceVals
                 | not isInLeftHalf && isInsideRect rayTL rayBR (x,y)   = createColor (round $ 255 * dimFactor,0,round $ 255 * dimFactor, 255)
                 | otherwise                                            = bgColor
            where 
                posTL = (round $ posx p, round $ posy p)
                posBR = (round (posx p) + size p-1, round (posy p) + size p-1)
                isInLeftHalf = checkInLeftHalf x

                raysIdx = (x - (mapWidth `div` 2)) `div` rayBlockWidth
                (_, (dist, dimFactor)) = if raysIdx < length rays then rays !! raysIdx else last rays 
                rayLineHeight = mapSize * mapHeight `div` dist
                rayTL   = (x, 160 - rayLineHeight `div`2)
                rayBR   = (x, (160 - rayLineHeight `div`2) + rayLineHeight)

main = do 
    let startAngle = 0
        player     = Player {posx = fromIntegral (4*mapSize), posy = fromIntegral (14*mapSize) -160, size = mapSize, deltaX = 5* cos startAngle, deltaY = 5* sin startAngle, angle = startAngle}
        bg         = createRaycastImage player maze
    
    saveImage bg "gameState"