-- -- Inspired by 3DSage (YouTube)
-- -- Link: https://www.youtube.com/watch?v=gYRrGTC7GtA

-- import Codec.Picture (pixelAt)
-- import Picture (saveImage, generateBg, drawRect, Background (img, width, height, Background), PixelFunc, createColor, drawImg, drawLine)
-- import Pathfind (maze, Grid, isNotMapBorder)

-- mapWidth :: Int
-- mapWidth = 512
-- mapHeight :: Int
-- mapHeight = 256
-- mapSize :: Int
-- mapSize = 16
-- oneDegree :: Float
-- oneDegree = pi /180

-- data Player = Player {posx :: Float, posy :: Float, size :: Int, deltaX :: Float, deltaY :: Float, angle :: Float}

-- turnForward :: Player -> Player 
-- turnForward p = Player {posx = newX, posy = newY, size = size p, deltaX = deltaX p, deltaY = deltaY p, angle = angle p} 
--     where 
--         newX = posx p + deltaX p
--         newY = posy p + deltaY p

-- turnBack :: Player -> Player 
-- turnBack p = Player {posx = newX, posy = newY, size = size p, deltaX = deltaX p, deltaY = deltaY p, angle = angle p} 
--     where 
--         newX = posx p - deltaX p
--         newY = posy p - deltaY p

-- turnLeft :: Float -> Player -> Player 
-- turnLeft rad p = Player {posx = posx p, posy = posy p, size = size p, deltaX = newDeltaX, deltaY = newDeltaY, angle = newAngle} 
--     where 
--         preAngle  = angle p - rad
--         newAngle  = if preAngle < 0 then preAngle + 2 * pi else preAngle 
--         newDeltaX = 5* cos newAngle
--         newDeltaY = 5* sin newAngle

-- turnRight :: Float -> Player -> Player 
-- turnRight rad p = Player {posx = posx p, posy = posy p, size = size p, deltaX = newDeltaX, deltaY = newDeltaY, angle = newAngle} 
--     where 
--         preAngle  = angle p + rad
--         newAngle  = if preAngle > 2*pi then preAngle - 2 * pi else preAngle 
--         newDeltaX = 5* cos newAngle
--         newDeltaY = 5* sin newAngle

-- -- | The function `toCell` takes a 1-to-81 position in the Sudoku
-- -- square and returns its corresponding cell.
-- -- toCell :: Int -> (Int,Int)
-- -- toCell c = (x+1, y+1) where (x,y) = (c-1) `divMod` 9 here?
-- castRay :: Int -> Player -> Background -> Grid -> Int -> Background 
-- castRay rays p bg gameMap 0 = bg
-- castRay rays p bg gameMap ray = castRay rays p rayLineOnBg gameMap (ray-1)
--     where 
--         mapW = length $ head gameMap
--         mapH = length gameMap
--         dof  = 40 -- how far to look

--         px = posx p + (fromIntegral (size p)/2)
--         py = posy p + (fromIntegral (size p)/2)

--         ra = calcAngle $ angle p

--         calcAngle :: Float -> Float 
--         calcAngle pa | rayAngle < 0    = rayAngle + 2*pi 
--                      | rayAngle > 2*pi = rayAngle - 2*pi
--                      | otherwise       = rayAngle 
--             where 
--                 deg = (-30 +(fromIntegral ray))
--                 rayAngle = pa - (oneDegree* deg)

--         deltaX = 5 * cos ra 
--         deltaY = 5 * sin ra

--         (rx, ry) = findCollision (px, py) (deltaX, deltaY) gameMap dof

--         findCollision :: (Float, Float) -> (Float, Float) -> Grid -> Int -> (Int, Int)
--         findCollision (x, y) (dX, dY) _ 0                                                                = (round x, round y)
--         findCollision (x, y) (dX, dY) gameMap loop | check && not (isNotMapBorder (gameMap !! my !! mx)) = (round x, round y)
--                                                        | otherwise                                       = findCollision (x+dX, y+dY) (dX,dY) gameMap (loop -1)
--             where 
--                 my = round y `div` mapSize 
--                 mx = round x `div` mapSize 
--                 check = my < mapH && my >= 0 && mx < mapW && mx >= 0 

--         rayLineDist = round $ distance (round px, round py) (rx, ry)
--         rayLineHeight = mapSize * mapHeight `div` rayLineDist
--         rayLineWidth  = 4
--         rayDimFactor = if rayLineDist > 50 then fromIntegral 50 / fromIntegral rayLineDist else 1 -- will error if going up the way
--         rayOnBg  = drawLine (round px, round py) (rx, ry) (255,0,255) bg
--         rayLineOnBg = drawRect (512-(ray * rayLineWidth), (160 - rayLineHeight `div`2)) rayLineWidth rayLineHeight (round $ 255 * rayDimFactor,0,round $ 255 * rayDimFactor) rayOnBg

-- distance :: (Int, Int) -> (Int, Int) -> Float
-- distance (x, y) (x', y') = sqrt $ fromIntegral (x'-x)^2 + fromIntegral (y'-y)^2

-- -- castRay :: Int -> Player -> Background -> Grid -> Background
-- -- castRay num p bg gameMap = colRayOnBg
-- --     -- essentially from players pos
-- --     -- find first horisontal collision
-- --     -- find rist vertical collision
-- --     -- get distance of each
-- --     -- cast ray of shortest distance one
-- --     -- draw this as line on right side
-- --     where 
-- --         mapW    = length $ head gameMap
-- --         mapH    = length gameMap
-- --         midX :: Int
-- --         midX    = round (posx p + (fromIntegral (size p)/2))
-- --         midY :: Int
-- --         midY    = round (posy p + (fromIntegral (size p)/2))

-- --         ra      = angle p
-- --         aTan    = -1 / tan ra

-- --         py = midY
-- --         px = midX

-- --         -- horisontal wall check
-- --         firstHorizon :: Float -> (Int, Int, Int, Int, Int)
-- --         firstHorizon ra | ra < pi  = -- looking up
-- --                             let ry = (py `div` mapSize) * mapSize -- first row
-- --                                 rx = round (fromIntegral (py - ry) * aTan + fromIntegral px)
-- --                             in (ry, rx, -mapSize, round (fromIntegral mapSize * aTan), 8)
-- --                         | ra > pi = -- looking down
-- --                             let ry = ((py `div` mapSize) * mapSize) + mapSize
-- --                                 rx = round (fromIntegral (py - ry) * aTan+ fromIntegral px)
-- --                             in (ry, rx, mapSize, round (fromIntegral (-mapSize) * aTan), 8)
-- --                         | ra == pi || ra == 2*pi || ra == 0 = (py, px, 0, 0, 0) -- looking left right

-- --         findHCollision :: (Int, Int, Int, Int, Int) -> (Int, Int)
-- --         findHCollision (ry, rx, _, _, 0)     = (rx, ry)
-- --         findHCollision (ry, rx, yo, xo, dof) | constraint && not (isNotMapBorder (gameMap !! mapY !! mapX)) = (rx, ry)
-- --                                              | otherwise                                                    = findHCollision (ry+yo, rx+xo, yo, xo, dof-1)
-- --             where
-- --                 mapY   = ry `div` mapSize
-- --                 mapX   = rx `div` mapSize
-- --                 constraint = mapX < mapW-1 && mapY < mapH-1

-- --         -- vertical wall check
-- --         nTan = -tan ra

-- --         firstColumn :: Float -> (Int, Int, Int, Int, Int)
-- --         firstColumn ra | ra < (pi/2) || ra > (3*pi/2) = -- left
-- --                             let rx = (px `div` mapSize) * mapSize -- first row
-- --                                 ry = round (fromIntegral (px - rx) * nTan+ fromIntegral py)
-- --                             in (ry, rx, round (fromIntegral mapSize * nTan), -mapSize,  8)
-- --                         | ra > (pi/2) && ra < (3*pi/2) = -- right
-- --                             let rx = ((px `div` mapSize) * mapSize) + mapSize
-- --                                 ry = round (fromIntegral (px - rx) * nTan+ fromIntegral py)
-- --                             in (ry, rx, round (fromIntegral (-mapSize) * nTan), mapSize, 8)
-- --                         | ra == pi || ra == 2*pi || ra == 0 = (py, px, 0, 0, 0) -- looking up down

-- --         findVCollision :: (Int, Int, Int, Int, Int) -> (Int, Int)
-- --         findVCollision (ry, rx, _, _, 0)     = (rx, ry)
-- --         findVCollision (ry, rx, yo, xo, dof) | constraint && not (isNotMapBorder (gameMap !! mapY !! mapX)) = (rx, ry)
-- --                                              | otherwise                                                    = findVCollision (ry+yo, rx+xo, yo, xo, dof-1)
-- --             where
-- --                 mapY   = ry `div` mapSize
-- --                 mapX   = rx `div` mapSize
-- --                 constraint = mapX < mapW-1 && mapY < mapH-1

-- --         rowRayEndPoint = findHCollision $ firstHorizon ra
-- --         rowRayOnBg  = drawLine (midX, midY) rowRayEndPoint (0,255,255) bg

-- --         colRayEndPoint = findVCollision $ firstColumn ra
-- --         colRayOnBg  = drawLine (midX, midY) colRayEndPoint (255,0,255) rowRayOnBg


-- -- todo add support for placing sprites on image e.g. grapes and loading stations
-- renderMap :: Grid -> Background 
-- renderMap gameMap | rows * mapSize == mapHeight &&
--                     cols * mapSize * 2 == mapWidth = drawImg bg drawWalls
--                   | otherwise                      = error "Map must be a 16x16 grid"
--     where 
--         bg = generateBg mapWidth mapHeight (29,29,29)
--         rows       = length gameMap
--         cols       = length (head gameMap)

--         drawWalls :: PixelFunc
--         drawWalls bg (x, y) | isInLeftHalf && 
--                               row < rows && 
--                               col < cols && 
--                               not(isNotMapBorder (gameMap !! row !! col)) = createColor(0,0,0,255) -- wall colour
--                             | otherwise                                   = pixelAt (img bg) x y
--             where 
--                 isInLeftHalf = x <= (mapWidth * mapSize) `div` 2
--                 col = x `div` mapSize
--                 row = y `div` mapSize

-- rayCast :: Player -> Background -> Grid -> Background
-- rayCast player renderedMap gameMap = rayOnBg
--     where -- make it efficient ok ray cast
--         playerOnBg     = drawRect (round (posx player), round (posy player)) (size player) (size player) (255,0,0) renderedMap
--         playerRays     = castRay 1 player playerOnBg gameMap 60
--         midX           = posx player + (fromIntegral (size player)/2)
--         midY           = posy player + (fromIntegral (size player)/2)
--         dirStartCoords = (round midX, round midY)
--         dirEndCoords   = (round (midX + deltaX player *3), round (midY + deltaY player *3))
--         rayOnBg  = drawLine dirEndCoords dirStartCoords (0,255,255) playerRays
        

-- main = do
--     -- let startAngle = -1* (pi/4 +1)
--     let startAngle = -1*pi/2
--         player     = Player {posx = fromIntegral (4*mapSize), posy = fromIntegral (14*mapSize), size = mapSize, deltaX = 5* cos startAngle, deltaY = 5* sin startAngle, angle = startAngle}
--         bg         = renderMap maze

--     print $ deltaX player
--     print $ deltaY player
    
--     saveImage (rayCast player bg maze) "gameState"
        
