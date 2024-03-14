-- Utility Module to create graphical images easily and save them
-- Written by Akrit Ghimire - 09/11/23

module Picture where

import Codec.Picture ( writePng, generateImage, Image, PixelRGBA8(..), PixelRGB8, pixelAt )

data Background = Background {img :: Image PixelRGBA8, width :: Int, height :: Int}
type PixelFunc = Background -> (Int, Int) -> PixelRGBA8

createColor :: (Int, Int, Int, Int) -> PixelRGBA8
createColor (r,g,b,a) = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

generateBg :: Int -> Int -> (Int, Int, Int) -> Background -- generate solid colour img
generateBg w h (r,g,b) = Background {img = img, width = w, height = h}
    where 
        img = generateImage (\_ _ -> createColor(r,g,b,255)) w h

drawRect :: (Int, Int) -> Int -> Int -> (Int,Int,Int) -> Background -> Background
drawRect (x, y) w h (r,g,b) bg = Background {img = newBg, width = (width bg), height = (height bg)}
    where 
        endX = x+w
        endY = y+h
        newBg = generateImage func (width bg) (height bg)
        func imgX imgY | imgX >= x && imgX < endX && imgY >= y && imgY < endY = createColor(r,g,b,255)     -- if in rect range draw col
                       | otherwise                                              = pixelAt (img bg) imgX imgY -- else redraw the existing pixel col

drawLine :: (Int, Int) -> (Int, Int) -> (Int, Int, Int) -> Background -> Background
drawLine (x, y) (x', y') (r, g, b) bg = Background { img = newBg, width = width bg, height = height bg }
    where
        newBg = generateImage func (width bg) (height bg)

        func :: Int -> Int -> PixelRGBA8
        func imgX imgY
            | isOnLine (x, y) (x', y') (imgX, imgY) = createColor (r, g, b, 255)
            | otherwise                             = pixelAt (img bg) imgX imgY

-- allow user to customise placement of each pixel
drawImg :: Background -> PixelFunc -> Background
drawImg bg pixelFunc = newBg
  where 
    newBg = Background {img = generateImage func (width bg) (height bg), width = (width bg), height = (height bg)}
    func xmax ymax = pixelFunc bg (xmax, ymax)

saveImage :: Background -> String -> IO ()
saveImage bg name = writePng (name ++ ".png") (img bg)

-- Math Functions 🤢🤮
calculateGradient :: (Int, Int) -> (Int, Int) -> Float
calculateGradient (x1, y1) (x2, y2) | x2 - x1 == 0 = 1/0 -- don't cause error on zero
                                    | otherwise    = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)

isInsideRect :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool 
isInsideRect (a, b) (c, d) (x, y) = x >= a && x <= c && y >=b && y <= d
isOnLine :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isOnLine (x, y) (x', y') (a, b) | x == x'   = x == a && constraint -- vertical line
                                | y == y'   = b == y && constraint -- horisontal line
                                | otherwise = abs (fromIntegral b - lineFunc a) < 1 && constraint -- pixel Closeness
    where 
        constraint = (a >= min x x' && a <= max x x') && (b >= min y y' && b <= max y y')

        m = calculateGradient (x, y) (x', y')

        lineFunc :: Int -> Float 
        lineFunc a = m * fromIntegral (a - x) + fromIntegral y

