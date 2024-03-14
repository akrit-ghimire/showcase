import Terminal (tellUser, clearScreen, fmt, Style(Red, Blue, Magenta), wait)
import Pathfind (Grid, Direction(Start,Up,Down,MoveRight,MoveLeft, TurnLeft, TurnRight, End), algorithm, createPath, Path(Nil), getFullPath, getPathDirections, printPoints, findCoordOf, addTurnPoints)
import Raycast  (Player(Player, posx, posy, size, deltaX, deltaY, angle), createRaycastImage, mapSize)
import Picture (saveImage)
import Data.Maybe (fromJust)

turtleName :: String
turtleName = "TURTLE-BOT"

outputFilePath :: String 
outputFilePath = "output"

main :: IO ()
main = do
    clearScreen
    -- get path to maze file
    
    tellUser turtleName "Hello there, I am Turtle Bot. Yes, that is my name lol"
    tellUser turtleName "I'm feeling a tad bit restless and bored. Can I solve a maze for you???" 
    tellUser turtleName "Please enter the filepath for a maze.txt file so that I can kill this boredom!!!\n"
    
    putStrLn $ fmt Blue "--> "
    filePath <- getLine 
    -- read maze file
    fileContents <- readFile filePath
    let maze   = lines fileContents
        width  = length $ head maze 
        height = length maze
    -- check maze file dimension is valid - else err
    let mazeSizeConstraint = width == 16 && height == 16

    if not mazeSizeConstraint then do
        clearScreen
        tellUser turtleName "Unfortunately this maze is not the right size."
        tellUser turtleName "It should be a 16x16 but it is not."
        tellUser turtleName "I'm gonna be a lazy Turtle-Bot and quit the program..."
        tellUser turtleName "instead of letting you try again ;)\n"

        putStrLn $ fmt Red "Maze Size Error"
    else do
    -- check pathfinder has path - else err
        let startCoord = findCoordOf 'S' maze 
            endCoord   = findCoordOf 'G' maze

        if startCoord == Nothing || endCoord == Nothing then do 
            clearScreen
            tellUser turtleName "Hmmph. I'm having a hard time figuring out where the maze starts"
            tellUser turtleName "and where it stops. Have you included these markers???"
            tellUser turtleName "I'm gonna be a lazy Turtle-Bot and quit the program..."
            tellUser turtleName "instead of letting you try again ;)\n"

            putStrLn $ fmt Red "Missing Maze Data Error"
        else do   
            let value = algorithm maze [createPath(Nil, fromJust startCoord, 0, 0, 0)] [] (fromJust startCoord) (fromJust endCoord) 
            case value of
                Nothing -> do
                    clearScreen
                    tellUser turtleName "Hmmph. I'm having a hard time solving this maze."
                    tellUser turtleName "Have you enclosed the start or end point at all???"
                    tellUser turtleName "I'm gonna be a lazy Turtle-Bot and quit the program..."
                    tellUser turtleName "instead of letting you try again ;)\n"

                    putStrLn $ fmt Red "Maze Path Error" 

                Just x -> do
                    clearScreen
                    tellUser turtleName "Yay. This is a lovely maze. I've figured it out."
                    tellUser turtleName "Are you ready to watch me solve it in real time?!?!"
                    tellUser turtleName "Make sure you're watching my body-cam before pressing enter."
                    tellUser turtleName "This is the 'Body Cam' tab in Tools.html\n"

                    -- ask user ready to start
                    putStrLn $ fmt Blue "-->/Press Enter To Begin/--> "
                    waitOnEnter <- getLine

                    -- loop through all frames till finished
                    -- -- raycast each picture and save it
                    clearScreen
                    
                    let directions   = addTurnPoints $ getPathDirections $ reverse $ getFullPath x
                        startAngle   = decideStartAngle (head directions)
                        (posx, posy) = fromJust startCoord
                        turtle       = Player {posx = fromIntegral (posx * mapSize), posy = fromIntegral (posy*mapSize), size = mapSize, deltaX = 5* cos startAngle, deltaY = 5* sin startAngle, angle = startAngle}
                    
                    -- print (Start:directions) -- debug to see what path directions turtle is following
                    frameLoop maze turtle (Start:directions) -- clears screen itself after done

                    tellUser turtleName "I'm quite chuffed to bits and proud of myself lol."
                    tellUser turtleName "These little metallic turtle legs have carried me all the way"
                    tellUser turtleName "to the end of your supposedly 'difficult' maze."
                    tellUser turtleName "But, I'm quite tired now so I shall go back to sleep.\n"

                    putStrLn $ fmt Red "<--/The End Lol/-->\n"

    -- end

decideStartAngle :: Direction -> Float 
decideStartAngle Up       = -1* pi/2
decideStartAngle Down     = pi/2
decideStartAngle MoveLeft = pi
decideStartAngle any      = 0

displayTurtle :: Player -> Grid -> IO()
displayTurtle turtle maze = do 
    saveImage (createRaycastImage turtle maze) outputFilePath
    wait 500

moveX :: Int -> Player -> Grid -> IO Player
moveX step turtle maze = do 
    let newT = Player {posx = posx turtle + fromIntegral step, posy = posy turtle, size = size turtle, deltaX = deltaX turtle, deltaY = deltaY turtle, angle = angle turtle}
    displayTurtle newT maze
    return newT

moveY:: Int -> Player -> Grid -> IO Player
moveY step turtle maze = do 
    let newT = Player {posx = posx turtle, posy = posy turtle - fromIntegral step, size = size turtle, deltaX = deltaX turtle, deltaY = deltaY turtle, angle = angle turtle}
    displayTurtle newT maze
    return newT

turn :: Float -> Int -> Player -> Grid -> IO Player
turn _ 0 turtle _        = return turtle
turn rad num turtle maze = do 
    let newAngle = angle turtle + rad
        newT  = Player {posx = posx turtle, posy = posy turtle, size = size turtle, deltaX = 5* cos newAngle, deltaY = 5* sin newAngle, angle = newAngle}
    displayTurtle newT maze
    turn rad (num-1) newT maze

frameLoop :: Grid -> Player -> [Direction] -> IO()
frameLoop maze turtle [] = clearScreen 
frameLoop maze turtle (d:dirs) | d == MoveLeft = do 
                                    newT <- moveX (-16) turtle maze
                                    frameLoop maze newT dirs

                               | d == MoveRight = do 
                                    newT <- moveX 16 turtle maze
                                    frameLoop maze newT dirs

                               | d == Up = do 
                                    newT <- moveY 16 turtle maze
                                    frameLoop maze newT dirs

                               | d == Down = do 
                                    newT <- moveY (-16) turtle maze
                                    frameLoop maze newT dirs

                               | d == TurnRight = do 
                                    newT <- turn (pi/8) 4 turtle maze
                                    frameLoop maze newT dirs
                                
                               | d == TurnLeft = do 
                                    newT <- turn (-pi/8) 4 turtle maze
                                    frameLoop maze newT dirs
                                           
                               | otherwise = do 
                                    saveImage (createRaycastImage turtle maze) outputFilePath
                                    wait 1000
                                    frameLoop maze turtle dirs