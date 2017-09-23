import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (guard)
import Control.Monad.Reader 
import Control.Concurrent (threadDelay)
import Simulation
import Display
import System.Random (randomRIO)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    GLFW.init
    defaultWindowHints
    Just win <- GLFW.createWindow 640 640 "Game of Life" Nothing Nothing
    makeContextCurrent $ Just win
    setWindowSizeCallback win $ Just windowResized
    setKeyCallback win $ Just keyPressed
    setWindowCloseCallback win $ Just windowClosed
    let size = read $ args !! 0
    seed <- filterM (\_ -> liftM (==0) $ randomRIO (0, 1 :: Int)) [(x, y) | x <- [1..size] , y <- [1..size]]
    mainloop win $ World size size seed
    destroyWindow win
    terminate

mainloop :: Window -> World -> IO ()
mainloop win world = do
    let world' = runReader nextGen world
    now <- maybe 0 id <$> getTime
    onDisplay win world'
    now' <- maybe 0 id <$> getTime
    let frameLeft = ((recip 7) + now - now')
    when (frameLeft > 0) $
        threadDelay (truncate $ 1000000 * frameLeft)
    pollEvents
    mainloop win world'
   
keyPressed :: KeyCallback 
keyPressed win Key'Escape _ GLFW.KeyState'Pressed _ = windowClosed win
keyPressed _ _ _ _ _ = return ()  

windowClosed :: WindowCloseCallback
windowClosed win = do
  destroyWindow win
  terminate
  _ <- exitWith ExitSuccess
  return ()   

windowResized :: WindowSizeCallback
windowResized win w h = do
    let size = fromIntegral $ min w h
    viewport $= (Position 0 0, Size size size)