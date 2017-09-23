module Display (onDisplay) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import qualified Data.Vector as V
import Control.Monad (when)
import Simulation

onDisplay :: Window -> World -> IO ()
onDisplay win world = do
    clear [ColorBuffer]
    loadIdentity
    let width' = fromIntegral $ width world
    let scaleFactor = 2 / width'
    scale scaleFactor scaleFactor (1 :: GL.GLfloat)
    let offset = negate $ width' / 2 + 1
    translate $ GL.Vector3 offset offset (0 :: GL.GLfloat)
    renderCells world
    GL.flush
    swapBuffers win

renderCells w = V.imapM (\i -> V.imapM (\j-> renderCell (i+1, j+1))) (cells w)

renderCell :: (Int, Int) -> Bool -> IO ()
renderCell (x, y) b = when b $ rect (Vertex2 tx ty) (Vertex2 (tx+1) (ty+1))
    where (tx, ty) = (fromIntegral x, (fromIntegral y) :: GLfloat)