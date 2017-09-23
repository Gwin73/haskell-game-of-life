module Display (onDisplay) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
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

renderCells :: World -> IO [()]
renderCells = mapM renderCell . aliveCells

renderCell :: (Int, Int) -> IO ()
renderCell (x, y) = rect (Vertex2 tx ty) (Vertex2 (tx+1) (ty+1))
     where (tx, ty) = (fromIntegral x, (fromIntegral y) :: GLfloat) 