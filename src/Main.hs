module Main where

import qualified Graphics.UI.GLFW   as GLFW
import qualified Graphics.GL.Core44 as GL
import qualified Graphics.GL.Types  as GL
import Control.Exception
import Control.Monad
import Data.Maybe
import Control.Applicative
import Data.Version
import Foreign.Ptr
import Foreign.C.String


main :: IO ()
main = do
  print . showString "GLFW init: " . show =<< GLFW.init
  print =<< GLFW.getVersion
  print =<< GLFW.getVersionString
  bracket 
    (fromJust <$> GLFW.createWindow 200 200 "GLFW-GL Test" Nothing Nothing)
    (GLFW.destroyWindow)
    (withWindow)
  GLFW.terminate

withWindow :: GLFW.Window -> IO ()
withWindow win = do 
  print $ showString "setting context to: " (show win)
  GLFW.makeContextCurrent (Just win)
  Just current <- GLFW.getCurrentContext
  assert (win == current) $ do
    apiName <- GLFW.getWindowClientAPI current
    print . shows apiName . showString ": " . showVersion =<< glVersion current
    print . showString "version: " =<< getString GL.GL_VERSION
    print . showString "vendor: " =<< getString GL.GL_VENDOR
    print . showString "renderer: " =<< getString GL.GL_RENDERER
    print . showString "glsl: " =<< getString GL.GL_SHADING_LANGUAGE_VERSION
 where
  glVersion win = Version 
      <$> sequence [GLFW.getWindowContextVersionMajor win, GLFW.getWindowContextVersionMinor win, GLFW.getWindowContextVersionRevision win]
      <*> sequence [ showString "profile-" . show <$> GLFW.getWindowOpenGLProfile win
                   , showString "robustness-" . show <$> GLFW.getWindowContextRobustness win
                   , showString "forwardCompat-" . show <$> GLFW.getWindowOpenGLForwardCompat win
                   , showString "debug-" . show <$> GLFW.getWindowOpenGLDebugContext  win
                   ]

getString :: GL.GLenum -> IO String
getString = GL.glGetString >=> peekCString . castPtr
