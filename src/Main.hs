{-# LANGUAGE PatternSynonyms #-}
module Main where

import qualified Graphics.UI.GLFW          as GLFW
import qualified Graphics.GL.Core44        as GL
import qualified Graphics.GL.Types         as GL
import qualified Graphics.GL.Ext.KHR.Debug as GL
import qualified Graphics.GL.Ext.ARB.ShaderStorageBufferObject as GL
import qualified Graphics.GL.Ext.ARB.SparseTexture as GL
import Control.Exception
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Maybe
import Control.Applicative
import Data.Version
import Foreign.Ptr
import Foreign.C.String
import Text.Printf


windowHints =
  [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
  , GLFW.WindowHint'ContextVersionMajor 4
  , GLFW.WindowHint'ContextVersionMinor 3
  , GLFW.WindowHint'OpenGLForwardCompat True
  , GLFW.WindowHint'OpenGLDebugContext True
  , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  ]

main :: IO ()
main = do
  print . showString "GLFW init: " . show =<< GLFW.init
  print =<< GLFW.getVersion
  print =<< GLFW.getVersionString
  mapM_ GLFW.windowHint windowHints
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
    if GL.gl_KHR_debug 
      then installHook >> testMsg
      else print "gl_KHR_debug not supported"
    printf "gl_ARB_shader_storage_buffer_object: %s\n" (show GL.gl_ARB_shader_storage_buffer_object)
    printf "gl_ARB_sparse_texture: %s\n" (show GL.gl_ARB_sparse_texture)
    GLFW.swapBuffers win
 where
  glVersion win = Version 
      <$> sequence [GLFW.getWindowContextVersionMajor win, GLFW.getWindowContextVersionMinor win, GLFW.getWindowContextVersionRevision win]
      <*> sequence [ showString "profile-" . show <$> GLFW.getWindowOpenGLProfile win
                   , showString "robustness-" . show <$> GLFW.getWindowContextRobustness win
                   , showString "forwardCompat-" . show <$> GLFW.getWindowOpenGLForwardCompat win
                   , showString "debug-" . show <$> GLFW.getWindowOpenGLDebugContext  win
                   ]

  installHook = do
    cb <- GL.mkGLDEBUGPROC glCallback
    GL.glDebugMessageCallback cb nullPtr
    GL.glEnable GL.GL_DEBUG_OUTPUT_SYNCHRONOUS
    print "debug hook installed"

getString :: GL.GLenum -> IO String
getString = GL.glGetString >=> peekCString . castPtr

glCallback :: GL.GLenum -> GL.GLenum -> GL.GLuint -> GL.GLenum -> GL.GLsizei -> Ptr GL.GLchar -> Ptr () -> IO ()
glCallback source t ident severity _ message _ = do
  message' <- peekCString message
  printf "%s:[%s] %s (%d): {%s}\n" priority t' source' ident message'
 where
  source', t' :: String
  source' = case source of
    GL.GL_DEBUG_SOURCE_API               -> "API"
    GL.GL_DEBUG_SOURCE_WINDOW_SYSTEM     -> "Window System"
    GL.GL_DEBUG_SOURCE_SHADER_COMPILER   -> "Shader Compiler"
    GL.GL_DEBUG_SOURCE_THIRD_PARTY       -> "Third Party"
    GL.GL_DEBUG_SOURCE_APPLICATION       -> "Application"
    GL.GL_DEBUG_SOURCE_OTHER             -> "Other"
    _ -> "Unknown"

  t' = case t of
    GL.GL_DEBUG_TYPE_ERROR               -> "Error"
    GL.GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR -> "Deprecated Behaviour"
    GL.GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR  -> "Undefined Behaviour"
    GL.GL_DEBUG_TYPE_PORTABILITY         -> "Portability"
    GL.GL_DEBUG_TYPE_PERFORMANCE         -> "Performance"
    GL.GL_DEBUG_TYPE_OTHER               -> "Other"
    GL.GL_DEBUG_TYPE_MARKER              -> "Marker"
    _ -> "Unknown"

  priority = case severity of
    GL.GL_DEBUG_SEVERITY_HIGH            -> "CRITICAL"
    GL.GL_DEBUG_SEVERITY_MEDIUM          -> "ALERT"
    GL.GL_DEBUG_SEVERITY_LOW             -> "WARNING"
    GL.GL_DEBUG_SEVERITY_NOTIFICATION    -> "NOTICE"
    _ -> "INFO"

testMsg = withCString "An Application Test Msg" $ GL.glDebugMessageInsert GL.GL_DEBUG_SOURCE_APPLICATION GL.GL_DEBUG_TYPE_OTHER 42 GL.GL_DEBUG_SEVERITY_NOTIFICATION (-1)