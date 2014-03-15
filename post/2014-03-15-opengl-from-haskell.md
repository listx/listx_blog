---
title: OpenGL from Haskell
tags: haskell, opengl
mathjax: off
---

The following is my translation/adaptation of [tutorial #2](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-2-the-first-triangle/) at http://www.opengl-tutorial.org.
It draws a single red triangle on the screen with a dark blue background.
It takes quite a bit of boilerplate code just to get something on the screen!

My version uses the code from (https://github.com/YPares/Haskell-OpenGL3.1-Tutos), but does not use `Control.Applicative` and also does not put the shaders containing GLSL in a separate text file.
Everything is self-contained in one file, and it only really uses the `PackageImports` and `RecordWildCards` GHC extensions.
The `PackageImports` is only necessary if you have both `GLFW` and `GLFW-b` Hackage packages installed in your system (as they have a name clash of `Graphics.UI.GLFW`, you need to disambiguate this import by specifying the package name).
The `RecordWildCards` extension is pretty standard and exists purely for syntactic sugar (no type-level hoops and such) --- if you don't know about it you should google it.

I hereby release it into the Public Domain.
From what I can tell, YPares's code doesn't have a license... I think this should be OK.

```{.haskell .numberLines}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import System.Exit

data GLIDs = GLIDs
	{ progId :: !GLuint
	, vertexArrayId :: !GLuint
	, vertexBufferId :: !GLuint
	}

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

initialize :: IO GLFW.Window
initialize = do
	ok <- GLFW.init
	when (not ok) $ do
		fail "Failed to initialize GLFW"
		exitFailure
	mapM_ GLFW.windowHint
		[ GLFW.WindowHint'Samples 4 -- 4x antialiasing
		, GLFW.WindowHint'ContextVersionMajor 3 -- OpenGL 3.3
		, GLFW.WindowHint'ContextVersionMinor 3
		-- we don't want the old OpenGL
		, GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
		]

	win <- GLFW.createWindow 800 600 "Window Title" Nothing Nothing
	when (isNothing win) $ do
		fail "Failed to create OpenGL window"
		GLFW.terminate
		exitFailure
	let
		win' = fromJust win
	GLFW.makeContextCurrent win

	GLFW.setStickyKeysInputMode win' GLFW.StickyKeysInputMode'Enabled

	return win'

initializeGL :: IO GLIDs
initializeGL = do
	glClearColor 0 0 0.4 0
	progId <- loadProgram vertexShader1 fragmentShader1
	vaId <- newVAO
	bufId <- fillNewBuffer vertexBufferData
	return $ GLIDs
		{ progId = progId
		, vertexArrayId = vaId
		, vertexBufferId = bufId
		}

freeResources :: GLIDs -> IO ()
freeResources GLIDs{..} = do
	with vertexBufferId $ glDeleteBuffers 1
	with vertexArrayId $ glDeleteVertexArrays 1

newVAO :: IO GLuint
newVAO = do
	id <- withNewPtr (glGenVertexArrays 1)
	glBindVertexArray id
	return id

fillNewBuffer :: [GLfloat] -> IO GLuint
fillNewBuffer xs = do
	bufId <- withNewPtr (glGenBuffers 1)
	glBindBuffer gl_ARRAY_BUFFER bufId
	withArrayLen xs func -- give given vertices to OpenGL
	return bufId
	where
	func length ptr = glBufferData
		gl_ARRAY_BUFFER
		(fromIntegral (length * sizeOf (undefined :: GLfloat)))
		(ptr :: Ptr GLfloat)
		gl_STATIC_DRAW

bindBufferToAttrib :: GLuint -> GLuint -> IO ()
bindBufferToAttrib bufId attribLoc = do
	glEnableVertexAttribArray attribLoc
	glBindBuffer gl_ARRAY_BUFFER bufId
	glVertexAttribPointer
		attribLoc -- attribute location in the shader
		3 -- 3 components per vertex
		gl_FLOAT -- coord type
		(fromBool False) -- normalize?
		0 -- stride
		nullPtr -- vertex buffer offset

loadProgram :: String -> String -> IO GLuint
loadProgram vertShader fragShader = do
	shaderIds <- mapM (uncurry loadShader)
		[ (gl_VERTEX_SHADER, vertShader)
		, (gl_FRAGMENT_SHADER, fragShader)
		]
	progId <- glCreateProgram
	putStrLn "Linking program"
	mapM_ (glAttachShader progId) shaderIds
	glLinkProgram progId
	checkStatus gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog progId
	mapM_ glDeleteShader shaderIds
	return progId

loadShader :: GLenum -> String -> IO GLuint
loadShader shaderTypeFlag code = do
	id <- glCreateShader shaderTypeFlag
	withCString code $ \codePtr ->
		with codePtr $ \codePtrPtr ->
			glShaderSource id 1 codePtrPtr nullPtr
	putStrLn "Compiling shader..."
	glCompileShader id
	checkStatus gl_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog id
	return id

checkStatus :: (Integral a1, Storable a1)
	=> GLenum
	-> (t -> GLenum -> Ptr a1 -> IO a)
	-> (t -> a1 -> Ptr a3 -> Ptr Foreign.C.Types.CChar -> IO a2)
	-> t
	-> IO Bool
checkStatus statusFlag glGetFn glInfoLogFn id = do
	let
		fetch info = withNewPtr (glGetFn id info)
	status <- liftM toBool $ fetch statusFlag
	logLength <- fetch gl_INFO_LOG_LENGTH
	when (logLength > 0) $
		allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
			glInfoLogFn id logLength nullPtr msgPtr
			msg <- peekCString msgPtr
			(if status then putStrLn else fail) msg
	return status

fragmentShader1 :: String
fragmentShader1 = unlines
	[ "#version 330 core"
	, "out vec3 color;"
	, "void main()"
	, "{"
		, "color =  vec3(1,0,0);" -- paint it red!
	, "}"
	]

vertexShader1 :: String
vertexShader1 = unlines
	[ "#version 330 core"
	, "layout(location = 0) in vec3 vPosition_modelspace;"
	, "void main()"
	, "{"
		, "gl_Position.xyz = vPosition_modelspace;"
		, "gl_Position.w = 1.0;"
	, "}"
	]

vertexBufferData :: [GLfloat]
vertexBufferData =
	-- x, y, z
	[ -1, -1, 0
	,  1, -1, 0
	,  0,  1, 0
	]

main :: IO ()
main = do
	win <- initialize
	glids <- initializeGL
	inputLoop win glids
	freeResources glids
	GLFW.terminate
	return ()

inputLoop :: GLFW.Window -> GLIDs -> IO ()
inputLoop win glids = do
	drawStuff glids
	GLFW.swapBuffers win
	GLFW.pollEvents
	keyState <- GLFW.getKey win GLFW.Key'Escape
	closeWindow <- GLFW.windowShouldClose win
	when (keyState /= GLFW.KeyState'Pressed && closeWindow == False) $
		inputLoop win glids

drawStuff :: GLIDs -> IO ()
drawStuff GLIDs{..} = do
	glClear gl_COLOR_BUFFER_BIT
	glClear gl_DEPTH_BUFFER_BIT
	glUseProgram progId
	bindBufferToAttrib vertexBufferId 0
	glDrawArrays gl_TRIANGLES 0 3 -- for attrib array 0, draw 3 vertices
	glDisableVertexAttribArray 0 -- disable attrib array 0
```
