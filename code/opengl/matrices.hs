{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad
import Data.Maybe
import Data.Vec
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import System.Exit

data GLIDs = GLIDs
	{ progId :: !GLuint
	, vertexArrayId :: !GLuint
	, vertexAttrib :: !GLuint
	, vertexBufferId :: !GLuint
	, colorAttrib :: !GLuint
	, colorBufferId :: !GLuint
	, mvpMatrixUniform :: !GLint
	}

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

initialize :: IO GLFW.Window
initialize = do
	ok <- GLFW.init
	when (not ok) $ do
		_ <- fail "Failed to initialize GLFW"
		exitFailure
	mapM_ GLFW.windowHint
		[ GLFW.WindowHint'Samples 4 -- 4x antialiasing
		, GLFW.WindowHint'ContextVersionMajor 3 -- OpenGL 3.3
		, GLFW.WindowHint'ContextVersionMinor 3
		, GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
		]

	win <- GLFW.createWindow 800 600 "Window Title" Nothing Nothing
	when (isNothing win) $ do
		_ <- fail "Failed to create OpenGL window"
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
	progId <- loadProgram
		("vertexShader2", vertexShader2)
		("fragmentShader2", fragmentShader2)
	v <- withCString "vertexPosition_modelspace"
		$ glGetAttribLocation progId
	c <- withCString "vertexColor" $ glGetAttribLocation progId
	m <- withCString "MVP" $ glGetUniformLocation progId
	vertexAttrib <- findAttribUniform v "vertexPosition_modelspace"
	colorAttrib <- findAttribUniform c "vertexColor"
	mvpMatrixUniform <- findAttribUniform m "MVP"
	vertexArrayId <- newVAO
	vertexBufferId <- fillNewBuffer vertexBufferData
	colorBufferId <- fillNewBuffer colorBufferData
	return GLIDs{..}
	where
	vertexBufferData :: [GLfloat]
	vertexBufferData =
		-- x, y, z
		[ -1, -1, 0
		,  1, -1, 0
		,  0,  1, 0
		]
	colorBufferData :: [GLfloat]
	colorBufferData =
		[ 1, 0, 0
		, 0, 1, 0
		, 0, 0, 1
		]
	findAttribUniform x name = if x < 0
		then error $ "`" ++ name ++ "' cannot be found!"
		else return $ fromIntegral x

freeResources :: GLIDs -> IO ()
freeResources GLIDs{..} = do
	with vertexBufferId $ glDeleteBuffers 1
	with colorBufferId $ glDeleteBuffers 1
	with vertexArrayId $ glDeleteVertexArrays 1

newVAO :: IO GLuint
newVAO = do
	vaId <- withNewPtr (glGenVertexArrays 1)
	glBindVertexArray vaId
	return vaId

fillNewBuffer :: [GLfloat] -> IO GLuint
fillNewBuffer xs = do
	bufId <- withNewPtr (glGenBuffers 1)
	glBindBuffer gl_ARRAY_BUFFER bufId
	withArrayLen xs func -- give given vertices to OpenGL
	return bufId
	where
	func len ptr = glBufferData
		gl_ARRAY_BUFFER
		(fromIntegral (len * sizeOf (undefined :: GLfloat)))
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

loadProgram :: (String, String) -> (String, String) -> IO GLuint
loadProgram vertShader fragShader = do
	shaderIds <- mapM (uncurry loadShader)
		[ (gl_VERTEX_SHADER, vertShader)
		, (gl_FRAGMENT_SHADER, fragShader)
		]
	progId <- glCreateProgram
	putStrLn "Linking program"
	mapM_ (glAttachShader progId) shaderIds
	glLinkProgram progId
	_ <- checkStatus
		gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog progId
	mapM_ glDeleteShader shaderIds
	return progId

loadShader :: GLenum -> (String, String) -> IO GLuint
loadShader shaderTypeFlag (name, code) = do
	shaderId <- glCreateShader shaderTypeFlag
	withCString code $ \codePtr ->
		with codePtr $ \codePtrPtr ->
			glShaderSource shaderId 1 codePtrPtr nullPtr
	putStrLn $ "Compiling shader `" ++ name ++ "'"
	glCompileShader shaderId
	_ <- checkStatus
		gl_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog shaderId
	return shaderId

checkStatus :: (Integral a1, Storable a1)
	=> GLenum
	-> (t -> GLenum -> Ptr a1 -> IO a)
	-> (t -> a1 -> Ptr a3 -> Ptr Foreign.C.Types.CChar -> IO a2)
	-> t
	-> IO Bool
checkStatus statusFlag glGetFn glInfoLogFn componentId = do
	let
		fetch info = withNewPtr (glGetFn componentId info)
	status <- liftM toBool $ fetch statusFlag
	logLength <- fetch gl_INFO_LOG_LENGTH
	when (logLength > 0) $
		allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
			_ <- glInfoLogFn componentId logLength nullPtr msgPtr
			msg <- peekCString msgPtr
			(if status then putStrLn else fail) msg
	return status

fragmentShader2 :: String
fragmentShader2 = unlines
	[ "#version 330 core"
	, "in vec3 fragmentColor;"
	, "out vec3 finalColor;"
	, "void main()"
	, "{"
		, "finalColor= fragmentColor;"
	, "}"
	]

vertexShader2 :: String
vertexShader2 = unlines
	[ "#version 330 core"
	-- Input vertex data, different for all executions of this shader.
	, "in vec3 vertexPosition_modelspace;"
	, "in vec3 vertexColor;"
	-- Values that stay constant for the whole mesh
	, "uniform mat4 MVP;"
	, "out vec3 fragmentColor;"
	, "void main()"
	, "{"
		, "fragmentColor = vertexColor;"
		, "vec4 v = vec4(vertexPosition_modelspace, 1);"
		, "gl_Position = MVP * v;"
	, "}"
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
	-- the (fromBool True) is because we are ROW-first (Data.Vec)
	with mvpMatrix
		$ glUniformMatrix4fv mvpMatrixUniform 1 (fromBool True)
		. castPtr
	bindBufferToAttrib vertexBufferId vertexAttrib
	bindBufferToAttrib colorBufferId colorAttrib
	glDrawArrays gl_TRIANGLES 0 3
	glDisableVertexAttribArray colorAttrib
	glDisableVertexAttribArray vertexAttrib

-- Some higher-order math helper functions. Depending on what math
-- library you use, you'd use the functions that comes with that
-- library. The functions here are from the Data.Vec package.
vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z:. ()

mvpMatrix :: Mat44 GLfloat
mvpMatrix = multmm (multmm projection view) model
	where
	projection = perspective 0.1 100 (pi/4) (4/3)
	view = lookAt (vec3 4 3 3) (vec3 0 0 0) (vec3 0 1 0)
	model = identity

-- The closest relative to this function is Data.Vec's `rotationLookAt`. We just
-- mirror the code found in the GLM library (glm.g-truc.net). An additional
-- resource is Jeremiah van Oosten's "Understanding the View Matrix", found at
-- http://3dgep.com/?p=1700.
lookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
lookAt eye target up = x :. y :. z :. h :. ()
	where
	forward = normalize $ target - eye
	right = normalize $ cross forward up
	up' = cross right forward
	x = snoc right (-(dot right eye))
	y = snoc up' (-(dot up' eye))
	z = snoc (-forward) (dot forward eye)
	h = 0 :. 0 :. 0 :. 1 :. ()
