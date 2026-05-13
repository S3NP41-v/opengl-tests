{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards, OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LexicalNegation #-}
module Main where


import Foreign

import System.Exit        ( exitFailure )
import System.IO
import System.CPUTime     ( getCPUTime )
import System.Directory
import System.FilePath

import SDL.Vect
import SDL                ( ($=) )
import Codec.Picture      ( readImage, generateImage, convertRGB8
                          , DynamicImage(..), Image(..), Pixel(..)
                          , PixelRGB8(..), imageWidth, imageHeight
                          , imageData, dynamicPixelMap 
                          )

import Control.Monad
import Control.Exception  ( handle, SomeException(..), displayException )
import Data.Maybe         ( fromJust )
import Data.Functor       ( (<&>) )
import Text.Printf        ( printf )
import Linear ()
import Linear.Quaternion ()
import Linear.OpenGL ()

import qualified SDL
import qualified Data.ByteString as BS
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  -- multisampling
  let config = SDL.OpenGLConfig { glColorPrecision = V4 8 8 8 0
                                , glDepthPrecision = 24
                                , glStencilPrecision = 8
                                , glMultisampleSamples = 4
                                , glProfile = SDL.Core SDL.Normal 4 5
                                }

  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
        putStrLn "Linear Texture filtering shit the bed"

  window <- SDL.createWindow "SDL2 + OpenGL" ( SDL.defaultWindow 
    { SDL.windowInitialSize = uncurry V2 res
    , SDL.windowGraphicsContext = SDL.OpenGLContext config
    } )
  
  SDL.showWindow window
  
  _ <- SDL.glCreateContext window
  resources <- initResources
  
  loop resources window
  
  SDL.destroyWindow window
  SDL.quit

loop :: Resources ->  SDL.Window -> IO ()
loop resources window = do
  events <- SDL.pollEvents <&> map SDL.eventPayload
  ticks <- SDL.ticks
  
  -- printScancodes events
  let quit = shouldQuit events

  d0 <- getCPUTime
  draw resources
  SDL.glSwapWindow window
  d1 <- getCPUTime
  
  let delta     = fromIntegral (d1-d0) / 1e12
      totalTime = fromIntegral ticks / 1000
  
  -- (_, deltas) <- takeMVar timing
  -- putMVar timing (delta, take 100 $ delta : deltas)

  unless quit (loop resources{delta=delta, totalTime=totalTime} window)

printScancodes :: [SDL.EventPayload] -> IO ()
printScancodes [] = pure ()
printScancodes (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym sc _ _)) : rest) = do
  SDL.getScancodeName sc >>= putStrLn
  printScancodes rest
printScancodes (_:rest) = printScancodes rest

shouldQuit :: [SDL.EventPayload] -> Bool
shouldQuit [] = False
shouldQuit (SDL.QuitEvent : _) = True
shouldQuit (SDL.WindowClosedEvent _ : _) = True
shouldQuit (SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym sc _ _)) : rest) = SDL.unwrapScancode sc == 20 || shouldQuit rest
shouldQuit (_:rest) = shouldQuit rest


initResources :: IO Resources
initResources = do
  -- multisampling
  GL.multisample $= GL.Enabled
  sb <- GL.get GL.sampleBuffers
  printf "available sample buffers: %d\n" sb
  
  programs <- loadAllPrograms

  -- texture
  tex <- loadTexture (TextureSourceFP "./assets/textures/blade-texture.png") >>= ensure id
  prog <- ensure (<> " initResources looking up program[object]") $ lookup "object" programs :: IO Compiled
  texLoc <- GL.get $ GL.uniformLocation prog.compiledProgram "vTexture"
  GL.uniform texLoc $= GL.TextureUnit 0

  -- depth testing
  GL.depthFunc $= Just GL.Less
  
  -- culling
  GL.cullFace $= Just GL.Back
  
  -- objects
  objects <- loadAllObjects

  let textures      = [ ("vTexture", tex)
                      ]
      arrayObjects  = [ -- ("debug_vao", vao)
                      ]
      resolution    = res
      totalTime     = 0
      delta         = 0


  pure $ Resources {..}

draw :: Resources -> IO ()
draw resources = do
  GL.clearColor $= GL.Color4 0.1 0.1 0.14 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer, GL.StencilBuffer]
  GL.viewport $= (GL.Position 0 0, uncurry GL.Size (both fromIntegral resources.resolution))
  
  
  prog <- ensure (<> " draw object resources.programs") $ lookup "object" resources.programs :: IO Compiled
  obj  <- ensure (<> " draw an object from resources.objects") $ lookup "blade" resources.objects :: IO OBJ
  GL.currentProgram $= Just prog.compiledProgram
  
  -- wiremode 
  -- GL.polygonMode $= (GL.Line, GL.Line)
  
  model       <- ensure (<> " lookup model prog.compiledUniforms")      $ lookup "model" prog.compiledUniforms :: IO GL.UniformLocation
  view        <- ensure (<> " lookup view prog.compiledUniforms")       $ lookup "view" prog.compiledUniforms :: IO GL.UniformLocation
  projection  <- ensure (<> " lookup projection prog.compiledUniforms") $ lookup "projection" prog.compiledUniforms :: IO GL.UniformLocation
  
  lightPos    <- ensure (<> " lookup lightPos prog.compiledUniforms")   $ lookup "lightPos" prog.compiledUniforms :: IO GL.UniformLocation
  viewPos     <- ensure (<> " lookup viewPos prog.compiledUniforms")    $ lookup "viewPos" prog.compiledUniforms :: IO GL.UniformLocation

  let rotQ    = axisAngle (V3 (sin resources.totalTime) 0 (cos resources.totalTime)) (radians (40 * resources.totalTime))
      rotM33  = fromQuaternion rotQ
      rotM33' = rotM33 !!* 0.5
      rotMatrix = mkTransformationMat rotM33' (V3 0 0 0 )
      s = greatestOutreach obj.objRaw.objBounds
      x = 0
      y = 0
      z = 0
      camX   = s + 10 * sin resources.totalTime
      camZ   = s + 10 * cos resources.totalTime
      camPos = V3 camX 0 camZ :: V3 GL.GLfloat
      camTrg = V3 0 0 0 :: V3 GL.GLfloat
      camUp  = V3 0 1 0 :: V3 GL.GLfloat
      camera = SDL.lookAt camPos camTrg camUp
      scaleMatrix = V4 (V4 s 0 0 0)
                       (V4 0 s 0 0)
                       (V4 0 0 s 0)
                       (V4 0 0 0 1) :: M44 GL.GLfloat
      transMatrix = V4 (V4 1 0 0 x)
                       (V4 0 1 0 y)
                       (V4 0 0 1 z)
                       (V4 0 0 0 1) :: M44 GL.GLfloat



      perspMatrix = perspective (radians 45) (fst res / snd res) 0.1 300 :: M44 GL.GLfloat
  
  GL.uniform model      $= transMatrix !*! scaleMatrix !*! rotMatrix
  GL.uniform view       $= camera
  GL.uniform projection $= perspMatrix
  
  GL.uniform lightPos   $= (GL.Vector3 10.0 5.0 10.0 :: GL.Vector3 GL.GLfloat)
  GL.uniform viewPos    $= (GL.Vector3 camX 0 camZ :: GL.Vector3 GL.GLfloat)

  -- TODO WARN: in the object.vert shader we use an inverse function, its very slow,
  -- instead calculate an inverse matrix here and send it as an uniform


  drawOBJ obj


-- | draws from an array of vertices (without indices) of format x, y, z, uv_x, uv_y
-- uses the "default" program from resources, crashes if it does not exist
-- uses the vao of "debug_vao" from resources, crashes if it does not exist
--
-- this function is here as a debugging function
drawFromArray :: Resources -> Int -> IO ()
drawFromArray resources l = do
  prog <- ensure (<> " drawFromArray resources.programs default") $ lookup "default" resources.programs :: IO Compiled
  vao <- ensure (<> " drawFromArray resources.arrayObjects debug_vao") $ lookup "debug_vao" resources.arrayObjects
  
  GL.currentProgram $= Just prog.compiledProgram
  GL.bindVertexArrayObject $= Just vao
  
  -- ttl <- ensure (<> " drawFromArray prog.compiledUniforms totalTime")   $ lookup "totalTime" prog.compiledUniforms
  -- dtl <- ensure (<> " drawFromArray prog.compiledUniforms delta")       $ lookup "delta"     prog.compiledUniforms
  rtl <- ensure (<> " drawFromArray prog.compiledUniforms rotation")    $ lookup "rotation"  prog.compiledUniforms

  -- rotation
  let rotQ    = axisAngle (V3 (sin resources.totalTime) 0.1 (cos resources.totalTime)) (radians (40 * resources.totalTime))
      rotM33  = fromQuaternion rotQ
      rotM33' = rotM33 !!* 0.5
      transformMatrix = mkTransformationMat rotM33' (V3 0 0 0)
  
  -- setting uniforms
  -- GL.uniform ttl $= resources.totalTime
  -- GL.uniform dtl $= resources.delta
  -- GL.uniform rsl $= (uncurry GL.Vector2 (both fromIntegral resources.resolution) :: GL.Vector2 Float)
  GL.uniform rtl $= transformMatrix

  GL.activeTexture $= GL.TextureUnit 0
  GL.drawArrays GL.Triangles 0 (fromIntegral l)
  
  GL.currentProgram $= Nothing
  GL.bindVertexArrayObject $= Nothing


loadTexture :: TextureSource -> IO (Either String GL.TextureObject)
loadTexture = \case
  TextureSourceFP fp -> do
    loadImage fp >>= \case
      Left e    -> pure $ Left e  
      Right img -> fromDynamicImage img <&> Right
  
  TextureSourceDI img -> fromDynamicImage img <&> Right

  _ -> undefined


fromDynamicImage :: DynamicImage -> IO GL.TextureObject
fromDynamicImage img = do
  let rgb8    = convertRGB8 (flipVertically img)
      iWidth  = fromIntegral $ imageWidth  rgb8
      iHeight = fromIntegral $ imageHeight rgb8
      iData   = imageData rgb8

  texb <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just texb

  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
  GL.textureFilter   GL.Texture2D      $= ((GL.Linear', Nothing), GL.Linear')

  V.unsafeWith iData $ \ptr -> do
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8 (GL.TextureSize2D iWidth iHeight) 0 (GL.PixelData GL.RGB GL.UnsignedByte (castPtr ptr))
    GL.generateMipmap' GL.Texture2D
        
  pure texb


loadImage :: FilePath -> IO (Either String DynamicImage)
loadImage path = handle (\(SomeException e) -> pure $ Left $ "Load Image could not read file at " <> path <> " because " <> displayException e) $ do
    readImage path >>= \case
      Left e -> do
        putStrLn $ "WARNING: Load Image could not parse file at " <> path <> " becase " <> e <> "\nReplacing with missing texture"
        pure $ Right $ missingTexture 512 512

      Right img -> pure $ Right img


flipVertically :: DynamicImage -> DynamicImage
flipVertically = dynamicPixelMap flipVertically'


flipVertically' :: (Pixel a) => Image a -> Image a
flipVertically' img@Image {..} =
  generateImage gen imageWidth imageHeight
  where
    gen x y = pixelAt img x (imageHeight - 1 - y)


loadAllPrograms :: IO [(String, Compiled)]
loadAllPrograms = do
  progs <- listDirectory "./shaders"
  mapM (\name -> do 
      prog <- loadProgram name
      pure (name, prog)
    ) progs

loadProgram :: String -> IO Compiled
loadProgram name = do
  let vert = "./shaders" </> name </> name <.> "vert"
      frag = "./shaders" </> name </> name <.> "frag"
  
  compileProgram vert frag

compileProgram :: String -> String -> IO Compiled
compileProgram vertPath fragPath = do
  vert <- compileShader (ShaderSourceFP GL.VertexShader vertPath)   >>= ensure ("Shader Compilation error:\n" <>)
  frag <- compileShader (ShaderSourceFP GL.FragmentShader fragPath) >>= ensure ("Shader Compilation error:\n" <>)
  
  prog <- GL.createProgram
  GL.attachShader prog vert
  GL.attachShader prog frag

  GL.linkProgram prog
  linkOK <- GL.get $ GL.linkStatus prog
  GL.validateProgram prog
  status <- GL.get $ GL.validateStatus prog

  unless (linkOK && status) $ do
    hPutStrLn stderr "Linking the program shit the bed"
    plog <- GL.get $ GL.programInfoLog prog
    putStrLn plog
    exitFailure
  
  GL.currentProgram $= Just prog
  attrs <- GL.activeAttribs prog
  uniforms <- GL.activeUniforms prog
  GL.currentProgram $= Nothing
  
  attrLocations <- mapM (\(i, (_, _, name)) -> do
      let location = GL.AttribLocation i
      GL.attribLocation prog name $= location
      pure (name, location)
    ) $ zip [0..] attrs
  
  uniformLocations <- mapM (\(_, _, name) -> do
      location <- GL.get (GL.uniformLocation prog name)
      pure (name, location)
    ) uniforms
  
  pure $ Compiled prog uniformLocations attrLocations


compileShader :: ShaderSource -> IO (Either String GL.Shader)
compileShader = \case
  ShaderSourceBS t bs -> compileShader' t bs
  ShaderSourceFP t path -> handle (\(SomeException e) -> pure (Left $ "Shader compile could not read file at " <> path <> " because " <> displayException e)) $ do
    bs <- BS.readFile path
    compileShader' t bs

  where
    compileShader' :: GL.ShaderType -> BS.ByteString -> IO (Either String GL.Shader)
    compileShader' st bs = do
      sd <- GL.createShader st
      GL.shaderSourceBS sd $= bs
      GL.compileShader sd
      sdOK <- GL.get $ GL.compileStatus sd

      if sdOK then do
        pure (Right sd)
      
      else do
        err <- GL.get $ GL.shaderInfoLog sd       
        
        pure (Left err)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

toBoth :: (a -> x) -> (a -> y) -> a -> (x, y)
toBoth f g a = (f a, g a)

vectorSize :: (Storable a, Num b) => V.Vector a -> b
vectorSize vs = fromIntegral $ V.length vs * (sizeOf $ V.head vs)

floatSize :: GL.GLsizei
floatSize = (fromIntegral $ sizeOf (0 :: GL.GLfloat)) :: GL.GLsizei

missingTexture :: Int -> Int -> DynamicImage
missingTexture sx sy
  | sx < 2 || sy < 2  = error "missingTexture is undefined for sizes smaller than 2x2"
  | otherwise         = ImageRGB8 $ generateImage (\x y -> if odd ((x `div` rx) `mod` rx) /= odd ((y `div` ry) `mod` ry) then PixelRGB8 251 60 249 else PixelRGB8 0 0 0) sx sy
  where
    d  = if sx < 10 || sy < 10 then 2 else 10
    rx = floor (fromIntegral sx / d :: Float) :: Int
    ry = floor (fromIntegral sy / d :: Float) :: Int



cubeUV :: V.Vector GL.GLfloat
cubeUV = V.fromList 
-- |    Positions     ||    UV    |
    [ -0.5, -0.5, -0.5,  0.0, 0.0
    ,  0.5, -0.5, -0.5,  1.0, 0.0
    ,  0.5,  0.5, -0.5,  1.0, 1.0
    ,  0.5,  0.5, -0.5,  1.0, 1.0
    , -0.5,  0.5, -0.5,  0.0, 1.0
    , -0.5, -0.5, -0.5,  0.0, 0.0
    
    , -0.5, -0.5,  0.5,  0.0, 0.0
    ,  0.5, -0.5,  0.5,  1.0, 0.0
    ,  0.5,  0.5,  0.5,  1.0, 1.0
    ,  0.5,  0.5,  0.5,  1.0, 1.0
    , -0.5,  0.5,  0.5,  0.0, 1.0
    , -0.5, -0.5,  0.5,  0.0, 0.0
    
    , -0.5,  0.5,  0.5,  1.0, 0.0
    , -0.5,  0.5, -0.5,  1.0, 1.0
    , -0.5, -0.5, -0.5,  0.0, 1.0
    , -0.5, -0.5, -0.5,  0.0, 1.0
    , -0.5, -0.5,  0.5,  0.0, 0.0
    , -0.5,  0.5,  0.5,  1.0, 0.0
    
    ,  0.5,  0.5,  0.5,  1.0, 0.0
    ,  0.5,  0.5, -0.5,  1.0, 1.0
    ,  0.5, -0.5, -0.5,  0.0, 1.0
    ,  0.5, -0.5, -0.5,  0.0, 1.0
    ,  0.5, -0.5,  0.5,  0.0, 0.0
    ,  0.5,  0.5,  0.5,  1.0, 0.0
    
    , -0.5, -0.5, -0.5,  0.0, 1.0
    ,  0.5, -0.5, -0.5,  1.0, 1.0
    ,  0.5, -0.5,  0.5,  1.0, 0.0
    ,  0.5, -0.5,  0.5,  1.0, 0.0
    , -0.5, -0.5,  0.5,  0.0, 0.0
    , -0.5, -0.5, -0.5,  0.0, 1.0
    
    , -0.5,  0.5, -0.5,  0.0, 1.0
    ,  0.5,  0.5, -0.5,  1.0, 1.0
    ,  0.5,  0.5,  0.5,  1.0, 0.0
    ,  0.5,  0.5,  0.5,  1.0, 0.0
    , -0.5,  0.5,  0.5,  0.0, 0.0
    , -0.5,  0.5, -0.5,  0.0, 1.0
    ]


data TextureSource
  = TextureSourceBS BS.ByteString
  | TextureSourceFP FilePath
  | TextureSourceDI DynamicImage

data ShaderSource
  = ShaderSourceBS GL.ShaderType BS.ByteString
  | ShaderSourceFP GL.ShaderType FilePath
  deriving ( Show )

data Resources = Resources 
  { programs          :: [(String, Compiled)]
  , textures          :: [(String, GL.TextureObject)]
  , objects           :: [(String, OBJ)]
  , arrayObjects      :: [(String, GL.VertexArrayObject)]

  , resolution        :: (Int, Int)
  , totalTime         :: Float
  , delta             :: Float
  }

data Compiled = Compiled
  { compiledProgram   :: GL.Program
  , compiledUniforms  :: [(String, GL.UniformLocation)]
  , compiledAttrs     :: [(String, GL.AttribLocation)]
  }


res :: Num a => (a, a)
res = (1280, 720)

targetFramerate :: Float
targetFramerate = 120

radians :: Floating a => a -> a
radians f = f * (pi/180)

lookup' :: Eq a => a -> [(a, b)] -> b
lookup' a = fromJust . lookup a

loadAllObjects :: IO [(String, OBJ)]
loadAllObjects = do
  objs <- listDirectory base
  mapM (\path -> parseOBJ (base </> path) <&> (takeBaseName (base </> path), ) ) objs
  where base = "./assets" </> "objects"


-- TODO: expand OBJ functionality in its own module
drawOBJ :: OBJ -> IO ()
drawOBJ obj = do
  -- binding
  GL.bindVertexArrayObject $= Just obj.objVAO
  GL.bindBuffer GL.ElementArrayBuffer $= Just obj.objEBO
  
  -- draw call
  GL.drawElements GL.Triangles (fromIntegral $ V.length obj.objRaw.objIndices) GL.UnsignedInt nullPtr

  -- unbinding
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing


parseOBJ :: FilePath -> IO OBJ
parseOBJ fp = do
  file <- TIO.readFile fp <&> T.lines
  let objRaw = generateInterleaved file
  
  printf "path: %s\n" fp
  printf "interleaved: %d\n" (V.length objRaw.objInterleaved `div` (3 + 2 + 3))
  showInterleaved (V.take (8 * 8) objRaw.objInterleaved) [3, 2, 3]
  printf "indices:     %s | %d\n" (show (V.take 50 objRaw.objIndices)) (V.length objRaw.objIndices)
  printf "normals:     %s | %d\n" (show (V.take 50 objRaw.objNormals)) (V.length objRaw.objNormals `div` 3)
  printf "vertices:    %d\n\n" (V.length objRaw.objVertices `div` 3)

  -- vertex array object (VAO)
  objVAO <- GL.genObjectName
  GL.bindVertexArrayObject $= Just objVAO
  
  -- element buffer
  objEBO <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just objEBO
  V.unsafeWith objRaw.objIndices $ \ptr -> do
    GL.bufferData GL.ElementArrayBuffer $= (vectorSize objRaw.objIndices, ptr, GL.StaticDraw)

  -- data buffer
  objVBO <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just objVBO
  V.unsafeWith objRaw.objInterleaved $ \ptr -> do
    GL.bufferData GL.ArrayBuffer $= (vectorSize objRaw.objInterleaved, ptr, GL.StaticDraw)


  let posAttrib = GL.AttribLocation 0
  GL.vertexAttribPointer posAttrib $= (GL.ToFloat, GL.VertexArrayDescriptor 
    3 GL.Float (fromIntegral $ 8 * floatSize) nullPtr)
  GL.vertexAttribArray posAttrib $= GL.Enabled

  let texAttrib = GL.AttribLocation 1
  GL.vertexAttribPointer texAttrib $= (GL.ToFloat, GL.VertexArrayDescriptor 
    2 GL.Float (fromIntegral $ 8 * floatSize) (plusPtr nullPtr (3 * fromIntegral floatSize)))
  GL.vertexAttribArray texAttrib $= GL.Enabled

  let normAttrib = GL.AttribLocation 2
  GL.vertexAttribPointer normAttrib $= (GL.ToFloat, GL.VertexArrayDescriptor 
    3 GL.Float (fromIntegral $ 8 * floatSize) (plusPtr nullPtr (5 * fromIntegral floatSize)))
  GL.vertexAttribArray normAttrib $= GL.Enabled

  GL.bindVertexArrayObject $= Nothing

  pure OBJ {..}


-- TODO: rewrite this to reduce size of generated vector
-- TODO: rewrite this to handle quad faces
-- TODO: rewrite this to handle cases where there are no vt and/or vn fields
-- TODO: use `partition` here instead or a similiar function to reduce on work needed to be done to parse
-- generate an interleaved vector of [V3 pos | V2 uv | V3 normal] and indices
generateInterleaved :: [T.Text] -> RawOBJ
generateInterleaved lns = let vertexLookup    = parseLookup $ filter ((==) "v " . T.take 2) lns
                              texCoordLookup  = parseLookup $ filter ((==) "vt" . T.take 2) lns
                              normalLookup    = parseLookup $ filter ((==) "vn" . T.take 2) lns
              
                              faceIndices     = parseFace $ filter ((==) "f " . T.take 2) lns
                              populated       = populateVectors ForLookup{..} faceIndices

                              in RawOBJ 
                                  { objVertices     = populated.vertices
                                  , objTexCoords    = populated.texCoords
                                  , objNormals      = populated.normals
                                  , objInterleaved  = interleaveVector populated
                                  , objIndices      = V.fromList $ [0..fromIntegral $ V.length populated.vertices `div` 3]
                                  , objBounds       = calculateBounds vertexLookup
                                  }


-- the greatest absolute distance of any of the bounds directions
greatestOutreach :: Bounds -> GL.GLfloat
greatestOutreach ((x, x'), (y, y'), (z, z')) = maximum $ map abs [x, y, z, x', y', z']

calculateBounds :: V.Vector GL.GLfloat -> Bounds
calculateBounds vs = (toBoth V.minimum V.maximum xs, toBoth V.minimum V.maximum ys, toBoth V.minimum V.maximum zs)
  where
    xs = V.unsafeBackpermute vs $ V.enumFromStepN 0 3 l
    ys = V.unsafeBackpermute vs $ V.enumFromStepN 1 3 l
    zs = V.unsafeBackpermute vs $ V.enumFromStepN 2 3 l
    l  = V.length vs `div` 3

type Bounds = ((GL.GLfloat, GL.GLfloat), (GL.GLfloat, GL.GLfloat), (GL.GLfloat, GL.GLfloat))


-- generate an interleaved vector from populated populateVectors
{-# INLINE interleaveVector #-}
interleaveVector :: ForInterleaving -> V.Vector GL.GLfloat
interleaveVector !ForInterleaving{..}
  | V.null vertices = V.empty
  | otherwise = V.take 3 vertices
             <> V.take 2 texCoords
             <> V.take 3 normals
             <> (interleaveVector $ ForInterleaving (V.drop 3 vertices) (V.drop 2 texCoords) (V.drop 3 normals))


{-# INLINE populateVectors #-}
populateVectors :: ForLookup -> [(Int, Int, Int)] -> ForInterleaving
populateVectors forLookup faceIndices = 
    ForInterleaving 
      { vertices  = V.backpermute forLookup.vertexLookup   $ V.fromList (flat3 vs)
      , texCoords = V.backpermute forLookup.texCoordLookup $ V.fromList (flat2 ts)
      , normals   = V.backpermute forLookup.normalLookup   $ V.fromList (flat3 ns)
      }
    where
      (vs, ts, ns) = unzip3 faceIndices


flat3 :: [Int] -> [Int]
flat3 [] = []
flat3 (x:xs) = 3*x : 3*x+1 : 3*x+2 : flat3 xs

flat2 :: [Int] -> [Int]
flat2 [] = []
flat2 (x:xs) = 2*x : 2*x+1 : flat2 xs

-- v 0.000000 -2.962015 0.000000
-- vt 0.130141 0.292594
-- vn -0.9541 -0.1176 0.2756
parseLookup :: [T.Text] -> V.Vector GL.GLfloat
parseLookup tss = V.fromList $ concatMap (\ts -> map tRead (drop 1 $ T.words ts)) tss

-- f 15/1/1 4/2/1 173/3/1
parseFace :: [T.Text] -> [(Int, Int, Int)]
parseFace tss = concatMap (\ts -> map readFSlash (drop 1 $ T.words ts)) tss


showInterleaved :: (V.Storable a, Show a) => V.Vector a -> [Int] -> IO ()
showInterleaved _ []  = pure ()
showInterleaved vs ss
  | V.null vs = pure ()
  | otherwise = showInterleaved' vs ss
             >> putChar '\n'
             >> showInterleaved (V.drop (sum ss) vs) ss


showInterleaved' :: (V.Storable a, Show a) => V.Vector a -> [Int] -> IO ()
showInterleaved' _  []     = pure ()
showInterleaved' vs (s:ss)
  | V.null vs = pure ()
  | otherwise = do
    V.forM_ (V.take s vs) (\v -> 
      let shown = show v 
      in if odd (length shown) 
         then
           putStr $ ' ' : shown <> " "
         
         else
           putStr $ shown <> " "
      )
    if ss == [] 
    then do
      showInterleaved' (V.drop s vs) ss

    else do
      putStr "|"
      showInterleaved' (V.drop s vs) ss



data ForLookup = ForLookup
  { vertexLookup    :: V.Vector GL.GLfloat
  , texCoordLookup  :: V.Vector GL.GLfloat
  , normalLookup    :: V.Vector GL.GLfloat
  }

data ForInterleaving = ForInterleaving
  { vertices    :: V.Vector GL.GLfloat
  , texCoords   :: V.Vector GL.GLfloat
  , normals     :: V.Vector GL.GLfloat
  }



readFSlash :: T.Text -> (Int, Int, Int)
readFSlash = readFSlash' . T.split (== '/')

readFSlash' :: [T.Text] -> (Int, Int, Int)
readFSlash' [a, b, c] = (max 0 $ tMRead a - 1, max 0 $ tMRead b - 1, max 0 $ tMRead c - 1)
readFSlash' _ = error "could not parse a wavefront file because of badly formatted face (vertex/uv/normal) indices"

tMRead :: T.Text -> Int
tMRead "" = 0
tMRead ts = tRead ts

tRead :: Read a => T.Text -> a
tRead = read . T.unpack


data OBJ = OBJ
  { objVAO      :: GL.VertexArrayObject
  , objEBO      :: GL.BufferObject
  , objVBO      :: GL.BufferObject
  , objRaw      :: RawOBJ
  } deriving ( Show, Eq )

data RawOBJ = RawOBJ
  { objVertices     :: V.Vector GL.GLfloat
  , objTexCoords    :: V.Vector GL.GLfloat
  , objNormals      :: V.Vector GL.GLfloat
  , objInterleaved  :: V.Vector GL.GLfloat
  , objIndices      :: V.Vector GL.GLuint
  , objBounds       :: Bounds
  } deriving ( Show, Eq )


{-
ensure :: (String -> String) -> Either String a -> IO a
ensure f (Left err) = putStrLn (f err) >> exitFailure
ensure _ (Right v)  = pure v
-}

class ResultFail a b where
  ensure :: (String -> String) -> a -> IO b

instance ResultFail (Either String b) b where
  ensure f (Left err) = putStrLn (f err) >> exitFailure
  ensure _ (Right v)  = pure v

instance ResultFail (Maybe b) b where
  ensure f Nothing  = putStrLn (f "ResultFail (Maybe a)") >> exitFailure
  ensure _ (Just v) = pure v

