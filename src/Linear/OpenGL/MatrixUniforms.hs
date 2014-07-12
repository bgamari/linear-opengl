{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Linear.OpenGL.MatrixUniforms () where

import Data.Maybe (fromJust)
import Data.Proxy
import Foreign

import Graphics.Rendering.OpenGL.GL hiding (Proxy)
import Graphics.Rendering.OpenGL.Raw.Core31

import Data.Distributive
import Linear

maxComponentSize :: Int
maxComponentSize = sizeOf (undefined :: GLint) `max` sizeOf (undefined :: GLfloat)

maxNumComponents :: Int
maxNumComponents = 16

maxUniformBufferSize :: Int
maxUniformBufferSize = maxComponentSize * maxNumComponents

class UniformMatrix f where
    setUniformMatrix :: Proxy (f (f a)) -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()

instance UniformMatrix V2 where
    setUniformMatrix _ = glUniformMatrix2fv
instance UniformMatrix V3 where
    setUniformMatrix _ = glUniformMatrix3fv
instance UniformMatrix V4 where
    setUniformMatrix _ = glUniformMatrix4fv

getInteger1 :: GLenum -> IO GLint
getInteger1 e = alloca $ \buf -> do
    glGetIntegeri_v e 1 buf
    peek buf

uniformMat :: forall f a.
              ( Storable (f (f a)), UniformMatrix f
              , Distributive f, Conjugate a)
           => UniformLocation -> StateVar (f (f a))
uniformMat (UniformLocation loc) = makeStateVar getter setter
  where
    getter = do
        -- Use this once @Program@ is exported
        --Program p <- fmap fromJust $ get currentProgram
        p <- fromIntegral `fmap` getInteger1 gl_CURRENT_PROGRAM
        allocaBytes maxUniformBufferSize $ \buf -> do
            glGetUniformfv p loc (castPtr buf)
            adjoint `fmap` peek buf
    setter mat = do
        program <- fmap fromJust $ get currentProgram
        allocaBytes maxUniformBufferSize $ \buf -> do
            poke buf (adjoint mat)
            setUniformMatrix (Proxy :: Proxy (f (f a))) loc 1 0 (castPtr buf)

uniformvMat :: forall f a. UniformMatrix f
            => UniformLocation -> GLsizei -> Ptr (f (f a)) -> IO ()
uniformvMat (UniformLocation loc) count ptr =
    setUniformMatrix (Proxy :: Proxy (f (f a))) loc count 0 (castPtr ptr)

-- | given in column-major order
instance Uniform (V2 (V2 GLfloat)) where
    uniform = uniformMat
    uniformv = uniformvMat

-- | given in column-major order
instance Uniform (V3 (V3 GLfloat)) where
    uniform = uniformMat
    uniformv = uniformvMat

-- | given in column-major order
instance Uniform (V4 (V4 GLfloat)) where
    uniform = uniformMat
    uniformv = uniformvMat
