module Linear.OpenGL
  ( m44GLmatrix
  , vertex1P, vertex2P, vertex3P, vertex4P
  , vector1V, vector2V, vector3V, vector4V
  ) where

import Linear
import Linear.Affine
import Graphics.Rendering.OpenGL.GL
import Control.Lens       
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import Linear.OpenGL.MatrixUniforms

glMatrixToM44 :: MatrixComponent a => GLmatrix a -> IO (M44 a)
glMatrixToM44 m = withMatrix m $ \order p ->
    traverse (traverse $ peekElemOff p) (go order)
  where
    go RowMajor =
        V4 (V4  0  1  2  3)
           (V4  4  5  6  7)
           (V4  8  9 10 11)
           (V4 12 13 14 15)
    go ColumnMajor =
        V4 (V4  0  4  8 12)
           (V4  1  5  9 13)
           (V4  2  6 10 14)
           (V4  3  7 11 15)
{-# INLINABLE glMatrixToM44 #-}

m44ToGLmatrix :: MatrixComponent a => M44 a -> IO (GLmatrix a)
m44ToGLmatrix m = withNewMatrix RowMajor go
  where
    go n = undefined
{-# INLINABLE m44ToGLmatrix #-}
  
m44GLmatrix :: MatrixComponent a => Iso' (M44 a) (GLmatrix a)
m44GLmatrix = iso (unsafePerformIO . m44ToGLmatrix) (unsafePerformIO . glMatrixToM44)
{-# INLINE m44GLmatrix #-}

vertex1P :: Iso' (Point V1 a) (Vertex1 a)
vertex1P = iso to from
  where
    to (P (V1 x)) = Vertex1 x
    from (Vertex1 x) = P (V1 x)
{-# INLINABLE vertex1P #-}

vertex2P :: Iso' (Point V2 a) (Vertex2 a)
vertex2P = iso to from
  where
    to (P (V2 x y)) = Vertex2 x y
    from (Vertex2 x y) = P (V2 x y)
{-# INLINABLE vertex2P #-}

vertex3P :: Iso' (Point V3 a) (Vertex3 a)
vertex3P = iso to from
  where
    to (P (V3 x y z)) = Vertex3 x y z
    from (Vertex3 x y z) = P (V3 x y z)
{-# INLINABLE vertex3P #-}

vertex4P :: Iso' (Point V4 a) (Vertex4 a)
vertex4P = iso to from
  where
    to (P (V4 x y z w)) = Vertex4 x y z w
    from (Vertex4 x y z w) = P (V4 x y z w)
{-# INLINABLE vertex4P #-}

vector1V :: Iso' (V1 a) (Vector1 a)
vector1V = iso to from
  where
    to (V1 x) = Vector1 x
    from (Vector1 x) = V1 x
{-# INLINABLE vector1V #-}

vector2V :: Iso' (V2 a) (Vector2 a)
vector2V = iso to from
  where
    to (V2 x y) = Vector2 x y
    from (Vector2 x y) = V2 x y
{-# INLINABLE vector2V #-}

vector3V :: Iso' (V3 a) (Vector3 a)
vector3V = iso to from
  where
    to (V3 x y z) = Vector3 x y z
    from (Vector3 x y z) = V3 x y z
{-# INLINABLE vector3V #-}

vector4V :: Iso' (V4 a) (Vector4 a)
vector4V = iso to from
  where
    to (V4 x y z w) = Vector4 x y z w
    from (Vector4 x y z w) = V4 x y z w
{-# INLINABLE vector4V #-}
