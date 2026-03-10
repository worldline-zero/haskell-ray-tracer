module Vector where

data Vec2 = Vec2 Float Float
  deriving (Show, Eq)
data Vec3 = Vec3 Float Float Float
  deriving (Show, Eq)
data Vec4 = Vec4 Float Float Float Float
  deriving (Show, Eq)

instance Num Vec2 where
  (Vec2 a b) + (Vec2 c d) = Vec2 (a + c) (b + d)
  (Vec2 a b) - (Vec2 c d) = Vec2 (a - c) (b - d)
  (Vec2 a b) * (Vec2 c d) = Vec2 (a * c) (b * d)
  negate v = v * (Vec2 (-1.0) (-1.0))
  abs (Vec2 a b) = Vec2 (abs a) (abs b)
  fromInteger n = Vec2 (fromInteger n) (fromInteger n)
  signum (Vec2 a b) = Vec2 (signum a) (signum b)

instance Num Vec3 where
  (Vec3 a b c) + (Vec3 d e f) = Vec3 (a + d) (b + e) (c + f)
  (Vec3 a b c) - (Vec3 d e f) = Vec3 (a - d) (b - e) (c - f)
  (Vec3 a b c) * (Vec3 d e f) = Vec3 (a * d) (b * e) (c * f)
  negate v = v * (Vec3 (-1.0) (-1.0) (-1.0))
  abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
  fromInteger n = Vec3 (fromInteger n) (fromInteger n) (fromInteger n)
  signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)

instance Num Vec4 where
  (Vec4 a b c d) + (Vec4 e f g h) = Vec4 (a + e) (b + f) (c + g) (d + h)
  (Vec4 a b c d) - (Vec4 e f g h) = Vec4 (a - e) (b - f) (c - g) (d - h)
  (Vec4 a b c d) * (Vec4 e f g h) = Vec4 (a * e) (b * f) (c * g) (d * h)
  negate v = v * (Vec4 (-1.0) (-1.0) (-1.0) (-1.0))
  abs (Vec4 a b c d) = Vec4 (abs a) (abs b) (abs c) (abs d)
  fromInteger n = Vec4 (fromInteger n) (fromInteger n) (fromInteger n) (fromInteger n)
  signum (Vec4 a b c d) = Vec4 (signum a) (signum b) (signum c) (signum d)

class Num a => Vector a where
  (<.>) :: a -> a -> Float -- (<.>) product
  mag :: a -> Float -- length
  vec2 :: a -> Vec2 -- convert to vec2
  vec3 :: a -> Vec3 -- convert to vec3
  vec4 :: a -> Vec4 -- convert to vec4
  idv :: a -- zero vector
  fillv :: Float -> a
  norm :: a -> a
  norm v = v * fillv (1.0 / (sqrt $ mag v))

instance Vector Vec2 where
  (<.>) (Vec2 a b) (Vec2 c d) = (a*c) + (b*d)
  mag v = (<.>) v v
  vec2 v = v
  vec3 (Vec2 a b) = Vec3 a b 0.0
  vec4 (Vec2 a b) = Vec4 a b 0.0 0.0
  idv = Vec2 0.0 0.0
  fillv n = Vec2 n n

instance Vector Vec3 where
  (<.>) (Vec3 a b c) (Vec3 d e f) = (a*d) + (b*e) + (c*f)
  mag v = (<.>) v v
  vec2 (Vec3 a b c) = Vec2 a b
  vec3 v = v
  vec4 (Vec3 a b c) = Vec4 a b c 0.0
  idv = Vec3 0.0 0.0 0.0
  fillv n = Vec3 n n n

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) = Vec3 (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)

instance Vector Vec4 where
  (<.>) (Vec4 a b c d) (Vec4 e f g h) = (a*e) + (b*f) + (c*g) + (d*h)
  mag v = (<.>) v v
  vec2 (Vec4 a b c d) = Vec2 a b
  vec3 (Vec4 a b c d) = Vec3 a b c
  vec4 v = v
  idv = Vec4 0.0 0.0 0.0 0.0
  fillv n = Vec4 n n n n
