module Matrix where

import Vector

data Mat2 = Mat2 Vec2 Vec2
  deriving (Show, Eq)

data Mat3 = Mat3 Vec3 Vec3 Vec3
  deriving (Show, Eq)

data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4
  deriving (Show, Eq)

class Num a => Matrix a where
  transpose :: a -> a
  det :: a -> Float
  idm :: a
  inverse :: a -> a

instance Matrix Mat2 where
  transpose (Mat2 (Vec2 a b) (Vec2 c d)) = Mat2
    (Vec2 a c) (Vec2 b d)
  det (Mat2 (Vec2 a b) (Vec2 c d)) = a*d - b*c 
  idm = Mat2 (Vec2 1 0) (Vec2 0 1)
  inverse m@(Mat2 (Vec2 a b) (Vec2 c d)) = Mat2 (Vec2 (d * x) (-b * x)) (Vec2 (-c * x) (a * x))
    where
      x = 1.0 / det m

instance Num Mat2 where
  (Mat2 a b) + (Mat2 c d) = Mat2 (a + c) (b + d)
  (Mat2 a b) - (Mat2 c d) = Mat2 (a - c) (b - d)
  (Mat2 a b) * m2@(Mat2 c d) = 
    let Mat2 c1 c2 = transpose m2 in
      Mat2 
        (Vec2 ((<.>) a c1) ((<.>) a c2))
        (Vec2 ((<.>) b c1) ((<.>) b c2))
  negate (Mat2 a b) = Mat2 (negate a) (negate b)
  abs (Mat2 a b) = Mat2 (abs a) (abs b)
  fromInteger n = Mat2 (fromInteger n) (fromInteger n)
  signum (Mat2 a b) = Mat2 (signum a) (signum b)


instance Matrix Mat3 where
  transpose (Mat3 (Vec3 a b c) (Vec3 d e f) (Vec3 g h i)) = Mat3
    (Vec3 a d g) (Vec3 b e h) (Vec3 c f i)
  det (Mat3 (Vec3 a b c) (Vec3 d e f) (Vec3 g h i)) = a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h
  idm = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1)
  inverse m@(Mat3 (Vec3 a b c) (Vec3 d e f) (Vec3 g h i)) = Mat3
    (Vec3 (x*(e*i - f*h)) (-x*(b*i - c*h)) (x*(b*f - c*e)))
    (Vec3 (-x*(d*i - f*g)) (x*(a*i - c*g)) (-x*(a*f - c*d)))
    (Vec3 (x*(d*h - e*g)) (-x*(a*h - b*g)) (x*(a*e - b*d)))
      where
        x = 1.0 / det m


instance Num Mat3 where
  (Mat3 a b c) + (Mat3 d e f) = Mat3 (a + d) (b + e) (c + f)
  (Mat3 a b c) - (Mat3 d e f) = Mat3 (a - d) (b - e) (c - f)
  (Mat3 a b c) * m2@(Mat3 d e f) = 
    let Mat3 c1 c2 c3 = transpose m2 in
      Mat3 
        (Vec3 ((<.>) a c1) ((<.>) a c2) ((<.>) a c3))
        (Vec3 ((<.>) b c1) ((<.>) b c2) ((<.>) b c3))
        (Vec3 ((<.>) c c1) ((<.>) c c2) ((<.>) c c3))
  negate (Mat3 a b c) = Mat3 (negate a) (negate b) (negate c)
  abs (Mat3 a b c) = Mat3 (abs a) (abs b) (abs c)
  fromInteger n = Mat3 (fromInteger n) (fromInteger n) (fromInteger n)
  signum (Mat3 a b c) = Mat3 (signum a) (signum b) (signum c)


instance Matrix Mat4 where
  transpose (Mat4 (Vec4 a b c d) (Vec4 e f g h) (Vec4 i j k l) (Vec4 m n o p)) = Mat4
    (Vec4 a e i m) (Vec4 b f j n) (Vec4 c g k o) (Vec4 d h l p)
  det (Mat4 (Vec4 a b c d) (Vec4 e f g h) (Vec4 i j k l) (Vec4 m n o p)) = (
    a * (det $ Mat3 (Vec3 f g h) (Vec3 j k l) (Vec3 n o p)) -
    b * (det $ Mat3 (Vec3 e g h) (Vec3 i k l) (Vec3 m o p)) +
    c * (det $ Mat3 (Vec3 e f h) (Vec3 i j l) (Vec3 m n p)) -
    d * (det $ Mat3 (Vec3 e f g) (Vec3 i j k) (Vec3 m n o)))
  idm = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)
  inverse m = m


instance Num Mat4 where
  (Mat4 a b c d) + (Mat4 e f g h) = Mat4 (a + e) (b + f) (c + g) (d + h)
  (Mat4 a b c d) - (Mat4 e f g h) = Mat4 (a - e) (b - f) (c - g) (d - h)
  (Mat4 a b c d) * m2@(Mat4 e f g h) = 
    let Mat4 c1 c2 c3 c4 = transpose m2 in
      Mat4 
        (Vec4 ((<.>) a c1) ((<.>) a c2) ((<.>) a c3) ((<.>) a c4))
        (Vec4 ((<.>) b c1) ((<.>) b c2) ((<.>) b c3) ((<.>) b c4))
        (Vec4 ((<.>) c c1) ((<.>) c c2) ((<.>) c c3) ((<.>) c c4))
        (Vec4 ((<.>) d c1) ((<.>) d c2) ((<.>) d c3) ((<.>) d c4))
  negate (Mat4 a b c d) = Mat4 (negate a) (negate b) (negate c) (negate d)
  abs (Mat4 a b c d) = Mat4 (abs a) (abs b) (abs c) (abs d)
  fromInteger n = Mat4 (fromInteger n) (fromInteger n) (fromInteger n) (fromInteger n)
  signum (Mat4 a b c d) = Mat4 (signum a) (signum b) (signum c) (signum d)

(<:>) :: Mat2 -> Vec2 -> Vec2
(Mat2 a b) <:> v = Vec2 ((<.>) a v) ((<.>) b v)

infixl 6 <:>

(<:.>) :: Mat3 -> Vec3 -> Vec3
(Mat3 a b c) <:.> v = Vec3 (a <.> v) (b <.> v) (c <.> v)

infixl 6 <:.>

(<::>) :: Mat4 -> Vec4 -> Vec4
(Mat4 a b c d) <::> v = Vec4 (a <.> v) (b <.> v) (c <.> v) (d <.> v)

infixl 6 <::>
