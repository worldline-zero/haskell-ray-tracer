module HRT where

import Vector
import Matrix
import BVH
import Model

import System.IO

type Color = Vec3

data Framebuffer = FB Int Int [Color]
  deriving (Show, Eq)

data Ray = Ray Vec3 Vec3
  deriving (Show, Eq)

data HitInfo = Miss | Hit
  deriving (Show, Eq)

instance Centroid Triangle where
  centroid (Tri v0 v1 v2) = (fillv (1.0/3.0)) * (v0 + v1 + v2)

instance Centroid (Triangle, Int) where
  centroid (t, i) = centroid t

instance Limited Triangle where
  minb (Tri (Vec3 v11 v12 v13) (Vec3 v21 v22 v23) (Vec3 v31 v32 v33)) = Vec3 (min (min v11 v21) v31) (min (min v12 v22) v32) (min (min v13 v23) v33)
  maxb (Tri (Vec3 v11 v12 v13) (Vec3 v21 v22 v23) (Vec3 v31 v32 v33)) = Vec3 (max (max v11 v21) v31) (max (max v12 v22) v32) (max (max v13 v23) v33)

instance Limited (Triangle, Int) where
  minb (t, i) = minb t
  maxb (t, i) = maxb t

writeFB :: Framebuffer -> IO ()
writeFB (FB w h xs) = do
  writeFile "output.ppm" ("P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n" ++ "255\n" ++ (writeFB' xs))
  return ()
writeFB' :: [Color] -> String
writeFB' [] = []
writeFB' ((Vec3 r g b):cs) = ((show $ round $ (r*255)) ++ " " ++ (show $ round $ (g*255)) ++ " " ++ (show $ round $ (b*255))) ++ " " ++ (writeFB' cs)

generateRays :: Int -> Int -> [Ray]
generateRays width height = map (\v -> Ray orig (norm (v - orig))) ds
  where
    w = fromIntegral width
    h = fromIntegral height
    f a b = 2.0 * ((fromIntegral a)/(b-1.0)) - 1.0
    ds = concat $ map (\y-> map (\x -> Vec3 (f x w) (-(f y h)) 0) [0..(width-1)]) [0..(height-1)]
    orig = Vec3 0.0 0.0 (-1.0)

view :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Mat4
view right up forward (Vec3 px py pz) = (transpose $ Mat4
  (vec4 right) (vec4 up) (vec4 forward) (Vec4 0 0 0 1.0)) * (Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 (-px) (-py) (-pz) 1.0))

lookAt :: Vec3 -> Vec3 -> Mat4
lookAt pos look = view xaxis yaxis zaxis (-pos)
  where
    up = Vec3 0.0 1.0 0.0
    zaxis = norm (pos - look)
    xaxis = norm (cross up zaxis)
    yaxis = cross zaxis xaxis

mollerTrumbore :: Triangle -> Ray -> HitInfo
mollerTrumbore (Tri v1 v2 v3) (Ray orig dir) = 
--  let a@(Mat3 x y z) = Mat3 (-dir) (v2 - v1) (v3 - v1) in
--  let b = orig - v1 in
--  let da = det a in
--  let t = (det $ Mat3 b y z) / da in
--  let u = (det $ Mat3 x b z) / da in
--  let v = (det $ Mat3 x y b) / da in
--  if abs da < 0.001 then 
--    Miss
--  else 
--    if u < 0.0 || u > 1.0 then
--      Miss
--    else
--      if v < 0.0 || u + v > 1.0 then
--        Miss
--      else
--        if t > 0.0001 then
--          Hit 
--        else
--          Miss
  let e1 = (v2 - v1) in
  let e2 = (v3 - v1) in
  let d = e1 <.> (cross dir e2) in
  if d > -0.0001 && d < 0.0001 then Miss
  else
    let inv_d = 1.0 / d in
    let u = inv_d * ((orig - v1) <.> (cross dir e2)) in
    if (u < 0 && (abs u) > 0.0001) || (u > 1 && (abs (u-1)) > 0.0001) then Miss
    else
      let v = inv_d * (dir <.> (cross (orig - v1) e1)) in
      if (v < 0 && (abs v) > 0.0001) || (u + v > 1 && (abs (u + v - 1)) > 0.0001) then Miss
      else
        let t = inv_d * (e2 <.> (cross (orig - v1) e1)) in
        if t > 0.0001 then Hit else Hit

rotateX :: Float -> Mat3
rotateX a = Mat3
  (Vec3 1 0 0)
  (Vec3 0 (cos a) (- (sin a)))
  (Vec3 0 (sin a) (cos a))

rotateY :: Float -> Mat3
rotateY a = Mat3
  (Vec3 (cos a) 0 (sin a))
  (Vec3 0 1 0)
  (Vec3 (- (sin a)) 0 (cos a))

getRayRotation :: Vec3 -> Mat3
getRayRotation (Vec3 x y z) = 
  let x' = Vec3 0 y z in
  let y' = Vec3 x 0 z in
  let n = Vec3 0 0 1 in
  let tx = -(acos $ (norm x') <.> n) in
  let ty = -(acos $ (norm y') <.> n) in
  (rotateX tx) * (rotateY ty)
  
 
tr m (Tri (Vec3 a b c) (Vec3 d e f) (Vec3 g h i)) = Tri (vec3 (m <::> (Vec4 a b c 1.0))) (vec3 (m <::> (Vec4 d e f 1.0))) (vec3 (m <::> (Vec4 g h i 1.0)))

intersectModel :: Model -> Ray -> Color
intersectModel (Model ts ms) r = if any isHit (map (\t -> mollerTrumbore t r) (map fst ts)) then Vec3 1.0 1.0 1.0 else Vec3 0.0 0.0 0.0

isHit :: HitInfo -> Bool
isHit Miss = False
isHit Hit = True

traceRays :: Vec3 -> Vec3 -> Model -> Int -> Int -> Framebuffer
traceRays pos look model w h = FB w h (map (intersectModel model) (map (\(Ray o d) -> Ray pos (norm ((Mat3 right up forward) <:.> d))) (generateRays w h)))
  where
    forward = norm (look - pos)
    up = Vec3 0 1 0
    right = norm $ cross forward up
