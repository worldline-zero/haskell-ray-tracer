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

data HitInfo = Hit Float (Triangle, Int) | Miss
  deriving (Show, Eq)

instance Centroid Triangle where
  centroid (Tri (Vertex v0 vn0 vt0) (Vertex v1 vn1 vt1) (Vertex v2 vn2 vt2)) = (fillv (1.0/3.0)) * (v0 + v1 + v2)

instance Centroid (Triangle, Int) where
  centroid (t, i) = centroid t

instance Limited Triangle where
  minb (Tri (Vertex (Vec3 v11 v12 v13) vn1 vt1) (Vertex (Vec3 v21 v22 v23) vn2 vt2) (Vertex (Vec3 v31 v32 v33) vn3 vt3)) =
    Vec3 (min (min v11 v21) v31) (min (min v12 v22) v32) (min (min v13 v23) v33)
  maxb (Tri (Vertex (Vec3 v11 v12 v13) vn1 vt1) (Vertex (Vec3 v21 v22 v23) vn2 vt2) (Vertex (Vec3 v31 v32 v33) vn3 vt3))= 
    Vec3 (max (max v11 v21) v31) (max (max v12 v22) v32) (max (max v13 v23) v33)

instance Limited (Triangle, Int) where
  minb (t, i) = minb t
  maxb (t, i) = maxb t

writeFB :: Framebuffer -> String -> IO ()
writeFB (FB w h xs) name = do
  writeFile name ("P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n" ++ "255\n" ++ (writeFB' xs))
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

mollerTrumbore :: (Triangle, Int) -> Ray -> HitInfo
mollerTrumbore tp@(Tri (Vertex v1 vn1 vt1) (Vertex v2 vn2 vt2) (Vertex v3 vn3 vt3), m_idx) (Ray orig dir) = 
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
        if t > 0.0001 then Hit t tp else Miss

intersectModel :: Model -> Ray -> Color
intersectModel (Model ts ms) r = if length results == 0 then Vec3 0.1 0.1 0.1 else Vec3 1 1 1
  where
    results = filter isHit (map (\t -> mollerTrumbore t r) ts)
    (Hit fmin (triMin, matIdx)) = foldr (\(Hit f tp) (Hit f' tp') -> if f < f' then Hit f tp else Hit f' tp') (head results) results

isHit :: HitInfo -> Bool
isHit Miss = False
isHit (Hit _ _) = True

traceRays :: Vec3 -> Vec3 -> [Model] -> Int -> Int -> Framebuffer
traceRays pos look ms w h =
  FB w h (map (intersectModel $ head ms) (map (\(Ray o d) -> Ray pos (norm $ view <:.> d)) (generateRays w h)))
    where
      forward = norm (look - pos)
      up = Vec3 0 1 0
      right = norm $ cross forward up
      view = inverse $ Mat3 right (cross right forward) forward
