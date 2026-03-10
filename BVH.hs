module BVH where

import Vector

import Data.List (sort)

x = (\(Vec3 a b c) -> a)
y = (\(Vec3 a b c) -> b)
z = (\(Vec3 a b c) -> c)

--data Triangle = Tri Vec3 Vec3 Vec3
--  deriving (Show, Eq)

data AABB = AABB Vec3 Vec3
  deriving (Show, Eq)

data BVH t = Node AABB (BVH t) (BVH t) | Leaf [t]
  deriving (Show, Eq)

class Centroid a where
  centroid :: a -> Vec3

--instance Centroid Triangle where
--  centroid (Tri v0 v1 v2) = (fillv (1.0/3.0)) * (v0 + v1 + v2)

class Limited a where
  minb :: a -> Vec3
  maxb :: a -> Vec3

--instance Limited Triangle where
--  minb (Tri (Vec3 v11 v12 v13) (Vec3 v21 v22 v23) (Vec3 v31 v32 v33)) = Vec3 (min (min v11 v21) v31) (min (min v12 v22) v32) (min (min v13 v23) v33)
--  maxb (Tri (Vec3 v11 v12 v13) (Vec3 v21 v22 v23) (Vec3 v31 v32 v33)) = Vec3 (max (max v11 v21) v31) (max (max v12 v22) v32) (max (max v13 v23) v33)

instance Limited [Vec3] where
  minb vs = Vec3 (foldl min 999999999.0 (map x vs)) (foldl min 999999999.0 (map y vs)) (foldl min 999999999.0 (map z vs))
  maxb vs = Vec3 (foldl max (-999999999.0) (map x vs)) (foldl max (-999999999.0) (map y vs)) (foldl max (-999999999.0) (map z vs))

splitOn :: [a] -> (a -> Bool) -> ([a], [a])
splitOn xs f = splitOn' xs f ([], [])
splitOn' :: [a] -> (a -> Bool) -> ([a], [a]) -> ([a], [a])
splitOn' [] f p = p
splitOn' (x:xs) f (p1, p2) = splitOn' xs f $ if f x then (x:p1, p2) else (p1, x:p2)

constructBVH :: (Centroid a, Limited a) => [a] -> Int -> BVH a
constructBVH cs n
  | (length cs) <= n = Leaf cs
  | otherwise = Node (constructAABB cs) (constructBVH l1 n) (constructBVH l2 n)
    where
      centroids = map centroid cs
      (xs, ys, zs) = let (xs', ys', zs') = (map x centroids, map y centroids, map z centroids) in (sort xs', sort ys', sort zs')
      (xr, yr, zr) = ((last xs) - (head xs), (last ys) - (head ys), (last zs) - (head zs))
      (l1, l2) = 
        if xr > yr then
          if xr > zr then
            splitOn cs (\c -> (x (centroid c)) > (xs!!(((length xs) `div` 2)-1)))
          else
            splitOn cs (\c -> (z (centroid c)) > (zs!!(((length zs) `div` 2)-1)))
        else
          if yr > zr then
            splitOn cs (\c -> (y (centroid c)) > (ys!!(((length ys) `div` 2)-1)))
          else
            splitOn cs (\c -> (z (centroid c)) > (zs!!(((length zs) `div` 2)-1)))

constructAABB :: Limited a => [a] -> AABB
constructAABB ls = AABB (minb (map minb ls)) (maxb (map maxb ls))
