module Model where

import Vector
import Parsing

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad

-- spec: models are comprised of a list of faces and materials, each face has 3 vertices and an index into the material list
-- todo: add materials
-- todo: add support for faces with arbitrary vertex count
-- todo: add support for texture maps with actual images

-- pos, norm, uv
data Vertex = Vertex Vec3 Vec3 Vec2
  deriving (Show, Eq)

data Triangle = Tri Vertex Vertex Vertex -- Int -- | NFace [Vertex] Int
  deriving (Show, Eq)

data Ambient = Ka Vec3
  deriving (Show, Eq)
data Diffuse = Kd Vec3
  deriving (Show, Eq)
data Specular = Ks Vec3
  deriving (Show, Eq)
data SpecularExp = Ns Float
  deriving (Show, Eq)
data Emissive = Ke Vec3
  deriving (Show, Eq)
data Refractive = Ni Float
  deriving (Show, Eq)
data Transparency = Tr Float
  deriving (Show, Eq)

data Material = Material Ambient Diffuse Specular SpecularExp Emissive Refractive Transparency
  deriving (Show, Eq)

data Model = Model [(Triangle, Int)] [Material]
  deriving (Show, Eq)

wavefrontObj :: String -> IO Model
wavefrontObj f = do
  contents <- readFile f
  let prefix = getPrefix f
  let libs = mtllibs contents prefix
  mats <- reduceListM (map materials libs)
  let faces = elements contents (map snd mats)
  let model = Model faces (map fst mats)
  --putStrLn $ show model
  return model

reduceListM :: Monad m => [ m [a] ] -> m [a]
reduceListM [] = return []
reduceListM (x:[]) = x
reduceListM (x:(y:xs)) = reduceListM ((liftM2 (++) x y):xs)

element = token (some (sat $ (not . isSpace)))

float :: Parser Float
float = do
  d <- integer
  char '.'
  mantissa <- some digit
  return $ read ((show d) ++ "." ++ mantissa)

v2 :: Parser Vec2
v2 = token v2'
v2' = do
  f1 <- float
  f2 <- float
  return $ Vec2 f1 f2

v3 :: Parser Vec3
v3 = token v3'
v3' = do
  f1 <- float
  f2 <- float
  f3 <- float
  return $ Vec3 f1 f2 f3

face :: Parser ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))
face = token face'
face' = do
  f1p <- nat
  char '/'
  f1n <- nat
  char '/'
  f1t <- nat
  space
  f2p <- nat
  char '/'
  f2n <- nat
  char '/'
  f2t <- nat
  space
  f3p <- nat
  char '/'
  f3n <- nat
  char '/'
  f3t <- nat
  return $ ((f1p-1, f1n-1, f1t-1), (f2p-1, f2n-1, f2t-1), (f3p-1, f3n-1, f3t-1))

filePrefix :: Parser String
filePrefix = do
  name <- some (sat (\c -> c /= '/'))
  char '/'
  rest <- some item
  return rest

getPrefix :: String -> String
getPrefix fname = case parse filePrefix (reverse fname) of
  [(p, _)] -> (reverse p) ++ "/"
  [] -> ""

elements :: String -> [String] -> [(Triangle, Int)]
elements inp matnames = elements' inp ([], [], [], matnames, 0) []
elements' :: String -> ([Vec3], [Vec3], [Vec2], [String], Int) -> [(Triangle, Int)] -> [(Triangle, Int)]
elements' "" arrs fs = fs
elements' inp arrs@(ps, ns, ts, ms, idx) fs = case parse element inp of
  [("v", inp')] -> let [(pos, inp'')] = parse (v3 <|> (return $ Vec3 0 0 0)) inp' in
    elements' inp'' (ps ++ [pos], ns, ts, ms, idx) fs
  [("vn", inp')] -> let [(norm, inp'')] = parse (v3 <|> (return $ Vec3 0 0 0)) inp' in
    elements' inp'' (ps, ns ++ [norm], ts, ms, idx) fs
  [("vt", inp')] -> let [(uv, inp'')] = parse (v2 <|> (return $ Vec2 0 0)) inp' in
    elements' inp'' (ps, ns, ts ++ [uv], ms, idx) fs
  [("usemtl", inp')] -> let [(matname, inp'')] = parse (element <|> return "") inp' in
    elements' inp'' (ps, ns, ts, ms, fromMaybe 333333 (elemIndex matname ms)) fs
  [("f", inp')] -> let [(((f1p, f1n, f1t), (f2p, f2n, f2t), (f3p, f3n, f3t)), inp'')] = parse (face <|> return ((0,0,0),(0,0,0),(0,0,0))) inp' in
    elements' inp'' (ps, ns, ts, ms, idx) (fs ++ 
      [(Tri (Vertex (ps!!f1p) (ns!!f1n) (ts!!f1t)) (Vertex (ps!!f2p) (ns!!f2n) (ts!!f2t)) (Vertex (ps!!f3p) (ns!!f3n) (ts!!f3t)), idx)])
  [(_, inp')] -> elements' inp' arrs fs
  _ -> fs

mtllibs :: String -> String -> [String]
mtllibs inp prefix = case parse element inp of
  [("mtllib", inp')] -> 
    let [(matlibname, inp'')] = parse (element <|> return "") inp' in
    [prefix ++ matlibname] ++ (mtllibs inp'' prefix)
  [(_, inp')] -> mtllibs inp' prefix
  _ -> []

    
    

materials :: String -> IO [(Material, String)]
materials fname = do
  inp <- readFile fname
  return $ materials' inp []
materials' :: String -> [(Material, String)] -> [(Material, String)]
materials' "" ms = ms
materials' inp ms = case parse element inp of
  [("newmtl", inp')] -> 
    let [(matname, inp'')] = parse element inp' in
    let (m, inp''') = material inp'' matname in
    materials' inp''' (ms ++ [(m, matname)])
  [(_, inp')] -> materials' inp' ms
  _ -> ms
   
material :: String -> String -> (Material, String)
material inp matname = material' inp (idv, idv, idv, 0.0, idv, 0.0, 0.0)
material' :: String -> (Vec3, Vec3, Vec3, Float, Vec3, Float, Float) -> (Material, String)
material' inp mat@(ka, kd, ks, ns, ke, rf, tr) = case parse element inp of
  [("Ka", inp')] -> let [(ka', inp'')] = parse (v3 <|> (return $ Vec3 0 0 0)) inp' in
    material' inp'' (ka', kd, ks, ns, ke, rf, tr)
  [("Kd", inp')] -> let [(kd', inp'')] = parse (v3 <|> (return $ Vec3 0 0 0)) inp' in
    material' inp'' (ka, kd', ks, ns, ke, rf, tr)
  [("Ks", inp')] -> let [(ks', inp'')] = parse (v3 <|> (return $ Vec3 0 0 0)) inp' in
    material' inp'' (ka, kd, ks', ns, ke, rf, tr)
  [("Ns", inp')] -> let [(ns', inp'')] = parse (float <|> (return 0.0)) inp' in
    material' inp'' (ka, kd, ks, ns', ke, rf, tr)
  [("Ke", inp')] -> let [(ke', inp'')] = parse (v3 <|> (return $ Vec3 0 0 0)) inp' in
    material' inp'' (ka, kd, ks, ns, ke', rf, tr)
  [("Ni", inp')] -> let [(rf', inp'')] = parse (float <|> (return 0.0)) inp' in
    material' inp'' (ka, kd, ks, ns, ke, rf', tr)
  [("d", inp')] -> let [(tr', inp'')] = parse (float <|> (return 0.0)) inp' in
    material' inp'' (ka, kd, ks, ns, ke, rf, tr')
  [("Tr", inp')] -> let [(tr', inp'')] = parse (float <|> (return 0.0)) inp' in
    material' inp'' (ka, kd, ks, ns, ke, rf, 1.0 - tr')
  [("newmtl", inp')] -> (Material (Ka ka) (Kd kd) (Ks ks) (Ns ns) (Ke ke) (Ni rf) (Tr tr), inp)
  [(_, inp')] -> material' inp' mat
  _ -> (Material (Ka ka) (Kd kd) (Ks ks) (Ns ns) (Ke ke) (Ni rf) (Tr tr), inp)
