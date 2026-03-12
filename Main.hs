import Vector
import Matrix
import BVH
import HRT
import Model

import Text.Printf

main :: IO ()
main = do
  --cube@(Model ts ms) <- wavefrontObj "resources/cube-untextured.obj"
  --writeFB (traceRays (Vec3 2 (-2) 2) (Vec3 0 0 0) [cube] 400 400) "output.ppm"
  renderVideo (cameraMove 60)

cameraMove :: Int -> [(Vec3, Int)]
cameraMove nframes = map (\x -> 
  let t = 4.0 / fromIntegral nframes in
  ((Vec3 2 (-2.0 + (fromIntegral x) * t) 2), x)) [0..nframes]

--map (\t -> let t' = fromIntegral t * 0.1 in ((Mat3 
--  (Vec3 (cos t') 0 (sin t'))
--  (Vec3 0 1 0)
--  (Vec3 (-1 * sin t') 0 (cos t'))) <:.> (Vec3 3 2 3), t)) [0..nframes]


renderVideo :: [(Vec3, Int)] -> IO ()
renderVideo [] = return ()
renderVideo ((p@(Vec3 a b c), n) : xs) = do
  cube@(Model ts ms) <- wavefrontObj "resources/cube-untextured.obj"
  let fname = printf "output/output%04d.ppm" n
  writeFB (traceRays p (Vec3 0 0 0) [cube] 200 200) fname
  putStrLn fname
  renderVideo xs
--putStrLn $ show $
--  constructBVH [Tri (Vec3 (2.14) (-7.15) (5.53)) (Vec3 (-7.86) (-0.33) (6.64)) (Vec3 (-8.18) (-3.36) (4.56)),Tri (Vec3 (7.98) (-7.0) (6.47)) (Vec3 (-8.78) (-6.63) (-0.02)) (Vec3 (4.15) (4.49) (4.64))] 2
