import Vector
import BVH
import Matrix
import HRT
import Model

main :: IO ()
main = do
  cube@(Model ts ms) <- wavefrontObj "resources/cube-untextured.obj"
  writeFB $ traceRays (Vec3 1 1 (-5)) (Vec3 0 0 0) cube 160 90
--putStrLn $ show $
--  constructBVH [Tri (Vec3 (2.14) (-7.15) (5.53)) (Vec3 (-7.86) (-0.33) (6.64)) (Vec3 (-8.18) (-3.36) (4.56)),Tri (Vec3 (7.98) (-7.0) (6.47)) (Vec3 (-8.78) (-6.63) (-0.02)) (Vec3 (4.15) (4.49) (4.64))] 2
