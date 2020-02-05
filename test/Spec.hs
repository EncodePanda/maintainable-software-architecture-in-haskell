import           FM.DoStuffSpec as FM.DoStuffSpec
import           Hedgehog.Main
main :: IO ()
main = defaultMain
 [ FM.DoStuffSpec.spec
 ]
