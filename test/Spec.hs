import qualified Depender.File.Glob.Spec
import qualified Depender.Configuration.Spec
import Test.DocTest (doctest)
import Test.Hspec
import qualified Depender.Pattern.Yaml.Spec

main :: IO ()
main = do
  putStrLn "Doctests: "
  doctest ["-isrc", "src/"]
  hspec $ do
    Depender.File.Glob.Spec.spec
    Depender.Configuration.Spec.spec
    Depender.Pattern.Yaml.Spec.spec
