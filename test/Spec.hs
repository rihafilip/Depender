import qualified Depender.File.Glob.Spec
import qualified Depender.Pattern.Regex.Spec
import Test.DocTest (doctest)
import Test.Hspec

main :: IO ()
main = do
  putStrLn "Doctests: "
  doctest ["-isrc", "src/"]
  hspec $ do
    Depender.File.Glob.Spec.spec
    Depender.Pattern.Regex.Spec.spec
