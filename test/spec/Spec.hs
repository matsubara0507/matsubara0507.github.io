import qualified Spec.Utils
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< spec

spec :: IO TestTree
spec = testGroup "App" <$> sequence
  [ testSpec "Utils" Spec.Utils.spec
  ]
