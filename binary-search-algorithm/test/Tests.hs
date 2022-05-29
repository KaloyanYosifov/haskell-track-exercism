import           Test.Hspec        (Spec, it, shouldBe)
import           Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import           BSA               (search)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
    let list = [1..100]

    it "returns index of number" $
        mapM_ (\x -> search list x `shouldBe` Just (x - 1)) list

    it "returns nothing if not in list" $
        search list 101 `shouldBe` Nothing
