{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable     (for_)
import Data.String       (fromString)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import TPalindrome (isPalindrome)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "isPalindrome" $ for_ cases test
  where
    test Case{..} = it description $ isPalindrome (fromString input) `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "with an empty word"
               , input       = ""
               , expected    = True
               }
        , Case { description = "with word where reversed equals the original"
               , input       = "abc*cba"
               , expected    = True
               }
        , Case { description = "with multiple words separated by brackets"
               , input       = "abc(ah(otto)(okko)ha)bca"
               , expected    = True
               }
        , Case { description = "when one of the words is not a palindrome"
               , input       = "abc((otto)ah(koko)ha))bca"
               , expected    = False
               }
        ]
