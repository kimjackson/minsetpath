module MinSetPathTests where

import MinSetPath
import Test.Hspec
import Text.Printf (printf)

main = hspec $ 
    describe "MinSetPath" $ do 
        context "maxofmins" $ do
            it "should return 2" $
                maxofmins (1,4) (2,6) `shouldBe` 2
        context "overlaps" $ do
            it "should return True" $
                overlaps (1,4) (2,6) `shouldBe` True
            it "should return True" $
                overlaps (2,6) (1,4) `shouldBe` True
            it "should return False" $
                overlaps (1,3) (4,6) `shouldBe` False
            it "should return False" $
                overlaps (4,6) (1,3) `shouldBe` False
        context "dist" $ do
            it "should return 3" $
                dist (1,4) (7,8) `shouldBe` 3
            it "should return 3" $
                dist (7,8) (1,4) `shouldBe` 3
        context "minpath" $ do
            it "should return 0" $ do
                minpath 0 [] `shouldBe` 0
            it "should return 1" $ do
                minpath 1 [] `shouldBe` 1
            it "should return 0" $ do
                minpath 0 [(1,2)] `shouldBe` 0
            it "should return 2" $ do
                minpath 2 [(1,2)] `shouldBe` 2
        context "minsetpath" $ do
            it "should return 3" $
                minsetpath [(1,4),(2,6),(7,8)] `shouldBe` 3
            it "should return 1" $
                minsetpath [(1,4),(2,6),(5,8)] `shouldBe` 1
            it "should return 0" $
                minsetpath [(1,7),(2,6),(2,3)] `shouldBe` 0
            it "should return 3000000000000000" $
                minsetpath [(-1000000000000,1000000000000000),(4000000000000000,9000000000000000)] `shouldBe` 3000000000000000
            context "endpoints" $ do
                it "should return [3,4]" $
                    minsetpath [(1,3),(4,5)] `shouldBe` 1
                it "should return [1,0]" $
                    minsetpath [(1,3),(-2,0)] `shouldBe` 1
                it "should return [3]" $
                    minsetpath [(3,3),(2,4)] `shouldBe` 0
                it "should return [2]" $
                    minsetpath [(1,6),(2,2)] `shouldBe` 0
                it "should return [1,2]" $
                    minsetpath [(1,1),(2,4)] `shouldBe` 1
            context "initial" $ do
                it "should return [4,7]" $
                    minsetpath [(1,4),(2,6),(7,8)] `shouldBe` 3
                it "should return [4,5]" $
                    minsetpath [(1,4),(2,6),(5,8)] `shouldBe` 1
                it "should return [2]" $
                    minsetpath [(1,7),(2,6),(2,3)] `shouldBe` 0
                it "should return [2]" $
                    minsetpath [(2,4),(0,8),(2,3)] `shouldBe` 0
                it "should return [3,6]" $
                    minsetpath [(1,3),(3,5),(6,7)] `shouldBe` 3
            context "trivial, multiple solutions" $ do
                it "should return [2]" $
                    minsetpath [(1,4),(2,6),(0,3)] `shouldBe` 0
                it "should return [3]" $
                    minsetpath [(1,4),(2,6),(3,8)] `shouldBe` 0
                it "should return [3,5]" $
                    minsetpath [(2,4),(1,3),(5,6)] `shouldBe` 2
                it "should return [2,0]" $
                    minsetpath [(2,4),(1,3),(0,0)] `shouldBe` 2
            context "local minima / maxima" $ do
                it "should return [2,3,5]" $
                    minsetpath [(1,2),(3,4),(5,6)] `shouldBe` 3
                it "should return [2,0,5]" $
                    minsetpath [(2,4),(1,3),(0,0),(5,6)] `shouldBe` 7
                it "should return [-10,-11,-2,3,2]" $
                    minsetpath [(-10,-4),(-20,-11),(-2,-1),(3,5),(1,2)] `shouldBe` 16
                it "should return [3,5,8,4,7,5,8]" $
                    minsetpath [(1,3),(5,6),(8,9),(1,4),(7,8),(2,5),(8,9)] `shouldBe` 17
            context "overlaps" $ do
                it "should return [3,4,5,6,5,1]" $
                    minsetpath [(1,3),(4,5),(5,7),(6,8),(4,7),(2,5),(0,1)] `shouldBe` 8
                it "should return [3,5]" $
                    minsetpath [(1,7),(2,6),(2,3),(5,8)] `shouldBe` 2
                it "should return 2" $
                    minsetpath [(2,6),(2,3),(5,8)] `shouldBe` 2

