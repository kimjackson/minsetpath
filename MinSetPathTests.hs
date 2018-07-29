module MinSetPathTests where

import MinSetPath
import Test.Hspec
import Text.Printf (printf)

main = hspec $ 
    describe "MinSetPath" $ do 
        context "minsetpath" $ do
            it "should return 3" $
                minsetpath [(1,4),(2,6),(7,8)] `shouldBe` 3
            it "should return 1" $
                minsetpath [(1,4),(2,6),(5,8)] `shouldBe` 1
            it "should return 0" $
                minsetpath [(1,7),(2,6),(2,3)] `shouldBe` 0
            it "should return 3000000000000000" $
                minsetpath [(-1000000000000,1000000000000000),(4000000000000000,9000000000000000)] `shouldBe` 3000000000000000
        context "sumdiffs" $ do
            it "should return 3" $
                sumdiffs [4,5,3] `shouldBe` 3
            it "should return 11" $
                sumdiffs [4,1,0,7] `shouldBe` 11
        context "minpath" $ do
            context "endpoints" $ do
                it "should return [3,4]" $
                    minpath [] [(1,3),(4,5)] `shouldBe` [3,4]
                it "should return [1,0]" $
                    minpath [] [(1,3),(0,-2)] `shouldBe` [1,0]
                it "should return [3]" $
                    minpath [] [(3,3),(2,4)] `shouldBe` [3]
                it "should return [2]" $
                    minpath [] [(1,6),(2,2)] `shouldBe` [2]
                it "should return [1,2]" $
                    minpath [] [(1,1),(2,4)] `shouldBe` [1,2]
            context "initial" $ do
                it "should return [4,7]" $
                    minpath [] [(1,4),(2,6),(7,8)] `shouldBe` [4,7]
                it "should return [4,5]" $
                    minpath [] [(1,4),(2,6),(5,8)] `shouldBe` [4,5]
                it "should return [2]" $
                    minpath [] [(1,7),(2,6),(2,3)] `shouldBe` [2]
                it "should return [2]" $
                    minpath [] [(2,4),(0,8),(2,3)] `shouldBe` [2]
                it "should return [3,6]" $
                    minpath [] [(1,3),(3,5),(6,7)] `shouldBe` [3,6]
            context "trivial, multiple solutions" $ do
                it "should return [2]" $
                    minpath [] [(1,4),(2,6),(0,3)] `shouldBe` [2]
                it "should return [3]" $
                    minpath [] [(1,4),(2,6),(3,8)] `shouldBe` [3]
                it "should return [3,5]" $
                    minpath [] [(2,4),(1,3),(5,6)] `shouldBe` [3,5]
                it "should return [2,0]" $
                    minpath [] [(2,4),(1,3),(0,0)] `shouldBe` [2,0]
            context "local minima / maxima" $ do
                it "should return [2,3,5]" $
                    minpath [] [(1,2),(3,4),(5,6)] `shouldBe` [2,3,5]
                it "should return [2,0,5]" $
                    minpath [] [(2,4),(1,3),(0,0),(5,6)] `shouldBe` [2,0,5]
                it "should return [-10,-11,-2,3,2]" $
                    minpath [] [(-10,-4),(-20,-11),(-2,-1),(3,5),(1,2)] `shouldBe` [-10,-11,-2,3,2]
                it "should return [3,5,8,4,7,5,8]" $
                    minpath [] [(1,3),(5,6),(8,9),(1,4),(7,8),(2,5),(8,9)] `shouldBe` [3,5,8,4,7,5,8]
            context "overlaps" $ do
                it "should return [3,4,5,6,5,1]" $
                    minpath [] [(1,3),(4,5),(5,7),(6,8),(4,7),(2,5),(0,1)] `shouldBe` [3,4,5,6,5,1]
                it "should return [3,5]" $
                    minpath [] [(1,7),(2,6),(2,3),(5,8)] `shouldBe` [3,5]

