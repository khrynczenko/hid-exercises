module ParseIPSpec where

import Test.Tasty.Hspec

import IPTypes
import ParseIP

parseIPSpecs :: Spec
parseIPSpecs = describe "ParseIP" $ do
    spec_buildIP
    spec_parseIP

spec_buildIP :: Spec
spec_buildIP =
    describe "buildIP" $ do
        it "builds from zero" $
            buildIP [0, 0, 0, 0] `shouldBe` (IP 0)
        it "builds from one" $
            buildIP [0, 0, 0, 1] `shouldBe` (IP 1)
        it "builds from localhost" $
            buildIP [127, 0, 0, 1] `shouldBe` (IP $ 1 + 127 * 256^3)
        it "builds from arbitrary address" $
            buildIP [192, 168, 3, 15] `shouldBe` (IP $ 15 + 3 * 256 + 168 * 256^2 + 192 * 256^3)

spec_parseIP :: Spec
spec_parseIP =
    describe "parseIP" $ do
        it "parses zero" $
            parseIP "0.0.0.0" `shouldBe` Just (IP 0)
        it "parses one" $
            parseIP "0.0.0.1" `shouldBe` Just (IP 1)
        it "parses localhost" $
            parseIP "127.0.0.1" `shouldBe` Just (IP $ 1 + 127 * 256^3)
        it "parses arbitrary address" $
            parseIP "192.168.3.15" `shouldBe` Just (IP $ 15 + 3 * 256 + 168 * 256^2 + 192 * 256^3)
