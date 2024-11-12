module Development.Server.ParserSpec (spec) where

import Development.Lox.Server.Parser (parseLox, parseLoxFile)
import Development.Lox.Server.Types
import Paths_obloxious (getDataFileName)

import Test.Syd

spec :: Spec
spec = do
  describe "parseLoxFile" $
    it "hello.lox" $ do
      loxFile <- getDataFileName "test/data/hello.lox"
      prog <- parseLoxFile loxFile
      prog
        `shouldBe` LoxProgram
          [ PrintStmt
              (LoxString "Hello, world!")
          ]

  describe "parseLox" $
    it "hello.lox" $ do
      loxFile <- getDataFileName "test/data/hello.lox"
      loxText <- readFileBS loxFile

      let prog = parseLox (decodeUtf8 loxText)
      prog
        `shouldBeRight` LoxProgram
          [ PrintStmt
              (LoxString "Hello, world!")
          ]

shouldBeRight :: (Eq a, Eq b, Show a, Show b) => Either a b -> b -> IO ()
shouldBeRight actual = (actual `shouldBe`) . Right
