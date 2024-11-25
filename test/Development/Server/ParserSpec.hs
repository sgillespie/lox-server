module Development.Server.ParserSpec (spec) where

import Development.Lox.Server.Parser (parseLox, parseLoxFile)
import Development.Lox.Server.Types
import Paths_obloxious (getDataFileName)

import Test.Syd

spec :: Spec
spec = do
  describe "parseLoxFile" $ do
    it "hello.lox" $ do
      loxFile <- getDataFileName "test/data/hello.lox"
      prog <- parseLoxFile loxFile
      prog `shouldBe` helloWorldProg

    it "parse-error.lox" $ do
      loxFile <- getDataFileName "test/data/parse-error.lox"
      parseLoxFile loxFile `shouldThrow` isLoxParsingError

    it "full-program.lox" $ do
      loxFile <- getDataFileName "test/data/full-program.lox"
      _ <- parseLoxFile loxFile
      -- If we got this far it succeeded
      pure ()

  describe "parseLox" $
    it "hello.lox" $ do
      loxFile <- getDataFileName "test/data/hello.lox"
      loxText <- readFileBS loxFile

      let prog = parseLox Nothing (decodeUtf8 loxText)
      prog `shouldBe` Right helloWorldProg

helloWorldProg :: LoxProgram
helloWorldProg =
  LoxProgram
    [ PrintStmt
        (LoxString "Hello, world!")
    ]
