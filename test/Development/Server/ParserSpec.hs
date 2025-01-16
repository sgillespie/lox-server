module Development.Server.ParserSpec (spec) where

import Development.Lox.Server.Parser (parseLox, parseLoxFile, readLoxFile)
import Development.Lox.Server.Types
import Paths_obloxious (getDataFileName)

import Control.Exception (try)
import Development.Lox.Server.Span (LocatedLoxProgram, Position (..), mkLocated)
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

    it "multiple-errors.lox" $ do
      loxFile <- getDataFileName "test/data/multiple-errors.lox"
      res <- try @LoxError (parseLoxFile loxFile)
      case res of
        Right _ -> expectationFailure "Expected an error!"
        Left LoxFileNotFound -> expectationFailure "Expected a parsing error!"
        Left (LoxParsingErrors errs) ->
          length errs `shouldBe` 2

  describe "parseLox" $
    it "hello.lox" $ do
      loxFile <- getDataFileName "test/data/hello.lox"
      loxText <- readLoxFile loxFile

      let prog = parseLox Nothing loxText
      prog `shouldBe` Right helloWorldProg

helloWorldProg :: LocatedLoxProgram
helloWorldProg =
  LoxProgram
    [ PrintStmt
        (LoxString "Hello, world!" (mkRange (1, 6) (1, 21)))
        (mkRange (1, 0) (1, 22))
    ]
  where
    mkRange (l1, c1) (l2, c2) = mkLocated (Position l1 c1) (Position l2 c2)
