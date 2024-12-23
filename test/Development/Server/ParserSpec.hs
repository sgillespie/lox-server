module Development.Server.ParserSpec (spec) where

import Development.Lox.Server.Parser (parseLox, parseLoxFile)
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

    it "expressions.lox" $ do
      loxFile <- getDataFileName "test/data/expressions.lox"
      _ <- parseLoxFile loxFile
      -- If we got this far it succeeded
      pure ()

    it "statements.lox" $ do
      loxFile <- getDataFileName "test/data/statements.lox"
      _ <- parseLoxFile loxFile
      -- If we got this far it succeeded
      pure ()

    it "declarations.lox" $ do
      loxFile <- getDataFileName "test/data/declarations.lox"
      _ <- parseLoxFile loxFile
      -- If we got this far it succeeded
      pure ()

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
      loxText <- readFileBS loxFile

      let prog = parseLox Nothing (decodeUtf8 loxText)
      prog `shouldBe` Right helloWorldProg

helloWorldProg :: LocatedLoxProgram
helloWorldProg =
  LoxProgram
    [ PrintStmt
        (mkRange (2, 1) (2, 23))
        (LoxString (mkRange (2, 7) (2, 22)) "Hello, world!")
    ]
  where
    mkRange (l1, c1) (l2, c2) = mkLocated (Position l1 c1) (Position l2 c2)
