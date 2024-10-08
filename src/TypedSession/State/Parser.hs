{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module TypedSession.State.Parser (runProtocolParser) where

import qualified Data.List as L
import Data.Void (Void)
import Text.Megaparsec hiding (Label, label)
import Text.Megaparsec.Char (space1, string)
import qualified Text.Megaparsec.Char as LC
import qualified Text.Megaparsec.Char.Lexer as L
import TypedSession.State.Type

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

msg, label, branch, branchSt, goto, terminal :: Parser String
msg = symbol "Msg"
label = symbol "Label"
branch = symbol "Branch"
branchSt = symbol "BranchSt"
goto = symbol "Goto"
terminal = symbol "Terminal"

integer :: Parser Integer
integer = lexeme L.decimal

comma :: Parser String
comma = symbol ","

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dbg :: String -> m a -> m a
dbg _ ma = ma

constrOrType :: Parser String
constrOrType = dbg "constrOrType" $ do
  x <- LC.upperChar
  xs <- many LC.alphaNumChar
  spaceConsumer
  pure (x : xs)

mkParserA :: forall a. (Enum a, Bounded a, Show a) => Parser a
mkParserA = do
  let rg = [minBound @a .. maxBound]
      rg' =
        fmap snd
          . L.sortBy (\(a, _) (b, _) -> compare b a)
          $ zip (fmap (length . show) rg) rg
  a <-
    choice $ fmap (\r -> (string (show r) >> pure r)) rg'
  spaceConsumer
  pure a

parseMsg
  :: forall r
   . (Enum r, Bounded r, Show r)
  => Parser (MsgOrLabel Creat r)
parseMsg = dbg "Msg" $ do
  msg
  constr <- constrOrType
  args <- brackets ((some constrOrType) `sepBy` comma)
  from <- mkParserA @r
  to <- mkParserA @r
  pure $ Msg () constr args from to

parseLabel :: (Show r) => Parser (MsgOrLabel Creat r)
parseLabel = dbg "Label" $ do
  label
  i <- fromIntegral <$> integer
  pure $ Label () i

parseGoto :: (Show bst, Show r) => Parser (Protocol Creat r bst)
parseGoto = dbg "Goto" $ do
  goto
  i <- fromIntegral <$> integer
  pure $ Goto () i

parseTerminal :: (Show bst, Show r) => Parser (Protocol Creat r bst)
parseTerminal = dbg "Terminal" $ do
  terminal
  pure $ Terminal ()

parseBranchSt
  :: forall bst r
   . (Enum bst, Bounded bst, Show bst, Enum r, Bounded r, Show r)
  => Parser (BranchSt Creat r bst)
parseBranchSt = dbg "BranchSt" $ do
  branchSt
  bst <- mkParserA @bst
  args <- brackets ((some constrOrType) `sepBy` comma)
  prot <- parseProtocol @r @bst
  pure (BranchSt () bst args prot)

parseBranch
  :: forall r bst
   . (Enum r, Bounded r, Show r, Enum bst, Bounded bst, Show bst) => Parser (Protocol Creat r bst)
parseBranch = dbg "Branch" $ do
  branch
  r1 <- mkParserA @r
  st <- constrOrType
  braces $ do
    branchSts <- some (parseBranchSt @bst @r)
    pure (Branch () r1 st branchSts)

parseMsgOrLabel
  :: forall r bst
   . (Enum r, Bounded r, Show r, Enum bst, Bounded bst, Show bst)
  => Parser (Protocol Creat r bst)
parseMsgOrLabel = dbg "MsgOrLabel" $ do
  msgOrLabel <- choice [parseMsg @r, parseLabel]
  prot <- parseProtocol @r @bst
  pure (msgOrLabel :> prot)

parseProtocol
  :: forall r bst
   . (Enum r, Bounded r, Show r, Enum bst, Bounded bst, Show bst)
  => Parser (Protocol Creat r bst)
parseProtocol = dbg "Protocol" $ do
  choice [parseGoto, parseTerminal, parseBranch @r @bst, parseMsgOrLabel]

runProtocolParser
  :: forall r bst
   . (Enum r, Enum bst, Bounded r, Bounded bst, Show r, Show bst)
  => String
  -> Either String (Protocol Creat r bst)
runProtocolParser st =
  let res = runParser (between spaceConsumer eof $ parseProtocol @r @bst) "" st
   in case res of
        Left e -> Left $ errorBundlePretty @String @Void e
        Right a -> Right a
