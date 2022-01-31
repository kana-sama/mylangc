{-# LANGUAGE ImportQualifiedPost #-}

module Language.MyLang.Parser
  ( ParserError (..),
    parseFile,
  )
where

import Control.Monad.Combinators.Expr
import Data.Set qualified as Set
import Language.MyLang.CST
import Language.MyLang.Prelude
import Text.Megaparsec hiding (State, match)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser ()
comma = void (symbol ",")

parens, braces, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

integerL :: Parser Integer
integerL = lexeme L.decimal

stringL :: Parser String
stringL = lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

charL :: Parser Char
charL = lexeme (between (char '\'') (char '\'') L.charLiteral)

keywords :: Set String
keywords = Set.fromList ["for", "do", "od", "while", "repeat", "if", "elif", "then", "else", "fi", "skip", "fun", "local"]

ident :: Parser Ident
ident = try do
  name <- lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_") <?> "ident")
  guard (name `Set.notMember` keywords)
  pure name

idents :: Parser [Ident]
idents = ident `sepBy` comma

exprs :: Parser [Expr]
exprs = expr `sepBy` comma

expr :: Parser Expr
expr = expr1

expr1 :: Parser Expr
expr1 = makeExprParser expr2 operatorTable
  where
    operatorTable =
      [ [ binary "*" (BinOp (:*)),
          binary "/" (BinOp (:/)),
          binary "%" (BinOp (:%))
        ],
        [ binary "+" (BinOp (:+)),
          binary "-" (BinOp (:-))
        ],
        [ binary "<=" (BinOp (:<=)),
          binary ">=" (BinOp (:>=)),
          binary "<" (BinOp (:<)),
          binary ">" (BinOp (:>)),
          binary "==" (BinOp (:==)),
          binary "!=" (BinOp (:!=))
        ],
        [ binary "&&" (BinOp (:&&))
        ],
        [ binary "!!" (BinOp (:!!))
        ]
      ]
      where
        binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
        binary name f = InfixL (f <$ symbol name)

expr2 :: Parser Expr
expr2 =
  choice
    [ try do e <- expr3; symbol "."; symbol "length"; pure (Length e),
      expr3
    ]

expr3 :: Parser Expr
expr3 =
  choice
    [ try do e <- expr4; ixes <- some (brackets expr); pure (foldl At e ixes),
      expr4
    ]

expr4 :: Parser Expr
expr4 =
  choice
    [ parens expr,
      try do name <- ident; args <- parens exprs; pure (Apply name args),
      Var <$> ident,
      Number <$> integerL,
      Array <$> brackets exprs,
      String <$> stringL,
      Char <$> charL
    ]

stm :: Parser Stm
stm = foldr1 Seq <$> stm_ `sepBy1` symbol ";"
  where
    stm_ =
      choice
        [ Skip <$ symbol "skip",
          if_,
          for_,
          do symbol "while"; cond <- expr; symbol "do"; body <- stm; symbol "od"; pure (While cond body),
          do symbol "repeat"; body <- stm; symbol "until"; cond <- expr; pure (Repeat body cond),
          do symbol "return"; mexpr <- optional expr; pure (Return mexpr),
          try do f <- ident; args <- parens exprs; pure (Call f args),
          assign_
        ]

    assign_ = do
      var <- ident
      ixes <- many (brackets expr)
      symbol ":="
      val <- expr
      pure ((var, ixes) := val)

    if_ = do
      symbol "if"
      cond <- expr
      symbol "then"
      then_ <- stm
      elifs <- many do symbol "elif"; cond <- expr; symbol "then"; body <- stm; pure (cond, body)
      else_ <- optional (symbol "else" *> stm)
      symbol "fi"
      pure (If cond then_ elifs else_)

    for_ = do
      symbol "for"
      init <- stm
      symbol ","
      cond <- expr
      symbol ","
      step <- stm
      symbol "do"
      body <- stm
      symbol "od"
      pure (For init cond step body)

def :: Parser Definition
def = do
  name <- symbol "fun" *> ident
  args <- parens idents
  locals <- option [] do symbol "local" *> idents
  body <- braces stm
  pure Definition {name, args, locals, body}

unit :: Parser Unit
unit = do
  sc
  defs <- many def
  body <- stm
  eof
  pure Unit {body, defs}

data ParserError = ParserError String
  deriving stock (Show)

instance Exception ParserError where
  displayException (ParserError s) = s

parseSource :: FilePath -> String -> Either ParserError Unit
parseSource path src =
  case runParser unit path src of
    Right stm -> Right stm
    Left ex -> Left (ParserError (errorBundlePretty ex))

parseFile :: FilePath -> IO Unit
parseFile path = do
  src <- readFile path
  case parseSource path src of
    Right stm -> pure stm
    Left ex -> throwIO ex

main = parseTest (expr <* eof) "a[1].length"
