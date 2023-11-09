module Parser
    ( SExpression(..)
    , Operator(..)
    , Atom(..)
    , Symbol(..)
    , parseLine
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data SExpression = 
    SAtom Atom
    | SList [SExpression]
    deriving (Show, Eq)

data Symbol =
    Symbol String
    | SOperator Operator
    deriving (Show, Eq)

data Atom =
    ANum Integer
    | AString String
    | ASymbol Symbol
    | ANil
    deriving (Show, Eq)

data Operator =
    Plus
    | Minus
    | Multiply
    | Divide
    | If
    | Quote
    | Let
    | SetQ
    deriving (Show, Eq)


parseLine :: String -> Either (ParseErrorBundle String Void) SExpression
parseLine input = parse lispLine "repl" input

lispLine :: Parser SExpression
lispLine = do
    _ <- space
    m <- optional (char '(')
    e <- case m of
        Nothing -> expression
        Just _ -> do
            pe <- expression
            _ <- char ')'
            return pe
    _ <- eof
    return e


expression :: Parser SExpression
expression = try (try parenExp <|> atomExp) <|> quotedExp

lispNum :: Parser Atom
lispNum = do
    f <- L.decimal
    return $ ANum f

lispOperator :: Parser Symbol
lispOperator = do
    op <- lexeme $ choice
                    [ Plus <$ char '+'
                    , Minus <$ char '-'
                    , Multiply <$ char '*'
                    , Divide <$ char '/'
                    , If <$ string "if"
                    , Quote <$ string "quote"
                    , Let <$ string "let"
                    , SetQ <$ string "setq"
                    ]
    return $ SOperator op


sc :: Parser ()
sc = L.space space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parenExp :: Parser SExpression
parenExp =
    SList <$> some (lexeme atomExp <|> (lexeme (between (char '(') (char ')') expression)))


quotedExp :: Parser SExpression
quotedExp = do
    _ <- string "'("
    e <- some expression
    _ <- char ')'
    return $ SList ((SAtom (ASymbol (SOperator Quote))):e)

lispSymbol :: Parser Symbol
lispSymbol = try lispOperator <|> lispName

lispName :: Parser Symbol
lispName = do
    fc <- letterChar
    rest <- many alphaNumChar
    return $ Symbol (fc:rest)


atomExp :: Parser SExpression
atomExp = SAtom <$> lispAtom

lispAtom :: Parser Atom
lispAtom = try (ASymbol <$> lispSymbol) <|> lispNum <|> lispNil <|> lispString

lispNil :: Parser Atom
lispNil = ANil <$ string "()"

lispString :: Parser Atom
lispString = AString <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')
