module Parse where
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Data.Char
import qualified Data.Text as T
import Control.Monad ( void )
import Utils (Expr(..), Literal(..), Name)

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "#{"
  , Tok.commentEnd      = "}#"
  , Tok.commentLine     = "#"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = [
      "=","->","fn","let","in",
      "if","then","else",
      "true", "false"
    ]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

litInt :: Parser Int
litInt = fromIntegral <$> Tok.integer lexer

litReal :: Parser Double
litReal = Tok.float lexer

litChar :: Parser Char
litChar = Tok.charLiteral lexer

litString :: Parser String
litString = Tok.stringLiteral lexer


whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

varName :: Parser Name 
varName = fmap T.pack (Tok.identifier lexer)

spaces1 :: Parser ()
spaces1 = skipMany1 space

variable :: Parser Expr
variable = EVar <$> varName

lambda :: Parser Expr
lambda = do
    reserved "fn" <?> "token \"fn\""
    args <- many1 varName <?> "arg!"
    reserved "->" <?> "arrow \"->\""
    body <- application
    return $ foldr ELam body args


application :: Parser Expr
application = do
    xs <- sepBy1 expression spaces
    return $ foldl1 EApp xs

letIn :: Parser Expr
letIn = do
    reserved "let"
    var <- varName <?> "var!"
    reserved "="
    def <- application
    reserved "in"
    body <- application
    return $ ELet var def body

literal :: Parser Literal
literal = do
        LInt <$> litInt
    <|> LReal <$> litReal
    <|> (reserved "true" >> return (LBool True))
    <|> (reserved "false" >> return (LBool False))
    <|> fail "Can't Parse Literal!"


expression :: Parser Expr
expression = spaces >> (
    do  (try . lookAhead) (string "fn") >> lambda
    <|> ((try . lookAhead) (string "let") >> letIn)
    <|> ((try . lookAhead) (char '(') >> parens application)
    <|> try variable
    <|> ELit <$> literal
    <?> "Expression!")



parseExpr :: String -> Either ParseError Expr
parseExpr xs = parse expression "input" xs
