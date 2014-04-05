{-# LANGUAGE OverloadedStrings #-}

module Lex where

import Control.Applicative ((<|>))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
-- import qualified Data.Attoparsec.Combinator as Atto
-- import qualified Data.Attoparsec.Text as Atto
import Control.Monad (void)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Text.Lazy as Parsec

data Keyword
    = KAny
    | KBoolean
    | KBreak
    | KCase
    | KCatch
    | KClass
    | KConstructor
    | KContinue
    | KDelete
    | KEnum
    | KExport
    | KExtends
    | KFalse
    | KFor
    | KFunction
    | KIf
    | KIn
    | KInstanceOf
    | KInterface
    | KLet
    | KModule
    | KNew
    | KNull
    | KNumber
    | KPrivate
    | KProtected
    | KPublic
    | KReturn
    | KStatic
    | KString
    | KSuper
    | KSwitch
    | KThis
    | KThrow
    | KTrue
    | KTry
    | KTypeOf
    | KUndefined
    | KVar
    | KVoid
    | KWhile
    | KWith
    deriving (Show, Eq)

keywords :: [(Text, Keyword)]
keywords =
    [ ("any",          KAny)
    , ("boolean",      KBoolean)
    , ("break",        KBreak)
    , ("case",         KCase)
    , ("catch",        KCatch)
    , ("class",        KClass)
    , ("constructor",  KConstructor)
    , ("continue",     KContinue)
    , ("delete",       KDelete)
    , ("enum",         KEnum)
    , ("export",       KExport)
    , ("extends",      KExtends)
    , ("false",        KFalse)
    , ("for",          KFor)
    , ("function",     KFunction)
    , ("if",           KIf)
    , ("in",           KIn)
    , ("instanceof",   KInstanceOf)
    , ("interface",    KInterface)
    , ("let",          KLet)
    , ("module",       KModule)
    , ("new",          KNew)
    , ("null",         KNull)
    , ("number",       KNumber)
    , ("private",      KPrivate)
    , ("protected",    KProtected)
    , ("public",       KPublic)
    , ("return",       KReturn)
    , ("static",       KStatic)
    , ("string",       KString)
    , ("super",        KSuper)
    , ("switch",       KSwitch)
    , ("this",         KThis)
    , ("throw",        KThrow)
    , ("true",         KTrue)
    , ("try",          KTry)
    , ("typeof",       KTypeOf)
    , ("undefined",    KUndefined)
    , ("var",          KVar)
    , ("void",         KVoid)
    , ("while",        KWhile)
    , ("with",         KWith)
    ]

data Operator
    = OAmpersand
    | OAmpersandEquals
    | OBang
    | OCaret
    | OCaretEquals
    | OCloseBrace
    | OCloseBracket
    | OCloseParen
    | OColon
    | OComma
    | ODot
    | ODoubleAmpersand
    | ODoublePipe
    | OEquals
    | OFatRightArrow -- =>
    | OGreater
    | OGreaterEqual
    | OLess
    | OLessEqual
    | OMinus
    | OMinusEquals
    | OMinusMinus
    | ONotEqual
    | OOpenBrace
    | OOpenBracket
    | OOpenParen
    | OPercent
    | OPercentEquals
    | OPipe
    | OPipeEquals
    | OPlus
    | OPlusEquals
    | OPlusPlus
    | OQuestionMark
    | OSemicolon
    | OShiftLeft
    | OShiftLeftEquals
    | OShiftRight
    | OShiftRightEquals
    | OSignedShiftRight
    | OSignedShiftRightEquals
    | OSlash
    | OSlashEquals
    | OStar
    | OStarEquals
    | OStrictEqual
    | OStrictNotEqual
    | OTilde
    deriving (Show, Eq)

operators :: [(Text, Operator)]
operators =
    [ (">>>=",  OSignedShiftRightEquals)
    , ("<<=",   OShiftLeftEquals)
    , (">>=",   OShiftRightEquals)
    , (">>>",   OSignedShiftRight)
    , ("===",   OStrictEqual)
    , ("!==",   OStrictNotEqual)
    , ("/=",    OSlashEquals)
    , ("*=",    OStarEquals)
    , (">>",    OShiftRight)
    , ("&=",    OAmpersandEquals)
    , ("^=",    OCaretEquals)
    , ("&&",    ODoubleAmpersand)
    , ("||",    ODoublePipe)
    , ("=>",    OFatRightArrow)
    , (">=",    OGreaterEqual)
    , ("<=",    OLessEqual)
    , ("-=",    OMinusEquals)
    , ("--",    OMinusMinus)
    , ("!=",    ONotEqual)
    , ("%=",    OPercentEquals)
    , ("|=",    OPipeEquals)
    , ("+=",    OPlusEquals)
    , ("++",    OPlusPlus)
    , ("<<",    OShiftLeft)
    , ("~",     OTilde)
    , ("&",     OAmpersand)
    , ("!",     OBang)
    , ("^",     OCaret)
    , ("}",     OCloseBrace)
    , ("]",     OCloseBracket)
    , (")",     OCloseParen)
    , (":",     OColon)
    , (",",     OComma)
    , (".",     ODot)
    , ("=",     OEquals)
    , (">",     OGreater)
    , ("<",     OLess)
    , ("-",     OMinus)
    , ("{",     OOpenBrace)
    , ("[",     OOpenBracket)
    , ("(",     OOpenParen)
    , ("%",     OPercent)
    , ("|",     OPipe)
    , ("+",     OPlus)
    , ("?",     OQuestionMark)
    , (";",     OSemicolon)
    , ("/",     OSlash)
    , ("*",     OStar)
    ]

data Token
    = TKeyword Keyword
    | TOperator Operator
    | TStringLiteral Text
    | TNumericLiteral Text -- Wrong
    | TIdentifier Text
    deriving (Show, Eq)

data Lexeme = Lexeme
    { lToken :: Token
    , lLine :: Int
    , lColumn :: Int
    } deriving (Show, Eq)

whitespaceP :: Parsec.Parser ()
whitespaceP = void Parsec.space

lineCommentP :: Parsec.Parser ()
lineCommentP = Parsec.try $ do
    _ <- Parsec.string "//"
    _ <- Parsec.manyTill Parsec.anyChar Parsec.newline
    return ()

blockCommentP :: Parsec.Parser ()
blockCommentP = Parsec.try $ do
    _ <- Parsec.string "/*"
    _ <- Parsec.manyTill Parsec.anyChar (Parsec.string "*/")
    return ()

spaceP :: Parsec.Parser ()
spaceP = Parsec.skipMany (whitespaceP <|> lineCommentP <|> blockCommentP)

operatorP :: Parsec.Parser Operator
operatorP = Parsec.choice (map mkOperatorParser operators)
  where
    mkOperatorParser (str, op) = do
        _ <- Parsec.try $ Parsec.string $ Text.unpack str
        return op

wordP :: Parsec.Parser (Either Keyword Text)
wordP = do
    first <- Text.singleton <$> (Parsec.char '_' <|> Parsec.letter)
    rest <- Text.pack <$> Parsec.many (Parsec.char '_' <|> Parsec.alphaNum)
    let word = first <> rest
    return $ case lookup word keywords of
        Just kw -> Left kw
        Nothing -> Right word

stringLiteralP :: Parsec.Parser Text
stringLiteralP = do
    _ <- Parsec.char '"'
    let stringCharacterP = Parsec.noneOf "\\\""
        escapeSequenceP = Parsec.char '\\' >> Parsec.anyChar
    chars <- Parsec.many (escapeSequenceP <|> stringCharacterP)
    _ <- Parsec.char '"'
    return $ Text.pack chars

numericLiteralP :: Parsec.Parser Text
numericLiteralP = do
    first <- digitsP
    second <- Parsec.option "" $ do
        _ <- Parsec.char '.'
        more <- digitsP
        return $ Text.singleton '.' <> more

    return $ first <> second
  where
    digitsP = Text.pack <$> Parsec.many1 Parsec.digit

-- TODO: Octal, hex, exponential notation

documentElementP :: Parsec.Parser Lexeme
documentElementP = do
    pos <- Parsec.getPosition
    token <- Parsec.choice
        [ (either TKeyword TIdentifier) <$> wordP
        , TOperator <$> operatorP
        , TStringLiteral <$> stringLiteralP
        , TNumericLiteral <$> numericLiteralP
        ]

    return $ Lexeme
        { lToken = token
        , lLine = Parsec.sourceLine pos
        , lColumn = Parsec.sourceColumn pos
        }

documentP :: Parsec.Parser [Lexeme]
documentP = spaceP >> documentElementP `Parsec.sepEndBy` spaceP

lex :: Parsec.SourceName -> Text -> Either Parsec.ParseError [Lexeme]
lex = Parsec.parse documentP
