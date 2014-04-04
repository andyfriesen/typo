{-# LANGUAGE OverloadedStrings #-}

module Lex where

import Debug.Trace

import Control.Applicative ((<|>))
import Data.Functor ((<$>))
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Attoparsec.Combinator as Atto
import qualified Data.Attoparsec.Text as Atto

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
    , lLine :: Integer
    } deriving (Show, Eq)

whitespaceP :: Atto.Parser ()
whitespaceP = Atto.skip isSpace

lineCommentP :: Atto.Parser ()
lineCommentP = do
    _ <- Atto.string "//"
    _ <- Atto.manyTill Atto.anyChar Atto.endOfLine
    return ()

blockCommentP :: Atto.Parser ()
blockCommentP = do
    _ <- Atto.string "/*"
    _ <- Atto.manyTill Atto.anyChar (Atto.string "*/")
    return ()

spaceP :: Atto.Parser ()
spaceP = Atto.skipMany (whitespaceP <|> lineCommentP <|> blockCommentP)

operatorP :: Atto.Parser Operator
operatorP = Atto.choice (map mkOperatorParser operators)
  where
    mkOperatorParser (str, op) = do
        _ <- Atto.string str
        return op

wordP :: Atto.Parser (Either Keyword Text)
wordP = do
    first <- Text.singleton <$> Atto.satisfy (Atto.inClass "a-zA-Z")
    rest <- Atto.takeWhile (Atto.inClass "a-zA-Z0-9_")
    let word = first <> rest
    return $ case lookup word keywords of
        Just kw -> Left kw
        Nothing -> Right word

_traceM :: Monad m => String -> m ()
_traceM s = trace s (return ())

stringLiteralP :: Atto.Parser Text
stringLiteralP = do
    _ <- Atto.char '"'
    let stringCharacterP = Atto.satisfy (Atto.notInClass "\\\"")
        escapeSequenceP = Atto.char '\\' >> Atto.anyChar
    chars <- Atto.many' (escapeSequenceP <|> stringCharacterP)
    _ <- Atto.char '"'
    return $ Text.pack chars

numericLiteralP :: Atto.Parser Text
numericLiteralP = do
    first <- digitsP
    second <- Atto.option "" $ do
        _ <- Atto.char '.'
        more <- digitsP
        return $ Text.singleton '.' <> more

    return $ first <> second
  where
    digitsP = Atto.takeWhile1 (Atto.inClass "0-9")

documentElementP :: Atto.Parser Token
documentElementP = Atto.choice
    [ (either TKeyword TIdentifier) <$> wordP
    , TOperator <$> operatorP
    , TStringLiteral <$> stringLiteralP
    , TNumericLiteral <$> numericLiteralP
    ]

documentP :: Atto.Parser [Token]
documentP = documentElementP `Atto.sepBy` spaceP

lex :: Text -> Either String [Token]
lex = Atto.parseOnly documentP
