module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Lex
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as DTIO

main = do
    c <- TL.fromStrict <$> DTIO.readFile "ast.ts"
    case Lex.lex "ast.ts" c of
        Left l -> putStrLn $ "Left " ++ (show l)
        Right r -> forM_ r $ \lexeme -> do
            putStrLn $ "\t" ++ (show lexeme)
