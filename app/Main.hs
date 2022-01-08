module Main where

import           Data.Text.IO
import           Prelude hiding (getLine, print, putStrLn)
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Lib
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import Text.Megaparsec (parse)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runRepl
