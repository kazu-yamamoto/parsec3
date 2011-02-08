-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Text.Lazy
-- Copyright   :  (c) Kazu Yamamoto 2010
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Make strict Texts an instance of 'Stream' with 'Char' token type.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Parsec.Text.Lazy (
    Parser, GenParser, parseFromFile
  ) where

import Text.Parsec.Error
import Text.Parsec.Prim

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

instance (Monad m) => Stream T.Text m Char where
    uncons = return . T.uncons

type Parser = Parsec T.Text ()
type GenParser t st = Parsec T.Text st

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = do 
    input <- T.readFile fname
    return (runP p () fname input)
