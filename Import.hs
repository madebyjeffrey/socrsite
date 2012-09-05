module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
    , Text
#if __GLASGOW_HASKELL__ < 704
    , (<>)
#endif
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
#if __GLASGOW_HASKELL__ < 704
import Data.Monoid (Monoid (mappend, mempty, mconcat))
#else
import Data.Monoid (Monoid (mappend, mempty, mconcat), (<>))
#endif
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Settings.StaticFiles
import Settings.Development
import Data.Time (UTCTime)
import Data.Time.Format (formatTime)
import Text.Blaze (ToMarkup, toMarkup)
import Text.Blaze.Internal (string)
import System.Locale (defaultTimeLocale)

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

-- format date as     26 July 2012
--instance ToMarkup UTCTime where
--    toMarkup a = string (formatTime defaultTimeLocale "%e %B %Y" a)
    
