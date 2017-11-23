{-# LANGUAGE OverloadedStrings #-}
-- |
module M.Karen where

import Data.ByteString.Lazy
import Control.Exception
import Data.Aeson (decode)
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (bimap)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import Data.Thyme (Day)
import Network.HTTP.Conduit

import M.Types hiding (name, url, day)
import M.KarenJSON
import Util

-- | Handler for HttpExceptions
-- handle' :: IO a -> IO (Maybe a)
-- handle' a = handle handler (liftM Just a)
--   where handler :: HttpException -> IO (Maybe a)
--         handler _ = return Nothing

-- | Get a restaurant that kåren has.
getKaren :: Day -> T.Text -> String -> T.Text -> IO Restaurant
getKaren weekday name restUrl menuUrl =
  catch
    (r . go <$> simpleHttp restUrl)
    (pure . r . Left . HttpFail . (show :: HttpException -> String))
  where
    r :: Either NoMenu [Menu] -> Restaurant
    r m = Restaurant name menuUrl m

    go :: ByteString -> IO (Either NoMenu _)
    go s =
			first (DecodeError . (<> "\n\nThis is the culprit:\n" <> s)) (eitherDecode s)
			>>= (first (DecodeError . (<> "\n\nHere's the offending JSON:\n" <> s)) . parseEither . parseMenuForDay weekday)

{-
  return $
    Restaurant name menuUrl . maybe (Left SomethingWrong) id $ do
      text' <- text
      val <- decode text'
      res <- parseMaybe (parseMenuForDay weekday) val
      return $ case res of
        Nothing -> Left NoLunch
        Just l -> Right l
        -}
