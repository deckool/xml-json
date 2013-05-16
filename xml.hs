{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

data Guest = Guest { firstName, lastName :: String }
             deriving (Show)

-- A ToJSON instance allows us to encode a value as JSON.

instance ToJSON Guest where
  toJSON (Guest firstNameV lastNameV) = object [ "firstname" .= firstNameV,
                                  "lastname" .= lastNameV ]

-- A FromJSON instance allows us to decode a value from JSON. This
-- should match the format used by the ToJSON instance.

instance FromJSON Guest where
  parseJSON (Object v) = Guest <$>
                         v .: "firstname" <*>
                         v .: "lastname"
  parseJSON _ = empty

main :: IO ()
main = do
  let req = decode "{\"firstname\":\"John\",\"lastname\":\"Steinbeck\"}" :: Maybe Guest
  print req
--  guests <- runX (readDocument [withValidate no] "simple1.xml" 
--                    >>> getGuest2)
--  print guests
--  let reply = guests
--  BL.writeFile "json" (encode reply)
