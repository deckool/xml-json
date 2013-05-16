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

--instance FromJSON Guest where
--  parseJSON (Object v) = Guest <$>
--                         v .: firstName <*>
--                         v .: lastName
--  parseJSON _ = empty

-- / To json ------------------------------------------------------------------

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

getGuest2 = atTag "guest" >>>
  proc x -> do
    fname <- text <<< atTag "fname" -< x
    lname <- text <<< atTag "lname" -< x
    returnA -< Guest { firstName = fname, lastName = lname }


main :: IO ()
main = do
--  let req = decode "{\"firstname\":\"John\",\"lastname\":\"Steinbeck\"}" :: Maybe Guest
--  print req
  guests <- runX (readDocument [withValidate no] "simple1.xml" 
                    >>> getGuest2)
--  print guests
  let reply = guests
  BL.writeFile "json" (encode reply)
