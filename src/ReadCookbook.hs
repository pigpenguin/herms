{-# LANGUAGE OverloadedStrings #-}
module ReadCookbook (parseCookbook, showJson) where

import           Control.Applicative ((<|>), liftA2)
import           Data.Aeson ((.:),(.=),ToJSON(..),FromJSON(..))
import qualified Data.Aeson as Aeson (decode, encode, object, withObject)
import qualified Data.Aeson.Encode.Pretty as Aeson
  ( Config(..)
  , Indent(Spaces)
  , NumberFormat(Generic)
  , encodePretty'
  , keyOrder
  )
import           Data.ByteString.Lazy (ByteString, unpack)
import           Data.Char (chr)
import           Text.Read (readMaybe)

import Types (Ingredient(..), Recipe(..), RecipeBook)

-- Attempts to parse a textual representation of a cookbook. Returns the first
-- succesful attempt between reading in the old school format and a JSON format
parseCookbook :: ByteString -> Maybe RecipeBook
parseCookbook = readNative <||> readJson
  where
    (<||>) = liftA2 (<|>)


-- Attempts to parse a newline seperated list of shown recipes.
readNative :: ByteString -> Maybe RecipeBook
readNative = mapM readMaybe . lines . toString
  where
    toString = map (chr . fromIntegral) . unpack

-- Attempts to parse a JSON list of JSON encoded recipes
readJson :: ByteString -> Maybe RecipeBook
readJson = Aeson.decode

-- This dictates the order in which the pretty printer will display keys
keys = [ "recipeName"
       , "description"
       , "servingSize"
       , "ingredients"
       , "directions"
       , "tags"
       , "ingredientName"
       , "quantity"
       , "unit"
       , "attribute"
       ]

displayConfig :: Aeson.Config
displayConfig = Aeson.Config { Aeson.confIndent = Aeson.Spaces 2
                             , Aeson.confCompare = Aeson.keyOrder keys
                             , Aeson.confNumFormat = Aeson.Generic
                             , Aeson.confTrailingNewline = True}

showJson :: RecipeBook -> ByteString
showJson = Aeson.encodePretty' displayConfig

-- JSON parsing instances for Ingredient

instance FromJSON Ingredient where
  parseJSON = Aeson.withObject "ingredient" $ \o ->
    Ingredient <$> o .: "quantity"
               <*> o .: "unit"
               <*> o .: "ingredientName"
               <*> o .: "attribute"

instance ToJSON Ingredient where
  toJSON i = Aeson.object [ "quantity"       .= quantity i
                          , "unit"           .= unit i
                          , "ingredientName" .= ingredientName i
                          , "attribute"      .= attribute i
                          ]

-- JSON parsing instances for Recipe

instance FromJSON Recipe where
  parseJSON = Aeson.withObject "recipe" $ \o ->
    Recipe <$> o .: "recipeName"
           <*> o .: "description"
           <*> o .: "servingSize"
           <*> o .: "ingredients"
           <*> o .: "directions"
           <*> o .: "tags"

instance ToJSON Recipe where
  toJSON r = Aeson.object [ "recipeName"  .= recipeName r
                          , "description" .= description r
                          , "servingSize" .= servingSize r
                          , "ingredients" .= ingredients r
                          , "directions"  .= directions r
                          , "tags"        .= tags r
                          ]
