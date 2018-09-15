{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Data.Aeson
import Data.Monoid
import Data.Text (Text, pack)

import Parser

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app =
  do get root $
       text "Healthy\n"

     post "experiments/generate" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       text $ pack $ processRequests spec
