module Markup.Language.Env where

-- definition of a simple environment

data Env
  = Env {
      name :: String -- name of the page
    , stylePath :: FilePath -- path of css file
    } deriving Show

defaultEnv :: Env
defaultEnv = Env "Sample" "style.css"
