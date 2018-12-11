{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  )
where

import           Miso

import qualified OccDesk

main :: IO ()
main = do
  startApp App
    { model         = OccDesk.initialModel
    , view          = OccDesk.viewModel
    , update        = OccDesk.update
    , initialAction = OccDesk.NoOp
    , events        = defaultEvents
    , subs          = OccDesk.subscriptions
    , mountPoint    = Nothing
    }

