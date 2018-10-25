{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module OccDesk
  ( Model
  , Action(NoOp)
  , OccDesk.view
  , OccDesk.update
  , OccDesk.subscriptions
  , OccDesk.initialModel
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                )

-- import           Util

-- MODELS

data Model
    = Model
    deriving (Show, Eq)


initialModel :: Model
initialModel = Model


-- ACTIONS

data Action
    = NoOp
    deriving (Show, Eq)


-- VIEWS


view :: Model -> View Action
view _ = div_
  []
  [ 
  ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update action model = case (action, model) of
  _ -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = []

