{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE JavaScriptFFI       #-}
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

import qualified GHCJS.Types    as T
import           Miso                    hiding ( action_
                                                , model
                                                )

foreign import javascript unsafe "$($1).zinoMenu()" makeMenu :: T.JSString -> IO () 

-- MODELS

data Model
    = Model
    deriving (Show, Eq)


initialModel :: Model
initialModel = Model


-- ACTIONS

data Action
    = NoOp
    | ZinoMenu T.JSString
    deriving (Show, Eq)


-- VIEWS


view :: Model -> View Action
view _ = div_
  []
  [
    ul_ [ id_ "mainMenu", onCreated (ZinoMenu "#mainMenu") ]
      [ li_ [] [ a_ [href_ "#"] [ text "Hilferaum" ] ]
      , li_ [] [ a_ [href_ "#"] [ text "Begrüßungsraum" ] ]
      , li_ [] [ a_ [href_ "#"] [ text "Konferenzübersicht" ] ] 
      , li_ [] [ a_ [href_ "#"] [ text "???" ] ]
      , li_ [] [ a_ [href_ "#"] [ text "Terminals" ]
               , ul_ [] [ li_ [] [ a_ [href_ "#"] [ text "PIN Eingabe" ] ]
                        , li_ [] [ a_ [href_ "#"] [ text "Eingangshalle" ] ]
                        , li_ [] [ a_ [href_ "#"] [ text "Anrufhistorie" ] ]
                        , li_ [] [ a_ [href_ "#"] [ text "Wartefeld" ] ]
                        , li_ [] [ a_ [href_ "#"] [ text "Logging Console" ] ]
                        , li_ [] [ a_ [href_ "#"] [ text "Gehaltene Anrufe" ] ]
                        ] 
                ]
      , li_ [] [ a_ [href_ "#"] [ text "Kunden und Accounts" ] 
               , ul_ [] [ li_ [] [ a_ [href_ "#"] [ text "WMT" ] ]
                        , li_ [] [ a_ [href_ "#"] [ text "Kunden" ] ]
                        , li_ [] [ a_ [href_ "#"] [ text "Accounts" ] ]
                        ]
               ]
      , li_ [] [ a_ [href_ "#"] [ text "Einstellungen" ] ]
      ]
  ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update action model = case action of
  ZinoMenu elementId -> model <# do 
                            makeMenu elementId
                            return NoOp
  _ -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = []

