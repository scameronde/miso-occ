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

import           Miso                    hiding ( action_
                                                , model
                                                )
import Miso.String (ms, MisoString, unpack)
import Control.Concurrent (threadDelay)


foreign import javascript unsafe "$($1).draggable({ handle: '.titlebar', stack: '.window', snap: true })" makeDraggable :: MisoString -> IO ()
foreign import javascript unsafe "$($1).resizable({ grid: [5, 5] })" makeResizable :: MisoString -> IO ()



-- MODELS

data Model
    = Model { getList :: [(String, String)]
            , getText :: String
            , getCounter :: Int
            }
    deriving (Show, Eq)


initialModel :: Model
initialModel = Model [] "Lorem Ipsum" 0


-- ACTIONS

data Action
    = NoOp
    | MenuClicked MenuItem
    | WindowNew
    | WindowOpened MisoString
    | ClearText
    | FillText
    | ChangeText MisoString
    | StartCounter
    | CountUp
    deriving (Show, Eq)


data MenuItem
    = MenuPIN
    deriving (Show, Eq)

-- VIEWS


view :: Model -> View Action
view m = div_
  []
  [ viewMenu
  , viewWindows m
  ]

datatoggle_ ::  MisoString -> Attribute action
datatoggle_ = textProp "data-toggle"
  
viewMenu :: View Action
viewMenu =
  nav_ [ id_ "mainMenu", class_ "navbar navbar-default navbar-fixed-top"]
       [ div_ [ class_ "container-fluid"]
              [ div_ [ class_ "navbar-header" ]
                     [ a_ [ class_ "navbar-brand", href_ "#" ] [text "OCC"] ]
              , ul_ [ class_ "nav navbar-nav"]
                    [ li_ [] [ a_ [href_ "#"] [ text "Hilferaum" ] ]
                    , li_ [] [ a_ [href_ "#"] [ text "Begrüßungsraum" ] ]
                    , li_ [] [ a_ [href_ "#"] [ text "Konferenzübersicht" ] ] 
                    , li_ [] [ a_ [ href_ "#" ] [ text "???" ] ]
                    , li_ [ class_ "dropdown" ] 
                          [ a_ [ class_ "dropdown-toggle"
                               , datatoggle_ "dropdown"
                               , href_ "#"
                               ] 
                               [ text "Terminals"
                               , span_ [ class_ "caret" ] []  
                               ]
                          , ul_ [ class_ "dropdown-menu" ]
                                [ li_ [ onClick (MenuClicked MenuPIN) ] [ a_ [href_ "#"] [ text "PIN Eingabe" ] ]
                                , li_ [] [ a_ [href_ "#"] [ text "Eingangshalle" ] ]
                                , li_ [] [ a_ [href_ "#"] [ text "Anrufhistorie" ] ]
                                , li_ [] [ a_ [href_ "#"] [ text "Wartefeld" ] ]
                                , li_ [] [ a_ [href_ "#"] [ text "Logging Console" ] ]
                                , li_ [] [ a_ [href_ "#"] [ text "Gehaltene Anrufe" ] ]
                                ]
                          ]
                    , li_ [ class_ "dropdown" ] 
                          [ a_ [ class_ "dropdown-toggle"
                               , datatoggle_ "dropdown"
                               , href_ "#"] 
                               [ text "Kunden und Accounts" 
                               , span_ [ class_ "caret" ] []
                               ]
                          , ul_ [ class_ "dropdown-menu" ]
                                [
                                  li_ [] [ a_ [href_ "#"] [ text "WMT" ] ]
                                , li_ [] [ a_ [href_ "#"] [ text "Kunden" ] ]
                                , li_ [] [ a_ [href_ "#"] [ text "Accounts" ] ]
                                ]
                          ]
                    , li_ [] [ a_ [href_ "#"] [ text "Einstellungen" ] ]
                    ]
              ]
       ]


viewWindows :: Model -> View Action
viewWindows (Model list t _) = div_ [] (fmap (\(elementId, title) ->  viewWindow elementId title t) list)

viewWindow :: String -> String -> String -> View Action
viewWindow elementId title content =
  div_  [ id_ $ ms elementId
        , class_ "window"
        , title_ $ ms title
        , onCreated (WindowOpened $ ms ("#" ++ elementId))
        ] 
        [ div_ [ class_ "titlebar" ]
               [ span_ [ class_ "title" ]
                       [ text $ ms title ]
               ]
        , div_ [ class_ "windowcontent" ]
               [
                 textarea_ [ onChange ChangeText, value_ $ ms content ] [ ]
               , button_ [ onClick ClearText ][ text "clear" ]
               , button_ [ onClick FillText ][ text "fill" ]
               , button_ [ onClick StartCounter ] [ text "start"]
               ]
        ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update action model = case action of
  MenuClicked MenuPIN -> batchEff model [ do
                                            putStrLn "Clicked: PIN"
                                            pure NoOp
                                        ,
                                          do
                                            putStrLn "Opening Window"
                                            pure WindowNew
                                        ]

  WindowNew -> noEff ( Model ( list ++ [("window_" ++ show num, "New Window " ++ show num)]) t c )
                   where list = getList model
                         t = getText model
                         c = getCounter model
                         num = length list

  WindowOpened elementId -> model <# do
                                        putStrLn "Window opened"
                                        makeDraggable elementId
                                        makeResizable elementId
                                        pure NoOp

  ClearText -> noEff ( Model ( getList model ) "" (getCounter model) )

  FillText -> noEff ( Model ( getList model ) "Hakuna Matata" (getCounter model) )

  ChangeText s -> noEff ( Model ( getList model) (unpack s) (getCounter model) )

  StartCounter -> model <# do
                             pure CountUp

  CountUp -> ( Model ( getList model) (show (getCounter model)) ((getCounter model) + 1) ) <# do
                putStrLn "Count Up"
                threadDelay 1000000
                pure CountUp

  _ -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = []

