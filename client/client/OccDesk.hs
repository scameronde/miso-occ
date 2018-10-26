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

-- import qualified GHCJS.Types    as T
import qualified Data.JSString as T
import           Miso                    hiding ( action_
                                                , model
                                                )
import Miso.String (ms)


foreign import javascript unsafe "$($1).zinoMenu()" makeMenu :: T.JSString -> IO () 
foreign import javascript unsafe "$($1).zinoMenu('close', $($2))" closeMenu :: T.JSString -> T.JSString -> IO () 
foreign import javascript unsafe "$($1).zinoOverlay({autoOpen: true, width: 300, height: 200, draggable: true, resizable: true, modal: false})" openWindow :: T.JSString -> IO ()

-- MODELS

data Model
    = Model {getList :: [(String, String)]}
    deriving (Show, Eq)


initialModel :: Model
initialModel = Model []


-- ACTIONS

data Action
    = NoOp
    | ZinoMenuCreated T.JSString
    | ZinoMenuClose T.JSString T.JSString
    | MenuClicked MenuItem
    | ZinoWindowNew
    | ZinoWindowOpened T.JSString
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


viewMenu :: View Action
viewMenu =
  ul_ [ id_ "mainMenu", onCreated (ZinoMenuCreated "#mainMenu") ]
  [ li_ [] [ a_ [href_ "#"] [ text "Hilferaum" ] ]
  , li_ [] [ a_ [href_ "#"] [ text "Begrüßungsraum" ] ]
  , li_ [] [ a_ [href_ "#"] [ text "Konferenzübersicht" ] ] 
  , li_ [] [ a_ [ href_ "#" ] [ text "???" ] ]
  , li_ [ id_ "terminalsMenu" ] [ a_ [ href_ "#" ] [ text "Terminals" ]
           , ul_ [] 
                 [ li_ [ onClick (MenuClicked MenuPIN)] [ a_ [ href_ "#" ] [ text "PIN Eingabe" ] ]
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


viewWindows :: Model -> View Action
viewWindows (Model list) = div_ [] (fmap (\(elementId, title) -> div_ [ id_ $ ms elementId
                                                                      , title_ $ ms title
                                                                      , onCreated (ZinoWindowOpened $ T.pack ("#" ++ elementId))
                                                                      ] 
                                                                      [ text "Huhu"
                                                                      ] ) list)



-- UPDATE

update :: Action -> Model -> Effect Action Model
update action model = case action of
  ZinoMenuCreated elementId -> model <# do 
                                          makeMenu elementId
                                          return NoOp

  ZinoMenuClose parentElement closeElement -> model <# do
                                                         closeMenu parentElement closeElement
                                                         return NoOp
  
  MenuClicked MenuPIN -> batchEff model [ do
                                            putStrLn "Clicked: PIN"
                                            return (ZinoMenuClose "#mainMenu" "#terminalsMenu")
                                        ,
                                          do
                                            putStrLn "Opening Window"
                                            return ZinoWindowNew
                                        ]

  ZinoWindowNew -> noEff (Model (list ++ [("window_" ++ show (length list), "New Window " ++ show (length list))]))
                   where list = getList model

  ZinoWindowOpened elementId -> model <# do
                                           putStrLn "Window opened"
                                           openWindow elementId
                                           return NoOp

  _ -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = []

