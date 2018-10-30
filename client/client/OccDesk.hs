{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RecordWildCards     #-}

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
import Miso.String (ms, unpack, MisoString)
import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe (maybeToList)
import Data.Aeson.Types


foreign import javascript unsafe "$($1).zinoMenu()" makeMenu :: MisoString -> IO () 
foreign import javascript unsafe "$($1).zinoMenu('close', $($2))" closeMenu :: MisoString -> MisoString -> IO () 


-- MODELS

data Window = Window { xPos :: Integer
                     , yPos :: Integer
                     , windowId :: MisoString
                     , active :: Bool
                     , title :: MisoString
                     , dragging :: Bool
                     , xDragDiff :: Integer
                     , yDragDiff :: Integer
                     }
              deriving (Show, Eq)

data Model
    = Model { getList :: [Window]
            , getText :: String }
    deriving (Show, Eq)


initialModel :: Model
initialModel = Model [] "Lorem Ipsum"


-- ACTIONS

data MenuItem
  = MenuPIN
  deriving (Show, Eq)

data Action
    = NoOp
    | ZinoMenuCreated MisoString
    | ZinoMenuClose MisoString MisoString
    | ZinoMenuClicked MenuItem
    | WindowNew
    | WindowTitleClicked MisoString
    | WindowDragStarted MisoString
    | WindowDragged MisoString Pos
    | ClearText
    | FillText
    | ChangeText MisoString
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
                 [ li_ [ onClick (ZinoMenuClicked MenuPIN)] [ a_ [ href_ "#" ] [ text "PIN Eingabe" ] ]
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
viewWindows (Model list t) = div_ [] (fmap (\window ->  viewWindow window t) list)

viewWindow :: Window -> String -> View Action
viewWindow Window{..} content =
  (nodeHtmlKeyed "div" (Key windowId))  
        [ id_ windowId
        , class_ "windowbackground"
        , title_ $ title
        , style_ $ Map.fromList [ ("position", "absolute")
                                , ("left", ms $ (show xPos) ++ "px")
                                , ("top", ms $ (show yPos) ++ "px")
                                , ("z-index", ms $ show (zOrder active))
                                , ("user-select", "none")
                                ]
        , onDragStart (WindowDragStarted windowId)
        , onDragWithPos $ WindowDragged windowId
        ] 
        [ p_ [] [ text (ms ( "Huhu " ++ (show xPos))) ]
        , textarea_ [ onChange ChangeText, value_ $ ms content ] [ ]
        , button_ [ onClick ClearText ][ text "clear" ]
        , button_ [ onClick FillText ][ text "fill" ]
        ]

zOrder :: Bool -> Integer
zOrder b = if b then 1 else 0

-- draggable_::  Bool -> Attribute action
-- draggable_ = boolProp "draggable"

-- UPDATE

update :: Action -> Model -> Effect Action Model
update action model = case action of
  ZinoMenuCreated elementId -> model <# do 
                                          makeMenu elementId
                                          return NoOp

  ZinoMenuClose parentElement closeElement -> model <# do
                                                         closeMenu parentElement closeElement
                                                         return NoOp
  
  ZinoMenuClicked MenuPIN -> batchEff model [ do
                                                putStrLn "Clicked: PIN"
                                                return (ZinoMenuClose "#mainMenu" "#terminalsMenu")
                                            ,
                                              do
                                                putStrLn "Opening Window"
                                                return WindowNew
                                            ]

  WindowNew -> noEff ( Model (windowList ++ [newWindow]) (getText model) )
               where
                 windowList = getList model
                 numWindows = length windowList
                 newWindowId = ms $ "window_" ++ (show numWindows)
                 newWindowTitle = ms $ "Window " ++ (show numWindows)
                 newWindow = Window 0 0 newWindowId True newWindowTitle
                                            
  WindowTitleClicked _ -> model <# do
                  putStrLn ("Window Title clicked")
                  return NoOp

  WindowDragStarted _ -> model <# do
                  putStrLn ("Drag started")
                  return NoOp

  WindowDragged wId Pos{..} -> Model newWindowList t <# do
                  putStrLn ("Drag Event: " ++ (show clientX) ++ ", " ++ (show offsetX) ++ ", " ++ (show clientY) ++ ", " ++ (show offsetY))
                  putStrLn ("  new Coordinates of window: " ++ (show newWindowList))
                  return NoOp
                where
                  windowList = getList model
                  t = getText model
                  otherWindows = fmap (\w -> w { active = False }) $ filter (\Window{..} -> windowId /= wId) windowList
                  windowToChange = find (\Window{..} -> windowId == wId) windowList
                  --newPosX = clientX - offsetX
                  --newPosY = clientY - offsetY
                  newWindow = fmap (\w -> w { active = True, xPos = sane (xPos w) ((xPos w) + offsetX), yPos = sane (yPos w) ((yPos w) + offsetY) }) windowToChange
                  newWindowList = otherWindows ++ (maybeToList newWindow)
                  sane x y = if (y <= 0) then x else y

  ClearText -> noEff ( Model ( getList model ) "" )

  FillText -> noEff ( Model ( getList model ) "Hakuna Matata" )

  ChangeText s -> noEff ( Model ( getList model) $ unpack s )

  _ -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = []



-- | Retrieves either "keyCode", "which" or "charCode" field in `Decoder`
data Pos = Pos { clientX :: Integer
               , clientY :: Integer
               , offsetX :: Integer
               , offsetY :: Integer
               }
            deriving (Show, Eq)
                           
onDragWithPos :: (Pos -> action) -> Attribute action
onDragWithPos = onWithOptions Miso.defaultOptions { preventDefault = True } "drag" posDecoder 

posDecoder :: Decoder Pos
posDecoder = Decoder decoder decodeAt
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "event" $ \o -> Pos <$> o .: "clientX" <*> o .: "clientY" <*> o .: "offsetX" <*> o .: "offsetY"
