{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module OccDesk
  ( Model
  , Action(NoOp)
  , OccDesk.viewModel
  , OccDesk.update
  , OccDesk.subscriptions
  , OccDesk.initialModel
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                , view
                                                )
import Miso.String (ms, MisoString, unpack)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Control.Lens
import Data.List (unfoldr)
import Control.Monad (sequence)

foreign import javascript unsafe "$($1).draggable({ handle: '.titlebar', stack: '.window', snap: true })" makeDraggable :: MisoString -> IO ()
foreign import javascript unsafe "$($1).resizable({ grid: [5, 5] })" makeResizable :: MisoString -> IO ()
foreign import javascript unsafe "$($1).bootstrapTable()" initBootstrapTable :: MisoString -> IO ()



-- MODELS

maxX :: Int
maxX = 20

maxY :: Int
maxY = 1000

type DataRow = [Int]
type DataTable = [DataRow]

data Model
    = Model { _windowList :: [(String, String)]
            , _commonText :: String
            , _counter :: Int
            , _counting :: Bool
            , _dataTable :: DataTable
            }
    deriving (Show, Eq)

makeLenses ''Model

initialModel :: Model
initialModel = Model [] "Lorem Ipsum" 0 False initialDataTable

initialDataRow :: DataRow
initialDataRow = unfoldr giveOne maxX
                  where giveOne count = if (count == 0) then Nothing else Just (1, count-1)

initialDataTable :: DataTable
initialDataTable = unfoldr giveRow maxY
                    where giveRow count = if (count ==0) then Nothing else Just (initialDataRow, count-1)

randomDataRow :: Int -> DataRow
randomDataRow v = unfoldr giveOne maxX
                    where giveOne count = if (count == 0) then Nothing else Just (v, count-1)

randomDataTable :: Int -> DataTable
randomDataTable v = unfoldr giveRow maxY
                      where giveRow count = if (count ==0) then Nothing else Just (randomDataRow v, count-1)
                    
-- ACTIONS

data Action
    = NoOp
    | MenuClicked MenuItem
    | WindowNew
    | WindowOpened MisoString
    | TableCreated MisoString
    | ClearText
    | FillText
    | ChangeText MisoString
    | StartCounter
    | StopCounter
    | CountUp
    | SetRandom Int DataTable
    deriving (Show, Eq)


data MenuItem
    = MenuPIN
    deriving (Show, Eq)

-- VIEWS


viewModel :: Model -> View Action
viewModel m = div_
  []
  [ viewMenu
  , viewWindows m
  ]

datatoggle_ ::  MisoString -> Attribute action
datatoggle_ = textProp "data-toggle"

datasearch_ ::  MisoString -> Attribute action
datasearch_ = textProp "data-search"

datapagination_ ::  MisoString -> Attribute action
datapagination_ = textProp "data-pagination"




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
viewWindows (Model list t _ ic dt) = div_ [] (fmap (\(elementId, title) ->  viewWindow elementId title t ic dt) list)

viewWindow :: String -> String -> String -> Bool -> DataTable -> View Action
viewWindow elementId title textContent isCounting table =
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
               [ div_ []  [
                            div_ [] [ textarea_ [ onChange ChangeText, value_ $ ms textContent ] [ ]
                                    , button_ [ onClick ClearText ][ text "clear" ]
                                    , button_ [ onClick FillText ][ text "fill" ]
                                    , if isCounting then (button_ [ onClick StopCounter ] [ text "stop"]) 
                                                    else (button_ [ onClick StartCounter ] [ text "start"])
                                    ]
                          , div_ [] [ viewTable elementId table]
                          ]
               ]
        ]

      

viewTableCell :: Int -> View Action
viewTableCell value = td_ [] [ text $ (ms $ show value) ]

viewTableRow :: DataRow -> View Action
viewTableRow row = tr_ [] ( fmap viewTableCell row )

viewTableHeaderCell :: Int -> View Action
viewTableHeaderCell columnNr = td_ [] [ text $ (ms $ show columnNr) ]

viewTableHeaderRow :: Int -> View Action
viewTableHeaderRow numColumns = tr_ [] ( fmap viewTableHeaderCell [1..numColumns] )

viewTable :: String -> DataTable -> View Action
viewTable windowId table = 
                                  table_ [ id_ $ ms (windowId ++ "_table")
                                        , datatoggle_ "table"
                                        , datasearch_ "true"
                                        , datapagination_ "true"
                                        , onCreated (TableCreated (ms ("#" ++ windowId ++ "_table")))
                                        ] 
                                        [ thead_ [] [( viewTableHeaderRow $ calcNumColumns table )]
                                        , tbody_ [                                                  
                                                 ] 
                                                 ( fmap viewTableRow table )
                                        ]

calcNumColumns :: DataTable -> Int
calcNumColumns table = length (table !! 0)




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

  WindowNew -> noEff ( windowList .~ ln $ model)
                  where 
                    lo = model ^. windowList
                    num = length lo
                    ln = lo ++ [("window_" ++ show num, "New Window " ++ show num)]

  WindowOpened elementId -> model <# do
                                        putStrLn "Window opened"
                                        makeDraggable elementId
                                        makeResizable elementId
                                        pure NoOp

  TableCreated elementId -> model <# do
                                        putStrLn "Table created"
                                        initBootstrapTable elementId
                                        pure NoOp

  ClearText -> noEff ( commonText .~ "" $ model )

  FillText -> noEff ( commonText .~ "Hakuna Matata" $ model)

  ChangeText s -> noEff ( commonText .~ (unpack s) $ model )

  StartCounter -> ( counting .~ True $ model ) <# pure CountUp
        
  StopCounter -> ( counting .~ False $ model ) <# pure CountUp
          
  CountUp -> ( (counter .~ cn) . (commonText .~ t) $ model ) <# do
                  threadDelay 1000000
                  r <- randomRIO (1, 100) 
                  if (model ^. counting) then pure (SetRandom r (randomDataTable r))
                                         else pure NoOp
              where
                co = model ^. counter
                cn = co + 1
                t  = show $ co

  SetRandom r rdt -> ( (counter .~ r) . (dataTable .~ rdt) $ model ) <# pure CountUp

  _ -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = []

