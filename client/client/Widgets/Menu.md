# Idee für eine Menustruktur / Typ


menuStructure = Menu ("mainMenu", [ ("Hilfemenü", MenuClicked MenuHelp)
                                  , ("Begrüßungsraum", MenuClicked MenuGreeting)
                                  , ("Konferenzübersicht", MenuClicked MenuOverview)
                                  , ("???", MenuClicked MenuUnknown)
                                  , ("Terminals", Menu ("terminalsMenu", [ ("PIN Eingabe", MenuClicked MenuPIN)
                                                                         , ("Eingangshalle", MenuClicked MenuPIN)
                                                                         , ("Anrufshistorie", MenuClicked MenuPIN)
                                                                         , ("Wartefeld", MenuClicked MenuPIN)
                                                                         , ("Logging Console", MenuClicked MenuPIN)
                                                                         , ("Gehaltene Anrufe", MenuClicked MenuPIN)
                                                                         ]
                                                       )
                                    )
                                  , ("Kunden und Accounts", Menu ("accountsMenu", [ ("WMT", MenuClicked MenuWMT)
                                                                                  , ("Kunden", MenuClicked MenuCustomers)
                                                                                  , ("Accounts", MenuClicked MenuAccounts)
                                                                                  ]
                                                                 )
                                    )
                                  ]
                        )

type MenuId = String
type Title = String

data Menu a = Menu MenuId [Entry a]
data Entry a = Entry Title (Clicked a) | Entry Title Menu
