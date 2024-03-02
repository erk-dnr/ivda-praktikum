# Load libraries ---------------------------------------------------------------
library(shiny)
library(shinythemes)


# Shiny UI ---------------------------------------------------------------------
shinyUI(fluidPage(
    titlePanel("Bäckerei Datenanalyse"),
    #shinythemes::themeSelector(),
    theme = shinytheme("flatly"),
    
    tabsetPanel(type="tabs",
                
                # Items Tab ------------------------------------------------------------        
                tabPanel("Items",
                         titlePanel(h3("Verteilung der Items")),
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("dropdownUi1"),
                                 uiOutput("sliderUi1")
                             ),
                             
                             mainPanel(
                                 plotOutput("distPlot1"),
                                 hr(),
                                 tableOutput("distTable1")
                             )
                         )
                ), # End Items1 tab
                
                
                # Transakt.~Tagesverlauf Tab -------------------------------------------
                tabPanel("Transakt.~Tagesverlauf",
                         titlePanel(h3("Transaktionen aggregiert über den Tagesverlauf")),
                         sidebarLayout(
                             sidebarPanel(
                                uiOutput("dropdownUi2")
                             ),
                             mainPanel(
                                 plotOutput("daytimePlot")
                             )
                         )
                ), # End Transakt.~Tagesverlauf Tab
                
                # tabPanel("Transakt.~Zeit",
                #          titlePanel(h3("Transaktionen über Zeit")),
                #          sidebarLayout(
                #              sidebarPanel(
                #                  uiOutput("selectModeUi")
                #              ),
                #              mainPanel(
                #                  plotOutput("b3Plot")
                #              )
                #          )
                # ),
                
                # Transakt.~Zeit Tab ---------------------------------------------------
                tabPanel("Transakt.~Zeit wchtl.",
                         titlePanel(h3("Transaktionen über Zeit woechentlich")),
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("dropdownUiWeek")
                                 # uiOutput("dateRangeUi3")
                             ),
                             mainPanel(
                                 plotOutput("distPlot3Week")
                             )
                         )
                ), # End Transakt.~Zeit Tab
                
                # Transakt.~Zeit Tab taeglich ---------------------------------------------------
                tabPanel("Transakt.~Zeit tgl",
                         titlePanel(h3("Transaktionen über Zeit taeglich")),
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("dropdownUidays")
                             ),
                             mainPanel(
                                 plotOutput("distPlot3days")
                             )
                         )
                ), # End Transakt.~Zeit Tab
                
                # Transakt.~Zeit Tab taeglich ---------------------------------------------------
                tabPanel("Transakt.~Zeit mntl.",
                         titlePanel(h3("Transaktionen über Zeit gesamt(monatlich)")),
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("dropdownUimonths")
                             ),
                             mainPanel(
                                 plotOutput("distPlot3months")
                             )
                         )
                ), # End Transakt.~Zeit Tab
                
                # #Items~Transakt. Tab -------------------------------------------------
                tabPanel("#Items~Transakt.",
                         titlePanel(h3("Anzahl Items pro Transaktion")),
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("dropdownUi3"),
                                 uiOutput("dropdownUi4"),
                                 uiOutput("transactionList1")
                             ),
                             mainPanel(
                               plotOutput("numberTransactions")
                             )
                         )
                ), # End #Items~Transakt. Tab
                
                
                # #ARules-Freq-Tab -------------------------------------------------
                tabPanel("ARules-Freq",
                         titlePanel(h3("")),
                         sidebarLayout(
                             sidebarPanel(
                                 
                             ),
                             mainPanel(
                                 plotOutput("frequencyArulesPlot"),
                             )
                         )
                ), # End ARules-Freq-Tab
                
                # #ARules-Conf-Tab -------------------------------------------------
                tabPanel("ARules-Confidence",
                         titlePanel(h3("")),
                         sidebarLayout(
                             sidebarPanel(
                                 
                             ),
                             mainPanel(
                                 plotOutput("rulesPlot1"),
                                 hr(),
                                 tableOutput("confidenceTable")
                             )
                         )
                ), # End #ARules-Conf-Tab
                
                # #ARules-Lift-Tab -------------------------------------------------
                tabPanel("ARules-Lift",
                         titlePanel(h3("")),
                         sidebarLayout(
                             sidebarPanel(
                                 
                             ),
                             mainPanel(
                                 plotOutput("rulesPlot2"),
                                 hr(),
                                 tableOutput("liftTable")
                             )
                         )
                ), # End #ARules-Lift-Tab
                
                # #ARules-Supp-Tab -------------------------------------------------
                tabPanel("ARules-Support",
                         titlePanel(h3("")),
                         sidebarLayout(
                             sidebarPanel(
                                 
                             ),
                             mainPanel(
                                 plotOutput("rulesPlot3"),
                                 hr(),
                                 tableOutput("supportTable")
                             )
                         )
                ) # End #ARules-Supp-Tab
                
                # # b2 vorher Tab ---------------------------------------------------------
                # tabPanel("(b2 vorher)",
                #          titlePanel(h3("")),
                #          sidebarLayout(
                #              sidebarPanel(
                #                  uiOutput("dropdownUi5"),
                #                  uiOutput("timeInput51"),
                #                  uiOutput("timeInput52")
                #              ),
                #              mainPanel(
                #                  plotOutput("distPlot5")
                #              )
                #          )
                # ) # End b2 vorher Tab
    )
    
)) # End Shiny UI
