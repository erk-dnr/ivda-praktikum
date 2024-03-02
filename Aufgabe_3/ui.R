library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  titlePanel("Wein-Klassen Datenanalyse"),
  theme = shinytheme("flatly"),
  
  tabsetPanel(type="tabs",
              tabPanel("Scatterplots",
                       titlePanel(h3("Scatterplot-Matrix")),
                       plotOutput("Scatplots", height = "1000px"),
                       hr(),
                       fluidRow(
                         column(3, uiOutput("PlotType")),
                         column(12, uiOutput("Attribute"))
                       )
              ),
              
              tabPanel("Evaluation",
                       sidebarLayout(
                          sidebarPanel(
                             uiOutput("ResamplingSelector"),
                             uiOutput("ClassifierSelector"),
                             width = 3
                          ),
                          mainPanel(
                             titlePanel(h3("Evaluation")),
                             verbatimTextOutput("ConfusionMatrix")
                          )
                        )
              )
  )
)
)
