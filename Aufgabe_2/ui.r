library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  titlePanel("Wein Datenanalyse"),
  theme = shinytheme("flatly"),
  
  tabsetPanel(type="tabs",
              tabPanel("Verteilung",
                titlePanel(h3("Verteilung der Attribute")),
                fluidRow(
                  column(12, align="center", h4("Alkohol")),
                  column(6, plotOutput("attribute1BP")),
                  column(6, plotOutput("attribute1HP")),
                  column(12, align="center", h4("Apfelsaeure")),
                  column(6, plotOutput("attribute2BP")),
                  column(6, plotOutput("attribute2HP")),
                  column(12, align="center", h4("Asche")),
                  column(6, plotOutput("attribute3BP")),
                  column(6, plotOutput("attribute3HP")),
                  column(12, align="center", h4("Aschen_Alkanitaet")),
                  column(6, plotOutput("attribute4BP")),
                  column(6, plotOutput("attribute4HP")),
                  column(12, align="center", h4("Magnesium")),
                  column(6, plotOutput("attribute5BP")),
                  column(6, plotOutput("attribute5HP")),
                  column(12, align="center", h4("Alle_Phenole")),
                  column(6, plotOutput("attribute6BP")),
                  column(6, plotOutput("attribute6HP")),
                  column(12, align="center", h4("Flavanoide")),
                  column(6, plotOutput("attribute7BP")),
                  column(6, plotOutput("attribute7HP")),
                  column(12, align="center", h4("Nichtflavanoide_Phenole")),
                  column(6, plotOutput("attribute8BP")),
                  column(6, plotOutput("attribute8HP")),
                  column(12, align="center", h4("Proanthocyanide")),
                  column(6, plotOutput("attribute9BP")),
                  column(6, plotOutput("attribute9HP")),
                  column(12, align="center", h4("Farbintensitaet")),
                  column(6, plotOutput("attribute10BP")),
                  column(6, plotOutput("attribute10HP")),
                  column(12, align="center", h4("Farbwert")),
                  column(6, plotOutput("attribute11BP")),
                  column(6, plotOutput("attribute11HP")),
                  column(12, align="center", h4("Proteinwert")),
                  column(6, plotOutput("attribute12BP")),
                  column(6, plotOutput("attribute12HP")),
                  column(12, align="center", h4("Prolinwert")),
                  column(6, plotOutput("attribute13BP")),
                  column(6, plotOutput("attribute13HP"))
                )
              ),

              tabPanel("Lin. Regression",
                       titlePanel(h3("Paarweise lineare Regressionen")),
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("xAxisSelector"),
                           uiOutput("yAxisSelector"),
                           width = 3
                         ),

                         mainPanel(
                           plotOutput("regressionPlot", height = "800px"),
                           hr(),
                           textOutput("regressionFunction"),
                           hr(),
                           plotOutput("residualsPlot", height = "250px"),
                           hr(),
                           textOutput("regressionProperties"),
                           tags$style(type="text/css", "#regressionFunction, #regressionProperties {white-space: pre-wrap;}"),
                           width = 9
                         )
                       )
              ),
 
              tabPanel("Sil. Coef.",
                       titlePanel(h3("Silhouette Coefficients fuer 2-5 cluster")),
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("SilCoeffFunc"),
                           uiOutput("SelectSilCoeff"),
                           textOutput("SortDatapointsLabel"),
                           uiOutput("SortDatapoints"),
                           uiOutput("TransformDataSil")
                         ),
                         
                         mainPanel(
                           plotOutput("SilhouetteClusters2"),
                           plotOutput("SilhouetteClusters3"),
                           plotOutput("SilhouetteClusters4"),
                           plotOutput("SilhouetteClusters5")
                         )
                       )
              ),
              
              tabPanel("ClustverVis. PCA",
                       titlePanel(h3("Clustervisualisierung mit PCA")),
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("SilCoeffFuncPCA"),
                           uiOutput("SelectSilCoeffPCA"),
                           uiOutput("TransformDataPCA")
                         ),
                         mainPanel(
                           plotOutput("PCAVisCluster2"),
                           plotOutput("PCAVisCluster3"),
                           plotOutput("PCAVisCluster4"),
                           plotOutput("PCAVisCluster5")
                         )
                       )

                       )
  )
)
)
