library(shiny)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Praktikumsaufgabe 4"),
  theme = shinytheme("flatly"),
  
  tabsetPanel(type="tabs",
              tabPanel("SVM",
                       titlePanel(h3("Support Vector Machine")),
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("kernelSelector"),
                           uiOutput("cSelector"),
                           uiOutput("gammaSelector"),
                           uiOutput("degreeSelector"),
                           uiOutput("coef0Selector"),
                           hr(),
                           uiOutput("xAxisSelector"),
                           uiOutput("yAxisSelector")
                         ),
                         mainPanel(
                           h4("Confusion Matrix on testing set"),
                           verbatimTextOutput("confusionMatrix"),
                           hr(),
                           h4("Statistics"),
                           textOutput("statistics"),
                           hr(),
                           plotOutput("clusterPlot"),
                           plotOutput("classificationPlot")
                         )
                        )
              ),
              
              tabPanel("SVM Tuning",
                       titlePanel(h3("SVM Tuning")),
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("kernelSelector2")
                         ),
                         mainPanel(
                           h4("Tuning Results"),
                           verbatimTextOutput("tuningStats"),
                           textOutput("tuningAccuracy")
                         )
                       )
              ),
              
              tabPanel("Neuronale Netze Version RSNNS",
                       column(4, uiOutput("RNNSLayer1Neuron1")),
                       column(4, uiOutput("RNNSLayer1Neuron2")),
                       column(4, uiOutput("RNNSLayer1Neuron3")),
                       
                       column(4, uiOutput("RNNSLayer2Neuron1")),
                       column(4, uiOutput("RNNSLayer2Neuron2")),
                       column(4, uiOutput("RNNSLayer2Neuron3")),
                       
                       column(4, uiOutput("RNNSAktivierung1")),
                       column(4, uiOutput("RNNSAktivierung2")),
                       column(4, uiOutput("RNNSAktivierung3")),
                       
                       column(4, uiOutput("RNNSEpochen1")),
                       column(4, uiOutput("RNNSEpochen2")),
                       column(4, uiOutput("RNNSEpochen3")),
                       
                       column(4, uiOutput("RNNScmptPred1")),
                       column(4, uiOutput("RNNScmptPred2")),
                       column(4, uiOutput("RNNScmptPred3")),
                       
                       hr(),
                       
                       column(4, plotOutput("RNNSPlot1")),
                       column(4, plotOutput("RNNSPlot2")),
                       column(4, plotOutput("RNNSPlot3")),
                       
                       
                       column(4, verbatimTextOutput("evalRNNS1")),
                       column(4, verbatimTextOutput("evalRNNS2")),
                       column(4, verbatimTextOutput("evalRNNS3")),
                       
                       column(4, plotOutput("RNNSTopol1")),
                       column(4, plotOutput("RNNSTopol2")),
                       column(4, plotOutput("RNNSTopol3")),
                       
                       column(4, verbatimTextOutput("RNNSTopolText1")),
                       column(4, verbatimTextOutput("RNNSTopolText2")),
                       column(4, verbatimTextOutput("RNNSTopolText3"))
              ),
              
              tabPanel("Neuronale Netze Version Keras",
                       column(4, uiOutput("Normalisierung1")),
                       column(4, uiOutput("Normalisierung2")),
                       column(4, uiOutput("Normalisierung3")),
                       
                       column(4, uiOutput("ActivationFunctionN1L1")),
                       column(4, uiOutput("ActivationFunctionN2L1")),
                       column(4, uiOutput("ActivationFunctionN3L1")),
                       
                       column(4, uiOutput("Layer1Neuronen1")),
                       column(4, uiOutput("Layer1Neuronen2")),
                       column(4, uiOutput("Layer1Neuronen3")),
                       
                       column(4, uiOutput("ActivationFunctionN1L2")),
                       column(4, uiOutput("ActivationFunctionN2L2")),
                       column(4, uiOutput("ActivationFunctionN3L2")),
                       
                       
                       column(4, uiOutput("Layer2Neuronen1")),
                       column(4, uiOutput("Layer2Neuronen2")),
                       column(4, uiOutput("Layer2Neuronen3")),
                       
                       column(4, uiOutput("Epochen1")),
                       column(4, uiOutput("Epochen2")),
                       column(4, uiOutput("Epochen3")),
                       
                       column(4, uiOutput("cmptPred1")),
                       column(4, uiOutput("cmptPred2")),
                       column(4, uiOutput("cmptPred3")),
                       
                       hr(),

                       column(4, plotOutput("Netz1", width=500)),
                       column(4, plotOutput("Netz2", width=500)),
                       column(4, plotOutput("Netz3", width=500)),
                       
                       column(4, verbatimTextOutput("predict1")),
                       column(4, verbatimTextOutput("predict2")),
                       column(4, verbatimTextOutput("predict3")),
                       
                       column(4, verbatimTextOutput("eval1")),
                       column(4, verbatimTextOutput("eval2")),
                       column(4, verbatimTextOutput("eval3")),
                       
                       column(4, verbatimTextOutput("Topology1")),
                       column(4, verbatimTextOutput("Topology2")),
                       column(4, verbatimTextOutput("Topology3"))
              )
              
  )
)