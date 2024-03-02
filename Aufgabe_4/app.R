library(shiny)

library(shinythemes)
library(caret)
library(e1071)
library(ggplot2)
library(tidyverse)
library(factoextra)
library(keras)
library(tensorflow)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(RSNNS)

dataset <- read.csv("data/wein-net.csv", header=TRUE, sep=",")
attributes <- colnames(dataset)
xAttributes <- attributes[!attributes=="Klasse"]

# teilt in zufälliger Reihenfolge Elemente in Trainings- und Testdaten auf
index <- createDataPartition(dataset$Klasse, p=0.7, list=FALSE)
training <- dataset[index,]
testing <- dataset[-index,]


wein_data <- read.csv("data/wein-net.csv", header=TRUE, sep=",")
letters <- c('A', 'B', 'C')
wein_data$Klasse <- match(wein_data$Klasse, letters)
wein_data$Klasse <- wein_data$Klasse-1
set.seed(123)

normalization <- function(given_data) {
  new_data <- (given_data - min(given_data)) / (max(given_data) - min(given_data))
  return (new_data)
}

z_transform <- function(given_data) {
  new_data <- (given_data - mean(given_data)) / sd(given_data)
  return (new_data)
}

normalize_data <- function(normalization_method) {
  if (normalization_method == "keras") {
    knn_data <- as.data.frame(normalize(as.matrix(wein_data[1:13])))
  }
  else if (normalization_method == "self build") {
    knn_data <- data.frame(lapply(wein_data[1:13], normalization))
  }
  else if (normalization_method == "z-transform")
  {
    knn_data <- data.frame(lapply(wein_data[1:13], z_transform))
  }
  knn_data$Klasse <- wein_data$Klasse
  return (knn_data)
}



wein_dataRSSN <- wein_data
wein_dataRSSN <- wein_dataRSSN[sample(1:nrow(wein_dataRSSN),length(1:nrow(wein_dataRSSN))),1:ncol(wein_dataRSSN)]
train.wein_dataRSSN <- wein_dataRSSN[,1:13]
target.wein_dataRSSN <- decodeClassLabels(wein_dataRSSN[,14])
wein_dataRSSN.preproc <- splitForTrainingAndTest(train.wein_dataRSSN, target.wein_dataRSSN, ratio=0.3)
wein_dataRSSN.preproc <- normTrainingAndTestSet(wein_dataRSSN.preproc)



# SERVER ------------------------------------------------------------------
shinyServer <- function(input, output, session){
  
  
  # UI COMPONENTS ---------------------------------------------------------
  
  # Aufgabe b)
  output$kernelSelector <- renderUI({
    selectInput("kernel",
                label="Kernel Type:",
                choices=c("linear",
                          "radial",
                          "polynomial",
                          "sigmoid"),
                selected="linear")
  })
  
  output$kernelSelector2 <- renderUI({
    selectInput("kernel2",
                label="Kernel Type:",
                choices=c("linear",
                          "radial",
                          "polynomial",
                          "sigmoid"),
                selected="linear")
  })
  
  output$cSelector <- renderUI({
    numericInput("C",
                 label="Parameter C:",
                 value=1,
                 min=0,
                 max=20,
                 step=0.1)
  })
  
  output$gammaSelector <- renderUI({
    numericInput("gamma",
                 label="Parameter gamma (except linear):",
                 value=1,
                 min=0,
                 max=20,
                 step=0.1)
  })
  
  output$degreeSelector <- renderUI({
    numericInput("degree",
                 label="Parameter degree (polynomial-only):",
                 value=3,
                 min=0,
                 max=20,
                 step=1)
  })
  
  output$coef0Selector <- renderUI({
    numericInput("coef0",
                 label="Parameter coef0 (polynomial&sigmoid-only):",
                 value=0,
                 min=0,
                 max=20,
                 step=0.1)
  })
  
  output$xAxisSelector <- renderUI({
    selectInput("xAttribute",
                label="x-Axis attribute:",
                choices=xAttributes,
                selected="Alkohol" )
  })
  
  output$yAxisSelector <- renderUI({
    selectInput("yAttribute",
                label="y-Axis attribute:",
                choices=xAttributes,
                selected="Apfelsaeure")
  })
  
  # Aufgabe c)
  output$Normalisierung1 <- renderUI({
    selectInput(inputId="Norm1",
                label="Normalisierungsfunktion Netz 1",
                choices=c("self build", "keras", "z-transform")
    )
  })
  
  output$Normalisierung2 <- renderUI({
    selectInput(inputId="Norm2",
                label="Normalisierungsfunktion Netz 2",
                choices=c("self build", "keras", "z-transform")
    )
  })
  
  output$Normalisierung3 <- renderUI({
    selectInput(inputId="Norm3",
                label="Normalisierungsfunktion Netz 3",
                choices=c("self build", "keras", "z-transform")
    )
  })
  
  output$ActivationFunctionN1L1 <- renderUI({
    selectInput(inputId="AFN1L1",
                label="AktivierungsFunktion Netz 1 Layer 1",
                choices=c("elu", "linear", "relu", "sigmoid", "tanh"))
  })
  
  output$ActivationFunctionN1L2 <- renderUI({
    selectInput(inputId="AFN1L2",
                label="AktivierungsFunktion Netz 1 Layer 2",
                choices=c("elu", "linear", "relu", "sigmoid", "tanh"))
  })
  
  output$ActivationFunctionN2L1 <- renderUI({
    selectInput(inputId="AFN2L1",
                label="AktivierungsFunktion Netz 2 Layer 1",
                choices=c("elu", "linear", "relu", "sigmoid", "tanh"))
  })
  
  output$ActivationFunctionN2L2 <- renderUI({
    selectInput(inputId="AFN2L2",
                label="AktivierungsFunktion Netz 2 Layer 2",
                choices=c("elu", "linear", "relu", "sigmoid", "tanh"))
  })
  
  output$ActivationFunctionN3L1 <- renderUI({
    selectInput(inputId="AFN3L1",
                label="AktivierungsFunktion Netz 3 Layer 1",
                choices=c("elu", "linear", "relu", "sigmoid", "tanh"))
  })
  
  output$ActivationFunctionN3L2 <- renderUI({
    selectInput(inputId="AFN3L2",
                label="AktivierungsFunktion Netz 3 Layer 2",
                choices=c("elu", "linear", "relu", "sigmoid", "tanh"))
  })
  
  output$Epochen1 <- renderUI({
    numericInput(inputId="Epoch1",
                 label="Anzahl der Epochen Netz 1",
                 min=1,
                 max=500,
                 step=1,
                 value=10)
  })
  
  output$Epochen2 <- renderUI({
    numericInput(inputId="Epoch2",
                 label="Anzahl der Epochen Netz 2",
                 min=1,
                 max=500,
                 step=1,
                 value=10)
  })
  
  output$Epochen3 <- renderUI({
    numericInput(inputId="Epoch3",
                 label="Anzahl der Epochen Netz 3",
                 min=1,
                 max=500,
                 step=1,
                 value=10)
  })
  
  output$Layer1Neuronen1 <- renderUI({
    sliderInput(inputId="Layer1Neuron1",
                label="Anzahl der Neuronen in Layer 1 fuer Netz1",
                min=0,
                max=50,
                step=1,
                value=9)
  })
  
  output$Layer2Neuronen1 <- renderUI({
    sliderInput(inputId="Layer2Neuron1",
                label="Anzahl der Neuronen in Layer 2 fuer Netz1",
                min=0,
                max=50,
                step=1,
                value=0)
  })
  
  output$Layer1Neuronen2 <- renderUI({
    sliderInput(inputId="Layer1Neuron2",
                label="Anzahl der Neuronen in Layer 1 fuer Netz2",
                min=0,
                max=50,
                step=1,
                value=9)
  })
  
  output$Layer2Neuronen2 <- renderUI({
    sliderInput(inputId="Layer2Neuron2",
                label="Anzahl der Neuronen in Layer 2 fuer Netz2",
                min=0,
                max=50,
                step=1,
                value=0)
  })
  
  output$Layer1Neuronen3 <- renderUI({
    sliderInput(inputId="Layer1Neuron3",
                label="Anzahl der Neuronen in Layer 1 fuer Netz3",
                min=0,
                max=50,
                step=1,
                value=9)
  })
  
  output$Layer2Neuronen3 <- renderUI({
    sliderInput(inputId="Layer2Neuron3",
                label="Anzahl der Neuronen in Layer 2 fuer Netz3",
                min=0,
                max=50,
                step=1,
                value=0)
  })
  
  output$cmptPred1 <- renderUI({
    actionButton(inputId="cmptPr1",
                 label="Model 1 berechnen")
  })
  
  output$cmptPred2 <- renderUI({
    actionButton(inputId="cmptPr2",
                 label="Model 2 berechnen")
  })
  
  output$cmptPred3 <- renderUI({
    actionButton(inputId="cmptPr3",
                 label="Model 3 berechnen")
  })
  
  output$RNNSLayer1Neuron1 <- renderUI({
    numericInput(inputId="RNNSL1N1",
                 min=0,
                 max=50,
                 label="Anzahl Neuronen Layer 1",
                 value=9)
  })
  
  output$RNNSLayer1Neuron2 <- renderUI({
    numericInput(inputId="RNNSL1N2",
                 min=0,
                 max=50,
                 label="Anzahl Neuronen Layer 1",
                 value=9)
  })
  
  output$RNNSLayer1Neuron3 <- renderUI({
    numericInput(inputId="RNNSL1N3",
                 min=0,
                 max=50,
                 label="Anzahl Neuronen Layer 1",
                 value=9)
  })
  
  output$RNNSLayer2Neuron1 <- renderUI({
    numericInput(inputId="RNNSL2N1",
                 min=0,
                 max=50,
                 label="Anzahl Neuronen Layer 2",
                 value=9)
  })
  
  output$RNNSLayer2Neuron2 <- renderUI({
    numericInput(inputId="RNNSL2N2",
                 min=0,
                 max=50,
                 label="Anzahl Neuronen Layer 2",
                 value=9)
  })
  
  output$RNNSLayer2Neuron3 <- renderUI({
    numericInput(inputId="RNNSL2N3",
                 min=0,
                 max=50,
                 label="Anzahl Neuronen Layer 2",
                 value=9)
  })
  
  output$RNNSAktivierung1 <- renderUI({
    selectInput(inputId="RNNSAF1",
                choices = c("Act_Signum", "Act_Exponential", "Act_TanH"),
                label="Aktivierungsfunktion")
  })
  
  output$RNNSAktivierung2 <- renderUI({
    selectInput(inputId="RNNSAF2",
                choices = c("Act_Signum", "Act_Exponential", "Act_TanH"),
                label="Aktivierungsfunktion")
  })
  
  output$RNNSAktivierung3 <- renderUI({
    selectInput(inputId="RNNSAF3",
                choices = c("Act_Signum", "Act_Exponential", "Act_TanH"),
                label="Aktivierungsfunktion")
  })
  
  output$RNNSEpochen1 <- renderUI({
    numericInput(inputId="RNNSEpoch1",
                 min=1,
                 max=500,
                 label="Anzahl Epochen",
                 value=10)
  })
  
  output$RNNSEpochen2 <- renderUI({
    numericInput(inputId="RNNSEpoch2",
                 min=1,
                 max=500,
                 label="Anzahl Epochen",
                 value=10)
  })
  
  output$RNNSEpochen3 <- renderUI({
    numericInput(inputId="RNNSEpoch3",
                 min=1,
                 max=500,
                 label="Anzahl Epochen",
                 value=10)
  })
  
  output$RNNScmptPred1 <- renderUI({
    actionButton(inputId="cmptRNNSPr1",
                 label="Model 1 berechnen")
  })
  
  output$RNNScmptPred2 <- renderUI({
    actionButton(inputId="cmptRNNSPr2",
                 label="Model 2 berechnen")
  })
  
  output$RNNScmptPred3 <- renderUI({
    actionButton(inputId="cmptRNNSPr3",
                 label="Model 3 berechnen")
  })
  
  
  
  # REACTIVES -------------------------------------------------------------
  handleInputs <- reactive({
    if(input$C <= 0) {
      updateNumericInput(session, "C", value = 0.001)
    }
    if(input$gamma <= 0) {
      updateNumericInput(session, "gamma", value = 0.001)
    }
    if(input$degree < 0) {
      updateNumericInput(session, "degree", value = 0)
    }
  })
  
  svm.model <- reactive({
    handleInputs()
    
    kernel <- input$kernel
    C <- input$C
    gamma <- input$gamma
    degree <- input$degree
    coef0 <- input$coef0
    
    if (kernel == "linear") {
      model <- svm(formula=Klasse ~ ., 
                  data=training,
                  method="C-classification",
                  kernel=kernel,
                  cost=C)
    } else if (kernel == "radial") {
      model <- svm(formula=Klasse ~ ., 
                   data=training,
                   method="C-classification",
                   kernel=kernel,
                   gamma=gamma,
                   cost=C)
    } else if (kernel == "polynomial") {
      model <- svm(formula=Klasse ~ ., 
                   data=training,
                   method="C-classification",
                   kernel=kernel,
                   gamma=gamma,
                   cost=C,
                   degree=degree,
                   coef0=coef0)
    } else if (kernel == "sigmoid") {
      model <- svm(formula=Klasse ~ ., 
                   data=training,
                   method="C-classification",
                   kernel=kernel,
                   gamma=gamma,
                   cost=C,
                   coef0=coef0)
    }
    return(model)
  })
  
  # svm.model.2dim <- reactive({
  #   handleInputs()
  #   
  #   kernel <- input$kernel
  #   C <- input$C
  #   gamma <- input$gamma
  #   degree <- input$degree
  #   coef0 <- input$coef0
  #   
  #   xAxis <- input$xAttribute
  #   yAxis <- input$yAttribute
  #   
  #   if (kernel == "linear") {
  #     model <- svm(formula=Klasse ~ xAxis + yAxis, 
  #                  data=training,
  #                  method="C-classification",
  #                  kernel=kernel,
  #                  cost=C)
  #   } else if (kernel == "radial") {
  #     model <- svm(formula=Klasse ~ xAxis + yAxis, 
  #                  data=training,
  #                  method="C-classification",
  #                  kernel=kernel,
  #                  gamma=gamma,
  #                  cost=C)
  #   } else if (kernel == "polynomial") {
  #     model <- svm(formula=Klasse ~ xAxis + yAxis, 
  #                  data=training,
  #                  method="C-classification",
  #                  kernel=kernel,
  #                  gamma=gamma,
  #                  cost=C,
  #                  degree=degree,
  #                  coef0=coef0)
  #   } else if (kernel == "sigmoid") {
  #     model <- svm(formula=Klasse ~ xAxis + yAxis, 
  #                  data=training,
  #                  method="C-classification",
  #                  kernel=kernel,
  #                  gamma=gamma,
  #                  cost=C,
  #                  coef0=coef0)
  #   }
  #   return(model)
  # })
  
  
  
  # performs 10-fold cross-validation
  svm.tuned <- reactive({
    kernel <- input$kernel2
    
    if (kernel == "linear") {
      tuned <- tune(method=svm,
                    train.x=Klasse ~ .,
                    data=training,
                    kernel=kernel,
                    ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 2, 3, 4, 5, 10)))
    } else if (kernel == "radial") {
      tuned <- tune(method=svm,
                    train.x=Klasse ~ .,
                    data=training,
                    kernel=kernel,
                    # gamma wird, wenn ich es hier definiere, trotzdem nicht einbezogen
                    ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 2, 3, 4, 5, 10)))
      # lange Berechnungszeit
    } else if (kernel == "sigmoid") {
      tuned <- tune(method=svm,
                    train.x=Klasse ~ .,
                    data=training,
                    kernel=kernel,
                    ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 2, 3, 4, 5, 10),
                                  # gamma wird, wenn ich es hier definiere, trotzdem nicht einbezogen
                                  coef0 = c(-10,-8,-6,-4,-2,0,2,4,6,8,10)))
      # viel zu lange Berechnungszeit
    } else if (kernel == "polynomial") {
      tuned <- tune(method=svm,
                    train.x=Klasse ~ .,
                    data=training,
                    kernel=kernel,
                    ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 2, 3, 4, 5, 10),
                                  # gamma wird, wenn ich es hier definiere, trotzdem nicht einbezogen
                                  degree = c(0,1,2,3,4,5),
                                  coef0 = c(-10,-8,-6,-4,-2,0,2,4,6,8,10)))
    }
    return(tuned)
  })
  
  
  
  # OUTPUT COMPONETS ------------------------------------------------------
  
  # Aufgabe b)
  output$confusionMatrix <- renderPrint({
    prediction <- predict(svm.model(), testing)
    table(pred = prediction, real = testing$Klasse)
  })
  
  output$statistics <- renderText({
    model <- svm.model()
    prediction <- predict(svm.model(), testing)
    paste("Accuracy: ", mean(prediction == testing$Klasse))
  })

  output$clusterPlot <- renderPlot({
    km <- kmeans(dataset[,c(input$xAttribute,input$yAttribute)], 3)
    fviz_cluster(km,
                 data=dataset[,c(input$xAttribute,input$yAttribute)],
                 ellipse.type="convex") +
                 theme_minimal()
  })
  
  output$classificationPlot <- renderPlot({
  #   model <- svm.model.2dim()
  #   
  #   #plot data and separating hyperplane
  #   plot(input$yAttribute ~ input$xAttribute,
  #        data = training,
  #        col = Klasse)
  #   (cf <- coef(model))
  #   abline(-cf[1]/cf[3], -cf[2]/cf[3], col = "red")
  #   
  #   # plot margin and mark support vectors
  #   abline(-(cf[1] + 1)/cf[3], -cf[2]/cf[3], col = "blue")
  #   abline(-(cf[1] - 1)/cf[3], -cf[2]/cf[3], col = "blue")
  #   points(m$SV, pch = 5, cex = 2)
  #   

  })
  
  output$tuningStats <- renderPrint({
    summary(svm.tuned())
  })
  
  output$tuningAccuracy <- renderText({
    tuned <- svm.tuned()
    tunedBest <- tuned$best.model
    predBest <- predict(tunedBest, testing)
    paste("Accuracy: ", mean(predBest == testing$Klasse))
  })
  
  
  # Aufgabe c)
  output$Netz1 <- renderPlot({
    compute_predictions1()
    if (! is.null(history1))
    {
      plot(history1)
    }
  })
  
  output$Netz2 <- renderPlot({
    compute_predictions2()
    if (! is.null(history2))
    {
      plot(history2)
    }
  })
  
  output$Netz3 <- renderPlot({
    compute_predictions3()
    if (! is.null(history3))
    {
      plot(history3)
    }
  })
  
  output$predict1 <- renderPrint({
    compute_predictions1()
    if (! is.null(model1))
    {
      print("confusion matrix:")
      predictions <- predict_classes(model1, testing.data1)
      confusion_matrix <- table(testing.target1, predictions)
      print(confusion_matrix)
    }
  })
  
  output$predict2 <- renderPrint({
    compute_predictions2()
    if (! is.null(model2))
    {
      print("confusion matrix:")
      predictions <- predict_classes(model2, testing.data2)
      confusion_matrix <- table(testing.target2, predictions)
      print(confusion_matrix)
    }
  })
  
  output$predict3 <- renderPrint({
    compute_predictions3()
    if (! is.null(model3))
    {
      print("confusion matrix:")
      predictions <- predict_classes(model3, testing.data3)
      confusion_matrix <- table(testing.target3, predictions)
      print(confusion_matrix)
    }
  })
  
  output$eval1 <- renderPrint({
    compute_predictions1()
    if (! is.null(model1))
    {
      print("Fehler/Genauigkeit:")
      evaluation <- evaluate(model1, testing.data1, testing.target_labels1)
      print(evaluation)
    }
  })
  
  output$eval2 <- renderPrint({
    compute_predictions2()
    if (! is.null(model2))
    {
      print("Fehler/Genauigkeit:")
      evaluation <- evaluate(model2, testing.data2, testing.target_labels2)
      print(evaluation)
    }
  })
  
  output$eval3 <- renderPrint({
    compute_predictions3()
    if (! is.null(model3))
    {
      print("Fehler/Genauigkeit:")
      evaluation <- evaluate(model3, testing.data3, testing.target_labels3)
      print(evaluation)
    }
  })
  
  output$Topology1 <- renderPrint({
    compute_predictions1()
    if(! is.null(model1))
    {
      print("Topology:")
      print(summary(model1))
      print(model1$weights)
    }
  })
  
  output$Topology2 <- renderPrint({
    compute_predictions2()
    if(! is.null(model2))
    {
      print("Topology:")
      print(summary(model2))
      print(model2$weights)
    }
  })
  
  output$Topology3 <- renderPrint({
    compute_predictions3()
    if(! is.null(model3))
    {
      print("Topology:")
      print(summary(model3))
      print(model3$weights)
    }
  })
  
  output$RNNSPlot1 <- renderPlot({
    compute_RNNS_predictions1()
    if (! is.null(RNNSmodel1))
    {
      plotIterativeError(RNNSmodel1, main="Lernkurve - schwarz: FitError - rot: TestError")
    }
  })
  
  output$RNNSPlot2 <- renderPlot({
    compute_RNNS_predictions2()
    if (! is.null(RNNSmodel2))
    {
      plotIterativeError(RNNSmodel2, main="Lernkurve - schwarz: FitError - rot: TestError")
    }
  })
  
  output$RNNSPlot3 <- renderPlot({
    compute_RNNS_predictions3()
    if (! is.null(RNNSmodel3))
    {
      plotIterativeError(RNNSmodel3, main="Lernkurve - schwarz: FitError - rot: TestError")
    }
  })
  
  output$evalRNNS1 <- renderPrint({
    compute_RNNS_predictions1()
    if(! is.null(RNNSmodel1))
    {
      predictions <- predict(RNNSmodel1,wein_dataRSSN.preproc$inputsTest)
      print("Confusion Matrix:")
      print(confusionMatrix(wein_dataRSSN.preproc$targetsTest,predictions))
    }
  })
  
  output$evalRNNS2 <- renderPrint({
    compute_RNNS_predictions2()
    if(! is.null(RNNSmodel2))
    {
      predictions <- predict(RNNSmodel2,wein_dataRSSN.preproc$inputsTest)
      print("Confusion Matrix:")
      print(confusionMatrix(wein_dataRSSN.preproc$targetsTest,predictions))
    }
  })
  
  output$evalRNNS3 <- renderPrint({
    compute_RNNS_predictions3()
    if(! is.null(RNNSmodel3))
    {
      predictions <- predict(RNNSmodel3,wein_dataRSSN.preproc$inputsTest)
      print("Confusion Matrix:")
      print(confusionMatrix(wein_dataRSSN.preproc$targetsTest,predictions))
    }
  })
  
  output$RNNSTopol1 <- renderPlot({
    compute_RNNS_predictions1()
    if (! is.null(RNNSmodel1))
    {
      plot.nnet(RNNSmodel1)
    }
  })
  
  output$RNNSTopol2 <- renderPlot({
    compute_RNNS_predictions2()
    if (! is.null(RNNSmodel2))
    {
      plot.nnet(RNNSmodel2)
    }
  })
  
  output$RNNSTopol3 <- renderPlot({
    compute_RNNS_predictions3()
    if (! is.null(RNNSmodel3))
    {
      plot.nnet(RNNSmodel3)
    }
  })
  
  output$RNNSTopolText1 <- renderPrint({
    compute_RNNS_predictions1()
    if (! is.null(RNNSmodel1))
    {
      print("Topology:")
      print(summary(RNNSmodel1))
    }
  })
  
  output$RNNSTopolText2 <- renderPrint({
    compute_RNNS_predictions2()
    if (! is.null(RNNSmodel2))
    {
      print("Topology:")
      print(summary(RNNSmodel2))
    }
  })
  
  output$RNNSTopolText3 <- renderPrint({
    compute_RNNS_predictions3()
    if (! is.null(RNNSmodel3))
    {
      print("Topology:")
      print(summary(RNNSmodel3))
    }
  })


    # TOM ---------------------------------------------------------

  
  compute_keras_nn <- function(mod, normal, l1neuron, l2neuron, l1act, l2act, epoch) ({
    knn_data <- normalize_data(normal)
    l1neuron <- as.numeric(l1neuron)
    l2neuron <- as.numeric(l2neuron)
    epoch <- as.numeric(epoch)
    
    indicator <- sample(2, nrow(knn_data), replace=TRUE, prob=c(0.7, 0.3))
    training.data <- as.matrix(knn_data[indicator==1, 1:13])
    testing.data <- as.matrix(knn_data[indicator==2, 1:13])
    training.target <- as.matrix(knn_data[indicator==1, 14])
    testing.target <- as.matrix(knn_data[indicator==2, 14])
    training.target_labels <- as.matrix(to_categorical(training.target))
    testing.target_labels <- as.matrix(to_categorical(testing.target))
    
    model <- keras_model_sequential()
    if (l1neuron > 0)
    {
      layer_dense(model, units=l1neuron, activation=l1act, input_shape = c(13))
      if (l2neuron > 0)
      {
        layer_dense(model, units=l2neuron, activation=l2act)
      }
      layer_dense(model, units=3, activation="softmax")
    }
    else
    {
      layer_dense(model, units=3, activation="softmax", input_shape = c(13))
    }
    compile(model, loss="mean_squared_error", optimizer="adam", metrics=c("accuracy"))
    history <- fit(model, training.data, training.target_labels, epochs=epoch, batch_size=5, validation_split=0.2)
    if (mod == 1)
    {
      assign("model1", model, envir=.GlobalEnv)
      assign("testing.data1", testing.data, envir=.GlobalEnv)
      assign("testing.target1", testing.target, envir=.GlobalEnv)
      assign("testing.target_labels1", testing.target_labels, envir=.GlobalEnv)
      assign("history1", history, envir=.GlobalEnv)
    }
    else if (mod == 2)
    {
      assign("model2", model, envir=.GlobalEnv)
      assign("testing.data2", testing.data, envir=.GlobalEnv)
      assign("testing.target2", testing.target, envir=.GlobalEnv)
      assign("testing.target_labels2", testing.target_labels, envir=.GlobalEnv)
      assign("history2", history, envir=.GlobalEnv)
    }
    else if (mod == 3)
    {
      assign("model3", model, envir=.GlobalEnv)
      assign("testing.data3", testing.data, envir=.GlobalEnv)
      assign("testing.target3", testing.target, envir=.GlobalEnv)
      assign("testing.target_labels3", testing.target_labels, envir=.GlobalEnv)
      assign("history3", history, envir=.GlobalEnv)
    }
  })
  
  
  compute_predictions1 <- eventReactive(input$cmptPr1, compute_keras_nn(1, input$Norm1, input$Layer1Neuron1, input$Layer2Neuron1, input$AFN1L1,
                                                                        input$AFN1L2, input$Epoch1))
  compute_predictions2 <- eventReactive(input$cmptPr2, compute_keras_nn(2, input$Norm2, input$Layer1Neuron2, input$Layer2Neuron2, input$AFN2L1, 
                                                                        input$AFN2L2, input$Epoch2))
  compute_predictions3 <- eventReactive(input$cmptPr3, compute_keras_nn(3, input$Norm3, input$Layer1Neuron3, input$Layer2Neuron3, input$AFN3L1, 
                                                                        input$AFN3L2, input$Epoch3))
  
  compute_RNNS_predictions1 <- eventReactive(input$cmptRNNSPr1, compute_RNNS_model(1, input$RNNSL1N1, input$RNNSL2N1, input$RNNSAF1, input$RNNSEpoch1))
  compute_RNNS_predictions2 <- eventReactive(input$cmptRNNSPr2, compute_RNNS_model(2, input$RNNSL1N2, input$RNNSL2N2, input$RNNSAF2, input$RNNSEpoch2))
  compute_RNNS_predictions3 <- eventReactive(input$cmptRNNSPr3, compute_RNNS_model(3, input$RNNSL1N3, input$RNNSL2N3, input$RNNSAF3, input$RNNSEpoch3))
  
  compute_RNNS_model <- function (mod, l1neur, l2neur, af, epoch)({
    layer1Neurons <- as.numeric(l1neur)
    layer2Neurons <- as.numeric(l2neur)
    activationfunction <- af
    epochen <- as.numeric(epoch)
    if (layer1Neurons > 0)
    {
      if (layer2Neurons > 0)
      {
        layer_size <- c(layer1Neurons, layer2Neurons)
      }
      else
      {
        layer_size <- c(layer1Neurons)
      }
    }
    else
    {
      layer_size <- 0
    }
    model <- mlp(wein_dataRSSN.preproc$inputsTrain, wein_dataRSSN.preproc$targetsTrain,
                     size=layer_size, learnFuncParams=c(0.1), maxit=epochen, inputsTest=wein_dataRSSN.preproc$inputsTest,
                     targetsTest=wein_dataRSSN.preproc$targetsTest, hiddenActFunc=activationfunction)
    if (mod == 1)
    {  
      assign("RNNSmodel1", model, envir=.GlobalEnv)
    }
    else if (mod == 2)
    {
      assign("RNNSmodel2", model, envir=.GlobalEnv)
    }
    else if (mod == 3)
    {
      assign("RNNSmodel3", model, envir=.GlobalEnv)
    }
  })

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



app <- shinyApp(ui, shinyServer)