library(shiny)
library(shinyWidgets)
library(ggplot2)
library(GGally)
library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(nnet)

wein_data <- read.csv("data/wein-classifier.csv", header=TRUE, sep=",")
wein_data$Klasse <- as.factor(wein_data$Klasse)


# teilt in zufälliger Reihenfolge Elemente in Trainings- und Testdaten auf
index <- createDataPartition(wein_data$Klasse, p=0.7, list=FALSE)
training <- wein_data[index,]
testing <- wein_data[-index,]

trainX <- training[,names(training) != "Klasse"]

# Pre-Processing
preProcValues <- c("center", "scale")

set.seed(400)
# Validation Features
cv_control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
                           # method = "repeatedcv", repeats = 10)
boot_control <- trainControl(method = "boot632")
metric <- "Accuracy"

# Logistic Regression

model.reg.cv <- nnet::multinom(Klasse ~., 
                               data = training, 
                               model = TRUE)
model.reg.boot <- nnet::multinom(Klasse ~., 
                                 data = training, 
                                 model = TRUE)

predict.reg.cv <- predict(model.reg.cv, testing)
predict.reg.boot <- predict(model.reg.boot, testing)

# Decision Tree

model.tree.cv <- train(Klasse ~.,
                       data = training,
                       method = "rpart",
                       trControl = cv_control)

model.tree.boot <- train(Klasse ~.,
                       data = training,
                       method = "rpart",
                       trControl = boot_control)

predict.tree.cv <- predict(model.tree.cv, testing, type = "raw")
predict.tree.boot <- predict(model.tree.boot, testing, type = "raw")

# K-Nearest Neighbors (k=3)

model.knn.cv <- train(Klasse ~., 
                    data = training, 
                    method = "knn", 
                    metric = metric, 
                    trControl = cv_control,
                    preProc = preProcValues,
                    tuneGrid = expand.grid(k = 3))

model.knn.boot <- train(Klasse ~., 
                    data = training, 
                    method = "knn", 
                    metric = metric, 
                    trControl = boot_control,
                    preProc = preProcValues,
                    tuneGrid = expand.grid(k = 3))

predict.knn.cv <- predict(model.knn.cv, testing)
predict.knn.boot <- predict(model.knn.boot, testing)



# SERVER ------------------------------------------------------------------
shinyServer(function(input, output){
  
  # gibt das zum Input gehörige Predicted Model zurück
  handle_selections <- function(validSelect, classSelect) {
    model <- NULL
    if (validSelect == "cv" && classSelect == "reg") {
      model <- predict.reg.cv
    } else if (validSelect == "cv" && classSelect == "tree") {
      model <- predict.tree.cv
    } else if (validSelect == "cv" && classSelect == "knn") {
      model <- predict.knn.cv
    } else if (validSelect == "boot" && classSelect == "reg") {
      model <- predict.reg.boot
    } else if (validSelect == "boot" && classSelect == "tree") {
      model <- predict.tree.boot
    } else if (validSelect == "boot" && classSelect == "knn") {
      model <- predict.knn.boot
    }
    
    return (model)
  }
  
  output$PlotType <- renderUI({
    selectInput(inputId = "plottype",
                label = "Funktion für Scatterplotmatrix",
                choices = c("Pairs", "GGPairs")
                )
  })
  
  output$Attribute <- renderUI({
    pickerInput(inputId = "attribute_names",
                label = "Attribute",
                choices = names(wein_data)[-14],
                multiple = TRUE,
                options = list("actions-box" = TRUE),
                selected = names(wein_data)[1:13]
                )
  })
  
  output$Scatplots <- renderPlot({
    if (input$plottype == "Pairs")
    {
      pairs(wein_data[,input$attribute_names], col=wein_data$Klasse)
    }
    else if (input$plottype == "GGPairs")
    {
      print(ggpairs(data=wein_data, columns=input$attribute_names, aes(color=Klasse)))
    }

  })
  
  output$ResamplingSelector <- renderUI({
    selectInput(inputId = "valid_type",
                label = "Resamplingsmethode",
                choices = c("10-fold Cross Validation" = "cv", 
                            "Bootstrapping .632" = "boot")
                )
  })
  
  output$ClassifierSelector <- renderUI({
    selectInput(inputId = "class_type",
                label = "Klassifizierungsmethode",
                choices = c("Logistic Regression" = "reg", 
                            "Decision Tree" = "tree", 
                            "k-nearest Neighbor (k=3)" = "knn")
    )
  })
  
  output$StatisticData <- renderTable({
    model <- handle_selections(input$valid_type, input$class_type)

  })
  
  output$ConfusionMatrix <- renderPrint({
    model <- handle_selections(input$valid_type, input$class_type)
    confusionMatrix(model, testing$Klasse, dnn = c("Prediction", "Reference"))
  })
   
})