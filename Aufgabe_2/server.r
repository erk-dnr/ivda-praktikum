library(shiny)
library(ggplot2)
library(cluster)
library(ggfortify)
library(shinyWidgets)

# read Data
data <- read.csv("data/wein.csv", header=TRUE, sep=",")
attributes <- colnames(data)

z_transform <- function(raw_data){
  transformed_data <- raw_data
  for (i in 1:nrow(raw_data))
  {
    new_row <- c()
    for (j in 1:length(raw_data))
    {
      new_row[j] <- (raw_data[i,j] - mean(raw_data[,j])) / sd(raw_data[,j])
    }
    transformed_data[i,] <- new_row
  }
  return (transformed_data)
}


#cluster Data
normalize_function <- function(x){
  min_value <- min(x)
  max_value <- max(x)
  normalized_vector <- (x - min_value)/(max_value - min_value)
  return (normalized_vector)
}

transform_via_normalization <- function(raw_data){
  new_data <- raw_data
  new_data$Alkohol <- normalize_function(raw_data$Alkohol)
  new_data$Apfelsaeure <- normalize_function(raw_data$Apfelsaeure)
  new_data$Asche <- normalize_function(raw_data$Asche)
  new_data$Aschen_Alkanitaet <- normalize_function(raw_data$Aschen_Alkanitaet)
  new_data$Magnesium <- normalize_function(raw_data$Magnesium)
  new_data$Alle_Phenole <- normalize_function(raw_data$Alle_Phenole)
  new_data$Flavanoide <- normalize_function(raw_data$Flavanoide)
  new_data$Nichtflavanoide_Phenole <- normalize_function(raw_data$Nichtflavanoide_Phenole)
  new_data$Proanthocyanide <- normalize_function(raw_data$Proanthocyanide)
  new_data$Farbintensitaet <- normalize_function(raw_data$Farbintensitaet)
  new_data$Farbwert <- normalize_function(raw_data$Farbwert)
  new_data$Proteinwert <- normalize_function(raw_data$Proteinwert)
  new_data$Prolinwert <- normalize_function(raw_data$Prolinwert)
  return (new_data)
}

transform_all_via_log2 <- function(raw_data){
  new_data <- raw_data
  new_data$Alkohol <- log2(raw_data$Alkohol)
  new_data$Apfelsaeure <- log2(raw_data$Apfelsaeure)
  new_data$Asche <- log2(raw_data$Asche)
  new_data$Aschen_Alkanitaet <- log2(raw_data$Aschen_Alkanitaet)
  new_data$Magnesium <- log2(raw_data$Magnesium)
  new_data$Alle_Phenole <- log2(raw_data$Alle_Phenole)
  new_data$Flavanoide <- log2(raw_data$Flavanoide)
  new_data$Nichtflavanoide_Phenole <- log2(raw_data$Nichtflavanoide_Phenole)
  new_data$Proanthocyanide <- log2(raw_data$Proanthocyanide)
  new_data$Farbintensitaet <- log2(raw_data$Farbintensitaet)
  new_data$Farbwert <- log2(raw_data$Farbwert)
  new_data$Proteinwert <- log2(raw_data$Proteinwert)
  new_data$Prolinwert <- log2(raw_data$Prolinwert)
  return (new_data)
}

transform_selected_via_log2 <- function(raw_data){
  new_data <- raw_data
  new_data$Apfelsaeure <- log2(raw_data$Apfelsaeure)
  new_data$Asche <- log2(raw_data$Asche)
  new_data$Aschen_Alkanitaet <- log2(raw_data$Aschen_Alkanitaet)
  new_data$Magnesium <- log2(raw_data$Magnesium)
  new_data$Proanthocyanide <- log2(raw_data$Proanthocyanide)
  new_data$Farbintensitaet <- log2(raw_data$Farbintensitaet)
  new_data$Farbwert <- log2(raw_data$Farbwert)
  return (new_data)
}

silhouetteCoefficientPlot <- function(k_value, transformation){
  clustered_data <- get_clustered_data(k_value, transformation)
  my_clustered_data <- data
  my_clustered_data['Cluster'] <- clustered_data$cluster
  silhouetteCoefficients <- silhouette(clustered_data$cluster, dist(data, method="euclidean"))
  silhouetteCoefficients <- sortSilhouette(silhouetteCoefficients)
  data_to_plot <- data.frame(formatC(as.character(paste("Datapoint:", seq(c(1:nrow(data))), "Cluster:", silhouetteCoefficients[,1], sep=" ")), width=3, format="s", flag="0"), silhouetteCoefficients[,1], silhouetteCoefficients[,3])
  names(data_to_plot) <- c("Datapoint", "Cluster", "SilhouetteCoefficient")
  data_to_plot
}

get_clustered_data <- function(k_value, transformation){
  new_data <- data
  if (transformation == "Log2 - Alle Attribute")
  {
    new_data <- transform_all_via_log2(new_data)
  }
  else if (transformation == "Normalisierung")
  {
    new_data <- transform_via_normalization(new_data)
  }
  else if (transformation == "Log2 - Attribute mit Ausreissern")
  {
   new_data <- transform_selected_via_log2(new_data) 
  }
  else if (transformation == "Z-Transformation")
  {
    new_data <- z_transform(new_data)
  }
  clustered_data <- kmeans(new_data, k_value)
  return (clustered_data)
}

get_datapoints_of_other_clusters <- function(x, dataset){
  new_dataset <- subset(dataset, dataset[,14] != x[,14])
  return (new_dataset)
}

get_datapoints_of_same_cluster <- function(x, dataset){
  new_dataset <- subset(dataset, dataset[,14] == x[,14])
  return (new_dataset)
}

my_sil_coeff <- function(k_value, transformation){
  clustered_data <- get_clustered_data(k_value, transformation)
  my_new_data <- data
  my_new_data[14] <- clustered_data$cluster
  my_new_data[15] <- c(1:nrow(my_new_data))
  dist_matr <- as.matrix(dist(data, method="euclidean"))
  sil_coeff_vec <- c()
  for (i in 1:nrow(my_new_data)){
    same_cluster <- get_datapoints_of_same_cluster(my_new_data[i,], my_new_data)[,15]
    other_clusters <- get_datapoints_of_other_clusters(my_new_data[i,], my_new_data)[,15]
    dist_same_cluster <- 0
    for (j in same_cluster){
      dist_same_cluster <- dist_same_cluster + dist_matr[i,j]
    }
    dist_same_cluster <- dist_same_cluster / length(same_cluster)
    min_value <- other_clusters[1]
    min_dist <- dist_matr[1,other_clusters[1]]
    for (j in other_clusters){
      if (dist_matr[i,j] < min_dist){
        min_dist <- dist_matr[i, j]
        min_value <- j
      }
    }
    min_dist_clust <- get_datapoints_of_same_cluster(my_new_data[min_value,], my_new_data)[,15]
    dist_min_dist_cluster <- 0
    for (j in min_dist_clust){
      dist_min_dist_cluster <- dist_min_dist_cluster + dist_matr[i,j]
    }
    dist_min_dist_cluster <- dist_min_dist_cluster / length(min_dist_clust)
    my_sil_coeff <- (dist_min_dist_cluster - dist_same_cluster) / max(dist_min_dist_cluster, dist_same_cluster)
    sil_coeff_vec[i] <- my_sil_coeff
  }
  silhouette_coefficient_frame <- data.frame(formatC(as.character(paste("Datapoint:", seq(c(1:nrow(my_new_data))), "Cluster:", my_new_data[,14], sep=" "))),
                                             my_new_data[,14], sil_coeff_vec)
  names(silhouette_coefficient_frame) <- c("Datapoint", "Cluster", "SilhouetteCoefficient")
  silhouette_coefficient_frame <- silhouette_coefficient_frame[order(silhouette_coefficient_frame$Cluster, -silhouette_coefficient_frame$SilhouetteCoefficient),]
  silhouette_coefficient_frame$Datapoint <- factor(silhouette_coefficient_frame$Datapoint, levels=silhouette_coefficient_frame$Datapoint)
  return (silhouette_coefficient_frame)
}






# Server -----------------------------------------------------------------
shinyServer(function(input, output){
  
  # Aufgabe a ------------------------------------------------------------
  output$attribute1BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Alkohol)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 15)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute2BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Apfelsaeure)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 6)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute3BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Asche)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 3.25))  + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute4BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Aschen_Alkanitaet)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 31)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute5BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Magnesium)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 170)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute6BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Alle_Phenole)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 4)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute7BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Flavanoide)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 5.5)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute8BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Nichtflavanoide_Phenole)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 0.7)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute9BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Proanthocyanide)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 4)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute10BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Farbintensitaet)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 15)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute11BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Farbwert)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 1.75)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute12BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Proteinwert)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 4.5)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute13BP <- renderPlot({
    print(ggplot(data, aes(x="", y=Prolinwert)) + 
            geom_boxplot() + coord_cartesian(ylim = c(0, 1750)) + labs(x="")) +
            theme(aspect.ratio = 1/2)
  })
  
  output$attribute1HP <- renderPlot({
    print(ggplot(data, aes(Alkohol)) +
            geom_histogram(binwidth = 0.2)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute2HP <- renderPlot({
    print(ggplot(data, aes(Apfelsaeure)) +
            geom_histogram(binwidth = 0.2)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute3HP <- renderPlot({
    print(ggplot(data, aes(Asche)) +
            geom_histogram(binwidth = 0.1)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute4HP <- renderPlot({
    print(ggplot(data, aes(Aschen_Alkanitaet)) +
            geom_histogram(binwidth = 1)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute5HP <- renderPlot({
    print(ggplot(data, aes(Magnesium)) +
            geom_histogram(binwidth = 5)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute6HP <- renderPlot({
    print(ggplot(data, aes(Alle_Phenole)) +
            geom_histogram(binwidth = 0.2)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute7HP <- renderPlot({
    print(ggplot(data, aes(Flavanoide)) +
            geom_histogram(binwidth = 0.2)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute8HP <- renderPlot({
    print(ggplot(data, aes(Nichtflavanoide_Phenole)) +
            geom_histogram(binwidth = 0.05)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute9HP <- renderPlot({
    print(ggplot(data, aes(Proanthocyanide)) +
            geom_histogram(binwidth = 0.2)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute10HP <- renderPlot({
    print(ggplot(data, aes(Farbintensitaet)) +
            geom_histogram(binwidth = 1)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute11HP <- renderPlot({
    print(ggplot(data, aes(Farbwert)) +
            geom_histogram(binwidth = 0.1)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute12HP <- renderPlot({
    print(ggplot(data, aes(Proteinwert)) +
            geom_histogram(binwidth = 0.2)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  output$attribute13HP <- renderPlot({
    print(ggplot(data, aes(Prolinwert)) +
            geom_histogram(binwidth = 100)) + labs(y = "Häufigkeit") +
            theme(aspect.ratio = 1/3)
  })
  
  # Aufgabe b ----------------------------------------------------------
  output$xAxisSelector <- renderUI({
    selectInput(inputId = "xAttribute",
                label = "x-Achse",
                #choices = setdiff(attributes, input$yAttribute),
                choices = attributes,
                selected = attributes[1]
                )
  })
  
  output$yAxisSelector <- renderUI({
    selectInput(inputId = "yAttribute",
                label = "y-Achse",
                #choices = setdiff(attributes, input$xAttribute),
                choices = attributes,
                selected = attributes[2]
    )
  })
  
  regressionModel <- reactive({
    xValues <- data[,input$xAttribute]
    yValues <- data[,input$yAttribute]
    model <- lm(yValues ~ xValues, data)
    print(summary(model))
    return(model)
  })

  output$regressionPlot <- renderPlot({
    xValues <- data[,input$xAttribute]
    yValues <- data[,input$yAttribute]
    model <- regressionModel()
    predicted <- predict(model)
    residuals <- residuals(model)
    plot <- ggplot(data, aes(x=xValues, y=yValues)) +
      geom_point(size = 2) + 
      geom_point(aes(y = predicted), shape = 1, size = 2) +
      geom_segment(aes(xend = xValues, yend = predicted), alpha = 0.2) +
      #geom_smooth(method = "lm", se = FALSE, color = "red") +
      geom_line(data = fortify(model), aes(x = xValues, y = .fitted), color = "red", size = 1) +
      labs(x=input$xAttribute, y=input$yAttribute) +
      theme(aspect.ratio = 3/5)
    print(plot)
  })
  
  output$regressionFunction <- renderText({
    # f(x) = mx + b 
    model <- regressionModel()
    m <- coef(summary(model))["xValues", "Estimate"]
    b <- coef(summary(model))["(Intercept)", "Estimate"]
    paste0("Regressionsgerade:\n\t", m, " * ", input$xAttribute, " + ", b)
  })
  
  output$residualsPlot <- renderPlot({
    xValues <- data[,input$xAttribute]
    plot <- ggplot(data, aes(x="", y=residuals(regressionModel()))) +
      geom_boxplot() +
      coord_flip() +
      labs(x = input$xAttribute, y = "Residuals") +
      theme(aspect.ratio = 1/3) +
      ggtitle("Residual Plot")
    print(plot)
  })
  
  output$regressionProperties <- renderText({
    model <- regressionModel()
    paste0("p-Value:\t\t", summary(model)$coefficients[2,4], "\nR-squared:\t", summary(model)$r.squared)
  })
  
  
  # Aufgabe c/d ----------------------------------------------------------
  output$SilCoeffFunc <- renderUI({
    selectInput(inputId = "silhouettecoefficientFunction",
                label = "verwendete Funktion fuer Silhouette Coefficient:",
                choices = c("Cluster", "Selbsterstellt"),
                selected = "Selbsterstellt"
                )
  })
  
  output$SilCoeffFuncPCA <- renderUI({
    selectInput(inputId = "silhouettecoefficientFunctionPCA",
                label = "verwendete Funktion fuer Silhouette Coefficient:",
                choices = c("Cluster", "Selbsterstellt"),
                selected = "Selbsterstellt"
    )
  })
  
  output$SelectSilCoeff <- renderUI({
    sliderInput(inputId = "SilCoeff",
                label = "nur Datenpunkte mit Silhouette-Coefficient in folgendem Bereich anzeigen:",
                min = -1,
                max = 1,
                step = 0.01,
                value = c(0, 1))
  })
  
  output$SelectSilCoeffPCA <- renderUI({
    sliderInput(inputId = "SilCoeff2",
                label = "nur Datenpunkte mit Silhouette-Coefficient in folgendem Bereich anzeigen:",
                min = -1,
                max = 1,
                step = 0.01,
                value = c(0, 1))
  })
  
  output$TransformDataSil <- renderUI({
    selectInput(inputId = "TransformSil",
                label = "Transformation der Datenpunkte:",
                choices = c("Keine", "Log2 - Alle Attribute", "Log2 - Attribute mit Ausreissern", "Normalisierung", "Z-Transformation"))
  })
  
  output$TransformDataPCA <- renderUI({
    selectInput(inputId = "TransformPCA",
                label = "Transformation der Datenpunkte:",
                choices = c("Keine", "Log2 - Alle Attribute", "Log2 - Attribute mit Ausreissern", "Normalisierung", "Z-Transformation"))
  })
  
  output$SortDatapointsLabel <- renderText({
    print("Sortierung der Datenpunkte absteigend nach berechnetem Silhouette Coefficient:")
  })
  
  output$SortDatapoints <- renderUI({
    switchInput(inputId = "sortDatapoints",
                label = "OFF",
                value = FALSE)
  })
  

  output$SilhouetteClusters2 <- renderPlot({
    transformation <- input$TransformSil
    if (input$silhouettecoefficientFunction == "Cluster")
    {
      data_to_plot <- silhouetteCoefficientPlot(2, transformation)
    }
    else
    {
      data_to_plot <- my_sil_coeff(2, transformation)
    }
    data_to_plot <- subset(data_to_plot, data_to_plot$SilhouetteCoefficient <= max(input$SilCoeff) & data_to_plot$SilhouetteCoefficient >= min(input$SilCoeff))
    if (input$sortDatapoints)
    {
      new_plot <- ggplot(data = data_to_plot, aes(x=reorder(data_to_plot$Datapoint, -data_to_plot$SilhouetteCoefficient), y=data_to_plot$SilhouetteCoefficient))
    }
    else
    {
      new_plot <- ggplot(data = data_to_plot, aes(x=data_to_plot$Datapoint, y=data_to_plot$SilhouetteCoefficient))
    }
    title_string <- ""
    avg_sil_of_cluster <- c()
    for (i in 1:2){
      sil_by_cluster <- subset(data_to_plot, data_to_plot$Cluster == i)
      avg_sil_of_cluster[i] <- sum(sil_by_cluster$SilhouetteCoefficient) / nrow(sil_by_cluster)
      title_string <- paste(title_string, "durchschnittlicher silhouette coeffiecient für cluster", i, ":", as.character(avg_sil_of_cluster[i]), sep=" ")
      title_string <- paste(title_string, "\n", sep="")
    }
    avg_sil_coeff <- as.character(sum(avg_sil_of_cluster) / length(avg_sil_of_cluster))
    cnt_datapoints <- as.character(nrow(data_to_plot))
    new_plot <- new_plot + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Datapoint-ID", y="Silhouette Coefficient") + 
      ggtitle(paste("Silhouette Coefficients fuer 2 Cluster", "\n", title_string, "durchschnittlicher Silhouette-Coefficient:", avg_sil_coeff, "Anzahl Datenpunkte:", cnt_datapoints))
    print(new_plot)
  })

  output$SilhouetteClusters3 <- renderPlot({
    transformation <- input$TransformSil
    if (input$silhouettecoefficientFunction == "Cluster")
    {
      data_to_plot <- silhouetteCoefficientPlot(3, transformation)
    }
    else
    {
      data_to_plot <- my_sil_coeff(3, transformation)
    }
    data_to_plot <- subset(data_to_plot, data_to_plot$SilhouetteCoefficient <= max(input$SilCoeff) & data_to_plot$SilhouetteCoefficient >= min(input$SilCoeff))
    if (input$sortDatapoints)
    {
      new_plot <- ggplot(data = data_to_plot, aes(x=reorder(data_to_plot$Datapoint, -data_to_plot$SilhouetteCoefficient), y=data_to_plot$SilhouetteCoefficient))
    }
    else
    {
      new_plot <- ggplot(data = data_to_plot, aes(x=data_to_plot$Datapoint, y=data_to_plot$SilhouetteCoefficient))
    }
    title_string <- ""
    avg_sil_of_cluster <- c()
    for (i in 1:3){
      sil_by_cluster <- subset(data_to_plot, data_to_plot$Cluster == i)
      avg_sil_of_cluster[i] <- sum(sil_by_cluster$SilhouetteCoefficient) / nrow(sil_by_cluster)
      title_string <- paste(title_string, "durchschnittlicher silhouette coeffiecient für cluster", i, ":", as.character(avg_sil_of_cluster[i]), sep=" ")
      title_string <- paste(title_string, "\n", sep="")
    }
    avg_sil_coeff <- as.character(sum(avg_sil_of_cluster) / length(avg_sil_of_cluster))
    cnt_datapoints <- as.character(nrow(data_to_plot))
    new_plot <- new_plot + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Datapoint-ID", y="Silhouette Coefficient") + 
      ggtitle(paste("Silhouette Coefficients fuer 3 Cluster", "\n", title_string, "durchschnittlicher Silhouette-Coefficient:", avg_sil_coeff, "Anzahl Datenpunkte:", cnt_datapoints))
    print(new_plot)
  })
  
  output$SilhouetteClusters4 <- renderPlot({
    transformation <- input$TransformSil
    if (input$silhouettecoefficientFunction == "Cluster")
    {
      data_to_plot <- silhouetteCoefficientPlot(4, transformation)
    }
    else
    {
      data_to_plot <- my_sil_coeff(4, transformation)
    }
    data_to_plot <- subset(data_to_plot, data_to_plot$SilhouetteCoefficient <= max(input$SilCoeff) & data_to_plot$SilhouetteCoefficient >= min(input$SilCoeff))
    if (input$sortDatapoints)
    {
      new_plot <- ggplot(data = data_to_plot, aes(x=reorder(data_to_plot$Datapoint, -data_to_plot$SilhouetteCoefficient), y=data_to_plot$SilhouetteCoefficient))
    }
    else
    {
      new_plot <- ggplot(data = data_to_plot, aes(x=data_to_plot$Datapoint, y=data_to_plot$SilhouetteCoefficient))
    }
    title_string <- ""
    avg_sil_of_cluster <- c()
    for (i in 1:4){
      sil_by_cluster <- subset(data_to_plot, data_to_plot$Cluster == i)
      avg_sil_of_cluster[i] <- sum(sil_by_cluster$SilhouetteCoefficient) / nrow(sil_by_cluster)
      title_string <- paste(title_string, "durchschnittlicher silhouette coeffiecient für cluster", i, ":", as.character(avg_sil_of_cluster[i]), sep=" ")
      title_string <- paste(title_string, "\n", sep="")
    }
    avg_sil_coeff <- as.character(sum(avg_sil_of_cluster) / length(avg_sil_of_cluster))
    cnt_datapoints <- as.character(nrow(data_to_plot))
    new_plot <- new_plot + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Datapoint-ID", y="Silhouette Coefficient") + 
      ggtitle(paste("Silhouette Coefficients fuer 4 Cluster", "\n", title_string, "durchschnittlicher Silhouette-Coefficient:", avg_sil_coeff, "Anzahl Datenpunkte:", cnt_datapoints))
    print(new_plot)
  })
  
  output$SilhouetteClusters5 <- renderPlot({
    transformation <- input$TransformSil
    if (input$silhouettecoefficientFunction == "Cluster")
    {
      data_to_plot <- silhouetteCoefficientPlot(5, transformation)
    }
    else
    {
      data_to_plot <- my_sil_coeff(5, transformation)
    }
    data_to_plot <- subset(data_to_plot, data_to_plot$SilhouetteCoefficient <= max(input$SilCoeff) & data_to_plot$SilhouetteCoefficient >= min(input$SilCoeff))
    if (input$sortDatapoints)
    {
      new_plot <- ggplot(data = data_to_plot, aes(x=reorder(data_to_plot$Datapoint, -data_to_plot$SilhouetteCoefficient), y=data_to_plot$SilhouetteCoefficient))
    }
    else
    {
      new_plot <- ggplot(data = data_to_plot, aes(x=data_to_plot$Datapoint, y=data_to_plot$SilhouetteCoefficient))
    }
    title_string <- ""
    avg_sil_of_cluster <- c()
    for (i in 1:5){
      sil_by_cluster <- subset(data_to_plot, data_to_plot$Cluster == i)
      avg_sil_of_cluster[i] <- sum(sil_by_cluster$SilhouetteCoefficient) / nrow(sil_by_cluster)
      title_string <- paste(title_string, "durchschnittlicher silhouette coeffiecient für cluster", i, ":", as.character(avg_sil_of_cluster[i]), sep=" ")
      title_string <- paste(title_string, "\n", sep="")
    }
    avg_sil_coeff <- as.character(sum(avg_sil_of_cluster) / length(avg_sil_of_cluster))
    cnt_datapoints <- as.character(nrow(data_to_plot))
    new_plot <- new_plot + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Datapoint-ID", y="Silhouette Coefficient") + 
      ggtitle(paste("Silhouette Coefficients fuer 5 Cluster", "\n", title_string, "durchschnittlicher Silhouette-Coefficient:", avg_sil_coeff, "Anzahl Datenpunkte:", cnt_datapoints))
    print(new_plot)
  })
  
  output$PCAVisCluster2 <- renderPlot({
    transformation <- input$TransformPCA
    clustered_data <- get_clustered_data(2, transformation)
    new_data <- data
    new_data$clusters <- as.character(clustered_data$cluster)
    if (input$silhouettecoefficientFunctionPCA == "Selbsterstellt")
    {
      new_data$silCoeff <- my_sil_coeff(2, transformation)$SilhouetteCoefficient
    }
    else
    {
      new_data$silCoeff <- silhouetteCoefficientPlot(2, transformation)$SilhouetteCoefficient
    }
    silcoeff_data <- subset(new_data, new_data$silCoeff <= max(input$SilCoeff2) & new_data$silCoeff >= min(input$SilCoeff2))
    data_w_clusters <- silcoeff_data
    data_w_clusters$silCoeff <- NULL
    data_excld_clusters <- data_w_clusters
    data_excld_clusters$clusters <- NULL
    pca_data <- prcomp(data_excld_clusters)
    autoplot(pca_data, data=data_w_clusters, colour='clusters')
  })
  
  output$PCAVisCluster3 <- renderPlot({
    transformation <- input$TransformPCA
    clustered_data <- get_clustered_data(3, transformation)
    new_data <- data
    new_data$clusters <- as.character(clustered_data$cluster)
    if (input$silhouettecoefficientFunctionPCA == "Selbsterstellt")
    {
      new_data$silCoeff <- my_sil_coeff(3, transformation)$SilhouetteCoefficient
    }
    else
    {
      new_data$silCoeff <- silhouetteCoefficientPlot(3, transformation)$SilhouetteCoefficient
    }
    silcoeff_data <- subset(new_data, new_data$silCoeff <= max(input$SilCoeff2) & new_data$silCoeff >= min(input$SilCoeff2))
    data_w_clusters <- silcoeff_data
    data_w_clusters$silCoeff <- NULL
    data_excld_clusters <- data_w_clusters
    data_excld_clusters$clusters <- NULL
    pca_data <- prcomp(data_excld_clusters)
    autoplot(pca_data, data=data_w_clusters, colour='clusters')
  })
  
  output$PCAVisCluster4 <- renderPlot({
    transformation <- input$TransformPCA
    clustered_data <- get_clustered_data(4, transformation)
    new_data <- data
    new_data$clusters <- as.character(clustered_data$cluster)
    if (input$silhouettecoefficientFunctionPCA == "Selbsterstellt")
    {
      new_data$silCoeff <- my_sil_coeff(4, transformation)$SilhouetteCoefficient
    }
    else
    {
      new_data$silCoeff <- silhouetteCoefficientPlot(4, transformation)$SilhouetteCoefficient
    }
    silcoeff_data <- subset(new_data, new_data$silCoeff <= max(input$SilCoeff2) & new_data$silCoeff >= min(input$SilCoeff2))
    data_w_clusters <- silcoeff_data
    data_w_clusters$silCoeff <- NULL
    data_excld_clusters <- data_w_clusters
    data_excld_clusters$clusters <- NULL
    pca_data <- prcomp(data_excld_clusters)
    autoplot(pca_data, data=data_w_clusters, colour='clusters')
  })
  
  output$PCAVisCluster5 <- renderPlot({
    transformation <- input$TransformPCA
    clustered_data <- get_clustered_data(5, transformation)
    new_data <- data
    new_data$clusters <- as.character(clustered_data$cluster)
    if (input$silhouettecoefficientFunctionPCA == "Selbsterstellt")
    {
      new_data$silCoeff <- my_sil_coeff(5, transformation)$SilhouetteCoefficient
    }
    else
    {
      new_data$silCoeff <- silhouetteCoefficientPlot(5, transformation)$SilhouetteCoefficient
    }
    silcoeff_data <- subset(new_data, new_data$silCoeff <= max(input$SilCoeff2) & new_data$silCoeff >= min(input$SilCoeff2))
    data_w_clusters <- silcoeff_data
    data_w_clusters$silCoeff <- NULL
    data_excld_clusters <- data_w_clusters
    data_excld_clusters$clusters <- NULL
    pca_data <- prcomp(data_excld_clusters)
    autoplot(pca_data, data=data_w_clusters, colour='clusters')
  })
  
})
