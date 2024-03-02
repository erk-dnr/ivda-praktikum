library(shiny)
library(ggplot2)
library(shinyTime)
library(shinyWidgets)
library(chron)
library(lubridate)
library(arules)
library(datasets)
library(dplyr)
library(arulesViz)

# Adjustments
source("./src/utility.R", local = T)

# Einlesen der Daten
df <- read_data()

test_df <- aggregate_transactions(df, as.POSIXlt("2016-30-10", format="%Y-%d-%m"))
test_df

new_df <- read.table("data/baeckerei-ivda-korrigiert.csv", sep=";")
names(new_df) <- c("Datum", "Zeit", "Transaktion", "Item")
new_df <- subset(new_df, new_df$Item != "NONE")
new_df <- data.frame(new_df[,4], new_df[,3])
new_df <- subset(new_df, new_df[,1] != "Item")
names(new_df) <- c("Item", "Transaktion")
new_data <- aggregate(as.character(Item) ~ Transaktion, data=new_df, c)
names(new_data) <- c("Transaktion", "Items")
new_data_1 <- new_data$Items
transactiondata <- as(new_data_1, "transactions")
rules <- apriori (transactiondata, parameter = list(supp=0.0045, confidence=0.5))


# 2-dim vector [Item, n]
items <- get_item_dist(df)

item_dist_min <- min(items$n)
item_dist_max <- max(items$n)

dates <- unique(as.vector(t(df[ ,"Datum"])))
date_min <- min(dates)
date_max <- max(dates)

weeks_and_transactions2 <- unique(data.frame(strftime(df$Datum, format = "%V"), df$Transaktion))
names(weeks_and_transactions2) <- c("Kalenderwoche", "Transaktionsnummer")

months_and_transaction <- data.frame(month(df$Datum), df$Transaktion)
names(months_and_transaction) <- c("Kalendermonat", "Transaktionsnummer")



# Server logic -----------------------------------------------------------------
shinyServer(function(input, output) {
  
  
# Inputs Setup ----------------------------------------------------

  output$dropdownUi1 <- renderUI({
    pickerInput(inputId = "item.cat",
                label   = "Kategorie", 
                choices = items["Item"], 
                options = list("actions-box" = TRUE),
                multiple = TRUE)
  })
  
  output$sliderUi1 <- renderUI({
    sliderInput(inputId = "item.freq", 
                label   = "Häufigkeiten", 
                min     = item_dist_min, 
                max     = item_dist_max, 
                step    = 1,
                value   = c(item_dist_min, item_dist_max)
    )
  })
  
  output$dropdownUi2 <- renderUI({
    pickerInput(inputId = "transaction.dates",
                label   = "Tag", 
                choices = dates, 
                # options = list("actions-box" = TRUE),
                multiple = FALSE)
  })
  
  # veraltet
  output$dropdownUi5 <- renderUI({
    selectInput(inputId = "transaction.dates2",
                label = "Tag",
                choices = dates,
                multiple = FALSE)
  })
  
  output$selectModeUi <- renderUI({
    selectInput(inputId = "b3.selectMode",
                label = "Modus",
                choices = c("Täglich", "Wöchentlich", "Gesamt(monatlich"),
                selected = choices[1],
                multiple = FALSE)
  })
  
  output$dropdownUidays <- renderUI({
    pickerInput(inputId = "b3.selectDates",
                label = "Tage:",
                choices = dates,
                multiple = TRUE,
                options = list("actions-box" = TRUE)
                )
  })
  
  output$dropdownUimonths <- renderUI({
    pickerInput(inputId = "b3.selectMonth",
                label = "Monate:",
                choices = unique(months_and_transaction["Kalendermonat"]),
                multiple = TRUE,
                options = list("actions-box" = TRUE)
                )
  })
  
  output$dropdownUiWeek <- renderUI({
    # todo: funktioniert aktuell nicht mit pickerInput
    selectInput(inputId = "b3.selectWeek",
                label = "Kalenderwoche:",
                choices = unique(weeks_and_transactions2["Kalenderwoche"]),
                multiple = TRUE
                #options = list("actions-box" = TRUE)
                )
  })
  
  # output$dateRangeUi2 <- renderUI({
  #   airDatepickerInput(inputId = "transaction.dates",
  #               label = "Kalender",
  #               multiple = TRUE,
  #               range = c(date_min, date_max),
  #               timepicker = TRUE,
  #               dateFormat = "dd.mm.yyyy",
  #               minDate = date_min,
  #               maxDate = date_max
  #   )
  # })
  
  output$dropdownUi3 <- renderUI({
    selectInput(inputId = "b4.begindate",
                label = "Starttag:",
                choices = dates,
                multiple = FALSE)
  })
  
  output$dropdownUi4 <- renderUI({
    selectInput(inputId = "b4.enddate",
                label = "Endtag:",
                choices = dates,
                multiple = FALSE)
  })
  
  
  # output$timeInput51 <- renderUI({
  #   timeInput("Zeit1", "Startzeit:")
  # })
  # 
  # output$timeInput52 <- renderUI({
  #   timeInput("Zeit2", "Endzeit:")
  # })
  # 
  # output$timeInput3 <- renderUI({
  #   timeInput("Zeit3", "Startzeit:")
  # })
  # 
  # output$timeInput4 <- renderUI({
  #   timeInput("Zeit4", "Endzeit:")
  # })
  
  output$dateRangeUi3 <- renderUI({
    dateRangeInput(inputId = "date.range",
                   label = "Kalender",
                   min = date_min,
                   max = date_max,
                   start = date_min,
                   end = date_max,
                   format = "dd.mm.yyyy",
                   language = "de",
                   weekstart = 1,
                   separator = "-"
    )
  })
  
  
  output$transactionList1 <- renderUI({
    pickerInput(inputId = "transactionList",
                label = "Transaktionen:",
                choices = selectTransactions(),
                options = list("actions-box" = TRUE),
                multiple = TRUE)
    
    
    
  })
  
  
  # Reactives Setup ----------------------------------------------------------
  
  table1_data <- reactive({
    new_data <- data.frame(intersect(subset(items, items$Item %in% input$item.cat), 
                                     subset(items, items$n >= min(input$item.freq) & items$n <= max(input$item.freq))))
    names(new_data) <- c("Item", "Anzahl")
    new_data
  })
  
  selectTransactions <- reactive({
    new_data <- subset(as.data.frame(df), df["Datum"] >= as.character(input$b4.begindate))
    new_data <- subset(as.data.frame(new_data), new_data["Datum"] <= as.character(input$b4.enddate))
    unique(new_data[,"Transaktion"])
  })
  
  # veraltet
  plot5_data <- reactive({
    # 1. Zeile verursacht Warnung in der Konsole, allerdings bisher kein Einfluss auf Plot
    new_data <- subset(as.data.frame(df), df["Datum"] == as.character(input$transaction.dates))
    new_data <- subset(new_data, new_data[,"Zeit"] >= parse_time(tail(strsplit(as.character(input$Zeit1, format="%Y-%m-%d %H:%M:%S"), " ")[[1]], 1)))
    new_data <- subset(new_data, new_data[,"Zeit"] <= parse_time(tail(strsplit(as.character(input$Zeit2, format="%Y-%m-%d %H:%M:%S"), " ")[[1]], 1)))
    get_item_dist(new_data)
  })

  
  # Plots / Visualizations ---------------------------------------------------
  
  output$distPlot1 <- renderPlot({
    
    data_to_plot <- table1_data()
    new_plot <- ggplot(data_to_plot, aes(x=reorder(data_to_plot$Item, -data_to_plot$Anzahl), y=data_to_plot$Anzahl)) + 
      geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Items", y="Anzahl")
    print(new_plot)
  })
  
  output$distTable1 <- renderTable({
    table1_data()
  })
  
  output$daytimePlot <- renderPlot({
    new_data <- aggregate_transactions(df, input$transaction.dates)
    print(ggplot(data = new_data, aes(x=new_data$Uhrzeit, y=new_data$Transaktionen)) + geom_bar(stat="identity") + labs(x="Uhrzeit", y="Anzahl Transaktionen"))
  })
  
  output$distPlot3Week <- renderPlot({
    data_to_plot <- subset(weeks_and_transactions2, weeks_and_transactions2$Kalenderwoche %in% input$b3.selectWeek)
    new_plot <- ggplot(data_to_plot, aes(x=data_to_plot$Kalenderwoche)) + geom_bar() +
      labs(x="Kalenderwoche", y="Anzahl Transaktionen") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(new_plot)
  })
  
  output$distPlot3days <- renderPlot({
    data_to_plot <- subset(df, as.character(df$Datum) %in% input$b3.selectDates)
    data_to_plot <- unique(data.frame(data_to_plot$Datum, data_to_plot$Transaktion))
    data_to_plot <- as.data.frame(table(data_to_plot[,1]))
    if (nrow(data_to_plot)>0)
    {
      names(data_to_plot) <- c("Datum", "Transaktion")
      new_plot <- ggplot(data_to_plot, aes(x=data_to_plot$Datum, y=data_to_plot$Transaktion)) +
        geom_bar(stat="identity") + labs(x="Datum", y="Anzahl Transaktionen") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      print(new_plot)
    }
  })
  
  output$distPlot3months <- renderPlot({
    data_to_plot <- subset(months_and_transaction, months_and_transaction$Kalendermonat %in% input$b3.selectMonth)
    data_to_plot <- unique(data.frame(data_to_plot$Kalendermonat, data_to_plot$Transaktion))
    data_to_plot <- as.data.frame(table(data_to_plot[,1]))
    if (nrow(data_to_plot)>0)
    {
      names(data_to_plot) <- c("Datum", "Transaktion")
      # todo: reihenfolge der balken chronologisch
      new_plot <- ggplot(data_to_plot, aes(x=data_to_plot$Datum, y=data_to_plot$Transaktion)) +
        geom_bar(stat="identity") + labs(x="Kalendermonat", y="Anzahl Transaktionen")
      print(new_plot)
    }
  })
  
  output$numberTransactions <- renderPlot({
    data_to_plot <- subset(df, df$Transaktion %in% input$transactionList)
    data_to_plot <- as.data.frame(table(data_to_plot["Transaktion"]))
    if (nrow(data_to_plot) > 0)
    {
      names(data_to_plot) <- c("Transaktion", "Anzahl")
      new_plot <- ggplot(data_to_plot, aes(x=data_to_plot$Transaktion, y=data_to_plot$Anzahl)) + geom_bar(stat="identity") +
        labs(x="Transaktionsnummer", y="Anzahl Items") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      print(new_plot)
    }
  })
  
  # veraltet
  output$distPlot5 <- renderPlot({
    data_to_plot <- plot5_data()
    new_plot <- ggplot(data_to_plot, aes(x=reorder(data_to_plot$Item, -data_to_plot$n), y=data_to_plot$n)) + 
      geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Items", y="Anzahl")
    print(new_plot)
  })
  
  output$frequencyArulesPlot <- renderPlot({
    print(itemFrequencyPlot(transactiondata, type="absolute", main="Item Frequency"))
  })
  
  output$rulesPlot1 <- renderPlot({
    print(plot(rules, method="grouped", measure="confidence", shading="confidence"))
  })
  
  output$rulesPlot2 <- renderPlot({
    print(plot(rules, method="grouped", measure="lift", shading="lift"))
  })
  
  output$rulesPlot3 <- renderPlot({
    print(plot(rules, method="grouped", measure="support", shading="support"))
  })
  
  output$confidenceTable <- renderTable({
    print(inspect(sort(rules, by="confidence", decreasing=TRUE)))
  })
  
  output$liftTable <- renderTable({
    print(inspect(sort(rules, by="lift", decreasing=TRUE)))
  })
  
  output$supportTable <- renderTable({
    print(inspect(sort(rules, by="support", decreasing=TRUE)))
  })
})
