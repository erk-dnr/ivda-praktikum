library(readr)
library(stringr)
library(tidyr)
library(tidyverse)

# Read & Manipulate Data
read_data <- function() {
  
  df <- read_delim("data/baeckerei-ivda-korrigiert.csv", ";",
                   na = c("", "NONE"), trim_ws = TRUE,
                   locale = locale(encoding = "UTF-8"),
                   col_types = cols(Datum = col_date(format = "%d.%m.%Y"), 
                                    Transaktion = col_integer(), 
                                    Zeit = col_time(format = "%H:%M:%S"))
  )
  df
  
}

# Item Distribution
get_item_dist <- function(df) {
  df %>%
    select(Item) %>%
    na.omit(Item) %>%
    dplyr::count(Item) %>%
    arrange(Item)
}

# Prepare data for daytime aggregation
aggregate_transactions <- function(df, dates) {
  new_data <- subset(df, df$Datum == dates)
  new_data <- unique(data.frame(new_data[,"Transaktion"], hour(first(new_data[,"Zeit"]))))[2]
  new_data <- data.frame(formatC(new_data[,1], width=2, format="d", flag="0"), formatC(new_data[,1]+1, width=2, format="d", flag="0"))
  new_data <- data.frame(paste(as.character(new_data[,1]), as.character(new_data[,2]), sep="-"))
  new_data <- as.data.frame(table(new_data))
  names(new_data) <- c("Uhrzeit", "Transaktionen")
  new_data
}

