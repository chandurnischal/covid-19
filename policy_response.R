library(ggplot2)
library(dplyr)
source("missing.R")

fill_na_indices <- function(data, column_name) {
  data <- data[order(data$date), ]
  
  current_value <- 0
  
  for (i in 1:nrow(data)) {
    if (is.na(data[[column_name]][i])) {
      data[[column_name]][i] <- current_value
    } else {
      current_value <- data[[column_name]][i]
    }
  }
  
  return(data)
}

fill_na_indices_multiple_columns<- function(data, columns){
  data[columns]<- lapply(columns, function(column){
    fill_na_indices(data, column)[[column]]
  })
  return(data)
}

fill_na_per_state<- function(data, columns){
  grouped<- split(data, data$abbrev_state)
  
  grouped<- lapply(grouped, function(group){
    fill_na_indices_multiple_columns(group, columns)
  })
  
  res<- do.call(rbind, grouped)
  
  rownames(res)<- NULL
  
  return(res)
}

data<- read.csv("data/filled.csv")
data$date<- as.Date(data$date)
data<- data[order(data$date), ]


policy<- c("school_closing", "workplace_closing", "cancel_events", "gatherings_restrictions", "transport_closing", "stay_home_restrictions", "internal_movement_restrictions", "international_movement_restrictions", "information_campaigns", "testing_policy", "contact_tracing", "facial_coverings", "vaccination_policy", "elderly_people_protection")
response<- c("government_response_index", "stringency_index", "containment_health_index", "economic_support_index")


full<- fill_na_per_state(data, policy)
full<- fill_na_per_state(data, response)

write.csv(full, "data/clean.csv")
