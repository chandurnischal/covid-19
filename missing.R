library(ggplot2)
library(dplyr)
library(tidyr)

get_state_abbreviations<- function(){
  state_names <- c("Northern Mariana Islands", "Minnesota", "California", "Florida", "Wyoming", 
                   "Virgin Islands", "South Dakota", "Kansas", "Nevada", "Virginia", "Washington", 
                   "Oregon", "Wisconsin", "New Jersey", "Rhode Island", "Vermont", "North Carolina", 
                   "Oklahoma", "Alabama", "Delaware", "Guam", "Missouri", "American Samoa", "Utah", 
                   "Mississippi", "Connecticut", "Indiana", "Georgia", "Texas", "Pennsylvania", 
                   "Massachusetts", "Maine", "Tennessee", "Michigan", "Idaho", "Illinois", "Louisiana", 
                   "New Mexico", "Arizona", "Arkansas", "Nebraska", "West Virginia", "South Carolina", 
                   "New York", "District of Columbia", "Kentucky", "Ohio", "Alaska", "New Hampshire", 
                   "North Dakota", "Iowa", "Montana", "Hawaii", "Maryland", "Puerto Rico", "Colorado")
  
  abbrev_states <- c("MP", "MN", "CA", "FL", "WY", "VI", "SD", "KS", "NV", "VA", "WA", "OR", "WI", 
                     "NJ", "RI", "VT", "NC", "OK", "AL", "DE", "GU", "MO", "AS", "UT", "MS", "CT", 
                     "IN", "GA", "TX", "PA", "MA", "ME", "TN", "MI", "ID", "IL", "LA", "NM", "AZ", 
                     "AR", "NE", "WV", "SC", "NY", "DC", "KY", "OH", "AK", "NH", "ND", "IA", "MT", 
                     "HI", "MD", "PR", "CO")
  
  
  return(data.frame(state = state_names, abbrev_state = abbrev_states))
}

get_state_name<-function(abbrev){
  states<- get_state_abbreviations()
  return(states$state[states$abbrev_state == abbrev])
}

get_state_abbreviation<- function(state){
  states<- get_state_abbreviations()
  return(states$abbrev_state[states$state == state])
}

load_data<- function(path){
  
  data<- read.csv(path)
  
  data$date <- as.Date(data$date, format="%Y-%m-%d")
  
  remove = c("administrative_area_level", "administrative_area_level_1", "administrative_area_level_3", "iso_alpha_3", "iso_alpha_2", "iso_numeric", "iso_currency", "key_local", "key_google_mobility", "key_apple_mobility", "key_jhu_csse", "key_nuts", "key_gadm", "vent")
  data<- data[ , !(names(data) %in% remove)]
  colnames(data)[which(names(data) == "administrative_area_level_2")] <- "state"
  
  states<- get_state_abbreviations()
  
  data<- data %>% 
    left_join(states, by="state")
  
  territories<- c("AS", "GU", "MP", "PR", "VI")
  data<- data[!(data$abbrev_state %in% territories), ]
  
  
  return(data)
  
}

count_total_missing_values<- function(data, columns) {
  n<- nrow(data)
  return(sapply(columns, function(col) round(sum(is.na(data[[col]])) / n, 4)))
}

count_missing_values_per_state<- function(data, columns) {
  n<- nrow(data)
  return(data %>%
           group_by(abbrev_state) %>%
           summarise(across(all_of(columns), 
                            ~ round(sum(is.na(.)) / n(), 4)
           ))
  )
}

plot_heatmap<- function(data){
  long <- data %>%
    pivot_longer(cols = -abbrev_state, 
                 names_to = "Variable", 
                 values_to = "Value")
  
  ggplot(long, aes(x = abbrev_state, y = Variable, fill = Value)) +
    geom_tile() + 
    scale_fill_gradient(low = "white", high = "red", limits = c(0, 1)) +
    theme_minimal() + 
    labs(title = "Proportion of Missing Values Per State", x = "State", y = "Variable") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}