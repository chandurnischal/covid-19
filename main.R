library(ggplot2)
library(dplyr)
source("missing.R")


plot_scatter_by_state <- function(data, column) {
  ggplot(data, aes_string(x = "date", y = column, color = "abbrev_state")) +
    geom_point() +
    labs(x = "Date", y = column, title = paste("Scatter plot of", column, "by State"))
}


data<- load_data("data/raw.csv")

identifiers = c("id", "date")
epidemiological = c("confirmed", "deaths", "recovered", "tests", "vaccines", "people_vaccinated", "people_fully_vaccinated", "hosp", "icu", "population")
policy = c("school_closing", "workplace_closing", "cancel_events", "gatherings_restrictions", "transport_closing", "stay_home_restrictions", "internal_movement_restrictions", "international_movement_restrictions", "information_campaigns", "testing_policy", "contact_tracing", "facial_coverings", "vaccination_policy", "elderly_people_protection")
response = c("government_response_index", "stringency_index", "containment_health_index", "economic_support_index")
location = c("state", "latitude", "longitude")

states<- c("CA", "TX")

small <- data[data$abbrev_state %in% states, ]