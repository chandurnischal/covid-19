library(ggplot2)
library(dplyr)
source("missing.R")
source("fillna.R")

data<- load_data("data/raw.csv")

identifiers = c("id", "date")
epidemiological = c("confirmed", "deaths", "recovered", "tests", "vaccines", "people_vaccinated", "people_fully_vaccinated", "hosp", "icu")
policy = c("school_closing", "workplace_closing", "cancel_events", "gatherings_restrictions", "transport_closing", "stay_home_restrictions", "internal_movement_restrictions", "international_movement_restrictions", "information_campaigns", "testing_policy", "contact_tracing", "facial_coverings", "vaccination_policy", "elderly_people_protection")
response = c("government_response_index", "stringency_index", "containment_health_index", "economic_support_index")
location = c("state", "latitude", "longitude")

non_na<- data[!is.na(data$confirmed), ]
start<- min(non_na$date, na.rm=TRUE)
end<- max(non_na$date, na.rm=TRUE)

states<- data.frame(abbrev_state=unique(data$abbrev_state))
timeline<- data.frame(date=seq(start, end, by="day"))
result<- merge(states, timeline, by = NULL)
clean<- merge(result, data, by=c("abbrev_state", "date"), all.x=TRUE)

filled<- fill_na_per_state(clean, columns=epidemiological)

write.csv(filled, "data/filled.csv")