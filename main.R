library(dplyr)

data<- read.csv("data\\2.csv", header=TRUE)

india<- data %>% 
          filter(iso_alpha_3 == 'IND')

write.csv(india, "data\\india_2.csv", row.names = FALSE)