library(ggplot2)
library(dplyr)

plot_scatter_by_state <- function(data, column) {
  
  ggplot(data, aes(x = date, y = .data[[column]], color = abbrev_state)) +
    geom_line() +
    labs(x = "Date", y = column, title = paste("Scatter plot of", column, "by State"))
}

# Function to fill NA values for specific columns using spline interpolation
fill_na <- function(data, columns) {
  data<- data[order(data$date), ]
  # Convert dates to numeric for spline fitting
  data$date_num <- as.numeric(data$date)
  
  for (col in columns) {
    # Check if the column has at least one non-NA value
    if (any(!is.na(data[[col]]))) {
      
      # Subset non-NA values for fitting
      non_na_indices <- !is.na(data[[col]])
      
      # Fit the spline function
      fit_spline <- splinefun(data$date_num[non_na_indices], data[[col]][non_na_indices])
      
      # Fill NA values using the spline
      data[[col]][!non_na_indices] <- fit_spline(data$date_num[!non_na_indices])
    }
  }
  
  # Remove the temporary numeric date column
  data <- data %>% select(-date_num)
  return(data)
}

# Function to apply `fill_na` per state
fill_na_per_state <- function(data, columns) {
  
  df_filled <- data %>%
    group_by(abbrev_state) %>%     # Group by state
    group_split() %>%              # Split into list of data frames
    lapply(fill_na, columns = columns) %>% # Apply `fill_na` to each group
    bind_rows()                    # Combine the results back into a single data frame
  
  return(df_filled)
}


