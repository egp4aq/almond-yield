# Define function to calculate yield anomaly of almonds in California
#' 
#' @param climate_df Dataframe. Must contain wy (water year), day, month, tmin_c (daily minimum temperature), tmax_c (daily maximum temperature), precip (daily precipitation), 
#' 

almond_model <- function(climate_df) {
    time_series <- climate_df |> 
        group_by(month, wy) |> # group by month and water year
        summarise(ave_min_temp = mean(tmin_c), # calculate average minimum temperature in each month
                  total_precip = sum(precip)) |> # calculate total precipitation
        ungroup()
    
    yield_anomaly <- time_series |> 
        mutate(month = as.integer(month)) |> # make sure that month is an integer
        arrange(wy, month) |> # Arrange by year 
        group_by(wy) |> # Group by water year for further calculations
        mutate(january_precip = first(total_precip[month==1]), # Save the total january precipitation for each row in each year
               feb_min_temp = first(ave_min_temp[month==2])) |> # Save the average
        ungroup() |> # Ungroup to avoid error
        
        # Calculate the anomaly uncertainty 
        mutate(anomaly = -0.015*feb_min_temp - 0.0046*feb_min_temp^2 - 0.07*january_precip + 0.0043*january_precip^2 + 0.28)

    # Create dataframe to return min, max, and mean stats
    anomaly_stats <- data.frame(min = min(yield_anomaly$anomaly),
                                max = max(yield_anomaly$anomaly),
                                mean = mean(yield_anomaly$anomaly))
    
    # Return dataframe
    #return(anomaly_stats)
    return(yield_anomaly)
}
