#' Define function to calculate the profit from almond yield
#'
#' @param production_cost default value is $4,500 / acre
#' @param baseline_profit default value $2,700 / acre calculated from 0.9 tons/acre for baseline_yield * $3,000/tons for baseline_price
#' @param climate_df Dataframe. Must contain wy (water year), day, month, tmin_c (daily minimum temperature), tmax_c (daily maximum temperature), precip (daily precipitation)
#' @return value in $
#' 
#' 

profit <- function(climate_df, baseline_profit = 2700, production_cost = 4500) {
  
  # Source the function
  source(here('yield.R'))
  
  # Pass in dataframe and call yield function
  yield_anomaly <- almond_model(clim)
  
  # year_yield_anomaly <- yield_anomaly$anomaly[yield_anomaly$wy == wy]
  
  yield_anomaly <- yield_anomaly %>% 
    mutate(profit = (baseline_profit * yield_anomaly$anomaly) - production_cost)
  
  return(yield_anomaly)
}
