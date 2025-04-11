yield <- function(min_temp, precip) {
    Y = -0.015*min_temp - 0.0046*min_temp^2 - 0.07*precip + 0.0043*precip^2 + 0.28
}