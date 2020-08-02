calculate_age_range <- function(x) {
  cut(x, 
      right = FALSE,
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
      labels = c(
        "0 a 9 anos",
        "10 a 19 anos",
        "20 a 29 anos",
        "30 a 39 anos",
        "40 a 49 anos",
        "50 a 59 anos",
        "60 a 69 anos",
        "70 a 79 anos",
        "80+"))
}


