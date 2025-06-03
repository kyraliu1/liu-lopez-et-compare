fclos <- function(short, long) {
  
  shortdates <- as.Date(short$begindate) # less frequent obs
  longdates <- as.Date(long$begindate) # more frequent obs
  
  numlong <- length(longdates)
  
  closest_dates <- rep(as.Date(NA), numlong) # initialize
  closest_indices <- rep(NA_integer_, numlong) # initialize
  
  for (i in 1:numlong) {
    long_date <- longdates[i]
    dateDifferences <- abs(as.numeric(shortdates - long_date)) # difference in dates
    closest_index <- which.min(dateDifferences) # closest date
    # within 2 weeks 
    if (dateDifferences[closest_index] <= 14) {
      closest_dates[i] <- shortdates[closest_index]
      closest_indices[i] <- closest_index
    }
  }
  
  result <- data.frame(cldate = closest_dates, ind = closest_indices)
  return(result)
}