# use cover to get rid of dirt pixels

library(raster)
library(splancs)

# this finds the value at the percentile that indicates bare ground
findthreshold <- function(area, samples) {
  bare <- c()
  i <- 1
  for (i in 1:length(samples)) {
    smallarea <- crop(area, samples[[i]]) #gets raster surrounding plot
    df <- as.data.frame(smallarea, xy = TRUE) #converts raster to df
    inside <- pip(df, samples[[i]]) #creates df with raster points that are within plot
    bottom <- quantile(inside[,3], probs = (1-all.prairie$coverTotal[i]/100))
    bare[length(bare) + 1] <- bottom
    i <- i + 1
  }
  return(bare)
}

# make a function that calculates avg VI excluding pixels below a threshold
# area: raster
# samples: list of polygons
# threshold: the value of cutoff to make ratio
avgoverthreshold <- function(area, samples, threshold) {
  avgthreshold <- c(1:length(samples))
  for (i in 1:length(samples)) {
    low <- c()
    high <- c()
    smallarea <- crop(area, samples[[i]]) #gets raster surrounding plot
    df <- as.data.frame(smallarea, xy = TRUE) #converts raster to df
    inside <- pip(df, samples[[i]]) #creates df with raster points that are within plot
    avgover <- mean(inside[,3][which(inside[,3] > threshold[i])])
    avgthreshold[i] <- avgover
  }
  return(avgthreshold)
}


# make a function that calculates avg VI excluding pixels below a threshold, variable
# area: raster
# samples: list of polygons
# threshold: the value of cutoff to make ratio
avgoverthreshold.var <- function(area, samples, threshold) {
  avgthreshold <- c(1:length(samples))
  for (i in 1:length(samples)) {
    low <- c()
    high <- c()
    smallarea <- crop(area, samples[[i]]) #gets raster surrounding plot
    df <- as.data.frame(smallarea, xy = TRUE) #converts raster to df
    inside <- pip(df, samples[[i]]) #creates df with raster points that are within plot
    avgover <- mean(inside[,3][which(inside[,3] > threshold[i])])
    avgthreshold[i] <- avgover
  }
  return(avgthreshold)
}

# make a function that calculates VI range 
# area: raster
# samples: list of polygons
range.plot <- function(area, samples) {
  ranges <- c(1:length(samples))
  for (i in 1:length(samples)) {
    smallarea <- crop(area, samples[[i]]) #gets raster surrounding plot
    df <- as.data.frame(smallarea, xy = TRUE) #converts raster to df
    inside <- pip(df, samples[[i]]) #creates df with raster points that are within plot
    low <- min(inside[,3])
    high <- max(inside[,3])
    range <- high - low
    ranges[i] <- range
  }
  return(ranges)
}
