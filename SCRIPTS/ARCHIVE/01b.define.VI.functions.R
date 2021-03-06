# use cover to get rid of dirt pixels

library(raster)
library(splancs)

# make a function to calculate average pixel value from raster
# area: raster
# samples: list of polygons
avgvalues <- function(area, samples) {
  avglist <- c(1:length(samples))
  i <- 1
  for (i in 1:length(samples)) {
    smallarea <- crop(area, samples[[i]]) #gets raster surrounding plot
    df <- as.data.frame(smallarea, xy = TRUE) #converts raster to df
    inside <- pip(df, samples[[i]]) #creates df with raster points that are within plot
    avg <- mean(inside[,3]) #finds mean of values
    avglist[i] <- avg
    i <- i + 1
  }
  return(avglist)
}


# this finds the value at the percentile that indicates bare ground
findthreshold <- function(area, samples) {
  bare <- c()
  i <- 1
  for (i in 1:length(samples)) {
    smallarea <- crop(area, samples[[i]]) #gets raster surrounding plot
    df <- as.data.frame(smallarea, xy = TRUE) #converts raster to df
    inside <- pip(df, samples[[i]]) #creates df with raster points that are within plot
    bottom <- quantile(inside[,3], probs = (1-all.prairie$dcover[i]/100))
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
    avgover <- mean(inside[,3][which(inside[,3] > threshold)])
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


# function that calculates volume of each plot
# area: raster
# samples: list of polygons
vol.plot <- function(area, samples) {
  vols <- c(1:length(samples))
  for (i in 1:length(samples)){
    smallarea <- crop(area, samples[[i]]) #gets raster surrounding plot
    df <- as.data.frame(smallarea, xy = TRUE) #converts raster to df
    inside <- pip(df, samples[[i]]) #creates df with raster points that are within plot
    inside$height <- inside[,3] - min(inside[,3])
    vol <- sum(inside[,4])
    vols[i] <- vol
    i <- i + 1
  }
  return(vols)
}


