library(raster)
library(rgdal)
library(rgeos)
library(splancs)

# read in tifs and plots ----
setwd("C:/Users/clane_897q3pb/Documents/tiffs_final")
ndvi <- raster("NDVI.tif")
gndvi <- raster("GNDVI.tif")
gdvi2 <- raster("GDVI2.tif")
red <- raster("RED.tif")
nir <- raster("NIR.tif")
gre <- raster("GRE.tif")
reg <- raster("REG.tif")
dsm <- raster("DSM.tif")
dtm <- raster("DTM.tif")
plots <- readOGR(dsn = ".", layer = "plots")
setwd("C:/Users/clane_897q3pb/Documents/GitHub/prairie-biomass-2018/SCRIPTS")


# stack rasters
all <- stack(ndvi, gndvi, gdvi2, red, nir, gre, reg)

# plot
#plot(ndvi)
#plot(gndvi)
#plot(gdvi2)
#plot(red)
#plot(gre)
#plot(reg)
#plot(nir)
#plot(dsm)
#plot(dtm)
#plot(plots, add = TRUE)

# remove edges ----
proj4string(plots) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #UTM 16N

plotsTest <- spTransform(plots, CRS("+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))

intPlots <- gBuffer(plotsTest, width = -0.25, byid = T)

finalPlots <- spTransform(intPlots, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))






# get average of all pixels in plot ----
ALL <- as.data.frame(1:7)
for (i in 1:length(test)) {
  small <- crop(all, finalPlots[i,]) # makes a smaller raster
  df <- extract(small, finalPlots[i,], df = T) # makes df of points in raster
  tog <- colMeans(df, na.rm = T)
  ALL[i] <- tog[2:8]
}

ALL <- t(ALL)
colnames(ALL) <- c("NDVI", "GNDVI", "GDVI2", "RED", "NIR", "GRE", "REG")
rownames(ALL) <- 1:length(ALL[,1])


# get average of non-dirt pixels (NDVI > 0.1) ----
VEG <- as.data.frame(1:7)
for (i in 1:length(test)) {
  small <- crop(all, finalPlots[i,]) # makes a smaller raster
  df <- extract(small, finalPlots[i,], df = T) # makes df of points in raster
  tog <- colMeans(df[which(df$NDVI > 0.1),], na.rm = T)
  VEG[i] <- tog[2:8]
}

VEG <- t(VEG)
colnames(VEG) <- c("NDVI.ND", "GNDVI.ND", "GDVI2.ND", "RED.ND", "NIR.ND", "GRE.ND", "REG.ND")
rownames(VEG) <- 1:length(VEG[,1])

test <- finalPlots[1:3,]

# get average of pixels correcting for individual "weed" values ----
weeds <- all.prairie$dcover - all.prairie$coverTotal


tifAn <- function (rast, plots, correction, threshold = -100, reps = 100) {
  resu <- list()
  for (i in 1:length(plots)) {
    if (correction == "mean") {
      weedCor <- mean(weeds, na.rm = T)
    } else {
      weeds[is.na(weeds) == T] <- 0
      weedCor <- weeds[i]
    }
    
    if (weedCor < 0){ # "growth" correction, change dirt to veg
      small <- crop(rast, plots[i,]) # makes a smaller raster
      df <- extract(small, plots[i,], df = T) # makes df of points in raster
      veg <- df[which(df[,2] > 0.1),] # separate out veg pixels
      dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      
      # Sometimes the number of pixels that needs to be replaces is greater than 
      # the number of pixels available. If that is the case, replace all available pixels:
      if (abs(weedCor)/100 * length(df$ID) > length(dirt$ID)) {
        numRep <- length(dirt$ID)
      } else {
        numRep <- abs(weedCor)/100 * length(df$ID)
      }
      
      # if there are no veg pixels to replace dirt with, use veg pixels from previous plot:
      if (length(veg$ID) < 1){
        small <- crop(rast, plots[i-1,]) # makes a smaller raster
        df <- extract(small, plots[i-1,], df = T) # makes df of points in raster
        veg <- df[which(df[,2] > 0.1),] # separate out dirt pixels
      }
      
      # if there are still no veg pixels to replace dirt with, use veg pixels from random plot:
      while (length(veg$ID) < 1){
        randPlot <- sample(1:length(plots), 1)
        small <- crop(rast, plots[randPlot,]) # makes a smaller raster
        df <- extract(small, plots[randPlot,], df = T) # makes df of points in raster
        veg <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      }
      
      # now random pixels x number of times, save in vegW
      vegW <- as.data.frame(1:7)
      for (j in 1:reps) {
        correct <- sample(1:length(dirt$ID), numRep) # choose dirt rows to replace
        selRan <- sample(1:length(veg$ID), numRep, replace = T) # select veg rows that will replace dirt
        newDirt <- dirt 
        newDirt[correct,] <- veg[selRan,] # replace dirt pixels with random veg
        df <- rbind(veg, newDirt) # put all pixels back together
        tog <- colMeans(df[which(df[,2] > threshold),], na.rm = T) # get mean of each column
        vegW[j] <- tog[2:8]
      }
      
    } 
    else { # "weed" correction, change veg to dirt
      small <- crop(rast, plots[i,]) # makes a smaller raster
      df <- extract(small, plots[i,], df = T) # makes df of points in raster
      veg <- df[which(df[,2] > 0.1),] # separate out veg pixels
      dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      
      # Sometimes the number of pixels that needs to be replaces is greater than 
      # the number of pixels available. If that is the case, replace all available pixels:
      if (abs(weedCor)/100 * length(df$ID) > length(veg$ID)) {
        numRep <- length(veg$ID)
      } else {
        numRep <- abs(weedCor)/100 * length(df$ID)
      }
      
      # if there are no dirt pixels to replace veg with, use dirt pixels from previous plot:
      if (length(dirt$ID) < 1){
        small <- crop(rast, plots[i-1,]) # makes a smaller raster
        df <- extract(small, plots[i-1,], df = T) # makes df of points in raster
        dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      }
      
      # if there are still no dirt pixels to replace veg with, use dirt pixels from random plot:
      while (length(dirt$ID) < 1){
        randPlot <- sample(1:length(plots), 1)
        small <- crop(rast, plots[randPlot,]) # makes a smaller raster
        df <- extract(small, plots[randPlot,], df = T) # makes df of points in raster
        dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      }
      
      # now random pixels x number of times, save in vegW
      vegW <- as.data.frame(1:7)
      for (j in 1:reps) {
        correct <- sample(1:length(veg$ID), numRep) # select veg rows to replace
        selRan <- sample(1:length(dirt$ID), numRep, replace = T) # select dirt rows that will replace veg
        newVeg <- veg 
        newVeg[correct,] <- dirt[selRan,] # replace veg pixels with random dirt
        df <- rbind(newVeg, dirt) # put all pixels back together
        tog <- colMeans(df[which(df[,2] > threshold),], na.rm = T) # get mean of each column
        vegW[j] <- tog[2:8]
        
      }
      
    }
    
    vegW <- t(vegW)
    colnames(vegW) <- c("NDVI.CI", "GNDVI.CI", "GDVI2.CI", "RED.CI", "NIR.CI", "GRE.CI", "REG.CI")
    rownames(vegW) <- 1:length(vegW[,1])
    vegW <- as.data.frame(vegW)
    resu[[i]] <- vegW
  }
  return(resu)
}

noWeeds.CA <- tifAn(rast = all, plots = test, correction = "mean", reps = 20, threshold = -10)
noWeeds.CI <- tifAn(rast = all, plots = test, correction = "individual", reps = 20, threshold = -10)
noDirt.CA <- tifAn(rast = all, plots = test, correction = "mean", reps = 20, threshold = 0.1)
noDirt.CI <- tifAn(rast = all, plots = test, correction = "individual", reps = 20, threshold = 0.1)

# plot volume ----

# subtract DSM from DTM to get vegetation height
vhm <- dsm - dtm

# find average height (m) across plot, then multiply by l and w (m)
heightPlots <- c()
for (i in 1:length(test)){
  small <- crop(vhm, finalPlots[i,])
  avg <- extract(small, finalPlots[i,], fun = mean, na.rm = TRUE)
  heightPlots[i] <- as.numeric(avg)
}

volPlots <- c()
for (i in 1:length(test)) {
  p <- crop(vhm, finalPlots[i,])
  numPixels <- rasterize(finalPlots[i,], p, 1)
  numPixels <- cellStats(numPixels, "sum")
  dim <- sqrt(numPixels) * 1.15 / 100
  vol <- dim * dim * heightPlots[i]
  vol <- vol/numPixels # calculate volume per pixel
  vol <- vol * 17013 # multiply by number of pixels in 1.5x1.5 m plot
  volPlots[i] <- vol
}

# combine products ----

ALL <- as.data.frame(ALL)
ND <- as.data.frame(VEG)

# average correction
CA <- as.data.frame(1:7)
for (i in 1:length(noWeeds.CA)){
  avg <- colMeans(noWeeds.CA[[i]])
  avg <- as.vector(avg)
  CA[i] <- avg
}
CA <- t(CA)
colnames(CA) <- c("NDVI.CA", "GNDVI.CA", "GDVI2.CA", "RED.CA", "NIR.CA", "GRE.CA", "REG.CA")
rownames(CA) <- 1:length(CA[,1])
CA <- as.data.frame(CA)

# individual correction
CI <- as.data.frame(1:7)
for (i in 1:length(noWeeds.CI)){
  avg <- colMeans(noWeeds.CI[[i]])
  avg <- as.vector(avg)
  CI[i] <- avg
}
CI <- t(CI)
colnames(CI) <- c("NDVI.CI", "GNDVI.CI", "GDVI2.CI", "RED.CI", "NIR.CI", "GRE.CI", "REG.CI")
rownames(CI) <- 1:length(CI[,1])
CI <- as.data.frame(CI)


# avg of values excluding dirt, average correction
ND.CA <- as.data.frame(1:7)
for (i in 1:length(noDirt.CA)){
  avg <- colMeans(noDirt.CA[[i]])
  avg <- as.vector(avg)
  ND.CA[i] <- avg
}
ND.CA <- t(ND.CA)
colnames(ND.CA) <- c("NDVI.ND.CA", "GNDVI.ND.CA", "GDVI2.ND.CA", "RED.ND.CA", "NIR.ND.CA", "GRE.ND.CA", "REG.ND.CA")
rownames(ND.CA) <- 1:length(ND.CA[,1])
ND.CA <- as.data.frame(ND.CA)

# avg of values excluding dirt, individual correction
ND.CI <- as.data.frame(1:7)
for (i in 1:length(noDirt.CI)){
  avg <- colMeans(noDirt.CI[[i]])
  avg <- as.vector(avg)
  ND.CI[i] <- avg
}
ND.CI <- t(ND.CI)
colnames(ND.CI) <- c("NDVI.ND.CI", "GNDVI.ND.CI", "GDVI2.ND.CI", "RED.ND.CI", "NIR.ND.CI", "GRE.ND.CI", "REG.ND.CI")
rownames(ND.CI) <- 1:length(ND.CI[,1])
ND.CI <- as.data.frame(ND.CI)

VOL <- volPlots

allRS <- cbind(ALL, ND, CA, CI, ND.CA, ND.CI, VOL)

write.csv(allRS, "../DATA/allRS.csv")

save(noWeeds.CA, noWeeds.CI, noDirt.CA, noDirt.CI, file = "C:/Users/clane_897q3pb/Documents/tiffs_final/RSdata.rdata")





# old code ----
noWeeds.CI <- list()
for (i in 1:length(test)) {
  if (weeds[i] < 0){ # "growth" correction, change dirt to veg
    small <- crop(all, finalPlots[i,]) # makes a smaller raster
    df <- extract(small, finalPlots[i,], df = T) # makes df of points in raster
    veg <- df[which(df[,2] > 0.1),] # separate out veg pixels
    dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
    
    # Sometimes the number of pixels that needs to be replaces is greater than 
    # the number of pixels available. If that is the case, replace all available pixels:
    if (abs(weeds[i])/100 * length(df$ID) > length(dirt$ID)) {
      numRep <- length(dirt$ID)
    } else {
      numRep <- abs(weeds[i])/100 * length(df$ID)
    }
    
    # if there are no veg pixels to replace dirt with, use veg pixels from previous plot:
    if (length(veg$ID) < 1){
      small <- crop(all, finalPlots[i-1,]) # makes a smaller raster
      df <- extract(small, finalPlots[i-1,], df = T) # makes df of points in raster
      veg <- df[which(df[,2] > 0.1),] # separate out dirt pixels
    }
    
    # if there are still no veg pixels to replace dirt with, use veg pixels from random plot:
    while (length(veg$ID) < 1){
      randPlot <- sample(1:length(finalPlots), 1)
      small <- crop(all, finalPlots[randPlot,]) # makes a smaller raster
      df <- extract(small, finalPlots[randPlot,], df = T) # makes df of points in raster
      veg <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
    }
    
    # now random pixels x number of times, save in vegW
    vegW <- as.data.frame(1:7)
    for (j in 1:10) {
      correction <- sample(1:length(dirt$ID), numRep) # choose dirt rows to replace
      selRan <- sample(1:length(veg$ID), numRep, replace = T) # select veg rows that will replace dirt
      newDirt <- dirt 
      newDirt[correction,] <- veg[selRan,] # replace dirt pixels with random veg
      df <- rbind(veg, newDirt) # put all pixels back together
      tog <- colMeans(df, na.rm = T) # get mean of each column
      vegW[j] <- tog[2:8]
    }
    
  } 
  else { # "weed" correction, change veg to dirt
    small <- crop(all, finalPlots[i,]) # makes a smaller raster
    df <- extract(small, finalPlots[i,], df = T) # makes df of points in raster
    veg <- df[which(df[,2] > 0.1),] # separate out veg pixels
    dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
    
    # Sometimes the number of pixels that needs to be replaces is greater than 
    # the number of pixels available. If that is the case, replace all available pixels:
    if (abs(weeds[i])/100 * length(df$ID) > length(veg$ID)) {
      numRep <- length(veg$ID)
    } else {
      numRep <- abs(weeds[i])/100 * length(df$ID)
    }
    
    # if there are no dirt pixels to replace veg with, use dirt pixels from previous plot:
    if (length(dirt$ID) < 1){
      small <- crop(all, finalPlots[i-1,]) # makes a smaller raster
      df <- extract(small, finalPlots[i-1,], df = T) # makes df of points in raster
      dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
    }
    
    # if there are still no dirt pixels to replace veg with, use dirt pixels from random plot:
    while (length(dirt$ID) < 1){
      randPlot <- sample(1:length(finalPlots), 1)
      small <- crop(all, finalPlots[randPlot,]) # makes a smaller raster
      df <- extract(small, finalPlots[randPlot,], df = T) # makes df of points in raster
      dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
    }
    
    # now random pixels x number of times, save in vegW
    vegW <- as.data.frame(1:7)
    for (j in 1:10) {
      correction <- sample(1:length(veg$ID), numRep) # select veg rows to replace
      selRan <- sample(1:length(dirt$ID), numRep, replace = T) # select dirt rows that will replace veg
      newVeg <- veg 
      newVeg[correction,] <- dirt[selRan,] # replace veg pixels with random dirt
      df <- rbind(newVeg, dirt) # put all pixels back together
      tog <- colMeans(df, na.rm = T) # get mean of each column
      vegW[j] <- tog[2:8]
      
    }
    
  }
  
  vegW <- t(vegW)
  colnames(vegW) <- c("NDVI.CI", "GNDVI.CI", "GDVI2.CI", "RED.CI", "NIR.CI", "GRE.CI", "REG.CI")
  rownames(vegW) <- 1:length(vegW[,1])
  vegW <- as.data.frame(vegW)
  noWeeds.CI[[i]] <- vegW
}




tifAn2 <- function(rast = all, plots = test){
  resu <- list()
  for (i in 1:length(plots)) {
    if (weeds[i] < 0){ # "growth" correction, change dirt to veg
      small <- crop(rast, plots[i,]) # makes a smaller raster
      df <- extract(small, plots[i,], df = T) # makes df of points in raster
      veg <- df[which(df[,2] > 0.1),] # separate out veg pixels
      dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      
      # Sometimes the number of pixels that needs to be replaces is greater than 
      # the number of pixels available. If that is the case, replace all available pixels:
      if (abs(weeds[i])/100 * length(df$ID) > length(dirt$ID)) {
        numRep <- length(dirt$ID)
      } else {
        numRep <- abs(weeds[i])/100 * length(df$ID)
      }
      
      # if there are no veg pixels to replace dirt with, use veg pixels from previous plot:
      if (length(veg$ID) < 1){
        small <- crop(rast, plots[i-1,]) # makes a smaller raster
        df <- extract(small, plots[i-1,], df = T) # makes df of points in raster
        veg <- df[which(df[,2] > 0.1),] # separate out dirt pixels
      }
      
      # if there are still no veg pixels to replace dirt with, use veg pixels from random plot:
      while (length(veg$ID) < 1){
        randPlot <- sample(1:length(plots), 1)
        small <- crop(rast, plots[randPlot,]) # makes a smaller raster
        df <- extract(small, plots[randPlot,], df = T) # makes df of points in raster
        veg <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      }
      
      # now random pixels x number of times, save in vegW
      vegW <- as.data.frame(1:7)
      for (j in 1:10) {
        correction <- sample(1:length(dirt$ID), numRep) # choose dirt rows to replace
        selRan <- sample(1:length(veg$ID), numRep, replace = T) # select veg rows that will replace dirt
        newDirt <- dirt 
        newDirt[correction,] <- veg[selRan,] # replace dirt pixels with random veg
        df <- rbind(veg, newDirt) # put all pixels back together
        tog <- colMeans(df, na.rm = T) # get mean of each column
        vegW[j] <- tog[2:8]
      }
      
    } 
    else { # "weed" correction, change veg to dirt
      small <- crop(rast, plots[i,]) # makes a smaller raster
      df <- extract(small, plots[i,], df = T) # makes df of points in raster
      veg <- df[which(df[,2] > 0.1),] # separate out veg pixels
      dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      
      # Sometimes the number of pixels that needs to be replaces is greater than 
      # the number of pixels available. If that is the case, replace all available pixels:
      if (abs(weeds[i])/100 * length(df$ID) > length(veg$ID)) {
        numRep <- length(veg$ID)
      } else {
        numRep <- abs(weeds[i])/100 * length(df$ID)
      }
      
      # if there are no dirt pixels to replace veg with, use dirt pixels from previous plot:
      if (length(dirt$ID) < 1){
        small <- crop(rast, plots[i-1,]) # makes a smaller raster
        df <- extract(small, plots[i-1,], df = T) # makes df of points in raster
        dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      }
      
      # if there are still no dirt pixels to replace veg with, use dirt pixels from random plot:
      while (length(dirt$ID) < 1){
        randPlot <- sample(1:length(plots), 1)
        small <- crop(rast, plots[randPlot,]) # makes a smaller raster
        df <- extract(small, plots[randPlot,], df = T) # makes df of points in raster
        dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
      }
      
      # now random pixels x number of times, save in vegW
      vegW <- as.data.frame(1:7)
      for (j in 1:10) {
        correction <- sample(1:length(veg$ID), numRep) # select veg rows to replace
        selRan <- sample(1:length(dirt$ID), numRep, replace = T) # select dirt rows that will replace veg
        newVeg <- veg 
        newVeg[correction,] <- dirt[selRan,] # replace veg pixels with random dirt
        df <- rbind(newVeg, dirt) # put all pixels back together
        tog <- colMeans(df, na.rm = T) # get mean of each column
        vegW[j] <- tog[2:8]
        
      }
      
    }
    
    vegW <- t(vegW)
    colnames(vegW) <- c("NDVI.CI", "GNDVI.CI", "GDVI2.CI", "RED.CI", "NIR.CI", "GRE.CI", "REG.CI")
    rownames(vegW) <- 1:length(vegW[,1])
    vegW <- as.data.frame(vegW)
    resu[[i]] <- vegW
  }
  return(resu)
}

testCI2 <- tifAn2(rast = all, plots = test)
