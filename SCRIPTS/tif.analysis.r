
library(raster)
library(rgdal)
library(rgeos)
library(splancs)

# read in tifs and plots ----
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
for (i in 1:length(finalPlots)) {
  small <- crop(all, finalPlots[i,]) # makes a smaller raster
  df <- extract(small, finalPlots[i,], df = T) # makes df of points in raster
  avgNDVI <- mean(df[,2], na.rm = T)
  avgGNDVI <- mean(df[,3], na.rm = T)
  avgGDVI2 <- mean(df[,4], na.rm = T)
  avgRED <- mean(df[,5], na.rm = T)
  avgNIR <- mean(df[,6], na.rm = T)
  avgGRE <- mean(df[,7], na.rm = T)
  avgREG <- mean(df[,8], na.rm = T)
  tog <- c(avgNDVI, avgGNDVI, avgGDVI2, avgRED, avgNIR, avgGRE, avgREG)
  ALL[i] <- tog
}

ALL <- t(ALL)
colnames(ALL) <- c("NDVI", "GNDVI", "GDVI2", "RED", "NIR", "GRE", "REG")
rownames(ALL) <- 1:length(ALL[,1])


# get average of non-dirt pixels (NDVI > 0.1) ----

VEG <- as.data.frame(1:7)
for (i in 1:length(finalPlots)) {
  small <- crop(all, finalPlots[i,]) # makes a smaller raster
  df <- extract(small, finalPlots[i,], df = T) # makes df of points in raster
  avgNDVI <- mean(df[,2][which(df[,2] > 0.1)], na.rm = T)
  avgGNDVI <- mean(df[,3][which(df[,2] > 0.1)], na.rm = T)
  avgGDVI2 <- mean(df[,4][which(df[,2] > 0.1)], na.rm = T)
  avgRED <- mean(df[,5][which(df[,2] > 0.1)], na.rm = T)
  avgNIR <- mean(df[,6][which(df[,2] > 0.1)], na.rm = T)
  avgGRE <- mean(df[,7][which(df[,2] > 0.1)], na.rm = T)
  avgREG <- mean(df[,8][which(df[,2] > 0.1)], na.rm = T)
  tog <- c(avgNDVI, avgGNDVI, avgGDVI2, avgRED, avgNIR, avgGRE, avgREG)
  VEG[i] <- tog
}

VEG <- t(VEG)
colnames(VEG) <- c("NDVIveg", "GNDVIveg", "GDVI2veg", "REDveg", "NIRveg", "GREveg", "REGveg")
rownames(VEG) <- 1:length(VEG[,1])


# get average of pixels without weeds ----
weeds <- all.prairie$dcover - all.prairie$coverTotal
weeds[is.na(weeds) == T] <- 0

noWeeds <- list()
for (i in 1:length(finalPlots)) {
  small <- crop(all, finalPlots[i,]) # makes a smaller raster
  df <- extract(small, finalPlots[i,], df = T) # makes df of points in raster
  veg <- df[which(df[,2] > 0.1),] # separate out veg pixels
  dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
  dirtAVG <- as.vector(colMeans(dirt))
  
  vegW <- as.data.frame(1:7)
  for (j in 1:100) {
    weed <- runif((abs(weeds[i])/100 * length(df$ID)), 0, length(veg$ID)) # select random pixels as weeds NEED TO DEAL WITH NEGATIVE WEEDS
    newVeg <- veg
    newVeg[weed, 2] <- dirtAVG[2] # replace pixels with dirt avg
    newVeg[weed, 3] <- dirtAVG[3] # replace pixels with dirt avg
    newVeg[weed, 4] <- dirtAVG[4] # replace pixels with dirt avg
    newVeg[weed, 5] <- dirtAVG[5] # replace pixels with dirt avg
    newVeg[weed, 6] <- dirtAVG[6] # replace pixels with dirt avg
    newVeg[weed, 7] <- dirtAVG[7] # replace pixels with dirt avg
    newVeg[weed, 8] <- dirtAVG[8] # replace pixels with dirt avg
    df <- rbind(newVeg, dirt)
    avgNDVI <- mean(df[,2], na.rm = T)
    avgGNDVI <- mean(df[,3], na.rm = T)
    avgGDVI2 <- mean(df[,4], na.rm = T)
    avgRED <- mean(df[,5], na.rm = T)
    avgNIR <- mean(df[,6], na.rm = T)
    avgGRE <- mean(df[,7], na.rm = T)
    avgREG <- mean(df[,8], na.rm = T)
    tog <- c(avgNDVI, avgGNDVI, avgGDVI2, avgRED, avgNIR, avgGRE, avgREG)
    vegW[j] <- tog
  }
  
  vegW <- t(vegW)
  colnames(vegW) <- c("NDVINW", "GNDVINW", "GDVI2NW", "REDNW", "NIRNW", "GRENW", "REGNW")
  rownames(vegW) <- 1:length(vegW[,1])
  vegW <- as.data.frame(vegW)
  noWeeds[[i]] <- vegW

}
# ~18 min


# get average of pixels without dirt or weeds
noWeedsNoDirt <- list()
for (i in 1:length(finalPlots)) {
  small <- crop(all, finalPlots[i,]) # makes a smaller raster
  df <- extract(small, finalPlots[i,], df = T) # makes df of points in raster
  veg <- df[which(df[,2] > 0.1),] # separate out veg pixels
  dirt <- df[which(df[,2] <= 0.1),] # separate out dirt pixels
  dirtAVG <- as.vector(colMeans(dirt))
  
  vegW <- as.data.frame(1:7)
  for (j in 1:100) {
    weed <- runif((abs(weeds[i])/100 * length(df$ID)), 0, length(veg$ID)) # select random pixels as weeds NEED TO DEAL WITH NEGATIVE WEEDS
    newVeg <- veg
    newVeg[weed, 2] <- dirtAVG[2] # replace pixels with dirt avg
    newVeg[weed, 3] <- dirtAVG[3] # replace pixels with dirt avg
    newVeg[weed, 4] <- dirtAVG[4] # replace pixels with dirt avg
    newVeg[weed, 5] <- dirtAVG[5] # replace pixels with dirt avg
    newVeg[weed, 6] <- dirtAVG[6] # replace pixels with dirt avg
    newVeg[weed, 7] <- dirtAVG[7] # replace pixels with dirt avg
    newVeg[weed, 8] <- dirtAVG[8] # replace pixels with dirt avg
    df <- newVeg[which(newVeg[,2] > 0.1),]
    avgNDVI <- mean(df[,2], na.rm = T)
    avgGNDVI <- mean(df[,3], na.rm = T)
    avgGDVI2 <- mean(df[,4], na.rm = T)
    avgRED <- mean(df[,5], na.rm = T)
    avgNIR <- mean(df[,6], na.rm = T)
    avgGRE <- mean(df[,7], na.rm = T)
    avgREG <- mean(df[,8], na.rm = T)
    tog <- c(avgNDVI, avgGNDVI, avgGDVI2, avgRED, avgNIR, avgGRE, avgREG)
    vegW[j] <- tog
  }
  
  vegW <- t(vegW)
  colnames(vegW) <- c("NDVINWND", "GNDVINWND", "GDVI2NWND", "REDNWND", "NIRNWND", "GRENWND", "REGNWND")
  rownames(vegW) <- 1:length(vegW[,1])
  vegW <- as.data.frame(vegW)
  noWeedsNoDirt[[i]] <- vegW
  
}



# plot volume ----

# subtract DSM from DTM to get vegetation height
vhm <- dsm - dtm

# find average height (m) across plot, then multiply by l and w (m)
heightPlots <- c()
for (i in 1:length(testP)){
  small <- crop(vhm, testP[i,])
  avg <- extract(small, testP[i,], fun = mean, na.rm = TRUE)
  heightPlots[i] <- as.numeric(avg)
}

volPlots <- c()
for (i in 1:length(heightPlots)) {
  p <- crop(vhm, testP[i,])
  numPixels <- rasterize(testP[i,], p, 1)
  numPixels <- cellStats(numPixels, "sum")
  dim <- sqrt(numPixels) * 1.15 / 100
  vol <- dim * dim * heightPlots[i]
  vol <- vol/numPixels # calculate volume per pixel
  vol <- vol * 17013 # multiply by number of pixels in 1.5x1.5 m plot
  volPlots[i] <- vol
}






