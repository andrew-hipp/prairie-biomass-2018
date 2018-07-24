# calculate VIs for each plot

library(reshape2)

# ISSUE once the rasters are somewhere we can access them, these file names will need to change
pNDVI <- raster("/Users/lanescher/Desktop/tiffs_final/ndvi.tif")
pGNDVI <- raster("/Users/lanescher/Desktop/tiffs_final/gndvi.tif")
pGDVI2 <- raster("/Users/lanescher/Desktop/tiffs_final/gdvi2.tif")

# read in flower presence
flowers <- read.csv("DATA/plot.flowers.csv") 
VI$flowers <- flowers$flowers

# ISSUE add script that calculates avg VIs and band for each plot



# find threshold for each plot
VI$ndvi.threshold <- findthreshold(pNDVI, plotlist)
VI$gndvi.threshold <- findthreshold(pGNDVI, plotlist)
VI$gdvi2.threshold <- findthreshold(pGDVI2, plotlist)

# find average above AVG THRESHOLD for each plot
VI$ndvi.avg.threshold <- avgoverthreshold(pNDVI, plotlist, threshold = mean(VI$ndvi.threshold, na.rm = TRUE))
VI$gndvi.avg.threshold <- avgoverthreshold(pGNDVI, plotlist, threshold = mean(VI$gndvi.threshold, na.rm = TRUE))
VI$gdvi2.avg.threshold <- avgoverthreshold(pGDVI2, plotlist, threshold = mean(VI$gdvi2.threshold, na.rm = TRUE))

# find average above AVG THRESHOLD for each plot, without flower plots
VI$ndvi.avg.threshold.noflowers <- avgoverthreshold(pNDVI, plotlist, 
                                                         threshold = mean(VI$ndvi.threshold[which(VI$flowers == 0)], na.rm = TRUE))
VI$gndvi.avg.threshold.noflowers <- avgoverthreshold(pGNDVI, plotlist, 
                                                          threshold = mean(VI$gndvi.threshold[which(VI$flowers == 0)], na.rm = TRUE))
VI$gdvi2.avg.threshold.noflowers <- avgoverthreshold(pGDVI2, plotlist, 
                                                          threshold = mean(VI$gdvi2.threshold[which(VI$flowers == 0)], na.rm = TRUE))

# write VI df as VIdata.csv, all of these columns should be compiled into all.prairie
write.csv(VI, "DATA/VIdata.csv", row.names = TRUE)
