# calculate VIs for each plot

library(reshape2)

# ISSUE once the rasters are somewhere we can access them, these file names will need to change
pNDVI <- raster("/Users/clane_897q3pb/Documents/tiffs_final/ndvi.tif")
pGNDVI <- raster("/Users/clane_897q3pb/Documents/tiffs_final/gndvi.tif")
pGDVI2 <- raster("/Users/clane_897q3pb/Documents/tiffs_final/gdvi2.tif")
pRED <- raster("/Users/clane_897q3pb/Documents/tiffs_final/red.tif")
pGRE <- raster("/Users/clane_897q3pb/Documents/tiffs_final/green.tif")
pNIR <- raster("/Users/clane_897q3pb/Documents/tiffs_final/nir.tif")
pREG <- raster("/Users/clane_897q3pb/Documents/tiffs_final/rededge.tif")


# calculate avg VIs and band for each plot
VI$pNDVIvalues <- avgvalues(pNDVI, plotlist)
VI$pGNDVIvalues <- avgvalues(pGNDVI, plotlist)
VI$pGDVI2values <- avgvalues(pGDVI2, plotlist)
VI$pREDvalues <- avgvalues(pRED, plotlist)
VI$pGREvalues <- avgvalues(pGRE, plotlist)
VI$pNIRvalues <- avgvalues(pNIR, plotlist)
VI$pREGvalues <- avgvalues(pREG, plotlist)

# find threshold for each plot in each VI
VI$ndvi.threshold <- findthreshold(pNDVI, plotlist)
VI$gndvi.threshold <- findthreshold(pGNDVI, plotlist)
VI$gdvi2.threshold <- findthreshold(pGDVI2, plotlist)

# find average above AVG THRESHOLD for each plot
VI$ndvi.avg.threshold <- avgoverthreshold(pNDVI, plotlist, threshold = mean(VI$ndvi.threshold, na.rm = TRUE))
VI$gndvi.avg.threshold <- avgoverthreshold(pGNDVI, plotlist, threshold = mean(VI$gndvi.threshold, na.rm = TRUE))
VI$gdvi2.avg.threshold <- avgoverthreshold(pGDVI2, plotlist, threshold = mean(VI$gdvi2.threshold, na.rm = TRUE))


# find average above AVG THRESHOLD for each plot, without flower plots
VI$ndvi.avg.threshold.noflowers <- avgoverthreshold(pNDVI, plotlist, 
                                                         threshold = mean(VI$ndvi.threshold[which(all.prairie$flower.presence == 0)], na.rm = TRUE))
VI$gndvi.avg.threshold.noflowers <- avgoverthreshold(pGNDVI, plotlist, 
                                                          threshold = mean(VI$gndvi.threshold[which(all.prairie$flower.presence == 0)], na.rm = TRUE))
VI$gdvi2.avg.threshold.noflowers <- avgoverthreshold(pGDVI2, plotlist, 
                                                          threshold = mean(VI$gdvi2.threshold[which(all.prairie$flower.presence == 0)], na.rm = TRUE))

# write VI df as VIdata.csv, all of these columns should be compiled into all.prairie
write.csv(VI, "../DATA/VIdata.csv", row.names = TRUE)


# calculate average thresholds for each VI
avg.ndvi.threshold <- mean(VI$ndvi.threshold)
avg.ndvi.threshold.nf <- mean(VI$ndvi.threshold[which(all.prairie$flower.presence == 0)])

avg.gndvi.threshold <- mean(VI$gndvi.threshold)
avg.gndvi.threshold.nf <- mean(VI$gndvi.threshold[which(all.prairie$flower.presence == 0)])

avg.gdvi2.threshold <- mean(VI$gdvi2.threshold)
avg.gdvi2.threshold.nf <- mean(VI$gdvi2.threshold[which(all.prairie$flower.presence == 0)])

thresholds <- as.data.frame(c("ndvi", "gndvi", "gdvi2"))
thresholds <- cbind(thresholds, c(avg.ndvi.threshold, avg.gndvi.threshold, avg.gdvi2.threshold))
thresholds <- cbind(thresholds, c(avg.ndvi.threshold.nf, avg.gndvi.threshold.nf, avg.gdvi2.threshold.nf))
colnames(thresholds) <- c("VI", "threshold including flower plots", "threshold excluding flower plots")

write.csv(thresholds, "../OUT/TABLE.threshold.values.csv")
