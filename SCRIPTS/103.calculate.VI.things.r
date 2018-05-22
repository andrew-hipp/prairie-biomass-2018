# calculate VI things

library(reshape2)

pNDVI <- raster("/Users/lanescher/Desktop/tiffs_final/ndvi.tif")
pGNDVI <- raster("/Users/lanescher/Desktop/tiffs_final/gndvi.tif")
pGDVI2 <- raster("/Users/lanescher/Desktop/tiffs_final/gdvi2.tif")

all.prairie$gr <- all.prairie$pGREvalues / all.prairie$pREDvalues
all.prairie$ndvicover <- all.prairie$pNDVIvalues * (all.prairie$coverTotal/100)
all.prairie$gndvicover <- all.prairie$pGNDVIvalues * (all.prairie$coverTotal/100)
all.prairie$gdvi2cover <- all.prairie$pGDVI2values * (all.prairie$coverTotal/100)

all.prairie$coverTotal.mono <- NA
all.prairie$coverTotal.mono[which(all.prairie$Plot.category == "Monoculture")] <- 
  all.prairie$coverTotal[which(all.prairie$Plot.category == "Monoculture")]
all.prairie$coverTotal.tmts <- NA
all.prairie$coverTotal.tmts[which(all.prairie$Plot.category == "Treatment")] <- 
  all.prairie$coverTotal[which(all.prairie$Plot.category == "Treatment")]


# find threshold for each plot
all.prairie$ndvi.min <- findthreshold(pNDVI, plotlist)
all.prairie$gndvi.min <- findthreshold(pGNDVI, plotlist)
all.prairie$gdvi2.min <- findthreshold(pGDVI2, plotlist)

# find average above AVG THRESHOLD for each plot
all.prairie$ndvi.threshold <- avgoverthreshold(pNDVI, plotlist, threshold = mean(all.prairie$ndvi.min, na.rm = TRUE))
all.prairie$gndvi.threshold <- avgoverthreshold(pGNDVI, plotlist, threshold = mean(all.prairie$gndvi.min, na.rm = TRUE))
all.prairie$gdvi2.threshold <- avgoverthreshold(pGDVI2, plotlist, threshold = mean(all.prairie$gdvi2.min, na.rm = TRUE))

 # find avgerage above individual threshold for each plot
all.prairie$ndvi.threshold.var <- avgoverthreshold.var(pNDVI, plotlist, threshold = all.prairie$ndvi.min)
all.prairie$gndvi.threshold.var <- avgoverthreshold.var(pGNDVI, plotlist, threshold = all.prairie$gndvi.min)
all.prairie$gdvi2.threshold.var <- avgoverthreshold.var(pGDVI2, plotlist, threshold = all.prairie$gdvi2.min)


# find average above AVG THRESHOLD for each plot, without flower plots
all.prairie$ndvi.threshold.noflowers <- avgoverthreshold(pNDVI, plotlist, 
                                                         threshold = mean(all.prairie$ndvi.min[which(all.prairie$flowers == 0)], na.rm = TRUE))
all.prairie$gndvi.threshold.noflowers <- avgoverthreshold(pGNDVI, plotlist, 
                                                          threshold = mean(all.prairie$gndvi.min[which(all.prairie$flowers == 0)], na.rm = TRUE))
all.prairie$gdvi2.threshold.noflowers <- avgoverthreshold(pGDVI2, plotlist, 
                                                          threshold = mean(all.prairie$gdvi2.min[which(all.prairie$flowers == 0)], na.rm = TRUE))

write.csv(all.prairie, "DATA/all.prairie.with.VI.values.csv", row.names = TRUE)
