# calculate VI things

library(reshape2)

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

# find average above AVG THRESHOLD for each plot
all.prairie$ndvi.threshold <- avgoverthreshold(pNDVI, plotlist, threshold = mean(all.prairie$ndvi.min, na.rm = TRUE))
all.prairie$gndvi.threshold <- avgoverthreshold(pGNDVI, plotlist, threshold = mean(all.prairie$gndvi.min, na.rm = TRUE))

# find avgerage above individual threshold for each plot
all.prairie$ndvi.threshold.var <- avgoverthreshold.var(pNDVI, plotlist, threshold = all.prairie$ndvi.min)
all.prairie$gndvi.threshold.var <- avgoverthreshold.var(pGNDVI, plotlist, threshold = all.prairie$gndvi.min)

# find average above AVG THRESHOLD for each plot, without flower plots
all.prairie$ndvi.threshold.noflowers <- avgoverthreshold(pNDVI, plotlist, 
                                                         threshold = mean(all.prairie$ndvi.min[which(all.prairie$flowers == 0)], na.rm = TRUE))
all.prairie$gndvi.threshold.noflowers <- avgoverthreshold(pGNDVI, plotlist, 
                                                          threshold = mean(all.prairie$gndvi.min[which(all.prairie$flowers == 0)], na.rm = TRUE))

