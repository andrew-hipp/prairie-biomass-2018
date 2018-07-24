# old script taken out of 103.calculate.VI.things.r


all.prairie$coverTotal.mono <- NA
all.prairie$coverTotal.mono[which(all.prairie$Plot.category == "Monoculture")] <- 
  all.prairie$coverTotal[which(all.prairie$Plot.category == "Monoculture")]
all.prairie$coverTotal.tmts <- NA
all.prairie$coverTotal.tmts[which(all.prairie$Plot.category == "Treatment")] <- 
  all.prairie$coverTotal[which(all.prairie$Plot.category == "Treatment")]


# find avgerage above individual threshold for each plot
all.prairie$ndvi.threshold.var <- avgoverthreshold.var(pNDVI, plotlist, threshold = all.prairie$ndvi.min)
all.prairie$gndvi.threshold.var <- avgoverthreshold.var(pGNDVI, plotlist, threshold = all.prairie$gndvi.min)
all.prairie$gdvi2.threshold.var <- avgoverthreshold.var(pGDVI2, plotlist, threshold = all.prairie$gdvi2.min)