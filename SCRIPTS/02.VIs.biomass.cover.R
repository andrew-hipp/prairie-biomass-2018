# how do VIs predict biomass and cover

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

# predicting biomass from VIs
categories = c("biomass.all", "biomass.monocultures", "biomass.tmts")
predictors = c("pNDVIvalues","ndvicover", "pGNDVIvalues", "gndvicover", "pGDVI2values", "gdvi2cover")

# predicting cover from VIs
categories = c("coverTotal", "coverTotal.mono", "coverTotal.tmts")
predictors = c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")


all.rsqrs <- as.data.frame(c(1:length(predictors)))
all.coefs <- as.data.frame(c(1:length(predictors)))
for (j in 1:length(categories)) {
  rsqrs <- c()
  coefs <- c()
  for (i in 1:length(predictors)) {
    lm.test <- lm(all.prairie[[predictors[i]]] ~ all.prairie[[categories[j]]])
    rsqrs[length(rsqrs) + 1] <- summary(lm.test)$adj.r.squared
    coefs[length(coefs) + 1] <- summary(lm.test)$coef[2,1]
    i <- i + 1
  }
  all.rsqrs[j] <- rsqrs
  all.coefs[j] <- coefs
  j <- j + 1
}

colnames(all.rsqrs) <- categories
all.rsqrs$predictors <- predictors
colnames(all.coefs) <- categories
all.coefs$predictors <- predictors



all.rsqrs <- melt(all.rsqrs, id = c("predictors"))
all.coefs <- melt(all.coefs, id = c("predictors"))


ggplot(data = all.rsqrs, 
       aes(x = all.rsqrs$predictors, y = all.rsqrs$value)) +
  geom_point(aes(col = all.rsqrs$variable))

ggplot(data = all.coefs, 
       aes(x = all.coefs$predictors, y = all.coefs$value)) +
  geom_point(aes(col = all.coefs$variable))


