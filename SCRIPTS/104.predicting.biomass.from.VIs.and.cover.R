# how do VIs predict biomass and cover

###############################
## Simple linear regressions ##
###############################

# predicting biomass from VIs
categories = c("biomass.all", "biomass.monocultures", "biomass.tmts")
predictors = c("pNDVIvalues","ndvicover", 
               "pGNDVIvalues", "gndvicover", 
               "pGDVI2values", "gdvi2cover", 
               "ndvi.threshold", "ndvi.threshold.var",
               "gndvi.threshold", "gndvi.threshold.var",
               "ndvi.threshold.noflowers", "gndvi.threshold.noflowers")

# predicting cover from VIs
categories = c("coverTotal", "coverTotal.mono", "coverTotal.tmts")
predictors = c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values", 
               "ndvi.threshold", "gndvi.threshold")

i <- 1
j <- 1
all.rsqrs <- as.data.frame(c(1:length(predictors)))
all.coefs <- as.data.frame(c(1:length(predictors)))
for (j in 1:length(categories)) {
  rsqrs <- c()
  coefs <- c()
  for (i in 1:length(predictors)) {
    lm.test <- lm(all.prairie[[categories[j]]] ~ all.prairie[[predictors[i]]])
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
  geom_point(aes(col = all.rsqrs$variable)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_discrete(name = "predictor", 
                   limits=c("pNDVIvalues", "pGDVI2values", "pGNDVIvalues",
                            "ndvicover", "gdvi2cover", "gndvicover",
                            "ndvi.threshold", "ndvi.threshold.var",
                            "gndvi.threshold", "gndvi.threshold.var",
                            "ndvi.threshold.noflowers", "gndvi.threshold.noflowers"))

ggplot(data = all.coefs, 
       aes(x = all.coefs$predictors, y = all.coefs$value)) +
  geom_point(aes(col = all.coefs$variable))

plot(x = all.prairie$gndvi.threshold[which(all.prairie$Plot.category == "Monoculture")], 
     y = all.prairie$gndvicover[which(all.prairie$Plot.category == "Monoculture")],
     col = "blue")
points(x = all.prairie$gndvi.threshold[which(all.prairie$Plot.category == "Treatment")], 
     y = all.prairie$gndvicover[which(all.prairie$Plot.category == "Treatment")],
     col = "red")

boxplot(all.prairie$gndvi.range ~ all.prairie$Plot.category)


###############################
#### Multiple regressions #####
###############################

# scale all variables
all.prairie$ndvi.scaled <- scale(all.prairie$pNDVIvalues)
all.prairie$ndvi.threshold.scaled <- scale(all.prairie$ndvi.threshold)
all.prairie$ndvi.threshold.noflowers.scaled <- scale(all.prairie$ndvi.threshold.noflowers)

all.prairie$gndvi.scaled <- scale(all.prairie$pGNDVIvalues)
all.prairie$gndvi.threshold.scaled <- scale(all.prairie$gndvi.threshold)
all.prairie$gndvi.threshold.noflowers.scaled <- scale(all.prairie$gndvi.threshold.noflowers)

all.prairie$cover.scaled <- scale(all.prairie$coverTotal)

all.prairie$biomass.all.scaled <- scale(all.prairie$biomass.all)

all.prairie$AHOR_cm.scaled <- scale(all.prairie$AHOR_cm)

all.prairie$pH.scaled <- scale(all.prairie$pH)

# predict biomass from VI and cover

categories = c("biomass.all", "biomass.monocultures", "biomass.tmts")
predictors = c("ndvi.scaled","ndvi.threshold.scaled", 
               "ndvi.threshold.noflowers.scaled", "gndvi.scaled", 
               "gndvi.threshold.scaled", "gndvi.threshold.noflowers.scaled")

i <- 1
j <- 1
all.rsqrs <- as.data.frame(c(1:length(predictors)))
all.coefs <- as.data.frame(c(1:length(predictors)))
for (j in 1:length(categories)) {
  rsqrs <- c()
  coefs <- c()
  for (i in 1:length(predictors)) {
    lm.test <- lm(all.prairie[[categories[j]]] ~ all.prairie[[predictors[i]]] + all.prairie$cover.scaled)
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
  geom_point(aes(col = all.rsqrs$variable)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_discrete(name = "predictor", 
                   limits=c("ndvi.scaled","ndvi.threshold.scaled", 
                            "ndvi.threshold.noflowers.scaled", "gndvi.scaled", 
                            "gndvi.threshold.scaled", "gndvi.threshold.noflowers.scaled"))

ggplot(data = all.coefs, 
       aes(x = all.coefs$predictors, y = all.coefs$value)) +
  geom_point(aes(col = all.coefs$variable)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_discrete(name = "predictor", 
                   limits=c("ndvi.scaled","ndvi.threshold.scaled", 
                            "ndvi.threshold.noflowers.scaled", "gndvi.scaled", 
                            "gndvi.threshold.scaled", "gndvi.threshold.noflowers.scaled"))


hist(all.prairie$gndvi.threshold.noflowers)


#### also use ahor as a predictor
categories = c("biomass.all", "biomass.monocultures", "biomass.tmts")
predictors = c("ndvi.scaled","ndvi.threshold.scaled", 
               "ndvi.threshold.noflowers.scaled", "gndvi.scaled", 
               "gndvi.threshold.scaled", "gndvi.threshold.noflowers.scaled")

i <- 1
j <- 1
all.rsqrs <- as.data.frame(c(1:length(predictors)))
all.coefs <- as.data.frame(c(1:length(predictors)))
for (j in 1:length(categories)) {
  rsqrs <- c()
  coefs <- c()
  for (i in 1:length(predictors)) {
    lm.test <- lm(all.prairie[[categories[j]]] ~ all.prairie[[predictors[i]]] + all.prairie$coverTotal + all.prairie$AHOR_cm.scaled)
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
  geom_point(aes(col = all.rsqrs$variable)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_discrete(name = "predictor", 
                   limits=c("ndvi.scaled","ndvi.threshold.scaled", 
                            "ndvi.threshold.noflowers.scaled", "gndvi.scaled", 
                            "gndvi.threshold.scaled", "gndvi.threshold.noflowers.scaled"))

## cover 

# predicting cover from VIs
categories = c("coverTotal", "coverTotal.mono", "coverTotal.tmts")
predictors = c("pNDVIvalues", "pGNDVIvalues")

i <- 1
j <- 1
all.rsqrs <- as.data.frame(c(1:length(predictors)))
all.coefs <- as.data.frame(c(1:length(predictors)))
for (j in 1:length(categories)) {
  rsqrs <- c()
  coefs <- c()
  for (i in 1:length(predictors)) {
    lm.test <- lm(all.prairie[[categories[j]]] ~ all.prairie[[predictors[i]]] + all.prairie$AHOR_cm)
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
  geom_point(aes(col = all.rsqrs$variable)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_discrete(name = "predictor", 
                   limits=c("pNDVIvalues", "pGNDVIvalues"))
