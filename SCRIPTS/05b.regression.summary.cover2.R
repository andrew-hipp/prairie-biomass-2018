# make regression table for cover

# make scaled dataframe
scaled <- as.data.frame(prairie$plot)
scaled$Plot.category <- prairie$Plot.category
scaled$TMT.use <- prairie$TMT.use
scaled$biomass.all <- scale(prairie$biomass.all)
scaled$NDVI <- scale(prairie$NDVI)
scaled$GNDVI <- scale(prairie$GNDVI)
scaled$GDVI2 <- scale(prairie$GDVI2)
scaled$NDVINW <- scale(prairie$NDVINW)
scaled$GNDVINW <- scale(prairie$GNDVINW)
scaled$GDVI2NW <- scale(prairie$GDVI2NW)
scaled$NDVINWND <- scale(prairie$NDVINWND)
scaled$GNDVINWND <- scale(prairie$GNDVINWND)
scaled$GDVI2NWND <- scale(prairie$GDVI2NWND)
scaled$NDVIND <- scale(prairie$NDVIND)
scaled$GNDVIND <- scale(prairie$GNDVIND)
scaled$GDVI2ND <- scale(prairie$GDVI2ND)
scaled$coverTotal <- scale(prairie$coverTotal)
scaled$dcover <- scale(prairie$dcover)
scaled$AHOR_cm <- scale(prairie$AHOR_cm)
scaled$VOL <- scale(prairie$VOL)
scaled$trait.div <- prairie$trait.div
scaled$phylo.div <- prairie$phy.div
scaled$DensVol <- prairie$NDVI * prairie$VOL

use.prairie <- scaled[which(scaled$Plot.category == "Monoculture"),]

response <- "dcover"

# 1 predictor, VI
pred1 <- c("NDVI", "GNDVI", "GDVI2",
           "NDVIND", "GNDVIND", "GDVI2ND")


t1 <- as.data.frame(pred1)


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,3] <- "-"
  t1[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V5),]
t1$V5 <- as.numeric(t1$V5)
t1$deltaAIC <- t1$V5 - t1$V5[1]

colnames(t1) <- c("VI used", "VI", "volume", "R2", "AIC", "deltaAIC")



# 1 predictor, ahor
pred2 <- c("AHOR_cm")

t2 <- as.data.frame(pred2)

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,2] <- "-"
  t2[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V5),]
t2$V5 <- as.numeric(t2$V5)
t2$deltaAIC <- t2$V5 - t2$V5[1]

colnames(t2) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")

pred2.2 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm")

t3 <- as.data.frame(pred2.1)

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t3[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t3[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V5),]
t3$V5 <- as.numeric(t3$V5)
t3$deltaAIC <- t3$V5 - t3$V5[1]

colnames(t3) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")

# merge all tables
full <- rbind(t1, t2)
full <- rbind(full, t3)


# re-sort
full <- full[order(full$AIC),]
full$AIC <- as.numeric(full$AIC)
full$deltaAIC <- full$AIC - full$AIC[1]


# format a few things
full$`VI used` <- sub("pNDVIvalues", "NDVI", full$`VI used`)
full$`VI used` <- sub("pGNDVIvalues", "GNDVI", full$`VI used`)
full$`VI used` <- sub("pGDVI2values", "GDVI2", full$`VI used`)


full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
full$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", full$"A-horizon")

# save as csv
write.csv(full, "../OUT/TABLE.coverdrone.regression.csv")


#### Do it again for cover from the ground
response <- "coverTotal"

# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")


t1 <- as.data.frame(pred1)


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,3] <- "-"
  t1[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V5),]
t1$V5 <- as.numeric(t1$V5)
t1$deltaAIC <- t1$V5 - t1$V5[1]

colnames(t1) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")



# 1 predictor, ahor
pred2 <- c("AHOR_cm")

t2 <- as.data.frame(pred2)

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,2] <- "-"
  t2[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V5),]
t2$V5 <- as.numeric(t2$V5)
t2$deltaAIC <- t2$V5 - t2$V5[1]

colnames(t2) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")

pred2.2 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm")

t3 <- as.data.frame(pred2.1)

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t3[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t3[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V5),]
t3$V5 <- as.numeric(t3$V5)
t3$deltaAIC <- t3$V5 - t3$V5[1]

colnames(t3) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")

# merge all tables
full <- rbind(t1, t2)
full <- rbind(full, t3)


# re-sort
full <- full[order(full$AIC),]
full$AIC <- as.numeric(full$AIC)
full$deltaAIC <- full$AIC - full$AIC[1]


# format a few things
full$`VI used` <- sub("pNDVIvalues", "NDVI", full$`VI used`)
full$`VI used` <- sub("pGNDVIvalues", "GNDVI", full$`VI used`)
full$`VI used` <- sub("pGDVI2values", "GDVI2", full$`VI used`)


full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
full$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", full$"A-horizon")

write.csv(full, "../OUT/TABLE.coverground.regression.csv")


###############################
##### NOW JUST FOR TREATMENTS##
###############################

use.prairie <- scaled[which(scaled$Plot.category == "Monoculture"),]

response <- "dcover"

# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")


t1 <- as.data.frame(pred1)


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,3] <- "-"
  t1[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V5),]
t1$V5 <- as.numeric(t1$V5)
t1$deltaAIC <- t1$V5 - t1$V5[1]

colnames(t1) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")



# 1 predictor, ahor
pred2 <- c("AHOR_cm")

t2 <- as.data.frame(pred2)

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,2] <- "-"
  t2[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V5),]
t2$V5 <- as.numeric(t2$V5)
t2$deltaAIC <- t2$V5 - t2$V5[1]

colnames(t2) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")

pred2.2 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm")

t3 <- as.data.frame(pred2.1)

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t3[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t3[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V5),]
t3$V5 <- as.numeric(t3$V5)
t3$deltaAIC <- t3$V5 - t3$V5[1]

colnames(t3) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")

# merge all tables
full <- rbind(t1, t2)
full <- rbind(full, t3)


# re-sort
full <- full[order(full$AIC),]
full$AIC <- as.numeric(full$AIC)
full$deltaAIC <- full$AIC - full$AIC[1]


# format a few things
full$`VI used` <- sub("pNDVIvalues", "NDVI", full$`VI used`)
full$`VI used` <- sub("pGNDVIvalues", "GNDVI", full$`VI used`)
full$`VI used` <- sub("pGDVI2values", "GDVI2", full$`VI used`)


full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
full$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", full$"A-horizon")

# save as csv
write.csv(full, "../OUT/TABLE.coverdrone.regression-mono.csv")


#### Do it again for cover from the ground
response <- "coverTotal"

# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")


t1 <- as.data.frame(pred1)


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,3] <- "-"
  t1[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V5),]
t1$V5 <- as.numeric(t1$V5)
t1$deltaAIC <- t1$V5 - t1$V5[1]

colnames(t1) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")



# 1 predictor, ahor
pred2 <- c("AHOR_cm")

t2 <- as.data.frame(pred2)

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,2] <- "-"
  t2[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V5),]
t2$V5 <- as.numeric(t2$V5)
t2$deltaAIC <- t2$V5 - t2$V5[1]

colnames(t2) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")

pred2.2 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm")

t3 <- as.data.frame(pred2.1)

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t3[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t3[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V5),]
t3$V5 <- as.numeric(t3$V5)
t3$deltaAIC <- t3$V5 - t3$V5[1]

colnames(t3) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")

# merge all tables
full <- rbind(t1, t2)
full <- rbind(full, t3)


# re-sort
full <- full[order(full$AIC),]
full$AIC <- as.numeric(full$AIC)
full$deltaAIC <- full$AIC - full$AIC[1]


# format a few things
full$`VI used` <- sub("pNDVIvalues", "NDVI", full$`VI used`)
full$`VI used` <- sub("pGNDVIvalues", "GNDVI", full$`VI used`)
full$`VI used` <- sub("pGDVI2values", "GDVI2", full$`VI used`)


full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
full$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", full$"A-horizon")

write.csv(full, "../OUT/TABLE.coverground.regression-mono.csv")



###############################
##### NOW JUST FOR TREATMENTS##
###############################

use.prairie <- scaled[which(scaled$Plot.category == "Treatment"),]

response <- "dcover"

# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")


t1 <- as.data.frame(pred1)


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,3] <- "-"
  t1[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V5),]
t1$V5 <- as.numeric(t1$V5)
t1$deltaAIC <- t1$V5 - t1$V5[1]

colnames(t1) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")



# 1 predictor, ahor
pred2 <- c("AHOR_cm")

t2 <- as.data.frame(pred2)

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,2] <- "-"
  t2[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V5),]
t2$V5 <- as.numeric(t2$V5)
t2$deltaAIC <- t2$V5 - t2$V5[1]

colnames(t2) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")

pred2.2 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm")

t3 <- as.data.frame(pred2.1)

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t3[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t3[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V5),]
t3$V5 <- as.numeric(t3$V5)
t3$deltaAIC <- t3$V5 - t3$V5[1]

colnames(t3) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")

# merge all tables
full <- rbind(t1, t2)
full <- rbind(full, t3)


# re-sort
full <- full[order(full$AIC),]
full$AIC <- as.numeric(full$AIC)
full$deltaAIC <- full$AIC - full$AIC[1]


# format a few things
full$`VI used` <- sub("pNDVIvalues", "NDVI", full$`VI used`)
full$`VI used` <- sub("pGNDVIvalues", "GNDVI", full$`VI used`)
full$`VI used` <- sub("pGDVI2values", "GDVI2", full$`VI used`)


full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
full$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", full$"A-horizon")

# save as csv
write.csv(full, "../OUT/TABLE.coverdrone.regression-tmt.csv")


#### Do it again for cover from the ground
response <- "coverTotal"

# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")


t1 <- as.data.frame(pred1)


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,3] <- "-"
  t1[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V5),]
t1$V5 <- as.numeric(t1$V5)
t1$deltaAIC <- t1$V5 - t1$V5[1]

colnames(t1) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")



# 1 predictor, ahor
pred2 <- c("AHOR_cm")

t2 <- as.data.frame(pred2)

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,2] <- "-"
  t2[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V5),]
t2$V5 <- as.numeric(t2$V5)
t2$deltaAIC <- t2$V5 - t2$V5[1]

colnames(t2) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values")

pred2.2 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm")

t3 <- as.data.frame(pred2.1)

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t3[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t3[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V5),]
t3$V5 <- as.numeric(t3$V5)
t3$deltaAIC <- t3$V5 - t3$V5[1]

colnames(t3) <- c("VI used", "VI", "A-horizon", "R2", "AIC", "deltaAIC")

# merge all tables
full <- rbind(t1, t2)
full <- rbind(full, t3)


# re-sort
full <- full[order(full$AIC),]
full$AIC <- as.numeric(full$AIC)
full$deltaAIC <- full$AIC - full$AIC[1]


# format a few things
full$`VI used` <- sub("pNDVIvalues", "NDVI", full$`VI used`)
full$`VI used` <- sub("pGNDVIvalues", "GNDVI", full$`VI used`)
full$`VI used` <- sub("pGDVI2values", "GDVI2", full$`VI used`)


full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
full$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", full$"A-horizon")

write.csv(full, "../OUT/TABLE.coverground.regression-tmt.csv")
