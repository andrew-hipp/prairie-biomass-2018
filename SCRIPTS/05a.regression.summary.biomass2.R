# make regression table

# NDVI, NDVI.CA, NDVI.CI, NDVI.ND, NDVI.ND.CA, NDVI.ND.CI
scaled <- as.data.frame(scale(prairie.bio[,81:123]))
scaled$Plot.category <- prairie.bio$Plot.category
scaled$TMT.use <- prairie.bio$TMT.use
scaled$biomass.all <- scale(prairie.bio$biomass.all)
scaled$coverTotal <- scale(prairie.bio$coverTotal)
scaled$dcover <- scale(prairie.bio$dcover)
scaled$AHOR_cm <- scale(prairie.bio$AHOR_cm)
scaled$VOL <- scale(prairie.bio$VOL)
scaled$DensVol <- scale(prairie.bio$NDVI * prairie.bio$VOL)

groups <- list(scaled, 
            scaled[which(scaled$Plot.category == "Monoculture"),],
            scaled[which(scaled$Plot.category == "Treatment"),])
namesL <- c("all", "mono", "tmt")

for (j in 1:3){
  use.prairie <- groups[[j]]
  
  response <- "biomass.all"
  
  # 1 predictor, spectra ----
  pred1 <- colnames(scaled)[c(1:7, 22:28)]
  
  t1 <- as.data.frame(pred1)
  t1$cover.used <- "-"
  
  
  # make loop to fill in 1 predictor
  for (i in 1:length(pred1)) {
    coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
    pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
    t1[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
    t1[i,4] <- "-"
    t1[i,5] <- "-"
    t1[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
    t1[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  }
  
  t1 <- t1[order(t1$V7),]
  t1$V7 <- as.numeric(t1$V7)
  t1$deltaAIC <- t1$V7 - t1$V7[1]
  
  colnames(t1) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  
  #bestSpectra <- t1
  
  # 1 predictor, cover ----
  pred2 <- c("coverTotal", "dcover")
  
  
  t2 <- as.data.frame(pred2)
  t2$cover.used <- pred2
  
  # make loop to fill in 1 predictor
  for (i in 1:length(pred2)) {
    coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
    pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
    t2[i,3] <- "-"
    t2[i,4] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
    t2[i,5] <- "-"
    t2[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
    t2[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  }
  
  t2 <- t2[order(t2$V7),]
  t2$V7 <- as.numeric(t2$V7)
  t2$deltaAIC <- t2$V7 - t2$V7[1]
  
  colnames(t2) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  t2$`VI used` <- "-"
  
  
  
  # 1 predictor, volume----
  pred3 <- c("VOL")
  
  t3 <- as.data.frame(pred3)
  t3$cover.used <- "-"
  
  # make loop to fill in 1 predictor
  for (i in 1:length(pred3)) {
    coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,1]
    pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,4]
    t3[i,3] <- "-"
    t3[i,4] <- "-"
    t3[i,5] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
    t3[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$r.squared, 3), nsmall = 3)
    t3[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]])), 2), nsmall = 2)
  }
  
  t3 <- t3[order(t3$V7),]
  t3$V7 <- as.numeric(t3$V7)
  t3$deltaAIC <- t3$V7 - t3$V7[1]
  
  colnames(t3) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  t3$`VI used` <- "-"
  
  volPred <- t3
  
  # 2 predictors, VI and cover ----
  
  pred2.1 <- rep(colnames(scaled)[c(1:7, 22:28)], times = 2)
  
  pred2.2 <- rep(c("coverTotal", "dcover"), each = 14)
  
  t4 <- as.data.frame(pred2.1)
  t4$cover.used <- pred2.2
  
  for (i in 1:length(pred2.1)) {
    coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
    pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
    coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
    pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
    t4[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
    t4[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
    t4[i,5] <- "-"
    t4[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
    t4[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  }
  
  t4 <- t4[order(t4$V7),]
  t4$V7 <- as.numeric(t4$V7)
  t4$deltaAIC <- t4$V7 - t4$V7[1]
  
  colnames(t4) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  
  # 2 predictors, VI and volume ----
  
  pred2.1 <- rep(colnames(scaled)[c(1:7, 22:28)], times = 1)
  
  pred2.2 <- rep(c("VOL"), each = 14)
  
  
  t41 <- as.data.frame(pred2.1)
  t41$cover.used <- "-"
  
  for (i in 1:length(pred2.1)) {
    coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
    pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
    coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
    pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
    t41[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
    t41[i,4] <- "-"
    t41[i,5] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
    t41[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
    t41[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  }
  
  t41 <- t41[order(t41$V7),]
  t41$V7 <- as.numeric(t41$V7)
  t41$deltaAIC <- t41$V7 - t41$V7[1]
  
  colnames(t41) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  
  # 2 predictors, cover and volume ----
  
  pred2.1 <- c("coverTotal", "dcover")
  
  pred2.2 <- c("VOL", "VOL")
  
  
  t42 <- as.data.frame(pred2.1)
  t42$cover.used <- pred2.1
  t42$pred2.1 <- "-"
  
  for (i in 1:length(pred2.1)) {
    coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
    pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
    coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
    pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
    t42[i,3] <- "-"
    t42[i,4] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
    t42[i,5] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
    t42[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
    t42[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  }
  
  t42 <- t42[order(t42$V7),]
  t42$V7 <- as.numeric(t42$V7)
  t42$deltaAIC <- t42$V7 - t42$V7[1]
  
  colnames(t42) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  
  
  # 3 predictors ----
  
  pred3.1 <- rep(colnames(scaled)[c(1:7, 22:28)], times = 2)
  
  pred3.2 <- rep(c("coverTotal", "dcover"), each = 14)
  
  pred3.3 <- rep(c("VOL"), each = 28)
  
  
  
  t5 <- as.data.frame(pred3.1)
  t5$cover.used <- pred3.2
  
  for (i in 1:length(pred3.1)) {
    coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,1]
    pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,4]
    coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,1]
    pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,4]
    coef3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,1]
    pval3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,4]
    t5[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
    t5[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
    t5[i,5] <- paste(format(round(coef3, 2), nsmall = 2), ", p = ", format(round(pval3, 4), nsmall = 4), sep = "")
    t5[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$r.squared, 3), nsmall = 3)
    t5[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]])), 2), nsmall = 2)
  }
  
  t5 <- t5[order(t5$V7),]
  t5$V7 <- as.numeric(t5$V7)
  t5$deltaAIC <- t5$V7 - t5$V7[1]
  
  colnames(t5) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  
  
  # merge all tables ----
  full <- rbind(t1, t2, t3, t4, t41, t42, t5)
  
  # re-sort
  full <- full[order(full$AIC),]
  full$AIC <- as.numeric(full$AIC)
  full$deltaAIC <- full$AIC - full$AIC[1]
  
  
  # format a few things
  full$`cover used` <- sub("coverTotal", "ground", full$`cover used`)
  full$`cover used` <- sub("dcover", "drone", full$`cover used`)
  
  full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
  full$cover <- sub("p = 0.0000", "p < 0.0001", full$cover)
  full$volume <- sub("p = 0.0000", "p < 0.0001", full$volume)
  
  assign(x = paste0("full-", namesL[j]), value = full)
  
  write.csv(full, paste0("../OUT/TABLE.biomass.regression.full.", names[i], ".csv"))
  
  
}





# all VIs ----
#pred1 <- colnames(scaled)[c(1:3, 8:10, 15:17, 22:24, 29:31, 36:38)]
pred1 <- colnames(scaled)[c(1:43, 46:50)]


t1 <- as.data.frame(pred1)
t1$cover.used <- "-"


# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,4] <- "-"
  t1[i,5] <- "-"
  t1[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
}

t1 <- t1[order(t1$V7),]
t1$V7 <- as.numeric(t1$V7)
t1$deltaAIC <- t1$V7 - t1$V7[1]

colnames(t1) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")





















# make partial table for pub

# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values", 
           "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

t1 <- as.data.frame(pred1)
t1$cover.used <- "-"


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,4] <- "-"
  t1[i,5] <- "-"
  t1[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V7),]
t1$V7 <- as.numeric(t1$V7)
t1$deltaAIC <- t1$V7 - t1$V7[1]

colnames(t1) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# 1 predictor, cover
pred2 <- c("coverTotal", "dcover")


t2 <- as.data.frame(pred2)
t2$cover.used <- pred2

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,3] <- "-"
  t2[i,4] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,5] <- "-"
  t2[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V7),]
t2$V7 <- as.numeric(t2$V7)
t2$deltaAIC <- t2$V7 - t2$V7[1]

colnames(t2) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 1 predictor, ahor
pred3 <- c("AHOR_cm")

t3 <- as.data.frame(pred3)
t3$cover.used <- "-"

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred3)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,4]
  t3[i,3] <- "-"
  t3[i,4] <- "-"
  t3[i,5] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V7),]
t3$V7 <- as.numeric(t3$V7)
t3$deltaAIC <- t3$V7 - t3$V7[1]

colnames(t3) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t3$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred2.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal")

t4 <- as.data.frame(pred2.1)
t4$cover.used <- pred2.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t4[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t4[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t4[i,5] <- "-"
  t4[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t4[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t4 <- t4[order(t4$V7),]
t4$V7 <- as.numeric(t4$V7)
t4$deltaAIC <- t4$V7 - t4$V7[1]

colnames(t4) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")



# 3 predictors

pred3.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred3.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal")

pred3.3 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm")

t5 <- as.data.frame(pred3.1)
t5$cover.used <- pred3.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,4]
  coef3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,1]
  pval3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,4]
  t5[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t5[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t5[i,5] <- paste(format(round(coef3, 2), nsmall = 2), ", p = ", format(round(pval3, 4), nsmall = 4), sep = "")
  t5[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$r.squared, 3), nsmall = 3)
  t5[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t5 <- t5[order(t5$V7),]
t5$V7 <- as.numeric(t5$V7)
t5$deltaAIC <- t5$V7 - t5$V7[1]

colnames(t5) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# merge all tables
partial <- rbind(t1, t2)
partial <- rbind(partial, t3)
partial <- rbind(partial, t4)
partial <- rbind(partial, t5)


# re-sort
partial <- partial[order(partial$AIC),]
partial$AIC <- as.numeric(partial$AIC)
partial$deltaAIC <- partial$AIC - partial$AIC[1]


# format a few things
partial$`VI used` <- sub("pNDVIvalues", "NDVI", partial$`VI used`)
partial$`VI used` <- sub("pGNDVIvalues", "GNDVI", partial$`VI used`)
partial$`VI used` <- sub("pGDVI2values", "GDVI2", partial$`VI used`)
partial$`VI used` <- sub("gndvi.avg.threshold", "GNDVIt", partial$`VI used`)
partial$`VI used` <- sub("ndvi.avg.threshold", "NDVIt", partial$`VI used`)
partial$`VI used` <- sub("gdvi2.avg.threshold", "GDVI2t", partial$`VI used`)
partial$`VI used` <- sub("NDVIt.noflowers", "NDVIt-f", partial$`VI used`)
partial$`VI used` <- sub("GNDVIt.noflowers", "GNDVIt-f", partial$`VI used`)
partial$`VI used` <- sub("GDVI2t.noflowers", "GDVI2t-f", partial$`VI used`)

partial$`cover used` <- sub("coverTotal", "ground", partial$`cover used`)
partial$`cover used` <- sub("dcover", "drone", partial$`cover used`)

partial$VI <- sub("p = 0.0000", "p < 0.0001", partial$VI)
partial$cover <- sub("p = 0.0000", "p < 0.0001", partial$cover)
partial$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", partial$"A-horizon")



# save as csv
write.csv(full, "../OUT/TABLE.biomass.regression.full.csv")
write.csv(partial, "../OUT/TABLE.biomass.regression.partial.csv")


###############################
##### NOW JUST FOR MONOCULTURES
###############################
use.prairie <- scaled[which(scaled$Plot.category == "Monoculture"),]

response <- "biomass.all"


# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values", 
           "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold",
           "ndvi.avg.threshold.noflowers", "gndvi.avg.threshold.noflowers", "gdvi2.avg.threshold.noflowers")


t1 <- as.data.frame(pred1)
t1$cover.used <- "-"


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,4] <- "-"
  t1[i,5] <- "-"
  t1[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V7),]
t1$V7 <- as.numeric(t1$V7)
t1$deltaAIC <- t1$V7 - t1$V7[1]

colnames(t1) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# 1 predictor, cover
pred2 <- c("coverTotal", "dcover")


t2 <- as.data.frame(pred2)
t2$cover.used <- pred2

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,3] <- "-"
  t2[i,4] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,5] <- "-"
  t2[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V7),]
t2$V7 <- as.numeric(t2$V7)
t2$deltaAIC <- t2$V7 - t2$V7[1]

colnames(t2) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 1 predictor, ahor
pred3 <- c("AHOR_cm")

t3 <- as.data.frame(pred3)
t3$cover.used <- "-"

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred3)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,4]
  t3[i,3] <- "-"
  t3[i,4] <- "-"
  t3[i,5] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V7),]
t3$V7 <- as.numeric(t3$V7)
t3$deltaAIC <- t3$V7 - t3$V7[1]

colnames(t3) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t3$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold",
             "pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred2.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal",
             "dcover", "dcover", "dcover",
             "dcover", "dcover", "dcover")

t4 <- as.data.frame(pred2.1)
t4$cover.used <- pred2.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t4[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t4[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t4[i,5] <- "-"
  t4[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t4[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t4 <- t4[order(t4$V7),]
t4$V7 <- as.numeric(t4$V7)
t4$deltaAIC <- t4$V7 - t4$V7[1]

colnames(t4) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")



# 3 predictors

pred3.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold",
             "pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred3.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal",
             "dcover", "dcover", "dcover",
             "dcover", "dcover", "dcover")

pred3.3 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm")

t5 <- as.data.frame(pred3.1)
t5$cover.used <- pred3.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,4]
  coef3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,1]
  pval3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,4]
  t5[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t5[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t5[i,5] <- paste(format(round(coef3, 2), nsmall = 2), ", p = ", format(round(pval3, 4), nsmall = 4), sep = "")
  t5[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$r.squared, 3), nsmall = 3)
  t5[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t5 <- t5[order(t5$V7),]
t5$V7 <- as.numeric(t5$V7)
t5$deltaAIC <- t5$V7 - t5$V7[1]

colnames(t5) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# merge all tables
full <- rbind(t1, t2)
full <- rbind(full, t3)
full <- rbind(full, t4)
full <- rbind(full, t5)


# re-sort
full <- full[order(full$AIC),]
full$AIC <- as.numeric(full$AIC)
full$deltaAIC <- full$AIC - full$AIC[1]


# format a few things
full$`VI used` <- sub("pNDVIvalues", "NDVI", full$`VI used`)
full$`VI used` <- sub("pGNDVIvalues", "GNDVI", full$`VI used`)
full$`VI used` <- sub("pGDVI2values", "GDVI2", full$`VI used`)
full$`VI used` <- sub("gndvi.avg.threshold", "GNDVIt", full$`VI used`)
full$`VI used` <- sub("ndvi.avg.threshold", "NDVIt", full$`VI used`)
full$`VI used` <- sub("gdvi2.avg.threshold", "GDVI2t", full$`VI used`)
full$`VI used` <- sub("NDVIt.noflowers", "NDVIt-f", full$`VI used`)
full$`VI used` <- sub("GNDVIt.noflowers", "GNDVIt-f", full$`VI used`)
full$`VI used` <- sub("GDVI2t.noflowers", "GDVI2t-f", full$`VI used`)

full$`cover used` <- sub("coverTotal", "ground", full$`cover used`)
full$`cover used` <- sub("dcover", "drone", full$`cover used`)

full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
full$cover <- sub("p = 0.0000", "p < 0.0001", full$cover)
full$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", full$"A-horizon")

# make partial table for pub

# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values", 
           "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

t1 <- as.data.frame(pred1)
t1$cover.used <- "-"


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,4] <- "-"
  t1[i,5] <- "-"
  t1[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V7),]
t1$V7 <- as.numeric(t1$V7)
t1$deltaAIC <- t1$V7 - t1$V7[1]

colnames(t1) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# 1 predictor, cover
pred2 <- c("coverTotal", "dcover")


t2 <- as.data.frame(pred2)
t2$cover.used <- pred2

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,3] <- "-"
  t2[i,4] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,5] <- "-"
  t2[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V7),]
t2$V7 <- as.numeric(t2$V7)
t2$deltaAIC <- t2$V7 - t2$V7[1]

colnames(t2) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 1 predictor, ahor
pred3 <- c("AHOR_cm")

t3 <- as.data.frame(pred3)
t3$cover.used <- "-"

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred3)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,4]
  t3[i,3] <- "-"
  t3[i,4] <- "-"
  t3[i,5] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V7),]
t3$V7 <- as.numeric(t3$V7)
t3$deltaAIC <- t3$V7 - t3$V7[1]

colnames(t3) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t3$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred2.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal")

t4 <- as.data.frame(pred2.1)
t4$cover.used <- pred2.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t4[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t4[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t4[i,5] <- "-"
  t4[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t4[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t4 <- t4[order(t4$V7),]
t4$V7 <- as.numeric(t4$V7)
t4$deltaAIC <- t4$V7 - t4$V7[1]

colnames(t4) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")



# 3 predictors

pred3.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred3.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal")

pred3.3 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm")

t5 <- as.data.frame(pred3.1)
t5$cover.used <- pred3.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,4]
  coef3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,1]
  pval3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,4]
  t5[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t5[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t5[i,5] <- paste(format(round(coef3, 2), nsmall = 2), ", p = ", format(round(pval3, 4), nsmall = 4), sep = "")
  t5[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$r.squared, 3), nsmall = 3)
  t5[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t5 <- t5[order(t5$V7),]
t5$V7 <- as.numeric(t5$V7)
t5$deltaAIC <- t5$V7 - t5$V7[1]

colnames(t5) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# merge all tables
partial <- rbind(t1, t2)
partial <- rbind(partial, t3)
partial <- rbind(partial, t4)
partial <- rbind(partial, t5)


# re-sort
partial <- partial[order(partial$AIC),]
partial$AIC <- as.numeric(partial$AIC)
partial$deltaAIC <- partial$AIC - partial$AIC[1]


# format a few things
partial$`VI used` <- sub("pNDVIvalues", "NDVI", partial$`VI used`)
partial$`VI used` <- sub("pGNDVIvalues", "GNDVI", partial$`VI used`)
partial$`VI used` <- sub("pGDVI2values", "GDVI2", partial$`VI used`)
partial$`VI used` <- sub("gndvi.avg.threshold", "GNDVIt", partial$`VI used`)
partial$`VI used` <- sub("ndvi.avg.threshold", "NDVIt", partial$`VI used`)
partial$`VI used` <- sub("gdvi2.avg.threshold", "GDVI2t", partial$`VI used`)
partial$`VI used` <- sub("NDVIt.noflowers", "NDVIt-f", partial$`VI used`)
partial$`VI used` <- sub("GNDVIt.noflowers", "GNDVIt-f", partial$`VI used`)
partial$`VI used` <- sub("GDVI2t.noflowers", "GDVI2t-f", partial$`VI used`)

partial$`cover used` <- sub("coverTotal", "ground", partial$`cover used`)
partial$`cover used` <- sub("dcover", "drone", partial$`cover used`)

partial$VI <- sub("p = 0.0000", "p < 0.0001", partial$VI)
partial$cover <- sub("p = 0.0000", "p < 0.0001", partial$cover)
partial$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", partial$"A-horizon")



# save as csv
write.csv(full, "../OUT/TABLE.biomass.regression.full-mono.csv")
write.csv(partial, "../OUT/TABLE.biomass.regression.partial-mono.csv")




###############################
##### NOW JUST FOR TREATMENTS##
###############################
use.prairie <- scaled[which(scaled$TMT.use == 1),]

response <- "biomass.all"


# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values", 
           "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold",
           "ndvi.avg.threshold.noflowers", "gndvi.avg.threshold.noflowers", "gdvi2.avg.threshold.noflowers")


t1 <- as.data.frame(pred1)
t1$cover.used <- "-"


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,4] <- "-"
  t1[i,5] <- "-"
  t1[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V7),]
t1$V7 <- as.numeric(t1$V7)
t1$deltaAIC <- t1$V7 - t1$V7[1]

colnames(t1) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# 1 predictor, cover
pred2 <- c("coverTotal", "dcover")


t2 <- as.data.frame(pred2)
t2$cover.used <- pred2

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,3] <- "-"
  t2[i,4] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,5] <- "-"
  t2[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V7),]
t2$V7 <- as.numeric(t2$V7)
t2$deltaAIC <- t2$V7 - t2$V7[1]

colnames(t2) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 1 predictor, ahor
pred3 <- c("AHOR_cm")

t3 <- as.data.frame(pred3)
t3$cover.used <- "-"

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred3)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,4]
  t3[i,3] <- "-"
  t3[i,4] <- "-"
  t3[i,5] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V7),]
t3$V7 <- as.numeric(t3$V7)
t3$deltaAIC <- t3$V7 - t3$V7[1]

colnames(t3) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t3$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold",
             "pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred2.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal",
             "dcover", "dcover", "dcover",
             "dcover", "dcover", "dcover")

t4 <- as.data.frame(pred2.1)
t4$cover.used <- pred2.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t4[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t4[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t4[i,5] <- "-"
  t4[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t4[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t4 <- t4[order(t4$V7),]
t4$V7 <- as.numeric(t4$V7)
t4$deltaAIC <- t4$V7 - t4$V7[1]

colnames(t4) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")



# 3 predictors

pred3.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold",
             "pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred3.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal",
             "dcover", "dcover", "dcover",
             "dcover", "dcover", "dcover")

pred3.3 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm")

t5 <- as.data.frame(pred3.1)
t5$cover.used <- pred3.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,4]
  coef3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,1]
  pval3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,4]
  t5[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t5[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t5[i,5] <- paste(format(round(coef3, 2), nsmall = 2), ", p = ", format(round(pval3, 4), nsmall = 4), sep = "")
  t5[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$r.squared, 3), nsmall = 3)
  t5[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t5 <- t5[order(t5$V7),]
t5$V7 <- as.numeric(t5$V7)
t5$deltaAIC <- t5$V7 - t5$V7[1]

colnames(t5) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# merge all tables
full <- rbind(t1, t2)
full <- rbind(full, t3)
full <- rbind(full, t4)
full <- rbind(full, t5)


# re-sort
full <- full[order(full$AIC),]
full$AIC <- as.numeric(full$AIC)
full$deltaAIC <- full$AIC - full$AIC[1]


# format a few things
full$`VI used` <- sub("pNDVIvalues", "NDVI", full$`VI used`)
full$`VI used` <- sub("pGNDVIvalues", "GNDVI", full$`VI used`)
full$`VI used` <- sub("pGDVI2values", "GDVI2", full$`VI used`)
full$`VI used` <- sub("gndvi.avg.threshold", "GNDVIt", full$`VI used`)
full$`VI used` <- sub("ndvi.avg.threshold", "NDVIt", full$`VI used`)
full$`VI used` <- sub("gdvi2.avg.threshold", "GDVI2t", full$`VI used`)
full$`VI used` <- sub("NDVIt.noflowers", "NDVIt-f", full$`VI used`)
full$`VI used` <- sub("GNDVIt.noflowers", "GNDVIt-f", full$`VI used`)
full$`VI used` <- sub("GDVI2t.noflowers", "GDVI2t-f", full$`VI used`)

full$`cover used` <- sub("coverTotal", "ground", full$`cover used`)
full$`cover used` <- sub("dcover", "drone", full$`cover used`)

full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
full$cover <- sub("p = 0.0000", "p < 0.0001", full$cover)
full$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", full$"A-horizon")

# make partial table for pub

# 1 predictor, VI
pred1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values", 
           "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

t1 <- as.data.frame(pred1)
t1$cover.used <- "-"


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
  t1[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t1[i,4] <- "-"
  t1[i,5] <- "-"
  t1[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
  t1[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t1 <- t1[order(t1$V7),]
t1$V7 <- as.numeric(t1$V7)
t1$deltaAIC <- t1$V7 - t1$V7[1]

colnames(t1) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# 1 predictor, cover
pred2 <- c("coverTotal", "dcover")


t2 <- as.data.frame(pred2)
t2$cover.used <- pred2

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred2)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$coefficients[2,4]
  t2[i,3] <- "-"
  t2[i,4] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t2[i,5] <- "-"
  t2[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]]))$r.squared, 3), nsmall = 3)
  t2[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t2 <- t2[order(t2$V7),]
t2$V7 <- as.numeric(t2$V7)
t2$deltaAIC <- t2$V7 - t2$V7[1]

colnames(t2) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t2$`VI used` <- "-"

# 1 predictor, ahor
pred3 <- c("AHOR_cm")

t3 <- as.data.frame(pred3)
t3$cover.used <- "-"

i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(pred3)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$coefficients[2,4]
  t3[i,3] <- "-"
  t3[i,4] <- "-"
  t3[i,5] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t3[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]]))$r.squared, 3), nsmall = 3)
  t3[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t3 <- t3[order(t3$V7),]
t3$V7 <- as.numeric(t3$V7)
t3$deltaAIC <- t3$V7 - t3$V7[1]

colnames(t3) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")
t3$`VI used` <- "-"

# 2 predictors

pred2.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred2.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal")

t4 <- as.data.frame(pred2.1)
t4$cover.used <- pred2.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
  t4[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t4[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t4[i,5] <- "-"
  t4[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
  t4[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t4 <- t4[order(t4$V7),]
t4$V7 <- as.numeric(t4$V7)
t4$deltaAIC <- t4$V7 - t4$V7[1]

colnames(t4) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")



# 3 predictors

pred3.1 <- c("pNDVIvalues", "pGNDVIvalues", "pGDVI2values",
             "ndvi.avg.threshold", "gndvi.avg.threshold", "gdvi2.avg.threshold")

pred3.2 <- c("coverTotal", "coverTotal", "coverTotal",
             "coverTotal", "coverTotal", "coverTotal")

pred3.3 <- c("AHOR_cm", "AHOR_cm", "AHOR_cm",
             "AHOR_cm", "AHOR_cm", "AHOR_cm")

t5 <- as.data.frame(pred3.1)
t5$cover.used <- pred3.2

i <- 1
for (i in 1:length(pred2.1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[3,4]
  coef3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,1]
  pval3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$coefficients[4,4]
  t5[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
  t5[i,4] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
  t5[i,5] <- paste(format(round(coef3, 2), nsmall = 2), ", p = ", format(round(pval3, 4), nsmall = 4), sep = "")
  t5[i,6] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]]))$r.squared, 3), nsmall = 3)
  t5[i,7] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]])), 2), nsmall = 2)
  i <- i + 1
}

t5 <- t5[order(t5$V7),]
t5$V7 <- as.numeric(t5$V7)
t5$deltaAIC <- t5$V7 - t5$V7[1]

colnames(t5) <- c("VI used", "cover used", "VI", "cover", "A-horizon", "R2", "AIC", "deltaAIC")


# merge all tables
partial <- rbind(t1, t2)
partial <- rbind(partial, t3)
partial <- rbind(partial, t4)
partial <- rbind(partial, t5)


# re-sort
partial <- partial[order(partial$AIC),]
partial$AIC <- as.numeric(partial$AIC)
partial$deltaAIC <- partial$AIC - partial$AIC[1]


# format a few things
partial$`VI used` <- sub("pNDVIvalues", "NDVI", partial$`VI used`)
partial$`VI used` <- sub("pGNDVIvalues", "GNDVI", partial$`VI used`)
partial$`VI used` <- sub("pGDVI2values", "GDVI2", partial$`VI used`)
partial$`VI used` <- sub("gndvi.avg.threshold", "GNDVIt", partial$`VI used`)
partial$`VI used` <- sub("ndvi.avg.threshold", "NDVIt", partial$`VI used`)
partial$`VI used` <- sub("gdvi2.avg.threshold", "GDVI2t", partial$`VI used`)
partial$`VI used` <- sub("NDVIt.noflowers", "NDVIt-f", partial$`VI used`)
partial$`VI used` <- sub("GNDVIt.noflowers", "GNDVIt-f", partial$`VI used`)
partial$`VI used` <- sub("GDVI2t.noflowers", "GDVI2t-f", partial$`VI used`)

partial$`cover used` <- sub("coverTotal", "ground", partial$`cover used`)
partial$`cover used` <- sub("dcover", "drone", partial$`cover used`)

partial$VI <- sub("p = 0.0000", "p < 0.0001", partial$VI)
partial$cover <- sub("p = 0.0000", "p < 0.0001", partial$cover)
partial$"A-horizon" <- sub("p = 0.0000", "p < 0.0001", partial$"A-horizon")



# save as csv
write.csv(full, "../OUT/TABLE.biomass.regression.full-tmt.csv")
write.csv(partial, "../OUT/TABLE.biomass.regression.partial-tmt.csv")


# make scaled dataframe ----
scaled <- as.data.frame(prairie.bio$plot)
scaled$Plot.category <- prairie.bio$Plot.category
scaled$TMT.use <- prairie.bio$TMT.use
scaled$biomass.all <- scale(prairie.bio$biomass.all)
scaled$NDVI <- scale(prairie.bio$NDVI)
scaled$GNDVI <- scale(prairie.bio$GNDVI)
scaled$GDVI2 <- scale(prairie.bio$GDVI2)
scaled$NDVINW <- scale(prairie.bio$NDVINW)
scaled$GNDVINW <- scale(prairie.bio$GNDVINW)
scaled$GDVI2NW <- scale(prairie.bio$GDVI2NW)
scaled$NDVINWND <- scale(prairie.bio$NDVINWND)
scaled$GNDVINWND <- scale(prairie.bio$GNDVINWND)
scaled$GDVI2NWND <- scale(prairie.bio$GDVI2NWND)
scaled$NDVIND <- scale(prairie.bio$NDVIND)
scaled$GNDVIND <- scale(prairie.bio$GNDVIND)
scaled$GDVI2ND <- scale(prairie.bio$GDVI2ND)
scaled$coverTotal <- scale(prairie.bio$coverTotal)
scaled$dcover <- scale(prairie.bio$dcover)
scaled$AHOR_cm <- scale(prairie.bio$AHOR_cm)
scaled$VOL <- scale(prairie.bio$VOL)
scaled$DensVol <- prairie.bio$NDVI * prairie.bio$VOL


