# flower correction
library(reshape2)
library(ggplot2)
library(ggpubr)
# make regression table

prairie.NF <- prairie.bio[which(prairie$flower.presence == 0),]

# NDVI, NDVI.CA, NDVI.CI, NDVI.ND, NDVI.ND.CA, NDVI.ND.CI
scaled <- as.data.frame(scale(prairie.NF[,81:123]))
scaled$Plot.category <- prairie.NF$Plot.category
scaled$TMT.use <- prairie.NF$TMT.use
scaled$biomass.all <- scale(prairie.NF$biomass.all)
scaled$coverTotal <- scale(prairie.NF$coverTotal)
scaled$dcover <- scale(prairie.NF$dcover)
scaled$AHOR_cm <- scale(prairie.NF$AHOR_cm)
scaled$VOL <- scale(prairie.NF$VOL)
scaled$DensVol <- scale(prairie.NF$NDVI * prairie.NF$VOL)

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
  
  write.csv(full, paste0("../OUT/TABLE.biomass.regression.noFlowers.", namesL[j], ".csv"))
  
  
}


# make regression table for cover
scaled <- as.data.frame(scale(prairie.NF[,81:123]))
scaled$Plot.category <- prairie.NF$Plot.category
scaled$TMT.use <- prairie.NF$TMT.use
scaled$biomass.all <- scale(prairie.NF$biomass.all)
scaled$coverTotal <- scale(prairie.NF$coverTotal)
scaled$dcover <- scale(prairie.NF$dcover)
scaled$AHOR_cm <- scale(prairie.NF$AHOR_cm)
scaled$VOL <- scale(prairie.NF$VOL)
scaled$DensVol <- scale(prairie.NF$NDVI * prairie.bio$VOL)


use.prairie <- scaled

groups <- list(scaled, 
               scaled[which(scaled$Plot.category == "Monoculture"),],
               scaled[which(scaled$Plot.category == "Treatment"),])
namesL <- c("all", "mono", "tmt")
coverType <- c("dcover", "coverTotal")


for (k in 1:2) {
  response <- coverType[k]
  
  for (j in 1:3) {
    use.prairie <- groups[[j]]
    
    
    # 1 predictor
    pred1 <- colnames(scaled)[c(1:7)]
    
    t1 <- as.data.frame(pred1)
    
    # make loop to fill in 1 predictor
    for (i in 1:length(pred1)) {
      coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,1]
      pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$coefficients[2,4]
      t1[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
      t1[i,3] <- "-"
      t1[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]]))$r.squared, 3), nsmall = 3)
      t1[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred1[i]]])), 2), nsmall = 2)
    }
    
    t1 <- t1[order(t1$V5),]
    t1$V5 <- as.numeric(t1$V5)
    t1$deltaAIC <- t1$V5 - t1$V5[1]
    
    colnames(t1) <- c("VI used", "VI", "volume", "R2", "AIC", "deltaAIC")
    
    
    # 1 predictor, vol
    pred2 <- c("VOL")
    
    t2 <- as.data.frame(pred2)
    
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
    
    colnames(t2) <- c("VI used", "VI", "volume", "R2", "AIC", "deltaAIC")
    t2$`VI used` <- "-"
    
    # 2 predictors
    pred2.1 <- colnames(scaled)[c(1:7)]
    
    pred2.2 <- rep(c("VOL"), times = 7)
    
    t3 <- as.data.frame(pred2.1)
    
    for (i in 1:length(pred2.1)) {
      coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,1]
      pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[2,4]
      coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,1]
      pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$coefficients[3,4]
      t3[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
      t3[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
      t3[i,4] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]]))$r.squared, 3), nsmall = 3)
      t3[i,5] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]])), 2), nsmall = 2)
    }
    
    t3 <- t3[order(t3$V5),]
    t3$V5 <- as.numeric(t3$V5)
    t3$deltaAIC <- t3$V5 - t3$V5[1]
    
    colnames(t3) <- c("VI used", "VI", "volume", "R2", "AIC", "deltaAIC")
    
    # merge all tables
    full <- rbind(t1, t2)
    full <- rbind(full, t3)
    
    
    # re-sort
    full <- full[order(full$AIC),]
    full$AIC <- as.numeric(full$AIC)
    full$deltaAIC <- full$AIC - full$AIC[1]
    
    
    # format a few things
    full$VI <- sub("p = 0.0000", "p < 0.0001", full$VI)
    full$"volume" <- sub("p = 0.0000", "p < 0.0001", full$"volume")
    
    assign(paste0(x = "full-", namesL[j], coverType[k]), value = full)
    
    # save as csv
    write.csv(full, paste0("../OUT/TABLE.cover.regression.noFlowers.", namesL[j], ".", coverType[k],".csv"))
    
  }
  
}



NF.all <- read.csv("../OUT/TABLE.biomass.regression.noFlowers.all.csv")
NF.mono <- read.csv("../OUT/TABLE.biomass.regression.noFlowers.mono.csv")
NF.tmt <- read.csv("../OUT/TABLE.biomass.regression.noFlowers.tmt.csv")

F.all <- read.csv("../OUT/TABLE.biomass.regression.full.all.csv")
F.mono <- read.csv("../OUT/TABLE.biomass.regression.full.mono.csv")
F.tmt <- read.csv("../OUT/TABLE.biomass.regression.full.tmt.csv")


monoP <- merge(F.mono, NF.mono, by = "X")
monoP <- monoP[,c(1, 9, 17)]
monoP$deltaAIC.x.scaled <- scale(monoP$deltaAIC.x)
monoP$deltaAIC.y.scaled <- scale(monoP$deltaAIC.y)
monoP <- monoP[,c(1, 4, 5)]
plotmonoP <- melt(monoP, id = "X")
bioMono <- ggplot(plotmonoP, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "deltaAIC (scaled)", 
       title = "AIC of biomass models of monoculture plots") +
  theme_classic() +
  scale_x_discrete(labels = c("all plots", "only plots without flowers"))

tmtP <- merge(F.tmt, NF.tmt, by = "X")
tmtP <- tmtP[,c(1, 9, 17)]
tmtP$deltaAIC.x.scaled <- scale(tmtP$deltaAIC.x)
tmtP$deltaAIC.y.scaled <- scale(tmtP$deltaAIC.y)
tmtP <- tmtP[,c(1, 4, 5)]
plottmtP <- melt(tmtP, id = "X")
bioTmt <- ggplot(plottmtP, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "deltaAIC (scaled)", 
       title = "AIC of biomass models of treatment plots") +
  theme_classic() +
  scale_x_discrete(labels = c("all plots", "only plots without flowers"))


cNF.all <- read.csv("../OUT/TABLE.cover.regression.noFlowers.all.dcover.csv")
cNF.mono <- read.csv("../OUT/TABLE.cover.regression.noFlowers.mono.dcover.csv")
cNF.tmt <- read.csv("../OUT/TABLE.cover.regression.noFlowers.tmt.dcover.csv")

cF.all <- read.csv("../OUT/TABLE.cover.regression.all.dcover.csv")
cF.mono <- read.csv("../OUT/TABLE.cover.regression.mono.dcover.csv")
cF.tmt <- read.csv("../OUT/TABLE.cover.regression.tmt.dcover.csv")


cmonoP <- merge(cF.mono, cNF.mono, by = "X")
cmonoP <- cmonoP[,c(1, 7, 13)]
cmonoP$deltaAIC.x.scaled <- scale(cmonoP$deltaAIC.x)
cmonoP$deltaAIC.y.scaled <- scale(cmonoP$deltaAIC.y)
cmonoP <- cmonoP[,c(1, 4, 5)]
plotcmonoP <- melt(cmonoP, id = "X")
coverMono <- ggplot(plotcmonoP, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "deltaAIC (scaled)", 
       title = "AIC of cover models of monoculture plots") +
  theme_classic() +
  scale_x_discrete(labels = c("all plots", "only plots without flowers"))

ctmtP <- merge(cF.tmt, cNF.tmt, by = "X")
ctmtP <- ctmtP[,c(1, 7, 13)]
ctmtP$deltaAIC.x.scaled <- scale(ctmtP$deltaAIC.x)
ctmtP$deltaAIC.y.scaled <- scale(ctmtP$deltaAIC.y)
ctmtP <- ctmtP[,c(1, 4, 5)]
plotctmtP <- melt(ctmtP, id = "X")
coverTmt <- ggplot(plotctmtP, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "deltaAIC (scaled)", 
       title = "AIC of cover models of treatment plots") +
  theme_classic() +
  scale_x_discrete(labels = c("all plots", "only plots without flowers"))

ggarrange(bioMono, bioTmt, coverMono, coverTmt,
          nrow = 2, ncol = 2,
          labels = c("A", "B", "C", "D"),
          align = "hv")


RmonoP <- merge(F.mono, NF.mono, by = "X")
RmonoP <- RmonoP[,c(1, 7, 15)]
plotRmonoP <- melt(RmonoP, id = "X")
RbioMono <- ggplot(plotRmonoP, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "R2", 
       title = "R2 of biomass models of monoculture plots") +
  theme_classic() +
  scale_x_discrete(labels = c("all plots", "only plots without flowers")) +
  ylim(0, 1)

RtmtP <- merge(F.tmt, NF.tmt, by = "X")
RtmtP <- RtmtP[,c(1, 7, 15)]
plotRtmtP <- melt(RtmtP, id = "X")
RbioTmt <- ggplot(plotRtmtP, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "R2", 
       title = "R2 of biomass models of treatment plots") +
  theme_classic() +
  scale_x_discrete(labels = c("all plots", "only plots without flowers")) +
  ylim(0, 1)


RcmonoP <- merge(cF.mono, cNF.mono, by = "X")
RcmonoP <- RcmonoP[,c(1, 5, 11)]
plotRcmonoP <- melt(RcmonoP, id = "X")
RcoverMono <- ggplot(plotRcmonoP, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "R2", 
       title = "R2 of cover models of monoculture plots") +
  theme_classic() +
  scale_x_discrete(labels = c("all plots", "only plots without flowers")) +
  ylim(0, 1)

RctmtP <- merge(cF.tmt, cNF.tmt, by = "X")
RctmtP <- RctmtP[,c(1, 5, 11)]
plotRctmtP <- melt(RctmtP, id = "X")
RcoverTmt <- ggplot(plotRctmtP, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "R2", 
       title = "R2 of cover models of treatment plots") +
  theme_classic() +
  scale_x_discrete(labels = c("all plots", "only plots without flowers")) +
  ylim(0, 1)


ggarrange(RbioMono, RbioTmt, RcoverMono, RcoverTmt,
          nrow = 2, ncol = 2,
          labels = c("A", "B", "C", "D"),
          align = "hv")


ggarrange(bioMono, bioTmt, RbioMono, RbioTmt,
          nrow = 2, ncol = 2,
          labels = c("A", "B", "C", "D"),
          align = "hv")

ggarrange(coverMono, coverTmt, RcoverMono, RcoverTmt,
          nrow = 2, ncol = 2,
          labels = c("A", "B", "C", "D"),
          align = "hv")
