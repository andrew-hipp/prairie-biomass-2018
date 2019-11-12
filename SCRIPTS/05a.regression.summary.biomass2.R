library(lme4)
#library(nlme)
library(lmerTest)
library(MuMIn)
#library(caret)

# make regression table

# scale everything
scaled <- as.data.frame(scale(prairie.bio[,81:123]))
scaled$Plot.category <- prairie.bio$Plot.category
scaled$TMT.use <- prairie.bio$TMT.use
scaled$biomass.all <- as.numeric(scale(prairie.bio$biomass.all))
scaled$coverTotal <- as.numeric(scale(prairie.bio$coverTotal))
scaled$dcover <- as.numeric(scale(prairie.bio$dcover))
scaled$AHOR_cm <- as.numeric(scale(prairie.bio$AHOR_cm))
scaled$VOL <- as.numeric(scale(prairie.bio$VOL))
scaled$DensVol <- as.numeric(scale(prairie.bio$NDVI * prairie.bio$VOL))
scaled$block <- prairie.bio$block
scaled$block <- as.factor(scaled$block)

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
    outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred1[i]]] + (1|block), data = use.prairie)
    outm1 <- anova(outm)
    coef <- coef(outm)$`block`[1,2]
    pval <- outm1$`Pr(>F)`
    t1[i, 3] <- paste0(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
    t1[i, 4] <- "-"
    t1[i, 5] <- "-"
    t1[i, 6] <- round(r.squaredGLMM(outm)[,2], 3)
    t1[i, 7] <- format(round(AIC(outm), 2), nsmall = 3)
  }
  
  
  t1 <- t1[order(t1$V7),]
  t1$V7 <- as.numeric(t1$V7)
  t1$deltaAIC <- t1$V7 - t1$V7[1]
  
  colnames(t1) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  

  # 1 predictor, cover ----
  pred2 <- c("coverTotal", "dcover")
  
  
  t2 <- as.data.frame(pred2)
  t2$cover.used <- pred2
  
  # make loop to fill in 1 predictor
  for (i in 1:length(pred2)) {
    outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred2[i]]] + (1|block), data = use.prairie)
    outm1 <- anova(outm)
    coef <- coef(outm)$`block`[1,2]
    pval <- outm1$`Pr(>F)`
    t2[i, 3] <- "-"
    t2[i, 4] <- paste0(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
    t2[i, 5] <- "-"
    t2[i, 6] <- round(r.squaredGLMM(outm)[,2], 3)
    t2[i, 7] <- format(round(AIC(outm), 2), nsmall = 3)
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
    outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred2[i]]] + (1|block), data = use.prairie)
    outm1 <- anova(outm)
    coef <- coef(outm)$`block`[1,2]
    pval <- outm1$`Pr(>F)`
    t3[i, 3] <- "-"
    t3[i, 4] <- "-"
    t3[i, 5] <- paste0(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
    t3[i, 6] <- round(r.squaredGLMM(outm)[,2], 3)
    t3[i, 7] <- format(round(AIC(outm), 2), nsmall = 3)
  }
  
  t3 <- t3[order(t3$V7),]
  t3$V7 <- as.numeric(t3$V7)
  t3$deltaAIC <- t3$V7 - t3$V7[1]
  
  colnames(t3) <- c("VI used", "cover used", "VI", "cover", "volume", "R2", "AIC", "deltaAIC")
  t3$`VI used` <- "-"
  
  #volPred <- t3
  
  # 2 predictors, VI and cover ----
  
  pred2.1 <- rep(colnames(scaled)[c(1:7, 22:28)], times = 2)
  
  pred2.2 <- rep(c("coverTotal", "dcover"), each = 14)
  
  t4 <- as.data.frame(pred2.1)
  t4$cover.used <- pred2.2
  
  for (i in 1:length(pred2.1)) {
    outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]] + (1|block), data = use.prairie)
    outm1 <- anova(outm)
    coef <- coef(outm)$`block`[1,2]
    pval <- outm1$`Pr(>F)`[1]
    coef2 <- coef(outm)$`block`[1,3]
    pval2 <- outm1$`Pr(>F)`[2]
    t4[i, 3] <- paste0(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
    t4[i, 4] <- paste0(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4))
    t4[i, 5] <- "-"
    t4[i, 6] <- round(r.squaredGLMM(outm)[,2], 3)
    t4[i, 7] <- format(round(AIC(outm), 2), nsmall = 3)
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
    outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]] + (1|block), data = use.prairie)
    outm1 <- anova(outm)
    coef <- coef(outm)$`block`[1,2]
    pval <- outm1$`Pr(>F)`[1]
    coef2 <- coef(outm)$`block`[1,3]
    pval2 <- outm1$`Pr(>F)`[2]
    t41[i, 3] <- paste0(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
    t41[i, 4] <- "-"
    t41[i, 5] <- paste0(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4))
    t41[i, 6] <- round(r.squaredGLMM(outm)[,2], 3)
    t41[i, 7] <- format(round(AIC(outm), 2), nsmall = 3)
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
    outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]] + (1|block), data = use.prairie)
    outm1 <- anova(outm)
    coef <- coef(outm)$`block`[1,2]
    pval <- outm1$`Pr(>F)`[1]
    coef2 <- coef(outm)$`block`[1,3]
    pval2 <- outm1$`Pr(>F)`[2]
    t42[i, 3] <- "-"
    t42[i, 4] <- paste0(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
    t42[i, 5] <- paste0(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4))
    t42[i, 6] <- round(r.squaredGLMM(outm)[,2], 3)
    t42[i, 7] <- format(round(AIC(outm), 2), nsmall = 3)
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
    outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred3.1[i]]] + use.prairie[[pred3.2[i]]] + use.prairie[[pred3.3[i]]] + (1|block), data = use.prairie)
    outm1 <- anova(outm)
    coef <- coef(outm)$`block`[1,2]
    pval <- outm1$`Pr(>F)`[1]
    coef2 <- coef(outm)$`block`[1,3]
    pval2 <- outm1$`Pr(>F)`[2]
    coef3 <- coef(outm)$`block`[1,4]
    pval3 <- outm1$`Pr(>F)`[3]
    t5[i, 3] <- paste0(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
    t5[i, 4] <- paste0(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4))
    t5[i, 5] <- paste0(format(round(coef3, 2), nsmall = 2), ", p = ", format(round(pval3, 4), nsmall = 4))
    t5[i, 6] <- round(r.squaredGLMM(outm)[,2], 3)
    t5[i, 7] <- format(round(AIC(outm), 2), nsmall = 3)
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
  
  full$`cover used`[which(full$`cover used` == "drone")] <- "total"
  full$`cover used`[which(full$`cover used` == "ground")] <- "planted"
  
  assign(x = paste0("full-", namesL[j]), value = full)
  
  write.csv(full, paste0("../OUT/TABLE.biomass.mixedRegression.full.", namesL[j], ".csv"))
  
  
}



