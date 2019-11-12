# make regression table for cover
scaled <- as.data.frame(scale(prairie.bio[,81:123]))
scaled$Plot.category <- prairie.bio$Plot.category
scaled$TMT.use <- prairie.bio$TMT.use
scaled$biomass.all <- scale(prairie.bio$biomass.all)
scaled$coverTotal <- scale(prairie.bio$coverTotal)
scaled$dcover <- scale(prairie.bio$dcover)
scaled$AHOR_cm <- scale(prairie.bio$AHOR_cm)
scaled$VOL <- scale(prairie.bio$VOL)
scaled$DensVol <- scale(prairie.bio$NDVI * prairie.bio$VOL)
scaled$block <- prairie.bio$block
scaled$block <- as.factor(scaled$block)
scaled$flower <- prairie.bio$flower.presence

use.prairie <- scaled

groups <- list(scaled, 
               scaled[which(scaled$Plot.category == "Monoculture"),],
               scaled[which(scaled$Plot.category == "Treatment"),],
               scaled[which(scaled$flower == 0),],
               scaled[which(scaled$Plot.category == "Monoculture" &
                              scaled$flower == 0),])
namesL <- c("all", "mono", "tmt", "NFall", "NFmono")
coverType <- c("dcover")

### NORMAL REGRESSION ----
for (k in 1:length(coverType)) {
  response <- coverType[k]
  
  for (j in 1:length(namesL)) {
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
    write.csv(full, paste0("../OUT/TABLE.cover.regression.", namesL[j], ".", coverType[k],".csv"))
    
  }
  
}


## ADD RANDOM EFFECT ----

for (k in 1:length(coverType)) {
  response <- coverType[k]
  
  for (j in 1:length(namesL)) {
    use.prairie <- groups[[j]]
    
    
    # 1 predictor
    pred1 <- colnames(scaled)[c(1:7)]
    
    t1 <- as.data.frame(pred1)
    
    # make loop to fill in 1 predictor
    for (i in 1:length(pred1)) {
      outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred1[i]]] + (1|block), data = use.prairie)
      outm1 <- anova(outm)
      coef <- coef(outm)$`block`[1,2]
      pval <- outm1$`Pr(>F)`[1]
      t1[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
      t1[i,3] <- "-"
      t1[i,4] <- round(r.squaredGLMM(outm)[,2], 3)
      t1[i,5] <- format(round(AIC(outm), 2), nsmall = 3)
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
      outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred2[i]]] + (1|block), data = use.prairie)
      outm1 <- anova(outm)
      coef <- coef(outm)$`block`[1,2]
      pval <- outm1$`Pr(>F)`[1]
      t2[i,2] <- "-"
      t2[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
      t2[i,4] <- round(r.squaredGLMM(outm)[,2], 3)
      t2[i,5] <- format(round(AIC(outm), 2), nsmall = 3)
      
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
      outm <- lmer(use.prairie[[response]] ~ use.prairie[[pred2.1[i]]] + use.prairie[[pred2.2[i]]] + (1|block), data = use.prairie)
      outm1 <- anova(outm)
      coef <- coef(outm)$`block`[1,2]
      pval <- outm1$`Pr(>F)`[1]
      coef2 <- coef(outm)$`block`[1,3]
      pval2 <- outm1$`Pr(>F)`[2]
      t3[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4), sep = "")
      t3[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4), sep = "")
      t3[i,4] <- round(r.squaredGLMM(outm)[,2], 3)
      t3[i,5] <- format(round(AIC(outm), 2), nsmall = 3)
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
    write.csv(full, paste0("../OUT/TABLE.cover.mixedRegression.", namesL[j], ".", coverType[k],".csv"))
    
  }
  
}
