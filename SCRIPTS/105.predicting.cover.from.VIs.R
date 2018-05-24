# predicting cover from VIs

all.prairie <- read.csv("DATA/all.prairie.with.VI.values.csv")

all.prairie$ndvi.scaled <- scale(all.prairie$pNDVIvalues)
all.prairie$gdvi2.scaled <- scale(all.prairie$pGDVI2values)
all.prairie$gndvi.scaled <- scale(all.prairie$pGNDVIvalues)

all.prairie$ndvi.threshold.scaled <- scale(all.prairie$ndvi.threshold)
all.prairie$gdvi2.threshold.scaled <- scale(all.prairie$gdvi2.threshold)
all.prairie$gndvi.threshold.scaled <- scale(all.prairie$gndvi.threshold)

all.prairie$ndvi.threshold.noflowers.scaled <- scale(all.prairie$ndvi.threshold.noflowers)
all.prairie$gdvi2.threshold.no.flowers.scaled <- scale(all.prairie$gdvi2.threshold.noflowers)
all.prairie$gndvi.threshold.no.flowers.scaled <- scale(all.prairie$gndvi.threshold.noflowers)

all.prairie$cover.scaled <- scale(all.prairie$coverTotal)
all.prairie$AHOR_cm.scaled <- scale(all.prairie$AHOR_cm)

use.prairie <- all.prairie[which(all.prairie$Plot.category == "Monoculture" | 
                                   all.prairie$TMT.use == 1),]

use.prairie <- all.prairie[which(all.prairie$Plot.category == "Monoculture"),]

use.prairie <- all.prairie[which(all.prairie$TMT.use == 1),]


# make dataframe
response.c <- c(rep("cover", times = 6))
predictor1.c <- c("NDVI", "GDVI2", "GNDVI",
                  "NDVI", "GDVI2", "GNDVI")
predictor2.c <- c("", "", "",
                  "Ahor", "Ahor", "Ahor")
R2.c <- c("", "", "",
          "", "", "")
AIC.c <- c("", "", "",
           "", "", "")

# combine into dataframe
comb <- as.data.frame(response.c)
comb <- cbind(comb, predictor1.c)
comb <- cbind(comb, predictor2.c)
comb <- cbind(comb, R2.c)
comb <- cbind(comb, AIC.c)
comb$predictor1.c <- as.character(comb$predictor1.c)
comb$predictor2.c <- as.character(comb$predictor2.c)
comb$R2.c <- as.character(comb$R2.c)
comb$AIC.c <- as.character(comb$AIC.c)

colnames(comb) <- c("response", "predictor 1: VI", "predictor 2: A-horizon", "R2", "AIC")


## fill in values
coef <- summary(lm(use.prairie$coverTotal ~ use.prairie$pNDVIvalues))$coefficients[2,1]
pval <- summary(lm(use.prairie$coverTotal ~ use.prairie$pNDVIvalues))$coefficients[2,4]
comb[1,2] <- paste("NDVI, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[1,3] <- NA
comb[1,4] <- format(round(summary(lm(use.prairie$coverTotal ~ use.prairie$pNDVIvalues))$r.squared, 3), nsmall = 3)
comb[1,5] <- format(round(AIC(lm(use.prairie$coverTotal ~ use.prairie$pNDVIvalues)), 2), nsmall = 2)

coef <- summary(lm(use.prairie$coverTotal ~ use.prairie$pGDVI2values))$coefficients[2,1]
pval <- summary(lm(use.prairie$coverTotal ~ use.prairie$pGDVI2values))$coefficients[2,4]
comb[2,2] <- paste("GDVI2, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[2,3] <- NA
comb[2,4] <- format(round(summary(lm(use.prairie$coverTotal ~ use.prairie$pGDVI2values))$r.squared, 3), nsmall = 3)
comb[2,5] <- format(round(AIC(lm(use.prairie$coverTotal ~ use.prairie$pGDVI2values)), 2), nsmall = 2)

coef <- summary(lm(use.prairie$coverTotal ~ use.prairie$pGNDVIvalues))$coefficients[2,1]
pval <- summary(lm(use.prairie$coverTotal ~ use.prairie$pGNDVIvalues))$coefficients[2,4]
comb[3,2] <- paste("GNDVI, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[3,3] <- NA
comb[3,4] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues))$r.squared, 3), nsmall = 3)
comb[3,5] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues)), 2), nsmall = 2)


## + cover
coef1 <- summary(lm(use.prairie$coverTotal ~ use.prairie$ndvi.scaled + use.prairie$AHOR_cm.scaled))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$coverTotal ~ use.prairie$ndvi.scaled + use.prairie$AHOR_cm.scaled))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$coverTotal ~ use.prairie$ndvi.scaled + use.prairie$AHOR_cm.scaled))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$coverTotal ~ use.prairie$ndvi.scaled + use.prairie$AHOR_cm.scaled))$coefficients[3,4]
comb[4,2] <- paste("NDVI, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[4,3] <- paste("Ahor, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[4,4] <- format(round(summary(lm(use.prairie$coverTotal ~ use.prairie$ndvi.scaled + use.prairie$AHOR_cm.scaled))$r.squared, 3), nsmall = 3)
comb[4,5] <- format(round(AIC(lm(use.prairie$coverTotal ~ use.prairie$ndvi.scaled + use.prairie$AHOR_cm.scaled)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$coverTotal ~ use.prairie$gdvi2.scaled + use.prairie$AHOR_cm.scaled))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$coverTotal ~ use.prairie$gdvi2.scaled + use.prairie$AHOR_cm.scaled))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$coverTotal ~ use.prairie$gdvi2.scaled + use.prairie$AHOR_cm.scaled))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$coverTotal ~ use.prairie$gdvi2.scaled + use.prairie$AHOR_cm.scaled))$coefficients[3,4]
comb[5,2] <- paste("GDVI2, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[5,3] <- paste("Ahor, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[5,4] <- format(round(summary(lm(use.prairie$coverTotal ~ use.prairie$gdvi2.scaled + use.prairie$AHOR_cm.scaled))$r.squared, 3), nsmall = 3)
comb[5,5] <- format(round(AIC(lm(use.prairie$coverTotal ~ use.prairie$gdvi2.scaled + use.prairie$AHOR_cm.scaled)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$coverTotal ~ use.prairie$gndvi.scaled + use.prairie$AHOR_cm.scaled))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$coverTotal ~ use.prairie$gndvi.scaled + use.prairie$AHOR_cm.scaled))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$coverTotal ~ use.prairie$gndvi.scaled + use.prairie$AHOR_cm.scaled))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$coverTotal ~ use.prairie$gndvi.scaled + use.prairie$AHOR_cm.scaled))$coefficients[3,4]
comb[6,2] <- paste("GNDVI, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[6,3] <- paste("Ahor, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[6,4] <- format(round(summary(lm(use.prairie$coverTotal ~ use.prairie$gndvi.scaled + use.prairie$AHOR_cm.scaled))$r.squared, 3), nsmall = 3)
comb[6,5] <- format(round(AIC(lm(use.prairie$coverTotal ~ use.prairie$gndvi.scaled + use.prairie$AHOR_cm.scaled)), 2), nsmall = 2)


write.csv(comb, "OUT/regression.summary.cover.treatments.csv")
