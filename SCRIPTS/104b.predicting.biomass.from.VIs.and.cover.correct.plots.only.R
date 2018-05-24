# how do VIs predict biomass and cover
# REDONE USING ONLY THE RIGHT PLOTS

all.prairie <- read.csv("DATA/all.prairie.with.VI.values.csv")
use.prairie <- all.prairie[which(all.prairie$Plot.category == "Monoculture" | 
                                   all.prairie$TMT.use == 1),]

# make dataframe

response.c <- c(rep("biomass", times = 21))
predictor1.c <- c("NDVI", "GDVI2", "GNDVI", 
                  "NDVIt", "GDVI2t", "GNDVIt",
                  "NDVIt nf", "GDVI2t nf", "GNDVIt nf",
                  "NDVI", "GDVI2", "GNDVI",
                  "NDVIt", "GDVI2t", "GNDVIt",
                  "NDVI", "GDVI2", "GNDVI",
                  "NDVIt", "GDVI2t", "GNDVIt")
predictor2.c <- c("", "", "",
                  "", "", "",
                  "", "", "",
                  "cover", "cover", "cover",
                  "cover", "cover", "cover",
                  "cover", "cover", "cover",
                  "cover", "cover", "cover")
predictor3.c <- c("", "", "",
                  "", "", "",
                  "", "", "",
                  "", "", "",
                  "", "", "",
                  "Ahor", "Ahor", "Ahor",
                  "Ahor", "Ahor", "Ahor")
R2.c <- c("", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "")
AIC.c <- c("", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "")

# combine into dataframe
comb <- as.data.frame(response.c)
comb <- cbind(comb, predictor1.c)
comb <- cbind(comb, predictor2.c)
comb <- cbind(comb, predictor3.c)
comb <- cbind(comb, R2.c)
comb <- cbind(comb, AIC.c)
comb$predictor1.c <- as.character(comb$predictor1.c)
comb$predictor2.c <- as.character(comb$predictor2.c)
comb$predictor3.c <- as.character(comb$predictor3.c)
comb$R2.c <- as.character(comb$R2.c)
comb$AIC.c <- as.character(comb$AIC.c)

colnames(comb) <- c("response", "predictor 1: VI", "predictor 2: cover", "predictor 3: A-horizon", "R2", "AIC")


## fill in values
coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues))$coefficients[2,4]
comb[1,2] <- paste("NDVI, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[1,3] <- NA
comb[1,4] <- NA
comb[1,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues))$r.squared, 3), nsmall = 3)
comb[1,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues)), 2), nsmall = 2)

coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values))$coefficients[2,4]
comb[2,2] <- paste("GDVI2, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[2,3] <- NA
comb[2,4] <- NA
comb[2,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values))$r.squared, 3), nsmall = 3)
comb[2,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values)), 2), nsmall = 2)

coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues))$coefficients[2,4]
comb[3,2] <- paste("GNDVI, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[3,3] <- NA
comb[3,4] <- NA
comb[3,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues))$r.squared, 3), nsmall = 3)
comb[3,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues)), 2), nsmall = 2)


## threshold
coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold))$coefficients[2,4]
comb[4,2] <- paste("NDVIt, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[4,3] <- NA
comb[4,4] <- NA
comb[4,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold))$r.squared, 3), nsmall = 3)
comb[4,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold)), 2), nsmall = 2)

coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold))$coefficients[2,4]
comb[5,2] <- paste("GDVI2t, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[5,3] <- NA
comb[5,4] <- NA
comb[5,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold))$r.squared, 3), nsmall = 3)
comb[5,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold)), 2), nsmall = 2)

coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold))$coefficients[2,4]
comb[6,2] <- paste("GNDVIt, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[6,3] <- NA
comb[6,4] <- NA
comb[6,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold))$r.squared, 3), nsmall = 3)
comb[6,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold)), 2), nsmall = 2)

## threshold, no flowers
coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold.noflowers))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold.noflowers))$coefficients[2,4]
comb[7,2] <- paste("NDVIt nf, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[7,3] <- NA
comb[7,4] <- NA
comb[7,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold.noflowers))$r.squared, 3), nsmall = 3)
comb[7,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold.noflowers)), 2), nsmall = 2)

coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold.noflowers))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold.noflowers))$coefficients[2,4]
comb[8,2] <- paste("GDVI2t nf, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[8,3] <- NA
comb[8,4] <- NA
comb[8,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold.noflowers))$r.squared, 3), nsmall = 3)
comb[8,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold.noflowers)), 2), nsmall = 2)

coef <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold.noflowers))$coefficients[2,1]
pval <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold.noflowers))$coefficients[2,4]
comb[9,2] <- paste("GNDVIt nf, ", format(round(coef, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[9,3] <- NA
comb[9,4] <- NA
comb[9,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold.noflowers))$r.squared, 3), nsmall = 3)
comb[9,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold.noflowers)), 2), nsmall = 2)

## + cover
coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal))$coefficients[3,4]
comb[10,2] <- paste("NDVI, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[10,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[10,4] <- NA
comb[10,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal))$r.squared, 3), nsmall = 3)
comb[10,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal))$coefficients[3,4]
comb[11,2] <- paste("GDVI2, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[11,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[11,4] <- NA
comb[11,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal))$r.squared, 3), nsmall = 3)
comb[11,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal))$coefficients[3,4]
comb[12,2] <- paste("GNDVI, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[12,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[12,4] <- NA
comb[12,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal))$r.squared, 3), nsmall = 3)
comb[12,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal)), 2), nsmall = 2)


## t + cover
coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal))$coefficients[3,4]
comb[13,2] <- paste("NDVIt, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[13,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[13,4] <- NA
comb[13,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal))$r.squared, 3), nsmall = 3)
comb[13,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal))$coefficients[3,4]
comb[14,2] <- paste("GDVI2t, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[14,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[14,4] <- NA
comb[14,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal))$r.squared, 3), nsmall = 3)
comb[14,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal))$coefficients[3,4]
comb[15,2] <- paste("GNDVIt, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[15,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[15,4] <- NA
comb[15,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal))$r.squared, 3), nsmall = 3)
comb[15,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal)), 2), nsmall = 2)

## + cover + Ahor
coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,4]
coef3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,1]
pval3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,4]
comb[16,2] <- paste("NDVI, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[16,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[16,4] <- paste("Ahor, ", format(round(coef3, 2), nsmall = 2), ", ", format(round(pval3, 4), nsmall = 4))
comb[16,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$r.squared, 3), nsmall = 3)
comb[16,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,4]
coef3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,1]
pval3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,4]
comb[17,2] <- paste("GDVI2, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[17,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[17,4] <- paste("Ahor, ", format(round(coef3, 2), nsmall = 2), ", ", format(round(pval3, 4), nsmall = 4))
comb[17,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal + use.prairie$AHOR_cm))$r.squared, 3), nsmall = 3)
comb[17,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pGDVI2values + use.prairie$coverTotal + use.prairie$AHOR_cm)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,4]
coef3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,1]
pval3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,4]
comb[18,2] <- paste("GNDVI, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[18,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[18,4] <- paste("Ahor, ", format(round(coef3, 2), nsmall = 2), ", ", format(round(pval3, 4), nsmall = 4))
comb[18,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm))$r.squared, 3), nsmall = 3)
comb[18,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$pGNDVIvalues + use.prairie$coverTotal + use.prairie$AHOR_cm)), 2), nsmall = 2)


## t + cover + Ahor
coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,4]
coef3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,1]
pval3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,4]
comb[19,2] <- paste("NDVIt, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[19,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[19,4] <- paste("Ahor, ", format(round(coef3, 2), nsmall = 2), ", ", format(round(pval3, 4), nsmall = 4))
comb[19,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal))$r.squared, 3), nsmall = 3)
comb[19,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$ndvi.threshold + use.prairie$coverTotal)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,4]
coef3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,1]
pval3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,4]
comb[20,2] <- paste("GDVI2t, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval1, 4), nsmall = 4))
comb[20,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval2, 4), nsmall = 4))
comb[20,4] <- paste("Ahor, ", format(round(coef3, 2), nsmall = 2), ", ", format(round(pval3, 4), nsmall = 4))
comb[20,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$r.squared, 3), nsmall = 3)
comb[20,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$gdvi2.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm)), 2), nsmall = 2)

coef1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,1]
pval1 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[2,4]
coef2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,1]
pval2 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[3,4]
coef3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,1]
pval3 <- summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$coefficients[4,4]
comb[21,2] <- paste("GNDVIt, ", format(round(coef1, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[21,3] <- paste("cover, ", format(round(coef2, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[21,4] <- paste("Ahor, ", format(round(coef3, 2), nsmall = 2), ", ", format(round(pval, 4), nsmall = 4))
comb[21,5] <- format(round(summary(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm))$r.squared, 3), nsmall = 3)
comb[21,6] <- format(round(AIC(lm(use.prairie$biomass.all ~ use.prairie$gndvi.threshold + use.prairie$coverTotal + use.prairie$AHOR_cm)), 2), nsmall = 2)


