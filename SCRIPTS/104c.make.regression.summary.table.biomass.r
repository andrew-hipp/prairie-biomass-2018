# make tables with regression summaries

# make table
# make sure predictor matches column names -- use scaled for all
# after table is made
#   combine tables with diff numbers of predictors
#   sort by AIC
#   add delta AIC column
#   replace p = 0.0000 with p < 0.0001
#   rename VIs if necessary

response <- "biomass"
predictor1 <- c("NDVI", "GDVI2", "GNDVI", 
                "NDVIt", "GDVI2t", "GNDVIt",
                "NDVIt.nf", "GDVI2t.nf", "GNDVIt.nf")
predictor2.1 <- c("NDVI", "GDVI2", "GNDVI",
                  "NDVIt", "GDVI2t", "GNDVIt")
predictor2.2 <- c("cover", "cover", "cover",
                  "cover", "cover", "cover")
predictor3.1 <- c("NDVI", "GDVI2", "GNDVI",
                  "NDVIt", "GDVI2t", "GNDVIt")
predictor3.2 <- c("cover", "cover", "cover",
                  "cover", "cover", "cover")
predictor3.3 <- c("Ahor", "Ahor", "Ahor",
                  "Ahor", "Ahor", "Ahor")

# make table
VI.predictor.c <- c("NDVI", "GDVI2", "GNDVI", 
                  "NDVIt", "GDVI2t", "GNDVIt",
                  "NDVIt.nf", "GDVI2t.nf", "GNDVIt.nf", 
                  "NDVI", "GDVI2", "GNDVI",
                  "NDVIt", "GDVI2t", "GNDVIt",
                  "NDVI", "GDVI2", "GNDVI",
                  "NDVIt", "GDVI2t", "GNDVIt",
                  NA, NA)
predictor1.c <- c("NDVI", "GDVI2", "GNDVI", 
                  "NDVIt", "GDVI2t", "GNDVIt",
                  "NDVIt.nf", "GDVI2t.nf", "GNDVIt.nf",
                  "NDVI", "GDVI2", "GNDVI",
                  "NDVIt", "GDVI2t", "GNDVIt",
                  "NDVI", "GDVI2", "GNDVI",
                  "NDVIt", "GDVI2t", "GNDVIt",
                  "cover",
                  "ahor")
predictor2.c <- c("", "", "",
                  "", "", "",
                  "", "", "",
                  "cover", "cover", "cover",
                  "cover", "cover", "cover",
                  "cover", "cover", "cover",
                  "cover", "cover", "cover",
                  "", "")
predictor3.c <- c("", "", "",
                  "", "", "",
                  "", "", "",
                  "", "", "",
                  "", "", "",
                  "ahor", "ahor", "ahor",
                  "ahor", "ahor", "ahor",
                  "", "")
R2.c <- c("", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "", "",
          "", "")
AIC.c <- c("", "", "",
           "", "", "",
           "", "", "",
           "", "", "",
           "", "", "",
           "", "", "",
           "", "", "",
           "", "")

# combine into dataframe
comb <- as.data.frame(VI.predictor.c)
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

colnames(comb) <- c("VI used", "VI", "cover", "A-horizon", "R2", "AIC")

# make dataframe with the right columns
col.prairie <- as.data.frame(all.prairie$plot)
col.prairie$biomass <- scale(all.prairie$biomass.all)
col.prairie$category <- all.prairie$Plot.category
col.prairie$use <- all.prairie$TMT.use
col.prairie$NDVI <- scale(all.prairie$pNDVIvalues)
col.prairie$GDVI2 <- scale(all.prairie$pGDVI2values)
col.prairie$GNDVI <- scale(all.prairie$pGNDVIvalues)
col.prairie$NDVIt <- scale(all.prairie$ndvi.threshold)
col.prairie$GDVI2t <- scale(all.prairie$gdvi2.threshold)
col.prairie$GNDVIt <- scale(all.prairie$gndvi.threshold)
col.prairie$NDVIt.nf <- scale(all.prairie$ndvi.threshold.noflowers)
col.prairie$GDVI2t.nf <- scale(all.prairie$gdvi2.threshold.noflowers)
col.prairie$GNDVIt.nf <- scale(all.prairie$gndvi.threshold.noflowers)
col.prairie$ahor <- scale(all.prairie$AHOR_cm)
col.prairie$cover <- scale(all.prairie$coverTotal)


use.prairie <- col.prairie[which(col.prairie$category == "Monoculture" | 
                                   col.prairie$use == 1),]

use.prairie <- col.prairie[which(col.prairie$Plot.category == "Monoculture"),]

use.prairie <- col.prairie[which(col.prairie$TMT.use == 1),]


i <- 1
# make loop to fill in 1 predictor
for (i in 1:length(predictor1)) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1[i]]]))$coefficients[2,4]
  comb[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
  comb[i,3] <- NA
  comb[i,4] <- NA
  comb[i,5] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1[i]]]))$r.squared, 3), nsmall = 3)
  comb[i,6] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[predictor1[i]]])), 2), nsmall = 2)
  i <- i + 1
}

# make loop to fill in 2 predictors
for (i in (length(predictor1)+1):(length(predictor1) + length(predictor2.1))) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]]))$coefficients[3,4]
  comb[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
  comb[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4))
  comb[i,4] <- NA
  comb[i,5] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]]))$r.squared, 3), nsmall = 3)
  comb[i,6] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]])), 2), nsmall = 2)
  i <- i + 1
}

# make loop to fill in 3 predictors
for (i in (length(predictor1) + length(predictor2.1) + 1):(length(predictor1) + length(predictor2.1) + length(predictor3.1))) {
  coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]] + use.prairie[[predictor3.c[i]]]))$coefficients[2,1]
  pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]] + use.prairie[[predictor3.c[i]]]))$coefficients[2,4]
  coef2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]] + use.prairie[[predictor3.c[i]]]))$coefficients[3,1]
  pval2 <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]] + use.prairie[[predictor3.c[i]]]))$coefficients[3,4]
  coef3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]] + use.prairie[[predictor3.c[i]]]))$coefficients[4,1]
  pval3 <- summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]] + use.prairie[[predictor3.c[i]]]))$coefficients[4,4]
  comb[i,2] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
  comb[i,3] <- paste(format(round(coef2, 2), nsmall = 2), ", p = ", format(round(pval2, 4), nsmall = 4))
  comb[i,4] <- paste(format(round(coef3, 2), nsmall = 2), ", p = ", format(round(pval3, 4), nsmall = 4))
  comb[i,5] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]] + use.prairie[[predictor3.c[i]]]))$r.squared, 3), nsmall = 3)
  comb[i,6] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[VI.predictor.c[i]]] + use.prairie[[predictor2.c[i]]] + use.prairie[[predictor3.c[i]]])), 2), nsmall = 2)
  i <- i + 1
}


# fill in just cover
coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1.c[i]]]))$coefficients[2,1]
pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1.c[i]]]))$coefficients[2,4]
comb[i,2] <- NA
comb[i,3] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
comb[i,4] <- NA
comb[i,5] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1.c[i]]]))$r.squared, 3), nsmall = 3)
comb[i,6] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[predictor1.c[i]]])), 2), nsmall = 2)
i <- i + 1

# fill in just ahor
coef <- summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1.c[i]]]))$coefficients[2,1]
pval <- summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1.c[i]]]))$coefficients[2,4]
comb[i,2] <- NA
comb[i,3] <- NA
comb[i,4] <- paste(format(round(coef, 2), nsmall = 2), ", p = ", format(round(pval, 4), nsmall = 4))
comb[i,5] <- format(round(summary(lm(use.prairie[[response]] ~ use.prairie[[predictor1.c[i]]]))$r.squared, 3), nsmall = 3)
comb[i,6] <- format(round(AIC(lm(use.prairie[[response]] ~ use.prairie[[predictor1.c[i]]])), 2), nsmall = 2)
i <- i + 1


comb <- comb[order(comb$AIC),]
