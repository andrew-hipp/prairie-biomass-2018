## assumes you've already compiled data (script 01)

mono.plots = all.prairie[!is.na(all.prairie$monoSp), ]

mono.highplots <- monoplots[monoplots$pNDVIvalues >= 0.3, ] ##Excludes species that would not be present/have very low abundance

##biomass model w/ a-horion and block
biomass.highplots.mod <- lm(biomass.all ~ AHOR_cm + block, mono.highplots)
summary(biomass.highplots.mod)
anova(biomass.highplots.mod)

biomass.highplots.modA <- lm(biomass.all ~ AHOR_cm, mono.highplots)
summary(biomass.highplots.modA)
anova(biomass.highplots.modA)

biomass.highplots.modB <- lm(biomass.all ~ block, mono.highplots)
summary(biomass.highplots.modB)
anova(biomass.highplots.modB)

##NDVI model w/ a-horion and block
ndvi.highplots.mod <- lm(pNDVIvalues ~ AHOR_cm + block, mono.highplots)
summary(ndvi.highplots.mod)
anova(ndvi.highplots.mod)

ndvi.highplots.modA <- lm(pNDVIvalues  ~ AHOR_cm, mono.highplots)
summary(ndvi.highplots.modA)
anova(ndvi.highplots.modA)

ndvi.highplots.modB <- lm(pNDVIvalues ~ block, mono.highplots)
summary(ndvi.highplots.modB)
anova(ndvi.highplots.modB)

##GNDVI model w/ a-horion and block
gndvi.highplots.mod <- lm(pGNDVIvalues ~ AHOR_cm + block, mono.highplots)
summary(gndvi.highplots.mod)
anova(gndvi.highplots.mod)

gndvi.highplots.modA <- lm(pGNDVIvalues  ~ AHOR_cm, mono.highplots)
summary(gndvi.highplots.modA)
anova(gndvi.highplots.modA)

gndvi.highplots.modB <- lm(pGNDVIvalues ~ block, mono.highplots)
summary(gndvi.highplots.modB)
anova(gndvi.highplots.modB)

##GDVI2 model w/ a-horion and block
gdvi2.highplots.mod <- lm(pGDVI2values ~ AHOR_cm + block, mono.highplots)
summary(gdvi2.highplots.mod)
anova(gdvi2.highplots.mod)

gdvi2.highplots.modA <- lm(pGDVI2values  ~ AHOR_cm, mono.highplots)
summary(gdvi2.highplots.modA)
anova(gdvi2.highplots.modA)

gdvi2.highplots.modB <- lm(pGDVI2values ~ block, mono.highplots)
summary(gdvi2.highplots.modB)
anova(gdvi2.highplots.modB)

lm(pNDVIvalues ~ AHOR_cm + LOI + GSM + fWAS + pH + EC, mono.highplots)
