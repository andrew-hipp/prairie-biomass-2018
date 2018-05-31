## assumes you've already compiled data (script 01)

mono.plots = all.prairie[!is.na(all.prairie$monoSp), ]

mono.highplots <- temp[temp$ndvi >= 0.3, ]

mono.highplots.mod <- lm(biomass.all ~ AHOR_cm + block, mono.highplots)
summary(mono.highplots.mod)
anova(mono.highplots.mod)
