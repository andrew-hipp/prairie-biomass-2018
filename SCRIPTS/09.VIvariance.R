
t.test(prairie$vNDVI.CI ~ prairie$Plot.category)
# variance in GDVI2 is significantly higher in monoculture plots p = 0.006
plot(x = prairie$Plot.category, y = prairie$vGDVI2)

test <- prairie[,c("Plot.category", "vNDVI", "vGNDVI", "vGDVI2", 
                   "vNDVI.CI", "vGNDVI.CI", "vGDVI2.CI",
                   "vRED", "vGRE", "vNIR", "vREG",
                   "vRED.CI", "vGRE.CI", "vNIR.CI", "vREG.CI")]
monos <- test[which(test$Plot.category == "Monoculture"),]
tmts <- test[which(test$Plot.category == "Treatment"),]

tmtsM <- melt(tmts)
summary(aov(tmtsM$value ~ tmtsM$variable))
ggplot(data = tmtsM, aes(x = tmtsM$variable, y = tmtsM$value)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "anova",
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "")))


monosM <- melt(monos)
summary(aov(monosM$value ~ monosM$variable))
ggplot(data = monosM, aes(x = monosM$variable, y = monosM$value)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "anova",
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "")))



summary(aov(prairie$vNDVI ~ prairie$trait.div))
## phy div
# vNDVI p = 0.08
# vGNDVI p = 0.02
# vGDVI2 p = 0.05

plot(x = prairie$phy.div, y = prairie$vGNDVI)
# higher phy div has higher variance, opposite trend from mono/tmt

summary(aov(prairie$vGDVI2 ~ prairie$trait.div))
## trait div
# vNDVI p = 0.04
# vGNDVI p = 0.12
# vGDVI2 p = 0.15

plot(x = prairie$trait.div, y = prairie$vNDVI)
# low trait div has higher variance, same trend as mono/tmt


cNF.all <- read.csv("../OUT/TABLE.cover.regression.noFlowers.all.dcover.csv")
cNF.mono <- read.csv("../OUT/TABLE.cover.regression.noFlowers.mono.dcover.csv")
cNF.tmt <- read.csv("../OUT/TABLE.cover.regression.noFlowers.tmt.dcover.csv")

cF.all <- read.csv("../OUT/TABLE.cover.regression.all.dcover.csv")
cF.mono <- read.csv("../OUT/TABLE.cover.regression.mono.dcover.csv")
cF.tmt <- read.csv("../OUT/TABLE.cover.regression.tmt.dcover.csv")

cMonoM <- merge(cF.mono, cF.tmt, by = "X")
cMonoM <- cMonoM[,c(1, 5, 11)]
cMonoM <- melt(cMonoM, id = "X")

ggplot(cMonoM, aes(x = variable, y = value)) +
  geom_point() +
  geom_line(aes(group = X)) +
  labs(x = "", y = "R2", 
       title = "R2 of monoculture and treatment plots") +
  theme_classic() +
  scale_x_discrete(labels = c("monoculture plots", "treatment plots"))

F.mono <- read.csv("../OUT/TABLE.biomass.regression.full.mono.csv")
F.tmt <- read.csv("../OUT/TABLE.biomass.regression.full.tmt.csv")

bm <- merge(F.mono, F.tmt, by = "X")
bm <- bm[,c(1, 2, 3, 6, 7, 15)]
bm$VI <- substr(bm$VI.used.x, 1, 3)
bm$vol <- 1
bm$vol[which(bm$volume.x == "-")] <- 0
bm$cover <- 0
bm$cover[which(bm$cover.used.x == "drone")] <- "drone"
bm$cover[which(bm$cover.used.x == "ground")] <- "ground"

#bm$corrected <- "no"
#bm$corrected[which()]
bm <- melt(bm, id = c("X", "VI.used.x", "VI", "volume.x", "vol", "cover.used.x", "cover"))

ggplot(bm, aes(x = variable, y = value)) +
  geom_point(aes(size = 15, shape = factor(bm$cover), 
                 color = factor(bm$cover)), alpha = 1/5) +
  geom_line(aes(group = X, color = factor(bm$VI), linetype = factor(bm$vol))) +
  labs(x = "", y = "R2", 
       title = "R2 of monoculture and treatment plots") +
  theme_classic() +
  scale_x_discrete(labels = c("monoculture plots", "treatment plots")) +
  ylim(0.25, 0.55)
