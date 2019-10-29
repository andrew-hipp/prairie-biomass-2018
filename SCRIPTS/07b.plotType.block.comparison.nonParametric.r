# compare blocks and plot types

library(ggplot2)
library(colortools)
library(ggpubr)
library(reshape2)
library(FSA)

# use for biomass analyses
prairie.use.biomass <- prairie.bio

# uses for NDVI and cover analyses
prairie.use.other <- prairie


# compare biomass mono vs. tmt
biomean <- mean(prairie.use.biomass$biomass.all)
biomin <- min(prairie.use.biomass$biomass.all)
biomax <- max(prairie.use.biomass$biomass.all)

biomeant <- mean(prairie.use.biomass$biomass.all[which(prairie.use.biomass$Plot.category == "Treatment")])
biomint <- min(prairie.use.biomass$biomass.all[which(prairie.use.biomass$Plot.category == "Treatment")])
biomaxt <- max(prairie.use.biomass$biomass.all[which(prairie.use.biomass$Plot.category == "Treatment")])

biomeanm <- mean(prairie.use.biomass$biomass.all[which(prairie.use.biomass$Plot.category == "Monoculture")])
biominm <- min(prairie.use.biomass$biomass.all[which(prairie.use.biomass$Plot.category == "Monoculture")])
biomaxm <- max(prairie.use.biomass$biomass.all[which(prairie.use.biomass$Plot.category == "Monoculture")])

wbio <- wilcox.test(prairie.use.biomass$biomass.all ~ prairie.use.biomass$Plot.category)
wbiop <- wbio$p.value

shapiro.test(prairie.use.biomass$biomass.all)

#tbio <- t.test(prairie.use.biomass$biomass.all[prairie.use.biomass$Plot.category == "Monoculture"],
#               prairie.use.biomass$biomass.all[prairie.use.biomass$Plot.category == "Treatment"])
#tbiop <- tbio$p.value

prairie.use.biomass$block <- as.factor(prairie.use.biomass$block)
kbio <- kruskal.test(prairie.use.biomass$biomass.all ~ prairie.use.biomass$block)
kbiop <- kbio$p.value

#anovabio <- aov(prairie.use.biomass$biomass.all ~ prairie.use.biomass$block)
#anovabiop <- summary(anovabio)[[1]][["Pr(>F)"]][1]



# compare ground cover monoculture and treatment
### WHY DO SOME OF THESE HAVE NAS????
coverGmean <- mean(prairie.use.other$coverTotal, na.rm = T)
coverGmin <- min(prairie.use.other$coverTotal, na.rm = T)
coverGmax <- max(prairie.use.other$coverTotal, na.rm = T)

coverGmeanT <- mean(prairie.use.other$coverTotal[which(prairie.use.other$Plot.category == "Treatment")], na.rm = T)
coverGminT <- min(prairie.use.other$coverTotal[which(prairie.use.other$Plot.category == "Treatment")], na.rm = T)
coverGmaxT <- max(prairie.use.other$coverTotal[which(prairie.use.other$Plot.category == "Treatment")], na.rm = T)

coverGmeanM <- mean(prairie.use.other$coverTotal[which(prairie.use.biomass$Plot.category == "Monoculture")], na.rm = T)
coverGminM <- min(prairie.use.other$coverTotal[which(prairie.use.biomass$Plot.category == "Monoculture")], na.rm = T)
coverGmaxM <- max(prairie.use.other$coverTotal[which(prairie.use.biomass$Plot.category == "Monoculture")], na.rm = T)


wcoverG <- wilcox.test(prairie.use.other$coverTotal ~ prairie.use.other$Plot.category)
wcoverGp <- wcoverG$p.value

#tcoverG <- t.test(prairie.use.other$coverTotal[prairie.use.other$Plot.category == "Monoculture"],
#                  prairie.use.other$coverTotal[prairie.use.other$Plot.category == "Treatment"])
#tcoverGp <- tcoverG$p.value

prairie.use.other$block <- as.factor(prairie.use.other$block)
kcoverG <- kruskal.test(prairie.use.other$coverTotal ~ prairie.use.other$block)
kcoverGp <- kcoverG$p.value

#anovaCoverG <- aov(prairie.use.other$coverTotal ~ prairie.use.other$block)
#anovaCoverGp <- summary(anovaCoverG)[[1]][["Pr(>F)"]][1]

# compare drone cover monoculture and treatment
coverDmean <- mean(prairie.use.other$dcover, na.rm = T)
coverDmin <- min(prairie.use.other$dcover, na.rm = T)
coverDmax <- max(prairie.use.other$dcover, na.rm = T)

coverDmeanT <- mean(prairie.use.other$dcover[which(prairie.use.other$Plot.category == "Treatment")], na.rm = T)
coverDminT <- min(prairie.use.other$dcover[which(prairie.use.other$Plot.category == "Treatment")], na.rm = T)
coverDmaxT <- max(prairie.use.other$dcover[which(prairie.use.other$Plot.category == "Treatment")], na.rm = T)

coverDmeanM <- mean(prairie.use.other$dcover[which(prairie.use.biomass$Plot.category == "Monoculture")], na.rm = T)
coverDminM <- min(prairie.use.other$dcover[which(prairie.use.biomass$Plot.category == "Monoculture")], na.rm = T)
coverDmaxM <- max(prairie.use.other$dcover[which(prairie.use.biomass$Plot.category == "Monoculture")], na.rm = T)

wcoverD <- wilcox.test(prairie.use.other$dcover ~ prairie.use.other$Plot.category)
wcoverDp <- wcoverD$p.value

#tcoverD <- t.test(prairie.use.other$dcover[prairie.use.other$Plot.category == "Monoculture"],
#                  prairie.use.other$dcover[prairie.use.other$Plot.category == "Treatment"])
#tcoverDp <- tcoverD$p.value

kcoverD <- kruskal.test(prairie.use.other$dcover ~ prairie.use.other$block)
kcoverDp <- kcoverD$p.value

#anovaCoverD <- aov(prairie.use.other$dcover ~ prairie.use.other$block)
#anovaCoverDp <- summary(anovaCoverD)[[1]][["Pr(>F)"]][1]

dunnTest(prairie.use.other$dcover ~ prairie.use.other$block)

# compare VIs mono vs. tmt
NDVImean <- mean(prairie.use.other$NDVI)
NDVImin <- min(prairie.use.other$NDVI)
NDVImax <- max(prairie.use.other$NDVI)

NDVImeanM <- mean(prairie.use.other$NDVI[which(prairie.use.other$Plot.category == "Monoculture")])
NDVImeanT <- mean(prairie.use.other$NDVI[which(prairie.use.other$Plot.category == "Treatment")])

wNDVI <- wilcox.test(prairie.use.other$NDVI ~ prairie.use.other$Plot.category)
wNDVIp <- wNDVI$p.value

kNDVI <- kruskal.test(prairie.use.other$NDVI ~ prairie.use.other$block)
kNDVIp <- kNDVI$p.value

dunnTest(prairie.use.other$NDVI ~ prairie.use.other$block)


#tNDVI <- t.test(prairie.use.other$NDVI[which(prairie.use.other$Plot.category == "Monoculture")],
#                prairie.use.other$NDVI[which(prairie.use.other$Plot.category == "Treatment")])
#tNDVIp <- tNDVI$p.value

#anovaNDVI <- aov(prairie.use.other$NDVI ~ prairie.use.other$block)
#anovaNDVIp <- summary(anovaNDVI)[[1]][["Pr(>F)"]][1]


GNDVImean <- mean(prairie.use.other$GNDVI)
GNDVImin <- min(prairie.use.other$GNDVI)
GNDVImax <- max(prairie.use.other$GNDVI)

GNDVImeanM <- mean(prairie.use.other$GNDVI[which(prairie.use.other$Plot.category == "Monoculture")])
GNDVImeanT <- mean(prairie.use.other$GNDVI[which(prairie.use.other$Plot.category == "Treatment")])

wGNDVI <- wilcox.test(prairie.use.other$GNDVI ~ prairie.use.other$Plot.category)
wGNDVIp <- wGNDVI$p.value

kGNDVI <- kruskal.test(prairie.use.other$GNDVI ~ prairie.use.other$block)
kGNDVIp <- kGNDVI$p.value

dunnTest(prairie.use.other$GNDVI ~ prairie.use.other$block)


#tGNDVI <- t.test(prairie.use.other$GNDVI[which(prairie.use.other$Plot.category == "Monoculture")],
#                prairie.use.other$GNDVI[which(prairie.use.other$Plot.category == "Treatment")])
#tGNDVIp <- tGNDVI$p.value

#anovaGNDVI <- aov(prairie.use.other$GNDVI ~ prairie.use.other$block)
#anovaGNDVIp <- summary(anovaGNDVI)[[1]][["Pr(>F)"]][1]



GDVI2mean <- mean(prairie.use.other$GDVI2)
GDVI2min <- min(prairie.use.other$GDVI2)
GDVI2max <- max(prairie.use.other$GDVI2)

GDVI2meanM <- mean(prairie.use.other$GDVI2[which(prairie.use.other$Plot.category == "Monoculture")])
GDVI2meanT <- mean(prairie.use.other$GDVI2[which(prairie.use.other$Plot.category == "Treatment")])

wGDVI2 <- wilcox.test(prairie.use.other$GDVI2 ~ prairie.use.other$Plot.category)
wGDVI2p <- wGDVI2$p.value

kGDVI2 <- kruskal.test(prairie.use.other$GDVI2 ~ prairie.use.other$block)
kGDVI2p <- kGDVI2$p.value

dunnTest(prairie.use.other$GDVI2 ~ prairie.use.other$block)


#tGDVI2 <- t.test(prairie.use.other$GDVI2[which(prairie.use.other$Plot.category == "Monoculture")],
#                 prairie.use.other$GDVI2[which(prairie.use.other$Plot.category == "Treatment")])
#tGDVI2p <- tGDVI2$p.value

#anovaGDVI2 <- aov(prairie.use.other$GDVI2 ~ prairie.use.other$block)
#anovaGDVI2p <- summary(anovaGDVI2)[[1]][["Pr(>F)"]][1]


# put into a table
compStats <- matrix(nrow = 6, ncol = 7)
colnames(compStats) <- c("mean", "min", "max", "meanMonoculture", "meanTreatment", "plotTypeP", "blockP")
rownames(compStats) <- c("biomass", "coverGround", "coverDrone", "NDVI", "GNDVI", "GDVI2")

compStats <- as.data.frame(compStats)

compStats[1,] <- c(biomean, biomin, biomax, biomeanm, biomeant, wbiop, kbiop)
compStats[2,] <- c(coverGmean, coverGmin, coverGmax, coverGmeanM, coverGmeanT, wcoverGp, kcoverGp)
compStats[3,] <- c(coverDmean, coverDmin, coverDmax, coverDmeanM, coverDmeanT, wcoverDp, kcoverDp)
compStats[4,] <- c(NDVImean, NDVImin, NDVImax, NDVImeanM, NDVImeanT, wNDVIp, kNDVIp)
compStats[5,] <- c(GNDVImean, GNDVImin, GNDVImax, GNDVImeanM, GNDVImeanT, wGNDVIp, kGNDVIp)
compStats[6,] <- c(GDVI2mean, GDVI2min, GDVI2max, GDVI2meanM, GDVI2meanT, wGDVI2p, kGDVI2p)

compStats$range <- compStats$max - compStats$min

write.csv(compStats, file = "../OUT/plotType.and.block.comparison.stats.nonParametric.csv")

# make boxplots

# biomass
MB <- ggplot(data = prairie.use.biomass,
             aes(x = prairie.use.biomass$Plot.category, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "biomass (g)") +
  ylim(c(0, 4000))


BB <- ggplot(data = prairie.use.biomass,
             aes(x = prairie.use.biomass$block, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "biomass (g)") +
  ylim(c(0, 4000))

# NDVI
#Removed 45 rows containing non-finite values (stat_boxplot). 
BN <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$block, y = prairie.use.other$NDVI.CI)) + #chnaged from pNDVIvalues
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "NDVI") +
  ylim(c(0.3, 0.9))

#Removed 45 rows containing non-finite values (stat_boxplot).
MN <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$Plot.category, y = prairie.use.other$NDVI.CI)) + #chnaged from pNDVIvalues
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "NDVI") +
  ylim(c(0.3, 0.9))

# Cover
# Removed 5 rows containing non-finite values (stat_boxplot).
BC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$block, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "cover (%)") +
  ylim(c(0, 100))

#Removed 5 rows containing non-finite values (stat_boxplot).
MC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$Plot.category, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "cover (%)") +
  ylim(c(0, 100))


# format and save figure
jpeg("../OUT/FIGURE.boxplots.jpg", width = 510, height = 480)
ggarrange(MB, BB,
          MC, BC,
          MN, BN,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3,
          align = "hv",
          label.x = 0, label.y = 1)
dev.off()

# compare VIs

VI <- prairie.use.other[,c("NDVI", "GNDVI", "GDVI2")]
VI <- melt(VI)

VIaov <- kruskal.test(VI$value ~ VI$variable)
VIaov$p.value

out <- TukeyHSD(VIaov)
tmp <- as.data.frame(out$`VI$variable`)

