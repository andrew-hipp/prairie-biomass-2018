# compare blocks and plot types

library(ggplot2)
library(colortools)
library(ggpubr)
library(reshape2)
library(plotrix)

# use for biomass analyses
prairie.use.biomass <- prairie.bio

# uses for NDVI and cover analyses
prairie.use.other <- prairie

outTab <- matrix(nrow = 7, ncol = 13)
outTab <- as.data.frame(outTab)
colnames(outTab) <- c("mean ALL", "min, max", "mean MONO", "min, max",
                      "mean TMT", "min, max", "plot type p", "block p", 
                      "SEM", "monoSEM", "tmtSEM", "W", "K")
rownames(outTab) <- c("biomass", "planted cover", "total cover",
                      "NDVI", "GNDVI", "GDVI2", "VOL")


vari <- c("biomass.all", "coverTotal", "dcover",
          "NDVI", "GNDVI", "GDVI2", "VOL")

for (i in 1:length(vari)) {
  use <- vari[i]
  
  if (use == "biomass.all") {
    df <- prairie.use.biomass
  } else {
    df <- prairie.use.other
  }
  
  outTab[i,1] <- format(round(mean(df[,use], na.rm = T), 2), nsmall = 2)
  outTab[i,2] <- paste0(format(round(min(df[,use], na.rm = T), 2), nsmall = 2), ", ", format(round(max(df[,use], na.rm = T), 2), nsmall = 2))
  outTab[i,9] <- std.error(df[,use], na.rm = T)
  
  outTab[i,3] <- format(round(mean(df[,use][which(prairie.use.biomass$Plot.category == "Monoculture")], na.rm = T), 2), nsmall = 2)
  mini <- format(round(min(df[,use][which(df$Plot.category == "Monoculture")], na.rm = T), 2), nsmall = 2)
  maxi <- format(round(max(df[,use][which(df$Plot.category == "Monoculture")], na.rm = T), 2), nsmall = 2)
  outTab[i,4] <- paste0(mini, ", ", maxi)
  outTab[i,10] <- std.error(df[,use][which(df$Plot.category == "Monoculture")], na.rm = T)
  
  outTab[i,5] <- format(round(mean(df[,use][which(df$Plot.category == "Treatment")], na.rm = T), 2), nsmall = 2)
  mini <- format(round(min(df[,use][which(df$Plot.category == "Treatment")], na.rm = T), 2), nsmall = 2)
  maxi <- format(round(max(df[,use][which(df$Plot.category == "Treatment")], na.rm = T), 2), nsmall = 2)
  outTab[i,6] <- paste0(mini, ", ", maxi)
  outTab[i,11] <- std.error(df[,use][which(df$Plot.category == "Treatment")], na.rm = T)
  
  w <- wilcox.test(df[,use] ~ df$Plot.category)
  outTab[i,7] <- format(round(w$p.value, 4), nsmall = 4)
  outTab[i,12] <- w$statistic
  
  df$block <- as.factor(df$block)
  k <- kruskal.test(df[,use] ~ df$block)
  outTab[i,8] <- format(round(k$p.value, 4), nsmall = 4)
  outTab[i,13] <- k$statistic
  
}


write.csv(outTab, file = "../OUT/TABLE.Summary.Stats.nonParametric.csv")

# make boxplots

# biomass
MB <- ggplot(data = prairie.use.biomass,
             aes(x = prairie.use.biomass$Plot.category, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "", y = "biomass (g)") +
  ylim(c(0, 4000))


BB <- ggplot(data = prairie.use.biomass,
             aes(x = prairie.use.biomass$block, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "", y = "biomass (g)") +
  ylim(c(0, 4000))

# NDVI
#Removed 45 rows containing non-finite values (stat_boxplot). 
BN <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$block, y = prairie.use.other$NDVI.CI)) + #chnaged from pNDVIvalues
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "", y = "NDVI") +
  ylim(c(0, 1))

#Removed 45 rows containing non-finite values (stat_boxplot).
MN <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$Plot.category, y = prairie.use.other$NDVI.CI)) + #chnaged from pNDVIvalues
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "", y = "NDVI") +
  ylim(c(0, 1))

# Cover
# Removed 5 rows containing non-finite values (stat_boxplot).
BC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$block, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "", y = "planted cover (%)") +
  ylim(c(0, 100))

#Removed 5 rows containing non-finite values (stat_boxplot).
MC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$Plot.category, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "", y = "planted cover (%)") +
  ylim(c(0, 100))

# Volume
# Removed 5 rows containing non-finite values (stat_boxplot).
BV <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$block, y = prairie.use.other$VOL)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = expression(paste("volume (", m^2, ")"))) +
  ylim(c(0, 4))

#Removed 5 rows containing non-finite values (stat_boxplot).
MV <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$Plot.category, y = prairie.use.other$VOL)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = expression(paste("volume (", m^2, ")"))) +
  ylim(c(0, 4))

# format and save figure
jpeg("../OUT/FIGURE.boxplots.jpg", width = 550, height = 700)
ggarrange(MB, BB,
          MC, BC,
          MN, BN,
          MV, BV,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 2, nrow = 4,
          align = "hv",
          label.x = 0, label.y = 1)
dev.off()

# compare VIs

VI <- prairie.use.other[,c("NDVI", "GNDVI", "GDVI2")]
VI <- melt(VI)

VIaov <- kruskal.test(VI$value ~ VI$variable)
VIaov$p.value

VIaov2 <- aov(VI$value ~ VI$variable)
out <- TukeyHSD(x = VIaov2)
tmp <- as.data.frame(out$`VI$variable`)

# compare soil blocks

soil <- prairie.use.other[,c("block", "AHOR_cm")]
soil <- melt(soil)

soil$block <- as.factor(soil$block)
soilAOV <- kruskal.test(soil$value ~ soil$block)
soilAOV$p.value

# effect of blocking on biomass -- different from loop above 
# because we only want monocultures
prairie.use.biomass$block <- as.factor(prairie.use.biomass$block)
bioAOV <- kruskal.test(prairie.use.biomass$biomass.all[which(prairie.use.biomass$Plot.category == "Monoculture")] ~
                         prairie.use.biomass$block[which(prairie.use.biomass$Plot.category == "Monoculture")])
bioAOV$p.value


# mean of block a vs mean of blocks b-f
mean(prairie.use.other$AHOR_cm[which(prairie.use.other$block == "A")])
mean(prairie.use.other$AHOR_cm[which(prairie.use.other$block != "A")])
