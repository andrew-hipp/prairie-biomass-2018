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

outTab <- matrix(nrow = 6, ncol = 8)
outTab <- as.data.frame(outTab)
colnames(outTab) <- c("mean ALL", "min, max", "mean MONO", "min, max",
                      "mean TMT", "min, max", "plot type p", "block p")
rownames(outTab) <- c("biomass", "planted cover", "total cover",
                      "NDVI", "GNDVI", "GDVI2")


vari <- c("biomass.all", "coverTotal", "dcover",
          "NDVI", "GNDVI", "GDVI2")

for (i in 1:length(vari)) {
  use <- vari[i]
  
  if (use == "biomass.all") {
    df <- prairie.use.biomass
  } else {
    df <- prairie.use.other
  }
  
  outTab[i,1] <- format(round(mean(df[,use], na.rm = T), 2), nsmall = 2)
  outTab[i,2] <- paste0(format(round(min(df[,use], na.rm = T), 2), nsmall = 2), ", ", format(round(max(df[,use], na.rm = T), 2), nsmall = 2))
  
  outTab[i,3] <- format(round(mean(df[,use][which(prairie.use.biomass$Plot.category == "Monoculture")], na.rm = T), 2), nsmall = 2)
  mini <- format(round(min(df[,use][which(df$Plot.category == "Monoculture")], na.rm = T), 2), nsmall = 2)
  maxi <- format(round(max(df[,use][which(df$Plot.category == "Monoculture")], na.rm = T), 2), nsmall = 2)
  outTab[i,4] <- paste0(mini, ", ", maxi)
  
  outTab[i,5] <- format(round(mean(df[,use][which(df$Plot.category == "Treatment")], na.rm = T), 2), nsmall = 2)
  mini <- format(round(min(df[,use][which(df$Plot.category == "Treatment")], na.rm = T), 2), nsmall = 2)
  maxi <- format(round(max(df[,use][which(df$Plot.category == "Treatment")], na.rm = T), 2), nsmall = 2)
  outTab[i,6] <- paste0(mini, ", ", maxi)
  
  w <- wilcox.test(df[,use] ~ df$Plot.category)
  outTab[i,7] <- format(round(w$p.value, 3), nsmall = 3)
  
  prairie.use.other$block <- as.factor(prairie.use.other$block)
  k <- kruskal.test(df[,use] ~ df$block)
  outTab[i,8] <- format(round(k$p.value, 3), nsmall = 3)
  
}


write.csv(outTab, file = "../OUT/TABLE.Summary.Stats.nonParametric.csv")

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

