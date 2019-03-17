# regression of biomass/cover on VI

library(ggplot2)
library(colortools)
library(ggpubr)

# use for biomass analyses
prairie.use.biomass <- prairie.bio

# uses for NDVI and cover analyses
prairie.use.other <- prairie


################ regression


# NDVI biomass regression
NBR <- ggplot(data = prairie.use.biomass,
                 aes(x = prairie.use.biomass$NDVI, y = prairie.use.biomass$biomass.all)) + #chnaged from pNDVIvalues
  geom_point(aes(color = factor(prairie.use.biomass$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use.biomass$Plot.category))) +
  labs(x = "NDVI", y = "biomass") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment"),
                      guide = FALSE) +
  theme_classic()

NBR.CI <- ggplot(data = prairie.use.biomass,
       aes(x = prairie.use.biomass$NDVI.CI, y = prairie.use.biomass$biomass.all)) + #chnaged from pNDVIvalues
  geom_point(aes(color = factor(prairie.use.biomass$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use.biomass$Plot.category))) +
  labs(x = "NDVI (excluding weeds)", y = "biomass (excluding weeds)") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment"),
                      guide = FALSE) +
  theme_classic()

# NDVI ground cover regression
NGCR.CI <- ggplot(data = prairie.use.other,
              aes(x = prairie.use.other$NDVI.CI, y = prairie.use.other$coverTotal)) + #chnaged from pNDVIvalues
  geom_point(aes(color = factor(prairie.use.other$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use.other$Plot.category))) +
  labs(x = "NDVI (excluding weeds)", y = "percent cover (excluding weeds)") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment")) +
  theme_classic() +
  ylim(c(0, 100))

# NDVI drone cover regression
NDCR <- ggplot(data = prairie.use.other,
              aes(x = prairie.use.other$NDVI, y = prairie.use.other$dcover)) + #chnaged from pNDVIvalues
  geom_point(aes(color = factor(prairie.use.other$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use.other$Plot.category))) +
  labs(x = "NDVI (including weeds)", y = "percent cover (including weeds)") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment")) +
  theme_classic() +
  ylim(c(0, 100))

# biomass ground cover regression
BGCR <- ggplot(data = prairie.use.biomass,
               aes(x = prairie.use.biomass$coverTotal, y = prairie.use.biomass$biomass.all)) +
  geom_point(aes(color = factor(prairie.use.biomass$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use.biomass$Plot.category))) +
  labs(x = "percent cover (excluding weeds)", y = "biomass (excluding weeds)") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment")) +
  theme_classic()


jpeg("../OUT/FIGURE.regressions.jpg", width = 900, height = 900)
ggarrange(NBR.CI, BGCR, NGCR.CI, NDCR, labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2,
          common.legend = TRUE, legend = "bottom",
          label.x = 1, label.y = 1, align = "hv")
dev.off()


############# boxplots

# biomass
MB <- ggplot(data = prairie.use.biomass,
             aes(x = prairie.use.biomass$Plot.category, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "biomass") +
  ylim(c(0, 4000))


BB <- ggplot(data = prairie.use.biomass,
       aes(x = prairie.use.biomass$block, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "biomass") +
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
  labs(x = "block", y = "cover") +
  ylim(c(0, 100))

#Removed 5 rows containing non-finite values (stat_boxplot).
MC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$Plot.category, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "cover") +
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



### old code ----
#Changed all.prairie needs to be changed to praire.mono


#prairie.use.biomass <- all.prairie[which(all.prairie$Plot.category == "Monoculture" |
#                                   all.prairie$TMT.use == 1),]
#prairie.use.biomass <- prairie.mono[which(prairie.mono$Plot.category == "Monoculture" |
#                                            prairie.mono$TMT.use == 1),]


#prairie.use.biomass$trait.div is entirely empty <NA>
#First made in 00a.readBiomass.R at biomass.mat. which biomass.mat$trait.div is not empty
#$phy.div is empty too

#prairie.use.other <- all.prairie[which(all.prairie$Plot.category == "Monoculture" |
#                                         all.prairie$Plot.category == "Treatment"),]
#prairie.use.other <- prairie.mono[which(prairie.mono$Plot.category == "Monoculture" |
#                                          prairie.mono$Plot.category == "Treatment"),]

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
  col
}

TB <- ggplot(data = prairie.use.biomass,
             aes(x = prairie.use.biomass$trait.div, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "biomass") +
  ylim(c(0, 4000))

#above error: Removed 206 rows containing missing values (stat_boxplot):


PB <- ggplot(data = prairie.use.biomass,
             aes(x = prairie.use.biomass$phy.div, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "biomass") +
  ylim(c(0, 4000))

#Above error: Removed 206 rows containing missing values (stat_boxplot):


#Removed 206 rows containing missing values (stat_boxplot). 
TN <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$trait.div, y = prairie.use.other$NDVI.CI)) + #chnaged from pNDVIvalues
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "NDVI") +
  ylim(c(0.3, 0.9))

##Removed 206 rows containing missing values (stat_boxplot). 
PN <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$phy.div, y = prairie.use.other$NDVI.CI)) + #chnaged from pNDVIvalues
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "NDVI") +
  ylim(c(0.3, 0.9))


#Removed 206 rows containing missing values (stat_boxplot). 
TC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$trait.div, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "cover") +
  ylim(c(0, 100))

PC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$phy.div, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "cover") +
  ylim(c(0, 100))
