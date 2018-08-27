# regression of biomass/cover on VI

library(ggplot2)
library(colortools)
library(ggpubr)


prairie.use.biomass <- all.prairie[which(all.prairie$Plot.category == "Monoculture" |
                                   all.prairie$TMT.use == 1),]

prairie.use.other <- all.prairie[which(all.prairie$Plot.category == "Monoculture" |
                                         all.prairie$Plot.category == "Treatment"),]


################ regression


# combined
# NDVI biomass regression
NBR <- ggplot(data = prairie.use.biomass,
       aes(x = prairie.use.biomass$pNDVIvalues, y = prairie.use.biomass$biomass.all)) +
  geom_point(aes(color = factor(prairie.use.biomass$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use.biomass$Plot.category))) +
  labs(x = "NDVI", y = "biomass") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment"),
                      guide = FALSE) +
  theme_classic()

# NDVI ground cover regression
NGCR <- ggplot(data = prairie.use.other,
              aes(x = prairie.use.other$pNDVIvalues, y = prairie.use.other$coverTotal)) +
  geom_point(aes(color = factor(prairie.use.other$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use.other$Plot.category))) +
  labs(x = "NDVI", y = "percent cover, ground") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment")) +
  theme_classic() +
  ylim(c(0, 100))

# NDVI drone cover regression
NDCR <- ggplot(data = prairie.use.other,
              aes(x = prairie.use.other$pNDVIvalues, y = prairie.use.other$dcover)) +
  geom_point(aes(color = factor(prairie.use.other$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use.other$Plot.category))) +
  labs(x = "NDVI", y = "percent cover, drone") +
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
  labs(x = "percent cover, ground", y = "biomass") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment")) +
  theme_classic()



jpeg("../OUT/FIGURE.regressions.jpg", width = 900, height = 900)
ggarrange(NBR, BGCR, NGCR, NDCR, labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2,
          common.legend = TRUE, legend = "bottom",
          label.x = 1, label.y = 1, align = "hv")
dev.off()


############# boxplots

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



# biomass
MB <- ggplot(data = prairie.use.biomass,
             aes(x = prairie.use.biomass$Plot.category, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "biomass") +
  ylim(c(0, 4000))

TB <- ggplot(data = prairie.use.biomass,
       aes(x = prairie.use.biomass$trait.div, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "biomass") +
  ylim(c(0, 4000))

PB <- ggplot(data = prairie.use.biomass,
       aes(x = prairie.use.biomass$phy.div, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "biomass") +
  ylim(c(0, 4000))

BB <- ggplot(data = prairie.use.biomass,
       aes(x = prairie.use.biomass$block, y = prairie.use.biomass$biomass.all)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "biomass") +
  ylim(c(0, 4000))

# NDVI

TN <- ggplot(data = prairie.use.other,
       aes(x = prairie.use.other$trait.div, y = prairie.use.other$pNDVIvalues)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "NDVI") +
  ylim(c(0.3, 0.9))

PN <- ggplot(data = prairie.use.other,
       aes(x = prairie.use.other$phy.div, y = prairie.use.other$pNDVIvalues)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "NDVI") +
  ylim(c(0.3, 0.9))

BN <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$block, y = prairie.use.other$pNDVIvalues)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "NDVI") +
  ylim(c(0.3, 0.9))

MN <- ggplot(data = prairie.use.other,
       aes(x = prairie.use.other$Plot.category, y = prairie.use.other$pNDVIvalues)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "NDVI") +
  ylim(c(0.3, 0.9))

# Cover
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

BC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$block, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "cover") +
  ylim(c(0, 100))

MC <- ggplot(data = prairie.use.other,
             aes(x = prairie.use.other$Plot.category, y = prairie.use.other$coverTotal)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "cover") +
  ylim(c(0, 100))


jpeg("../OUT/FIGURE.boxplots.jpg", width = 1100, height = 480)
ggarrange(MB, TB, PB, BB,
          MC, TC, PC, BC,
          MN, TN, PN, BN,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
          ncol = 4, nrow = 3,
          align = "hv",
          label.x = 0, label.y = 1)
dev.off()


