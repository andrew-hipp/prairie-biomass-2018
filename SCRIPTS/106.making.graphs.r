# regression of biomass/cover on VI

library(ggplot2)
library(colortools)
library(ggpubr)

all.prairie <- read.csv("DATA/all.prairie.with.VI.values.csv")


prairie.use <- all.prairie[which(all.prairie$Plot.category == "Monoculture" |
                                   all.prairie$TMT.use == 1),]

prairie.use <- all.prairie


################ regression


# combined
# NDVI biomass regression
NBR <- ggplot(data = prairie.use, 
       aes(x = prairie.use$pNDVIvalues, y = prairie.use$biomass.all)) +
  geom_point(aes(color = factor(prairie.use$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use$Plot.category))) +
  labs(x = "NDVI", y = "biomass") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment"),
                      guide = FALSE) +
  theme_classic()

# NDVI cover regression
NCR <- ggplot(data = prairie.use, 
              aes(x = prairie.use$pNDVIvalues, y = prairie.use$coverTotal)) +
  geom_point(aes(color = factor(prairie.use$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use$Plot.category))) +
  labs(x = "NDVI", y = "percent cover") +
  scale_colour_manual(values = c("goldenrod2", "cornflowerblue"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment")) +
  theme_classic()

jpeg("OUT/regressions.jpg", width = 900, height = 480)
ggarrange(NBR, NCR, labels = c("A", "B"), nrow = 1, ncol = 2, 
          common.legend = TRUE, legend = "bottom")
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
MB <- ggplot(data = prairie.use,
             aes(x = prairie.use$Plot.category, y = prairie.use$biomass.all)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "biomass") +
  ylim(c(0, 4000))

TB <- ggplot(data = prairie.use,
       aes(x = prairie.use$trait.div, y = prairie.use$biomass.all)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "biomass") +
  ylim(c(0, 4000))

PB <- ggplot(data = prairie.use,
       aes(x = prairie.use$phy.div, y = prairie.use$biomass.all)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "biomass") +
  ylim(c(0, 4000))

BB <- ggplot(data = prairie.use,
       aes(x = prairie.use$block, y = prairie.use$biomass.all)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "biomass") +
  ylim(c(0, 4000))

# NDVI

TN <- ggplot(data = prairie.use,
       aes(x = prairie.use$trait.div, y = prairie.use$pNDVIvalues)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "NDVI") +
  ylim(c(0.3, 0.9))

PN <- ggplot(data = prairie.use,
       aes(x = prairie.use$phy.div, y = prairie.use$pNDVIvalues)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "NDVI") +
  ylim(c(0.3, 0.9))

BN <- ggplot(data = prairie.use,
             aes(x = prairie.use$block, y = prairie.use$pNDVIvalues)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "NDVI") +
  ylim(c(0.3, 0.9))

MN <- ggplot(data = prairie.use,
       aes(x = prairie.use$Plot.category, y = prairie.use$pNDVIvalues)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "NDVI") +
  ylim(c(0.3, 0.9))

# Cover
TC <- ggplot(data = prairie.use,
             aes(x = prairie.use$trait.div, y = prairie.use$coverTotal)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "cover") +
  ylim(c(0, 100))

PC <- ggplot(data = prairie.use,
             aes(x = prairie.use$phy.div, y = prairie.use$coverTotal)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "cover") +
  ylim(c(0, 100))

BC <- ggplot(data = prairie.use,
             aes(x = prairie.use$block, y = prairie.use$coverTotal)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "cover") +
  ylim(c(0, 100))

MC <- ggplot(data = prairie.use,
             aes(x = prairie.use$Plot.category, y = prairie.use$coverTotal)) +
  geom_boxplot(fill = c("goldenrod2", "cornflowerblue")) +
  scale_x_discrete(limits = c("Monoculture", "Treatment"), labels = c("Monoculture", "Treatment")) +
  theme_classic() +
  labs(x = "plot type", y = "cover") +
  ylim(c(0, 100))


jpeg("OUT/boxplots.jpg", width = 1100, height = 480)
ggarrange(MB, TB, PB, BB, 
          MC, TC, PC, BC,
          MN, TN, PN, BN, 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
          ncol = 4, nrow = 3,
          align = "hv")
dev.off()


ggarrange(BB, MB, TB, PB, BN, MN, TN, PN, labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 4, nrow = 2,
          align = "hv")
