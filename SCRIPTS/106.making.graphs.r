# regression of biomass/cover on VI

library(ggplot2)
library(colortools)

all.prairie <- read.csv("DATA/all.prairie.with.VI.values.csv")


prairie.use <- all.prairie[which(all.prairie$Plot.category == "Monoculture" |
                                   all.prairie$TMT.use == 1),]

prairie.use <- all.prairie


################ regression

predictor = c("pNDVIvalues", "pGDVI2values", "pGNDVIvalues")
response = c("biomass.all", "coverTotal")
predictornames = c("NDVI", "GDVI2", "GNDVI")
responsenames = c("Biomass", "Cover")


i <- 1
j <- 1

for (i in 1:length(predictor)) {
  jpeg(paste0("plot_", predictornames[i], "_", responsenames[j], ".jpg"), width = 480, height = 480)
  ggplot(data = prairie.use, 
       aes(x = prairie.use[[predictor[i]]], y = prairie.use[[response[j]]])) +
    geom_point(aes(color = factor(prairie.use$Plot.category))) +
    geom_smooth(method = "lm", aes(color = factor(prairie.use$Plot.category))) +
    labs(title = paste0(predictornames[i], " vs ", responsenames[j]), 
       x = predictornames[i], y = responsenames[j]) +
    scale_colour_manual(values = c("#4682B4", "#B4464B"),
                    name = "Plot type",
                    breaks = c("Monoculture", "Treatment"))
  dev.off()
  i <- i + 1
}



i <- 1
j <- 2

jpeg(paste0("plot_", predictornames[i], "_", responsenames[j], ".jpg"), width = 480, height = 480)
ggplot(data = prairie.use, 
       aes(x = prairie.use[[predictor[i]]], y = prairie.use[[response[j]]])) +
  geom_point(aes(color = factor(prairie.use$Plot.category))) +
  geom_smooth(method = "lm", aes(color = factor(prairie.use$Plot.category))) +
  labs(title = paste0(predictornames[i], " vs ", responsenames[j]), 
       x = predictornames[i], y = responsenames[j]) +
  scale_colour_manual(values = c("#4682B4", "#B4464B"),
                      name = "Plot type",
                      breaks = c("Monoculture", "Treatment"))
dev.off()

# combined
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


TB <- ggplot(data = prairie.use,
       aes(x = prairie.use$trait.div, y = prairie.use$biomass.all)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "biomass")

PB <- ggplot(data = prairie.use,
       aes(x = prairie.use$phy.div, y = prairie.use$biomass.all)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "biomass")

BB <- ggplot(data = prairie.use,
       aes(x = prairie.use$block, y = prairie.use$biomass.all)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "biomass")

TN <- ggplot(data = prairie.use,
       aes(x = prairie.use$trait.div, y = prairie.use$pNDVIvalues)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "H"), labels = c("Low", "High")) +
  theme_classic() +
  labs(x = "trait diversity", y = "NDVI")

PN <- ggplot(data = prairie.use,
       aes(x = prairie.use$phy.div, y = prairie.use$pNDVIvalues)) +
  geom_boxplot(fill = c(lighten("cornflowerblue"), "cornflowerblue", darken("cornflowerblue"))) +
  scale_x_discrete(limits = c("L", "M", "H"), labels = c("Low", "Medium", "High")) +
  theme_classic() +
  labs(x = "phylogenetic diversity", y = "NDVI")

BN <- ggplot(data = prairie.use,
             aes(x = prairie.use$block, y = prairie.use$pNDVIvalues)) +
  geom_boxplot(fill = "darkolivegreen4") +
  scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F")) +
  theme_classic() +
  labs(x = "block", y = "NDVI")



jpeg("OUT/boxplots.jpg", width = 1100, height = 480)
ggarrange(TB, PB, BB, TN, PN, BN, labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2,
          align = "hv")
dev.off()
