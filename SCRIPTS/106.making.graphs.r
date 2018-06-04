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



i <- 3
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




############# boxplots



