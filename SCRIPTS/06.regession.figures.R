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


