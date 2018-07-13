## ordination of biomass and NDVI on traits
## ah, 2018-07-12 update:
##  clean up legend on biomass - ndvi correlation
##  removing problem spp.2017

library(vegan)
library(FD)
library(geiger)
library(ggplot2)
library(ggrepel)
library(magrittr)
library(latex2exp)

labels.mapping <- c('Stem dry matter content', 'Leaf circularity', 'Vegetative height',
                     'Leaf dry matter content', 'Leaf C content', 'Leaf length', 'Leaf thickness',
                     'Biomass', 'NDVI', 'GNDVI')
dat.traits.mds <- monoMDS(gowdis(all.prairie.ordi[, labels.traits.use]))
dat.mds <- as.data.frame(dat.traits.mds$points)

mds.index <- gsub(" ", "_", row.names(dat.mds))
dat.mds$Family <- factor(dat.fams[mds.index, 'Family.8'],
                          levels = c("Apocynaceae", "Asteraceae", "Cyperaceae",
                          "Fabaceae", "Lamiaceae", "Poaceae", "Rosaceae",
                          "Other"))
dat.mds$Biomass <- all.prairie.ordi[row.names(dat.mds), 'Biomass']

dat.traits.mapping <- all.prairie.ordi[labels.mapping]

dat.traits.env <- envfit(dat.traits.mds, dat.traits.mapping)
dat.traits.arrows <- as.data.frame(dat.traits.env$vector$arrows*sqrt(dat.traits.env$vectors$r) * 3)
dat.traits.arrows$Trait <- row.names(dat.traits.arrows)
dat.traits.arrows$Mapping_variable <-
  ifelse(dat.traits.arrows$Trait %in% c('Biomass', 'NDVI', 'GNDVI'), 'Productivity response', 'Plant trait') %>%
  factor(levels = c('Productivity response', 'Plant trait'))
p.mds <- ggplot(dat.mds, aes(x = MDS1, y = MDS2))
p.mds <- p.mds + geom_point(aes(size = Biomass, color = Family))
p.mds <- p.mds + scale_color_brewer("Plant family", type = 'qual', palette = 1)
#p.mds <- p.mds <- scale_color_brewer("Top 7 families")
p.mds <- p.mds + theme(legend.position = 'right',
                        legend.key = element_blank()
                        )
p.mds <- p.mds + geom_segment(data = dat.traits.arrows,
                              aes(x = 0, xend = MDS1, y = 0, yend = MDS2, linetype = Mapping_variable),
                              arrow = arrow(length = unit(0.5, 'cm')), color = 'black'
                              )
p.mds <- p.mds + geom_label_repel(data = dat.traits.arrows,
                                  aes(x = MDS1, y = MDS2, label = Trait),
                                point.padding = 0.5, segment.size = 0)
#p.mds <- p.mds + coord_fixed()

pdf('../OUT/FIGURE.trait.ordination.withBiomass.pdf',12, 8)
print(p.mds)
dev.off()

if(!exists("dat.traits.lambda")) {
  dat.traits.lambda <- fitContinuous(tr.prairie.phylosig,
  dat.traits[row.names(ndvi.mat.small), 1:13],
  model = 'lambda')
}

dat.traits.cor <- data.frame(
  NDVI = cor(all.prairie.ordi$NDVI, all.prairie.ordi[, labels.traits.continuous])[1, ],
  Biomass = cor(all.prairie.ordi$Biomass, all.prairie.ordi[, labels.traits.continuous])[1, ],
  lambda = prairie.phylosignal[labels.traits.continuous, 'lambda']
  )

p <- ggplot(dat.traits.cor, aes(x = NDVI, y = Biomass))
#p <- p + geom_abline(intercept = 0, slope= 1, lwd = 0.5, lty = 'dashed')
p <- p + geom_hline(yintercept = 0, lty = 'dashed')
p <- p + geom_vline(xintercept = 0, lty = 'dashed')
p <- p + geom_point(aes(size = lambda), color = 'black')
#p <- p + geom_text(label = row.names(dat.traits.cor), hjust = 0, vjust = 0)
p <- p + geom_label_repel(label = row.names(dat.traits.cor),
                          point.padding = 1)
p <- p + scale_x_continuous("NDVI correlation (r) with individual traits")
p <- p + scale_y_continuous("Biomass correlation (r) with individual traits")
p <- p + theme(
               #legend.background = element_blank(),
               legend.position = c(0.93,0.1)
               )
p <- p + scale_radius(TeX("Pagel's $\\lambda$"))

pdf('../OUT/FIGURE.biomass.ndvi.correlation.regression.pdf', 8, 8)
print(p)
dev.off()
