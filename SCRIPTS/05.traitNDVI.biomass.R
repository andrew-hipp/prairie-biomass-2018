## ordination of biomass and NDVI on traits
## ah, done 2018-07-12

library(vegan)
library(FD)
library(geiger)
library(ggplot2)
library(ggrepel)
library(magrittr)

dat.fams <- read.csv('../DATA/families.csv', row.names = 1, as.is = T)
dat.traits <- read.csv('../DATA/ImputedMiceTraits.2016-01-06.csv',
                       as.is = F, row.names = 1)

dat.traits.use <- c("seedMass", "LDMC", "SLA", "LNC", "LCC", "LPC", "SDMC", "circularity",
                       "vegetativeHeight", "leafLength", "leafThickness", "leafWidth",
                       "petioleLength", "Lifeform", "photosyntheticPathway", "rhizomes",
                       "habitClean", "nFixer",
                       "wis_IL", "genome_Use")
dat.traits.continuous <- c("seedMass", "LDMC", "SLA", "LNC", "LCC", "LPC", "SDMC", "circularity",
                                              "vegetativeHeight", "leafLength", "leafThickness", "leafWidth",
                                              "petioleLength","genome_Use")
dat.traits.scaled <- dat.traits[row.names(ndvi.mat.small), dat.traits.use]
for(i in dat.traits.continuous) dat.traits.scaled[[i]] <- as.numeric(scale(dat.traits.scaled[[i]]))
dat.fams$Family.8 <- dat.fams$family
dat.fams$Family.8[!dat.fams$family %in% names(tail(sort(table(dat.fams$family)), 7))] <- "Other"
dat.traits.mds <- monoMDS(gowdis(dat.traits.scaled))
dat.mds <- as.data.frame(dat.traits.mds$points)
dat.mds$Family <- dat.fams[row.names(dat.mds), 'family']
dat.mds$Family.8 <- dat.fams[row.names(dat.mds), 'Family.8']
dat.mds$Biomass <- ndvi.mat.mean[row.names(dat.mds), 'biomass.all']

dat.traits.mapping <- data.frame(dat.traits.scaled[c('SDMC', 'circularity', 'vegetativeHeight',
                                'LDMC', 'LCC', 'leafLength', 'leafThickness')],
                                Biomass = ndvi.mat.mean[row.names(dat.mds), 'biomass.all'],
                                NDVI = ndvi.mat.mean[row.names(dat.mds), 'ndvi']
                                )
dat.traits.env <- envfit(dat.traits.mds, dat.traits.mapping)
dat.traits.arrows <- as.data.frame(dat.traits.env$vector$arrows*sqrt(dat.traits.env$vectors$r) * 3)
dat.traits.arrows$Trait <- row.names(dat.traits.arrows)
dat.traits.arrows$Mapping_variable <-
  ifelse(dat.traits.arrows$Trait %in% c('Biomass', 'NDVI'), 'Productivity response', 'Plant trait') %>%
  factor(levels = c('Productivity response', 'Plant trait'))
p.mds <- ggplot(dat.mds, aes(x = MDS1, y = MDS2))
p.mds <- p.mds + geom_point(aes(size = Biomass, color = Family.8))
p.mds <- p.mds + scale_color_brewer(type = 'qual', palette = 1)
#p.mds <- p.mds <- scale_color_brewer("Top 7 families")
# p.mds <- p.mds + geom_label_repel(label = row.names(dat.mds),point.padding = 1)
#p.mds <- p.mds + theme(legend.position = c(0.9,0.1))
p.mds <- p.mds + geom_segment(data= dat.traits.arrows,
                              aes(x = 0, xend = MDS1, y = 0, yend = MDS2, linetype = Mapping_variable),
                              arrow = arrow(length = unit(0.5, 'cm')), color = 'black',
                              inherit_aes = FALSE)
p.mds <- p.mds + geom_label_repel(data = dat.traits.arrows,
                                  aes(x = MDS1, y = MDS2, label = Trait),
                                point.padding = 0.5, segment.size = 0)
#p.mds <- p.mds + coord_fixed()

pdf('../OUT/trait.ordination.withBiomass.pdf',12, 8)
print(p.mds)
dev.off()

#plot(dat.traits.env)

dat.traits.lambda <- fitContinuous(tr.prairie.phylosig,
  dat.traits[row.names(ndvi.mat.small), 1:13],
  model = 'lambda')

dat.traits.cor <- data.frame(
  NDVI = cor(ndvi.mat.small[, 'NDVI'], dat.traits[row.names(ndvi.mat.small), 1:13])[1, ],
  Biomass = cor(ndvi.mat.small[, 'Biomass'], dat.traits[row.names(ndvi.mat.small), 1:13])[1, ],
  lambda = sapply(dat.traits.lambda, function(x) x$opt$lambda)
  )

plot(dat.traits.cor, pch = 19, cex = dat.traits.cor[, 'lambda'])
abline(a = 0, b = 1, lty = 'dashed')

p <- ggplot(dat.traits.cor, aes(x = NDVI, y = Biomass))
#p <- p + geom_abline(intercept = 0, slope= 1, lwd = 0.5, lty = 'dashed')
p <- p + geom_hline(yintercept = 0, lty = 'dashed')
p <- p + geom_vline(xintercept = 0, lty = 'dashed')
p <- p + geom_point(aes(size = lambda), color = 'red')
#p <- p + geom_text(label = row.names(dat.traits.cor), hjust = 0, vjust = 0)
p <- p + geom_label_repel(label = row.names(dat.traits.cor),
                          point.padding = 1)
p <- p + scale_x_continuous("NDVI correlation (r) with individual traits")
p <- p + scale_y_continuous("Biomass correlation (r) with individual traits")
p <- p + theme(legend.position = c(0.95,0.1))

pdf('../OUT/biomass.ndvi.correlation.regression.pdf', 8, 8)
print(p)
dev.off()
