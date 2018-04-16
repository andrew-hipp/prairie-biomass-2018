## ordination of biomass and NDVI on traits

library(vegan)
library(geiger)
library(ggplot2)
library(ggrepel)

dat.fams <- read.delim('../DATA/prairie.spp.list.v11.2016-01-05.tsv', as.is = T)
dat.traits <- read.csv('../DATA/ImputedMiceTraits.2016-01-06.csv',
                       as.is = T, row.names = 1)

dat.traits.mds <- metaMDS(dat.traits[row.names(ndvi.mat.small), 1:13])

dat.mds <- as.data.frame(dat.traits.mds$points)
dat.mds$Biomass <- ndvi.mat.mean[row.names(dat.mds), 'biomass.all']

p.mds <- ggplot(dat.mds, aes(x = MDS1, y = MDS2))
p.mds <- p.mds + geom_point(aes(size = Biomass))
# p.mds <- p.mds + geom_label_repel(label = row.names(dat.mds),point.padding = 1)
p.mds <- p.mds + theme(legend.position = c(0.9,0.1))
print(p.mds)

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
p <- p + geom_abline(intercept = 0, slope= 1, lwd = 0.5, lty = 'dashed')
p <- p + geom_point(aes(size = lambda), color = 'red')
#p <- p + geom_text(label = row.names(dat.traits.cor), hjust = 0, vjust = 0)
p <- p + geom_label_repel(label = row.names(dat.traits.cor),
                          point.padding = 1)
p <- p + theme(legend.position = c(0.9,0.1))
print(p)
