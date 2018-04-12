## first version of phylogenetic figs
## ahipp 2018-04-12
## assumes you've already read in data (script 00)

library(ape)
library(ggtree)
library(magrittr)

ndvi.mat$sp <- dat$blocks$monoTreeName[match(ndvi.mat$plot, dat$blocks$plot)]
ndvi.mat.spl <- split(ndvi.mat, ndvi.mat$sp)
ndvi.mat.mean <- sapply(ndvi.mat.spl, function(x) {
  apply(x, 2, function(y) mean(as.numeric(y), rm.na = T))
  }) %>%
t %>%
scale %>%
as.data.frame

p <- ggtree(tr.prairie, layout = 'circular')
p <- gheatmap(p, data = ndvi.mat.mean[c('biomass.monocultures', 'ndvi')])
print(p)
