## first version of phylogenetic figs
## ahipp 2018-04-12
## assumes you've already read in data (script 00)

library(ape)
library(ggtree)
library(magrittr)
library(caper)

ndvi.mat$sp <- dat$blocks$monoTreeName[match(ndvi.mat$plot, dat$blocks$plot)]
ndvi.mat.spl <- split(ndvi.mat, ndvi.mat$sp)
ndvi.mat.mean <- sapply(ndvi.mat.spl, function(x) {
  apply(x, 2, function(y) mean(as.numeric(y), rm.na = T))
  }) %>%
t %>%
as.data.frame

ndvi.mat.small <- ndvi.mat.mean[c('biomass.monocultures', 'ndvi')]
names(ndvi.mat.small) <- c('Biomass', 'NDVI')
#for(i in 1:2) ndvi.mat.small[[i]] <- log(ndvi.mat.small[[i]])
#for(i in 1:2) ndvi.mat.small[[i]] <- sqrt(ndvi.mat.small[[i]])
## remove all NAs and -INF and prune tree
problem.rows <- which(apply(ndvi.mat.small, 1, function(x) any(c(-Inf, NaN) %in% x)))
if(length(problem.rows) > 0) ndvi.mat.small <- ndvi.mat.small[-problem.rows, ]

ndvi.mat.small <- apply(ndvi.mat.small, 2, function(x) (x - min(x)) / max(x - min(x)))
problem.rows <- which(apply(ndvi.mat.small, 1, sum, na.rm = T) < 0.10)
if(length(problem.rows) > 0) ndvi.mat.small <- ndvi.mat.small[-problem.rows, ]

tr.prairie.biomassPlot <- drop.tip(tr.prairie, names(problem.rows)) %>%
  multi2di
tr.prairie.biomassPlot$node.label[tr.prairie.biomassPlot$node.label %in% c('', 'NA')] <- NA
tr.prairie.biomassPlot$node.label <-
  c(rep(NA, length(tr.prairie.biomassPlot$tip.label)), tr.prairie.biomassPlot$node.label)
tr.prairie.biomassPlot$tip.label[tr.prairie.biomassPlot$tip.label == 'Symphyotrichum_novaeangliae'] <- "Symphyotrichum_novae-angliae"
## rescale to 0:1

pdf('../OUT/prairie.biomass.pdf')
p <- ggtree(tr.prairie.biomassPlot
#            layout = 'fan',
#            open.anphylosignal(ndvi.mat.small[, tr.prairie.biomassPlot)gle = 15
          )
p <- p + geom_label(aes(x = branch), label = tr.prairie.biomassPlot$node.label, size = 2)
p <- gheatmap(p, data = ndvi.mat.small,
              low = 'white', high = 'black',
              colnames_angle = 315,
              font.size = 2,
              width = 0.1,
              hjust = 0,
              )
p <- p + theme(legend.position = c(0.05,0.9))
print(p)
dev.off()

tr.prairie.phylosig <- tr.prairie.biomassPlot
tr.prairie.phylosig$node.label <- NULL
tr.prairie.biomass.K <- list(
  biomass = phylosignal(ndvi.mat.small[tr.prairie.phylosig$tip.label, 'Biomass'], tr.prairie.phylosig),
  NDVI = phylosignal(ndvi.mat.small[tr.prairie.phylosig$tip.label, 'NDVI'], tr.prairie.phylosig)
#  ln.biomass = phylosignal(log(ndvi.mat.small[tr.prairie.phylosig$tip.label, 'Biomass']), tr.prairie.phylosig),
#  ln.NDVI = phylosignal(log(ndvi.mat.small[tr.prairie.phylosig$tip.label, 'NDVI']), tr.prairie.phylosig)
  )
