## Assumes you've already compiled data (script 01)
## v1: ahipp 2018-04-12
## v2: atuffin, sometime in May
## v3: ahipp, 2018-07-12. Edits include:
  ## -- removing all species excluded from 2017 analysis
  ## -- cleaning code
  ## -- coloring major clades?

library(ape)
library(ggtree)
library(magrittr)
library(picante)

source('../SCRIPTS/00b.problemSpp.2017.R')

tr.prairie.biomassPlot <- tr.prairie
tr.prairie.biomassPlot$tip.label[which(tr.prairie.biomassPlot$tip.label == "Symphyotrichum_novaeangliae")] <-
  "Symphyotrichum_novae-angliae"

##NDVI

all.prairie.spl <- split(all.prairie, all.prairie$monoTreeName) ##changed ndvi.mat to all.prairie to complie with 01.compileData.R
all.prairie.mean <- sapply(all.prairie.spl, function(x) {
  apply(x, 2, function(y) mean(as.numeric(y), rm.na = T))
  }) %>%
  t %>%
  as.data.frame

all.prairie.small <- all.prairie.mean[c('biomass.monocultures',
                                        'pNDVIvalues',
                                        'pGNDVIvalues',
                                        'pGDVI2values',
                                        'pNIRvalues',
                                        'pREGvalues',
                                        'pREDvalues',
                                        'pGREvalues')]

names(all.prairie.small) <- c('Biomass',
                              'NDVI',
                              'GNDVI',
                              'GDVI2',
                              'pNIR',
                              'pREG',
                              'pRED',
                              'pGRE')

message('CHECKING FOR 2017 PROBLEM SPECIES')
if(any(!spp.prob.2017 %in% row.names(all.prairie.small))) {
  message("... the following problem spp were not found:")
  print(setdiff(spp.prob.2017, row.names(all.prairie.small)))
  } else
  message("... and all are excluded from all.prairie.small")

all.prairie.small <- all.prairie.small[which(!row.names(all.prairie.small) %in% spp.prob.2017), ]

## remove all NAs and -INF and prune tree
problem.rows <- which(apply(all.prairie.small, 1, function(x) any(c(-Inf, NaN) %in% x)))
if(length(problem.rows) > 0) all.prairie.small <- all.prairie.small[-problem.rows, ]

all.prairie.small <- apply(all.prairie.small, 2, function(x) (x - min(x)) / max(x - min(x))) %>%
  as.data.frame
if(length(problem.rows) > 0) all.prairie.small <- all.prairie.small[-problem.rows, ]

tr.prairie.biomassPlot <- drop.tip(tr.prairie.biomassPlot, which(!tr.prairie.biomassPlot$tip.label %in% row.names(all.prairie.small))) %>%
  multi2di
tr.prairie.biomassPlot$node.label[tr.prairie.biomassPlot$node.label %in% c('', 'NA')] <- NA
tr.prairie.biomassPlot$node.label <-
  c(rep(NA, length(tr.prairie.biomassPlot$tip.label)), tr.prairie.biomassPlot$node.label)


  pdf('../OUT/prairie.biomass.allSpectra.pdf')
  p <- ggtree(tr.prairie.biomassPlot
  #            layout = 'fan',
  #            open.anphylosignal(all.prairie.small[, tr.prairie.biomassPlot)gle = 15
            )
  p <- p + geom_label(aes(x = branch), label = tr.prairie.biomassPlot$node.label, size = 2)
  p <- gheatmap(p, data = all.prairie.small,
                low = 'white', high = 'black',
                colnames_angle = 315,
                font.size = 2,
                width = 0.1,
                hjust = 0,
                )

  #p <- p + theme(legend.position = c(0.05,0.9))
  p <- p + theme(legend.position = 'none')
  print(p)
  dev.off()


tr.prairie.phylosig <- tr.prairie.biomassPlot
tr.prairie.phylosig$node.label <- NULL
tr.prairie.phylosignal.K <- sapply(names(all.prairie.small), function(x) {
  phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, x], tr.prairie.phylosig)[1,]
  }) %>% t %>%
  write.csv(file = '../OUT/TABLE.phylosignal.csv')
