## Assumes you've already compiled data (script 01)
## v1: ahipp 2018-04-12
## v2: atuffin, sometime in May
## v3: ahipp, 2018-07-12. Edits include:
  ## -- removing all species excluded from 2017 analysis - DONE
  ## -- cleaning code - DONE
  ## -- add tip labels - DONE
  ## -- coloring major clades? - DONE

library(ape)
library(ggtree)
library(magrittr)
library(picante)

relabelBranches = TRUE

source('../SCRIPTS/00b.problemSpp.2017.R')
source('../SCRIPTS/999.gheatmap.mod.R') # to suppress legend selectively
spp.prob.2017 <- gsub('_', ' ', spp.prob.2017, fixed = T)

tr.prairie.biomassPlot <- tr.prairie
tr.prairie.biomassPlot$tip.label[which(tr.prairie.biomassPlot$tip.label == "Symphyotrichum_novaeangliae")] <-
  "Symphyotrichum_novae-angliae"
tr.prairie.biomassPlot$tip.label <- gsub('_', ' ', tr.prairie.biomassPlot$tip.label, fixed = T)

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

row.names(all.prairie.small) <- gsub('_', ' ', row.names(all.prairie.small), fixed = T)

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

#stop('TREE FIXING STOP')

tr.prairie.biomassPlot$node.label[tr.prairie.biomassPlot$node.label %in% c('', 'NA')] <- NA
tr.prairie.biomassPlot$node.label <-
  c(rep(NA, length(tr.prairie.biomassPlot$tip.label)), tr.prairie.biomassPlot$node.label)

tr.mrca <- mrca(tr.prairie.biomassPlot)
tr.prairie.biomassPlot <- groupClade(tr.prairie.biomassPlot,
                                     c(tr.mrca['Helianthus occidentalis', 'Ratibida pinnata'],
                                   tr.mrca['Lespedeza capitata', 'Desmanthus illinoensis'],
                                   tr.mrca['Bromus kalmii', 'Bouteloua curtipendula'],
                                   tr.mrca['Pycnanthemum virginianum', 'Physostegia virginiana']
                                   )
                                 )
if(relabelBranches) {
  attr(tr.prairie.biomassPlot, 'group') <- as.factor(c('All other lineages',
                                      'Asteraceae - Sunflower Family',
                                      'Fabaceae - Bean Family',
                                      'Poaceae - Grass Family',
                                      'Lamiaceae - Mint Family')[attr(tr.prairie.biomassPlot, 'group')]
                                      )
                                    }

pdf('../OUT/FIGURE.prairie.biomass.allSpectra.withColors.pdf')
p <- ggtree(tr.prairie.biomassPlot,
            aes(color=group)
          )
p <- p + scale_color_manual("Major plant families",
                            values = c('black',
                                        'orange',
                                        'yellow',
                                        'lightgreen',
                                        'blue')
                                        )
#p <- p + geom_label(aes(x = branch), label = tr.prairie.biomassPlot$node.label, size = 2)
p <- gheatmap(p, data = all.prairie.small,
              low = 'white', high = 'black',
              colnames_angle = 315,
              font.size = 1.8,
              width = 0.2,
              hjust = 0,
              )
p <- p + geom_tiplab(fontface='italic', size = 1.7,
                      offset = 40,
                      color = 'black')
p <- p + theme(legend.position = c(0.15, 0.88),
               legend.title = element_text(size = 9.5),
               legend.text = element_text(size = 8),
               legend.key.size = unit(0.35, 'cm'),
               legend.box.background = element_rect(color = NA)
              )

p <- p + ggplot2::xlim(c(0, 280))
print(p)
dev.off()

tr.prairie.phylosig <- tr.prairie.biomassPlot
tr.prairie.phylosig$node.label <- NULL
prairie.phylosignal <- lapply(names(all.prairie.small), function(x) {
  phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, x], tr.prairie.phylosig)[1,]
  }) %>%
  do.call('rbind', .)
# row.names(prairie.phylosignal) <- names(all.prairie.small)

prairie.lambda <- list(estimated = fitContinuous(tr.prairie.phylosig, all.prairie.small, model = 'lambda'),
                       zero = fitContinuous(rescale(tr.prairie.phylosig, 'lambda', 0), all.prairie.small)
                       )
prairie.phylosignal <- cbind(prairie.phylosignal,
                             lambda = sapply(prairie.lambda$estimated, function(x) x$opt$lambda),
                             L.ratio = 2 * (sapply(prairie.lambda$estimated, function(x) x$opt$lnL) -
                                           sapply(prairie.lambda$zero, function(x) x$opt$lnL)),
                             L.ratio.p = pchisq(2 * (sapply(prairie.lambda$estimated, function(x) x$opt$lnL) -
                                           sapply(prairie.lambda$zero, function(x) x$opt$lnL)),
                                           df = 1,
                                           lower.tail = F)
                                           )


write.csv(prairie.phylosignal, file = '../OUT/TABLE.phylosignal.csv')
