

##GNDVI

all.prairie.spl <- split(all.prairie, all.prairie$monoTreeName) ##changed ndvi.mat to all.prairie to complie with 01.compileData.R
all.prairie.mean <- sapply(all.prairie.spl, function(x) {
  apply(x, 2, function(y) mean(as.numeric(y), rm.na = T))
  }) %>%
t %>%
as.data.frame

GNDVI.small <- all.prairie.mean[c('biomass.monocultures', 'pGNDVIvalues')]
names(GNDVI.small) <- c('Biomass', 'GNDVI')

## remove all NAs and -INF and prune tree
problem.rows <- which(apply(GNDVI.small, 1, function(x) any(c(-Inf, NaN) %in% x)))
if(length(problem.rows) > 0) GNDVI.small <- GNDVI.small[-problem.rows, ]

GNDVI.small <- apply(GNDVI.small, 2, function(x) (x - min(x)) / max(x - min(x)))
problem.rows <- which(apply(GNDVI.small, 1, sum, na.rm = T) < 0.30)
if(length(problem.rows) > 0) GNDVI.small <- GNDVI.small[-problem.rows, ]

tr.prairie.biomassPlot <- drop.tip(tr.prairie, names(problem.rows)) %>%
  multi2di
tr.prairie.biomassPlot$node.label[tr.prairie.biomassPlot$node.label %in% c('', 'NA')] <- NA
tr.prairie.biomassPlot$node.label <-
  c(rep(NA, length(tr.prairie.biomassPlot$tip.label)), tr.prairie.biomassPlot$node.label)
tr.prairie.biomassPlot$tip.label[tr.prairie.biomassPlot$tip.label == 'Symphyotrichum_novaeangliae'] <- "Symphyotrichum_novae-angliae"

pdf('../OUT/prairie.biomass.ALT_edit_GNDVI.pdf')
p <- ggtree(tr.prairie.biomassPlot
#            layout = 'fan',
#            open.anphylosignal(GNDVI.small[, tr.prairie.biomassPlot)gle = 15
          )
p <- p + geom_label(aes(x = branch), label = tr.prairie.biomassPlot$node.label, size = 2)
p <- gheatmap(p, data = GNDVI.small,
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
tr.prairie.biomass.K <- list(
  biomass = phylosignal(GNDVI.small[tr.prairie.phylosig$tip.label, 'Biomass'], tr.prairie.phylosig)[1,],
  GNDVI = phylosignal(GNDVI.small[tr.prairie.phylosig$tip.label, 'GNDVI'], tr.prairie.phylosig)[1,]
)

##GDVI2

GDVI2.small <- all.prairie.mean[c('biomass.monocultures', 'pGDVI2values')]
names(GDVI2.small) <- c('Biomass', 'GDVI2')

## remove all NAs and -INF and prune tree
problem.rows <- which(apply(GDVI2.small, 1, function(x) any(c(-Inf, NaN) %in% x)))
if(length(problem.rows) > 0) GDVI2.small <- GDVI2.small[-problem.rows, ]

GDVI2.small <- apply(GDVI2.small, 2, function(x) (x - min(x)) / max(x - min(x)))
problem.rows <- which(apply(GDVI2.small, 1, sum, na.rm = T) < 0.47)
if(length(problem.rows) > 0) GDVI2.small <- GDVI2.small[-problem.rows, ]

tr.prairie.biomassPlot <- drop.tip(tr.prairie, names(problem.rows)) %>%
  multi2di
tr.prairie.biomassPlot$node.label[tr.prairie.biomassPlot$node.label %in% c('', 'NA')] <- NA
tr.prairie.biomassPlot$node.label <-
  c(rep(NA, length(tr.prairie.biomassPlot$tip.label)), tr.prairie.biomassPlot$node.label)
tr.prairie.biomassPlot$tip.label[tr.prairie.biomassPlot$tip.label == 'Symphyotrichum_novaeangliae'] <- "Symphyotrichum_novae-angliae"

pdf('../OUT/prairie.biomass.ALT_edit_GDVI2.pdf')
p <- ggtree(tr.prairie.biomassPlot
#            layout = 'fan',
#            open.anphylosignal(GDVI2.small[, tr.prairie.biomassPlot)gle = 15
          )
p <- p + geom_label(aes(x = branch), label = tr.prairie.biomassPlot$node.label, size = 2)
p <- gheatmap(p, data = GDVI2.small,
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
tr.prairie.biomass.K <- list(
  biomass = phylosignal(GDVI2.small[tr.prairie.phylosig$tip.label, 'Biomass'], tr.prairie.phylosig)[1,],
  GDVI2 = phylosignal(GDVI2.small[tr.prairie.phylosig$tip.label, 'GDVI2'], tr.prairie.phylosig)[1,]
)

##Using spectra

spectra.small = all.prairie.mean[c('pNIRvalues', 'pREGvalues', 'pREDvalues', 'pGREvalues')]
problem.rows <- which(apply(spectra.small, 1, function(x) any(c(-Inf, NaN) %in% x)))
if(length(problem.rows) > 0) spectra.small <- spectra.small[-problem.rows, ]
spectra.small <- apply(spectra.small, 2, function(x) (x - min(x)) / max(x - min(x)))
if(length(problem.rows) > 0) spectra.small <- spectra.small[-problem.rows, ]

problem.rows <- which(apply(spectra.small, 1, function(x) any(c(-Inf, NaN) %in% x)))
if(length(problem.rows) > 0) spectra.small <- spectra.small[-problem.rows, ]

spectra.small <- apply(spectra.small, 2, function(x) (x - min(x)) / max(x - min(x)))
if(length(problem.rows) > 0) spectra.small <- spectra.small[-problem.rows, ]

tr.prairie.biomassPlot <- drop.tip(tr.prairie, names(problem.rows)) %>%     multi2di
tr.prairie.biomassPlot$node.label[tr.prairie.biomassPlot$node.label %in% c('', 'NA')] <- NA
tr.prairie.biomassPlot$node.label <-
     c(rep(NA, length(tr.prairie.biomassPlot$tip.label)), tr.prairie.biomassPlot$node.label)
tr.prairie.biomassPlot$tip.label[tr.prairie.biomassPlot$tip.label == 'Symphyotrichum_novaeangliae'] <- "Symphyotrichum_novae-angliae"
p <- ggtree(tr.prairie.biomassPlot
 )

p <- p + geom_label(aes(x = branch), label = tr.prairie.biomassPlot$node.label, size = 2)
p <- gheatmap(p, data = spectra.small,
               low = 'white', high = 'black',
               colnames_angle = 315,
               font.size = 2,
               width = 0.1,
               hjust = 0,
 )

#p <- p + theme(legend.position = c(0.05,0.9))
p <- p + theme(legend.position = 'none')
print(p)



list(
  NIR = phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, 'pNIRvalues'], tr.prairie.phylosig)[1,],
  REG = phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, 'pREGvalues'], tr.prairie.phylosig)[1,],
  RED = phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, 'pREDvalues'], tr.prairie.phylosig)[1,],
  GRE = phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, 'pGREvalues'], tr.prairie.phylosig)[1,],
  Biomass = phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, 'Biomass'], tr.prairie.phylosig)[1,],
  NDVI = phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, 'NDVI'], tr.prairie.phylosig)[1,],

  ) %>%
  do.call("rbind", .)

tr.prairie.phylosig <- tr.prairie.biomassPlot
tr.prairie.phylosig$node.label <- NULL
tr.prairie.biomass.K <- list(
  biomass = phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, 'Biomass'], tr.prairie.phylosig)[1,],
  NDVI = phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, 'NDVI'], tr.prairie.phylosig)[1,]
  ) %>%
  do.call("rbind", .)
