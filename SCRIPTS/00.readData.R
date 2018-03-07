## read data for prairie biomass and ndvi study
## ahipp@mortonarb.org
## 2018-03-06

library(ape)
library(magrittr)
library(ggplot2)

dat.headers.plugs <- c("plot", "type", "sp", "biomass.total", "b01", "b02", "b03", "b04",
"b05", "b06", "b07", "b08", "b09", "b10", "b11", "b12",
"b13", "b14", "b15", "groundLeaves", "collector",
"date")
dat.headers.mono <- c("plot", "type", "sp", "biomass.total",
"weightTotal", "weightBag", "gl", "glTotal", "glBag",
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
"collector",
"date")

tr.prairie <- read.tree('../DATA/tree.pruned.tre')

dat <- list(
    blocks = read.csv('../DATA/dat.blocksSoilCover.csv', row.names = 1, as.is = T),
    composition = read.csv('../DATA/dat.composition.2017.csv', row.names = 1, as.is = T),
    plotMeta = read.csv('../DATA/dat.cover.diversity.2017.csv', row.names = 1, as.is = T),
    biomass.raw = read.delim('../DATA/Biomass_Datasheet-2018-12-14v2-AHedit.tsv', as.is = T)
    )

spp.trans <- read.delim('../DATA/sp.trans.tsv.txt', as.is = T)
ndvi <- read.table('../DATA/plotNDVI', header = T)

## a little cleanup on two names
names(dat$composition) <- gsub('[.-]', '', names(dat$composition))
tr.prairie$tip.label <- gsub('[.-]', '', tr.prairie$tip.label)
dat$bin <- dat$composition
dat$bin[!is.na(dat$bin)] <- 1
dat$bin[is.na(dat$bin)] <- 0

names(dat$biomass.raw) <- dat.headers.plugs
dat$biomass.raw <- dat$biomass.raw[!dat$biomass.raw$sp == 'CONTROL', ]

dat$plugTrans <- dat$biomass.raw[dat$biomass.raw$type == 'Plug', ]

## this here is a fix!

dat$spp <- dat$plugTrans[, grep('b0|b1', names(dat$plugTrans), value = T)] %>%
 unlist %>%
 as.character %>%
 unique %>%
 sort %>%
 .[. != '']


dat$plugs <- dat$biomass.raw[which(dat$biomass.raw$type == 'Plug') + 1, ] %>%
  apply(., 2, as.numeric) %>%
  as.data.frame
dat$plugs[, c('plot', 'sp')] <- dat$biomass.raw[dat$biomass.raw$type == 'Plug', c('plot', 'sp')]
dat$plugs$phy <- substr(dat$plugs$sp, 1, 1)
dat$plugs$trt <- substr(dat$plugs$sp, 2, 2)
dat$plugs.mat <- matrix(NA, nrow = dim(dat$plugs)[1],
                            ncol = length(dat$spp),
                            dimnames = list(as.character(dat$plugs$plot),
                                            dat$spp))
for(i in seq(dim(dat$plugs)[1])) {
  if(dat$plugs[i, 'sp'] == 'CONTROL') next
  print(i)
  head.temp <- dat$plugTrans[i, grep('b0|b1', names(dat$plugTrans))] %>%
    as.character
  dat$plugs.mat[i, head.temp] <- as.numeric(dat$plugs[i, grep('b0|b1', names(dat$plugs))])
}

dat$mono <- dat$biomass.raw[which(dat$biomass.raw$type == 'Mono'), ] %>%
  apply(., 2, as.numeric) %>%
  as.data.frame
names(dat$mono) <- dat.headers.mono
dat$mono[, c('plot', 'type', 'sp')] <- dat$biomass.raw[dat$biomass.raw$type == 'Mono', c('plot', 'type', 'sp')]
dat$mono$noGL <- dat$mono$biomass.total - ifelse(is.na(dat$mono$gl), 0, dat$mono$gl)

mono.scalar <- 15/4 # accounts for the fact that 4 of 15 plants were collected from each monoculture
mono.rows <- match(ndvi$plot, dat$mono$plot)
tmt.rows <- match(ndvi$plot, dat$plugs$plot)
ndvi.mat <- data.frame(plot = ndvi$plot,
              biomass.monocultures = dat$mono[mono.rows, 'biomass.total'] * mono.scalar,
              biomass.monocultures.noGL = dat$mono[mono.rows, 'noGL'] * mono.scalar,
              biomass.tmts = dat$plugs[tmt.rows, 'biomass.total'],
              biomass.tmts.noGL = dat$plugs[tmt.rows, 'biomass.total'] -
                                  dat$plugs[tmt.rows, 'groundLeaves'],
              phy.div = dat$plugs[tmt.rows, 'phy'],
              trait.div = dat$plugs[tmt.rows, 'trt'],
              ndvi = ndvi$ndvi,
              as.is = T)

ndvi.mat$biomass.all <- apply(ndvi.mat[, c('biomass.monocultures', 'biomass.tmts')],
                              1, sum, na.rm = T)
ndvi.mat$biomass.all[which(apply(head(ndvi.mat[, c('biomass.monocultures', 'biomass.tmts')], 15),1,function(x) sum(is.na(x))) == 2)] <- NA


ndvi.mat$'Plot.category' = NA
ndvi.mat$'Plot.category'[which(!is.na(tmt.rows))] = "Treatment"
ndvi.mat$'Plot.category'[which(!is.na(mono.rows))] = "Monoculture"
ndvi.mat$Plot.category <- factor(ndvi.mat$Plot.category, levels = c('Treatment',
                                                                    'Monoculture'))

ndvi.mat <- ndvi.mat[-ndvi.mat$plot[is.na(ndvi.mat$Plot.category)], ]
