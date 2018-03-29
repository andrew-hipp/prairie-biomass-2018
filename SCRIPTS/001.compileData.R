require(dplyr)

source("../SCRIPTS/00.readData.R")

dat.soil.cover = read.csv("../DATA/dat.blocksSoilCover.csv")
dat.compo = read.csv("../DATA/dat.composition.2017.csv")
dat.phylo.cover = read.csv("../DATA/dat.cover.diversity.2017.csv")
source("../DATA/plot.design.r")

plot = c(1:437)

all.prairie = as.data.frame(plot)
all.prairie = merge(all.prairie, dat.soil.cover, by = "plot")
names(all.prairie)[names(all.prairie)=="block_BS.mod"] <- "block"
names(all.prairie)[names(all.prairie)=="ndvi_v1"] <- "ndvi"
all.prairie$X = NULL
names(dat.compo)[names(dat.compo)=="X"] <- "plot"

all.prairie = merge(all.prairie, dat.phylo.cover, all.x = T)
names(all.prairie)[names(all.prairie)=="sp"] <- "plot.ID"
all.prairie$X = NULL

all.prairie = merge(all.prairie, ndvi.mat, all.x = T)
all.prairie$as.is = NULL