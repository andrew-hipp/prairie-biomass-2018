library(magrittr)
library(ggplot2)

source("../SCRIPTS/00.readData.R")
source("../DATA/plot.design.R")

cover.diversity = read.csv("../DATA/dat.cover.diversity.2017.csv")
soil.cover = read.csv("../DATA/dat.blocksSoilCover.csv")

treatment.plots=grep("_",plot.design$plotVector,value=T,fixed=T)
plot.id = strsplit(treatment.plots, "_", fixed = T) %>%
+     lapply(., head, 2) %>%
+     sapply(., paste, collapse = "_")

##Block A and replicates NDVI

plots.A <- soil.cover$plot[which(soil.cover$block_BS.mod == 'A')]
block.A <- plot.id[names(plot.id) %in% as.character(plots.A)]

block.A.ndvi <- soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.A))]
names(block.A.ndvi) <- soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.A))]

block.A.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.A)]), names(block.A))]
block.A.rep.ndvi = soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.A.rep))]
names(block.A.rep.ndvi) <-soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.A.rep))]

names(block.A.ndvi) <- plot.id[names(block.A.ndvi)]
names(block.A.rep.ndvi) <- plot.id[names(block.A.rep.ndvi)]

block.A.ndvi.test=t.test(block.A.ndvi, block.A.rep.ndvi, paired=T)
block.A.ndvi.test ##t.test of treatment plots in Block A against its replicates

##Block A and replicates biomass

block.A.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.A))]
names(block.A.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.A))]

block.A.rep.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.A.rep))]
names(block.A.rep.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.A.rep))]

names(block.A.biomass) = plot.id[names(block.A.biomass)]
names(block.A.rep.biomass) = plot.id[names(block.A.rep.biomass)]
block.A.rep.biomass = block.A.rep.biomass[-11] #Plot 67 biomass is missing from A, so counterpart is removed from A.rep

block.A.biomass.test = t.test(block.A.biomass, block.A.rep.biomass, paired = T)
block.A.biomass.test

##Block A A-horizon depth

layout(matrix(1:2, 2))
hist(soil.cover$AHOR_cm[which(soil.cover$block_BS.mod %in% LETTERS[1:3])], 20, main = 'W superblock', xlab = 'A horizon depth', xlim = range(20,45), ylim = range(0,25))
hist(soil.cover$AHOR_cm[which(soil.cover$block_BS.mod %in% LETTERS[1])], 20, main = 'Block A', xlab = 'A horizon depth', xlim = range(20,45), ylim = range(0,25))



##Block B and replicates NDVI

plots.B <- soil.cover$plot[which(soil.cover$block_BS.mod == 'B')]
block.B <- plot.id[names(plot.id) %in% as.character(plots.B)]

block.B.ndvi <- soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.B))]
names(block.B.ndvi) <- soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.B))]

block.B.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.B)]), names(block.B))]
block.B.rep.ndvi = soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.B.rep))]
names(block.B.rep.ndvi) <-soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.B.rep))]

names(block.B.ndvi) <- plot.id[names(block.B.ndvi)]
names(block.B.rep.ndvi) <- plot.id[names(block.B.rep.ndvi)]

block.B.ndvi.test=t.test(block.B.ndvi, block.B.rep.ndvi, paired=T)
block.B.ndvi.test ##t.test of treatment plots in Block B against its replicates

##Block B and replicates biomass

block.B.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.B))]
names(block.B.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.B))]

block.B.rep.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.B.rep))]
names(block.B.rep.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.B.rep))]

names(block.B.biomass) = plot.id[names(block.B.biomass)]
names(block.B.rep.biomass) = plot.id[names(block.B.rep.biomass)]

block.B.biomass.test = t.test(block.B.biomass, block.B.rep.biomass, paired = T)
block.B.biomass.test

##Block C and replicates NDVI

plots.C <- soil.cover$plot[which(soil.cover$block_BS.mod == 'C')]
block.C <- plot.id[names(plot.id) %in% as.character(plots.C)]

block.C.ndvi <- soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.C))]
names(block.C.ndvi) <- soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.C))]

block.C.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.C)]), names(block.C))]
block.C.rep.ndvi = soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.C.rep))]
names(block.C.rep.ndvi) <-soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.C.rep))]

names(block.C.ndvi) <- plot.id[names(block.C.ndvi)]
names(block.C.rep.ndvi) <- plot.id[names(block.C.rep.ndvi)]

block.C.ndvi.test=t.test(block.C.ndvi, block.C.rep.ndvi, paired=T)
block.C.ndvi.test ##t.test of treatment plots in Block C against its replicates

##Block C and replicates biomass

block.C.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.C))]
names(block.C.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.C))]

block.C.rep.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.C.rep))]
names(block.C.rep.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.C.rep))]

names(block.C.biomass) = plot.id[names(block.C.biomass)]
names(block.C.rep.biomass) = plot.id[names(block.C.rep.biomass)]

block.C.biomass.test = t.test(block.C.biomass, block.C.rep.biomass, paired = T)
block.C.biomass.test

##Block D and replicates NDVI

plots.D <- soil.cover$plot[which(soil.cover$block_BS.mod == 'D')]
block.D <- plot.id[names(plot.id) %in% as.character(plots.D)]

block.D.ndvi <- soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.D))]
names(block.D.ndvi) <- soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.D))]

block.D.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.D)]), names(block.D))]
block.D.rep.ndvi = soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.D.rep))]
names(block.D.rep.ndvi) <-soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.D.rep))]

names(block.D.ndvi) <- plot.id[names(block.D.ndvi)]
names(block.D.rep.ndvi) <- plot.id[names(block.D.rep.ndvi)]

block.D.ndvi.test=t.test(block.D.ndvi, block.D.rep.ndvi, paired=T)
block.D.ndvi.test ##t.test of treatment plots in Block D against its replicates

##Block D and replicates biomass

block.D.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.D))]
names(block.D.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.D))]

block.D.rep.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.D.rep))]
names(block.D.rep.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.D.rep))]

names(block.D.biomass) = plot.id[names(block.D.biomass)]
names(block.D.rep.biomass) = plot.id[names(block.D.rep.biomass)]

block.D.biomass.test = t.test(block.D.biomass, block.D.rep.biomass, paired = T)
block.D.biomass.test


##Block E and replicates NDVI

plots.E <- soil.cover$plot[which(soil.cover$block_BS.mod == 'E')]
block.E <- plot.id[names(plot.id) %in% as.character(plots.E)]

block.E.ndvi <- soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.E))]
names(block.E.ndvi) <- soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.E))]

block.E.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.E)]), names(block.E))]
block.E.rep.ndvi = soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.E.rep))]
names(block.E.rep.ndvi) <-soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.E.rep))]

names(block.E.ndvi) <- plot.id[names(block.E.ndvi)]
names(block.E.rep.ndvi) <- plot.id[names(block.E.rep.ndvi)]

block.E.ndvi.test=t.test(block.E.ndvi, block.E.rep.ndvi, paired=T)
block.E.ndvi.test ##t.test of treatment plots in Block E against its replicates

##Block E and replicates biomass

block.E.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.E))]
names(block.E.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.E))]

block.E.rep.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.E.rep))]
names(block.E.rep.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.E.rep))]

names(block.E.biomass) = plot.id[names(block.E.biomass)]
names(block.E.rep.biomass) = plot.id[names(block.E.rep.biomass)]

block.E.biomass.test = t.test(block.E.biomass, block.E.rep.biomass, paired = T)
block.E.biomass.test

##Block F and replicates NDVI

plots.F <- soil.cover$plot[which(soil.cover$block_BS.mod == 'F')]
block.F <- plot.id[names(plot.id) %in% as.character(plots.F)]

block.F.ndvi <- soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.F))]
names(block.F.ndvi) <- soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.F))]

block.F.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.F)]), names(block.F))]
block.F.rep.ndvi = soil.cover$ndvi_v1[which(as.character(soil.cover$plot) %in% names(block.F.rep))]
names(block.F.rep.ndvi) <-soil.cover$plot[which(as.character(soil.cover$plot) %in% names(block.F.rep))]

names(block.F.ndvi) <- plot.id[names(block.F.ndvi)]
names(block.F.rep.ndvi) <- plot.id[names(block.F.rep.ndvi)]

block.F.ndvi.test=t.test(block.F.ndvi, block.F.rep.ndvi, paired=T)
block.F.ndvi.test ##t.test of treatment plots in Block F against its replicates

##Block F and replicates biomass

block.F.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.F))]
names(block.F.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.F))]

block.F.rep.biomass = ndvi.mat$biomass.tmts[which(as.character(ndvi.mat$plot) %in% names(block.F.rep))]
names(block.F.rep.biomass) = ndvi.mat$plot[which(as.character(ndvi.mat$plot) %in% names(block.F.rep))]

names(block.F.biomass) = plot.id[names(block.F.biomass)]
names(block.F.rep.biomass) = plot.id[names(block.F.rep.biomass)]
block.F.biomass = block.F.biomass[-10] #biomass is missing from F.rep, so counterpart is removed from F

block.F.biomass.test = t.test(block.F.biomass, block.F.rep.biomass, paired = T)
block.F.biomass.test
