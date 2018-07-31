library(magrittr)
library(ggplot2)
library(devtools)
install_github("ggbiplot", "vqv")
library(plyr)
library(dplyr)

source("../SCRIPTS/00.readData.R")
source("../SCRIPTS/01.compileData.R")
source("../DATA/plot.design.R")

cover.diversity = read.csv("../DATA/dat.cover.diversity.2017.csv")
soil.cover = read.csv("../DATA/dat.blocksSoilCover.csv")

treatment.plots=grep("_",plot.design$plotVector,value=T,fixed=T)
plot.id = strsplit(treatment.plots, "_", fixed = T) %>%
lapply(., head, 2) %>%
sapply(., paste, collapse = "_")

##Block A and replicates NDVI

plots.A <- all.prairie$plot[which(all.prairie$block == 'A')]
block.A <- plot.id[names(plot.id) %in% as.character(plots.A)]

block.A.ndvi <- all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.A))]
names(block.A.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.A))]

block.A.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.A)]), names(block.A))]
block.A.rep.ndvi = all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.A.rep))]
names(block.A.rep.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.A.rep))]

names(block.A.ndvi) <- plot.id[names(block.A.ndvi)]
names(block.A.rep.ndvi) <- plot.id[names(block.A.rep.ndvi)]

block.A.ndvi.test=t.test(block.A.ndvi, block.A.rep.ndvi, paired=T)
block.A.ndvi.test ##t.test of treatment plots in Block A against its replicates

##bA: GNDVI t.test

block.A.gndvi <- all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.A))]
names(block.A.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.A))]

block.A.rep.gndvi = all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.A.rep))]
names(block.A.rep.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.A.rep))]

names(block.A.gndvi) <- plot.id[names(block.A.gndvi)]
names(block.A.rep.gndvi) <- plot.id[names(block.A.rep.gndvi)]

block.A.gndvi.test=t.test(block.A.gndvi, block.A.rep.gndvi, paired=T)
block.A.gndvi.test

##bA GDVI2 t.test

block.A.gdvi2 <- all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.A))]
names(block.A.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.A))]

block.A.rep.gdvi2 = all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.A.rep))]
names(block.A.rep.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.A.rep))]

names(block.A.gdvi2) <- plot.id[names(block.A.gdvi2)]
names(block.A.rep.gdvi2) <- plot.id[names(block.A.rep.gdvi2)]

block.A.gdvi2.test=t.test(block.A.gdvi2, block.A.rep.gdvi2, paired=T)
block.A.gdvi2.test

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

##Block A vs replicate environmental factors

block.A.envir = data.frame(plots.app, all.prairie$AHOR_cm[which(as.character(all.prairie$plot) %in% plots.A)])
block.A.envir = data.frame(block.A.envir, all.prairie$pH[which(as.character(all.prairie$plot) %in% plots.A)])
block.A.envir = data.frame(block.A.envir, all.prairie$fWAS[which(as.character(all.prairie$plot) %in% plots.A)])
block.A.envir = data.frame(block.A.envir, all.prairie$EC[which(as.character(all.prairie$plot) %in% plots.A)])
block.A.envir = data.frame(block.A.envir, all.prairie$GSM[which(as.character(all.prairie$plot) %in% plots.A)])
block.A.envir = data.frame(block.A.envir, all.prairie$LOI[which(as.character(all.prairie$plot) %in% plots.A)])

block.A.envir = block.A.envir[-c(1)]
colnames(block.A.envir) <- c("AHOR_cm", "pH", "fWAS", "EC", "GSM", "LOI")
block.A.envir$block = "A"

block.repA.envir = data.frame(block.A.rep, all.prairie$AHOR_cm[which(as.character(all.prairie$plot) %in% names(block.A.rep))])
block.repA.envir = data.frame(block.repA.envir, all.prairie$pH[which(as.character(all.prairie$plot) %in% names(block.A.rep))])
block.repA.envir = data.frame(block.repA.envir, all.prairie$fWAS[which(as.character(all.prairie$plot) %in% names(block.A.rep))])
block.repA.envir = data.frame(block.repA.envir, all.prairie$EC[which(as.character(all.prairie$plot) %in% names(block.A.rep))])
block.repA.envir = data.frame(block.repA.envir, all.prairie$GSM[which(as.character(all.prairie$plot) %in% names(block.A.rep))])
block.repA.envir = data.frame(block.repA.envir, all.prairie$LOI[which(as.character(all.prairie$plot) %in% names(block.A.rep))])
block.repA.envir = block.repA.envir[-c(1)]
colnames(block.repA.envir) <- c("AHOR_cm", "pH", "fWAS", "EC", "GSM", "LOI")
block.repA.envir$block = "A replicate"

A.envir = merge(block.A.envir, block.repA.envir, all.y = T, all.x = T)

log.envir.A = log(A.envir[1:6])
View(log.envir.A)
pca.envir.A = prcomp(log.envir.A, center = TRUE, scale. = TRUE)
gg.pca.envir.A <- ggbiplot(pca.envir.A, obs.scale = 1, var.scale = 1, groups = A.envir[,7], ellipse = TRUE, circle = TRUE)
gg.pca.envir.A = gg.pca.envir.A + scale_color_discrete(name = 'Block')
print(gg.pca.envir.A)

##Block B and replicates NDVI

plots.B <- all.prairie$plot[which(all.prairie$block == 'B')]
block.B <- plot.id[names(plot.id) %in% as.character(plots.B)]

block.B.ndvi <- all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.B))]
names(block.B.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.B))]

block.B.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.B)]), names(block.B))]
block.B.rep.ndvi = all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.B.rep))]
names(block.B.rep.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.B.rep))]

names(block.B.ndvi) <- plot.id[names(block.B.ndvi)]
names(block.B.rep.ndvi) <- plot.id[names(block.B.rep.ndvi)]

block.B.ndvi.test=t.test(block.B.ndvi, block.B.rep.ndvi, paired=T)
block.B.ndvi.test ##t.test of treatment plots in Block B against its replicates

##bB: GNDVI t.test

block.B.gndvi <- all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.B))]
names(block.B.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.B))]

block.B.rep.gndvi = all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.B.rep))]
names(block.B.rep.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.B.rep))]

names(block.B.gndvi) <- plot.id[names(block.B.gndvi)]
names(block.B.rep.gndvi) <- plot.id[names(block.B.rep.gndvi)]

block.B.gndvi.test=t.test(block.B.gndvi, block.B.rep.gndvi, paired=T)
block.B.gndvi.test

##bB GDVI2 t.test

block.B.gdvi2 <- all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.B))]
names(block.B.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.B))]

block.B.rep.gdvi2 = all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.B.rep))]
names(block.B.rep.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.B.rep))]

names(block.B.gdvi2) <- plot.id[names(block.B.gdvi2)]
names(block.B.rep.gdvi2) <- plot.id[names(block.B.rep.gdvi2)]

block.B.gdvi2.test=t.test(block.B.gdvi2, block.B.rep.gdvi2, paired=T)
block.B.gdvi2.test

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

plots.C <- all.prairie$plot[which(all.prairie$block == 'C')]
block.C <- plot.id[names(plot.id) %in% as.character(plots.C)]

block.C.ndvi <- all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.C))]
names(block.C.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.C))]

block.C.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.C)]), names(block.C))]
block.C.rep.ndvi = all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.C.rep))]
names(block.C.rep.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.C.rep))]

names(block.C.ndvi) <- plot.id[names(block.C.ndvi)]
names(block.C.rep.ndvi) <- plot.id[names(block.C.rep.ndvi)]

block.C.ndvi.test=t.test(block.C.ndvi, block.C.rep.ndvi, paired=T)
block.C.ndvi.test ##t.test of treatment plots in Block C against its replicates

##bB GNDVI: t.test

block.C.gndvi <- all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.C))]
names(block.C.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.C))]

block.C.rep.gndvi = all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.C.rep))]
names(block.C.rep.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.C.rep))]

names(block.C.gndvi) <- plot.id[names(block.C.gndvi)]
names(block.C.rep.gndvi) <- plot.id[names(block.C.rep.gndvi)]

block.C.gndvi.test=t.test(block.C.gndvi, block.C.rep.gndvi, paired=T)
block.C.gndvi.test

##bC GDVI2 t.test

block.C.gdvi2 <- all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.C))]
names(block.C.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.C))]

block.C.rep.gdvi2 = all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.C.rep))]
names(block.C.rep.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.C.rep))]

names(block.C.gdvi2) <- plot.id[names(block.C.gdvi2)]
names(block.C.rep.gdvi2) <- plot.id[names(block.C.rep.gdvi2)]

block.C.gdvi2.test=t.test(block.C.gdvi2, block.C.rep.gdvi2, paired=T)
block.C.gdvi2.test

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

plots.D <- all.prairie$plot[which(all.prairie$block == 'D')]
block.D <- plot.id[names(plot.id) %in% as.character(plots.D)]

block.D.ndvi <- all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.D))]
names(block.D.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.D))]

block.D.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.D)]), names(block.D))]
block.D.rep.ndvi = all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.D.rep))]
names(block.D.rep.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.D.rep))]

names(block.D.ndvi) <- plot.id[names(block.D.ndvi)]
names(block.D.rep.ndvi) <- plot.id[names(block.D.rep.ndvi)]

block.D.ndvi.test=t.test(block.D.ndvi, block.D.rep.ndvi, paired=T)
block.D.ndvi.test ##t.test of treatment plots in Block D against its replicates

##bD GNDVI t.test

block.D.gndvi <- all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.D))]
names(block.D.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.D))]

block.D.rep.gndvi = all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.D.rep))]
names(block.D.rep.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.D.rep))]

names(block.D.gndvi) <- plot.id[names(block.D.gndvi)]
names(block.D.rep.gndvi) <- plot.id[names(block.D.rep.gndvi)]

block.D.gndvi.test=t.test(block.D.gndvi, block.D.rep.gndvi, paired=T)
block.D.gndvi.test

##bD GDVI2 t.test

block.D.gdvi2 <- all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.D))]
names(block.D.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.D))]

block.D.rep.gdvi2 = all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.D.rep))]
names(block.D.rep.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.D.rep))]

names(block.D.gdvi2) <- plot.id[names(block.D.gdvi2)]
names(block.D.rep.gdvi2) <- plot.id[names(block.D.rep.gdvi2)]

block.D.gdvi2.test=t.test(block.D.gdvi2, block.D.rep.gdvi2, paired=T)
block.D.gdvi2.test

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

plots.E <- all.prairie$plot[which(all.prairie$block == 'E')]
block.E <- plot.id[names(plot.id) %in% as.character(plots.E)]

block.E.ndvi <- all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.E))]
names(block.E.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.E))]

block.E.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.E)]), names(block.E))]
block.E.rep.ndvi = all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.E.rep))]
names(block.E.rep.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.E.rep))]

names(block.E.ndvi) <- plot.id[names(block.E.ndvi)]
names(block.E.rep.ndvi) <- plot.id[names(block.E.rep.ndvi)]

block.E.ndvi.test=t.test(block.E.ndvi, block.E.rep.ndvi, paired=T)
block.E.ndvi.test ##t.test of treatment plots in Block E against its replicates

##bE GNDVI t.test

block.E.gndvi <- all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.E))]
names(block.E.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.E))]

block.E.rep.gndvi = all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.E.rep))]
names(block.E.rep.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.E.rep))]

names(block.E.gndvi) <- plot.id[names(block.E.gndvi)]
names(block.E.rep.gndvi) <- plot.id[names(block.E.rep.gndvi)]

block.E.gndvi.test=t.test(block.E.gndvi, block.E.rep.gndvi, paired=T)
block.E.gndvi.test

##bE GDVI2 t.test

block.E.gdvi2 <- all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.E))]
names(block.E.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.E))]

block.E.rep.gdvi2 = all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.E.rep))]
names(block.E.rep.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.E.rep))]

names(block.E.gdvi2) <- plot.id[names(block.E.gdvi2)]
names(block.E.rep.gdvi2) <- plot.id[names(block.E.rep.gdvi2)]

block.E.gdvi2.test=t.test(block.E.gdvi2, block.E.rep.gdvi2, paired=T)
block.E.gdvi2.test

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

plots.F <- all.prairie$plot[which(all.prairie$block == 'F')]
block.F <- plot.id[names(plot.id) %in% as.character(plots.F)]

block.F.ndvi <- all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.F))]
names(block.F.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.F))]

block.F.rep <- plot.id[setdiff(names(plot.id[which(plot.id %in% block.F)]), names(block.F))]
block.F.rep.ndvi = all.prairie$pNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.F.rep))]
names(block.F.rep.ndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.F.rep))]

names(block.F.ndvi) <- plot.id[names(block.F.ndvi)]
names(block.F.rep.ndvi) <- plot.id[names(block.F.rep.ndvi)]

block.F.ndvi.test=t.test(block.F.ndvi, block.F.rep.ndvi, paired=T)
block.F.ndvi.test ##t.test of treatment plots in Block F against its replicates

##bF GNDVI t.test

block.F.gndvi <- all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.F))]
names(block.F.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.F))]

block.F.rep.gndvi = all.prairie$pGNDVIvalues[which(as.character(all.prairie$plot) %in% names(block.F.rep))]
names(block.F.rep.gndvi) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.F.rep))]

names(block.F.gndvi) <- plot.id[names(block.F.gndvi)]
names(block.F.rep.gndvi) <- plot.id[names(block.F.rep.gndvi)]

block.F.gndvi.test=t.test(block.F.gndvi, block.F.rep.gndvi, paired=T)
block.F.gndvi.test

##bF GDVI2 t.test

block.F.gdvi2 <- all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.F))]
names(block.F.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.F))]

block.F.rep.gdvi2 = all.prairie$pGDVI2values[which(as.character(all.prairie$plot) %in% names(block.F.rep))]
names(block.F.rep.gdvi2) <- all.prairie$plot[which(as.character(all.prairie$plot) %in% names(block.F.rep))]

names(block.F.gdvi2) <- plot.id[names(block.F.gdvi2)]
names(block.F.rep.gdvi2) <- plot.id[names(block.F.rep.gdvi2)]

block.F.gdvi2.test=t.test(block.F.gdvi2, block.F.rep.gdvi2, paired=T)
block.F.gdvi2.test

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

##PCA of environmental factors

log.envir = log(all.prairie [c(7,11,12,13,14,15)])
log.envir = log.envir[-c(409), ] ##AHOR_cm missing from plot 409
blocks = all.prairie[3]
blocks = data.frame(blocks[-409, ]) ##AHOR_cm missing from plot 409

pca.envir = prcomp(log.envir, center = TRUE, scale. = TRUE)

library(devtools)
install_github("ggbiplot", "vqv")
library(plyr)
library(dplyr)

gg.pca.envir <- ggbiplot(pca.envir, obs.scale = 1, var.scale = 1, groups = blocks[,1], ellipse = TRUE, circle = TRUE)
gg.pca.envir = gg.pca.envir + scale_color_discrete(name = 'Block')
print(gg.pca.envir)

##Boxplot of blocks NDVI vs block/replicates

all.block.ndvi = data.frame(block.A.ndvi, block.A.rep.ndvi,block.B.ndvi, block.B.rep.ndvi,block.C.ndvi, block.C.rep.ndvi,block.D.ndvi, block.D.rep.ndvi,block.E.ndvi, block.E.rep.ndvi,block.F.ndvi, block.F.rep.ndvi)

allstack.block.ndvi = stack(all.block.ndvi)

allstack.block.ndvi$block = 0
allstack.block.ndvi$rep = 0

allstack.block.ndvi$rep[1:24] = "block"
allstack.block.ndvi$rep[49:72] = "block"
allstack.block.ndvi$rep[97:120] = "block"
allstack.block.ndvi$rep[145:168] = "block"
allstack.block.ndvi$rep[193:216] = "block"
allstack.block.ndvi$rep[241:264] = "block"

allstack.block.ndvi$rep[which(allstack.block.ndvi$rep == "0")] = "replicate"

allstack.block.ndvi$block[1:48] = "A"
allstack.block.ndvi$block[49:96] = "B"
allstack.block.ndvi$block[97:144] = "C"
allstack.block.ndvi$block[145:192] = "D"
allstack.block.ndvi$block[193:240] = "E"
allstack.block.ndvi$block[241:288] = "F"

names(allstack.block.ndvi)[names(allstack.block.ndvi) == 'values'] <- 'NDVI'

box.blockrep.ndvi = ggplot(allstack.block.ndvi, aes(x=block, y=NDVI, fill=rep)) + geom_boxplot(position=position_dodge(.9))

##Boxplot of biomass vs blocks

all.block.biomass = data.frame(block.A.biomass)
allstack.block.biomass = stack(all.block.biomass)

stack.B = stack(data.frame(block.B.biomass))
allstack.block.biomass = merge(allstack.block.biomass, stack.B, all.x = T, all.y =T, sort = F)

stack.C = stack(data.frame(block.C.biomass))
allstack.block.biomass = merge(allstack.block.biomass, stack.C, all.x = T, all.y =T, sort = F)

stack.D = stack(data.frame(block.D.biomass))
allstack.block.biomass = merge(allstack.block.biomass, stack.D, all.x = T, all.y =T, sort = F)

stack.E = stack(data.frame(block.E.biomass))
allstack.block.biomass = merge(allstack.block.biomass, stack.E, all.x = T, all.y =T, sort = F)

stack.F = stack(data.frame(block.F.biomass))
allstack.block.biomass = merge(allstack.block.biomass, stack.F, all.x = T, all.y =T, sort = F)

names(allstack.block.biomass)[names(allstack.block.biomass) == 'values'] <- 'Biomass'
names(allstack.block.biomass)[names(allstack.block.biomass) == 'ind'] <- 'Block'

box.block.biomass = ggplot(allstack.block.biomass, aes(x=Block, y=Biomass, fill=Block)) + geom_boxplot()

names(allstack.block.biomass)[names(allstack.block.biomass) == 'Biomass'] <- ‘values’
names(allstack.block.biomass)[names(allstack.block.biomass) == 'Block'] <- ‘ind’

stack.A.rep = stack(data.frame(block.A.rep.biomass))
allstack.blockrep.biomass = merge(allstack.block.biomass, stack.A.rep, all.x = T, all.y =T, sort = F)

stack.B.rep = stack(data.frame(block.B.rep.biomass))
allstack.blockrep.biomass = merge(allstack.blockrep.biomass, stack.B.rep, all.x = T, all.y =T, sort = F)

stack.C.rep = stack(data.frame(block.C.rep.biomass))
allstack.blockrep.biomass = merge(allstack.blockrep.biomass, stack.C.rep, all.x = T, all.y =T, sort = F)

stack.D.rep = stack(data.frame(block.D.rep.biomass))
allstack.blockrep.biomass = merge(allstack.blockrep.biomass, stack.D.rep, all.x = T, all.y =T, sort = F)

stack.E.rep = stack(data.frame(block.E.rep.biomass))
allstack.blockrep.biomass = merge(allstack.blockrep.biomass, stack.E.rep, all.x = T, all.y =T, sort = F)

stack.F.rep = stack(data.frame(block.F.rep.biomass))
allstack.blockrep.biomass = merge(allstack.blockrep.biomass, stack.F.rep, all.x = T, all.y =T, sort = F)

allstack.blockrep.biomass$Block = 0
allstack.blockrep.biomass$Replicate = 0

allstack.blockrep.biomass$Replicate[1:70] = "Block"
allstack.blockrep.biomass$Replicate[which(allstack.blockrep.biomass$Replicate == "0")] = "Replicate"

allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.A.biomass")] = "A"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.B.biomass")] = "B"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.C.biomass")] = "C"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.D.biomass")] = "D"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.E.biomass")] = "E"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.F.biomass")] = "F"

allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.A.rep.biomass")] = "A"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.B.rep.biomass")] = "B"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.C.rep.biomass")] = "C"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.D.rep.biomass")] = "D"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.E.rep.biomass")] = "E"
allstack.blockrep.biomass$Block[which(allstack.blockrep.biomass$ind == "block.F.rep.biomass")] = "F"

names(allstack.blockrep.biomass)[names(allstack.blockrep.biomass) == 'values'] <- 'Biomass'

box.blockrep.biomass = ggplot(allstack.blockrep.biomass, aes(x=Block, y=Biomass, fill=Replicate)) + geom_boxplot(position=position_dodge(.9))

##Boxplot of NDVI vs blocks

just.block.ndvi = data.frame(block.A.ndvi,block.B.ndvi, block.C.ndvi, block.D.ndvi, block.E.ndvi, block.F.ndvi)

just.stack.block.ndvi = stack(just.block.ndvi)
names(just.stack.block.ndvi)[names(just.stack.block.ndvi) == 'values'] <- 'NDVI'
names(just.stack.block.ndvi)[names(just.stack.block.ndvi) == 'ind'] <- 'Block'

box.block.ndvi = ggplot(just.stack.block.ndvi, aes(x=Block, y=NDVI, fill=Block)) + geom_boxplot()

##Boxplot of GNDVI vs blocks/replicates

all.block.gndvi = data.frame(block.A.gndvi, block.A.rep.gndvi,block.B.gndvi, block.B.rep.gndvi,block.C.gndvi, block.C.rep.gndvi,block.D.gndvi, block.D.rep.gndvi,block.E.gndvi, block.E.rep.gndvi,block.F.gndvi, block.F.rep.gndvi)

allstack.block.gndvi = stack(all.block.gndvi)

allstack.block.gndvi$block = 0
allstack.block.gndvi$rep = 0

allstack.block.gndvi$rep[1:24] = "block"
allstack.block.gndvi$rep[49:72] = "block"
allstack.block.gndvi$rep[97:120] = "block"
allstack.block.gndvi$rep[145:168] = "block"
allstack.block.gndvi$rep[193:216] = "block"
allstack.block.gndvi$rep[241:264] = "block"

allstack.block.gndvi$rep[which(allstack.block.gndvi$rep == "0")] = "replicate"

allstack.block.gndvi$block[1:48] = "A"
allstack.block.gndvi$block[49:96] = "B"
allstack.block.gndvi$block[97:144] = "C"
allstack.block.gndvi$block[145:192] = "D"
allstack.block.gndvi$block[193:240] = "E"
allstack.block.gndvi$block[241:288] = "F"

names(allstack.block.gndvi)[names(allstack.block.gndvi) == 'values'] <- 'GNDVI'

box.blockrep.gndvi = ggplot(allstack.block.gndvi, aes(x=block, y=GNDVI, fill=rep)) + geom_boxplot(position=position_dodge(.9))

##Boxplot of GDVI2 vs blocks/replicates

all.block.gdvi2 = data.frame(block.A.gdvi2, block.A.rep.gdvi2,block.B.gdvi2, block.B.rep.gdvi2,block.C.gdvi2, block.C.rep.gdvi2,block.D.gdvi2, block.D.rep.gdvi2,block.E.gdvi2, block.E.rep.gdvi2,block.F.gdvi2, block.F.rep.gdvi2)

allstack.block.gdvi2 = stack(all.block.gdvi2)

allstack.block.gdvi2$block = 0
allstack.block.gdvi2$rep = 0

allstack.block.gdvi2$rep[1:24] = "block"
allstack.block.gdvi2$rep[49:72] = "block"
allstack.block.gdvi2$rep[97:120] = "block"
allstack.block.gdvi2$rep[145:168] = "block"
allstack.block.gdvi2$rep[193:216] = "block"
allstack.block.gdvi2$rep[241:264] = "block"

allstack.block.gdvi2$rep[which(allstack.block.gdvi2$rep == "0")] = "replicate"

allstack.block.gdvi2$block[1:48] = "A"
allstack.block.gdvi2$block[49:96] = "B"
allstack.block.gdvi2$block[97:144] = "C"
allstack.block.gdvi2$block[145:192] = "D"
allstack.block.gdvi2$block[193:240] = "E"
allstack.block.gdvi2$block[241:288] = "F"

names(allstack.block.gdvi2)[names(allstack.block.gdvi2) == 'values'] <- 'GDVI2'

box.blockrep.gdvi2 = ggplot(allstack.block.gdvi2, aes(x=block, y=GDVI2, fill=rep)) + geom_boxplot(position=position_dodge(.9))
