# master script that sources everything

# read all data
source('../SCRIPTS/00a.readBiomass.R')
source('../SCRIPTS/00b.compileData.R')
source('../SCRIPTS/00c.readTree-v3.R')
source('../SCRIPTS/00d.problemSpp.2017.R')
source('../SCRIPTS/00e.readData.traits.R')
allRS <- read.csv("../DATA/allRS.csv")
# source('../SCRIPTS/tif.analysis.r') only if there is new data


# add VI data to all.prairie df
source("../SCRIPTS/01d.add.VIdata.to.all.prairie.R")

### at this point, all the data has been read in and formatted
### into one df called all.prairie


# run phylogeny scripts -- add cover to phylo?
source('../SCRIPTS/02a.dataForPhyloAndOrdination.R')
source('../SCRIPTS/02b.phylo.v3.R')
source('../SCRIPTS/02c.phyloSignal.R')

# make PCA with environmental factors
source("../SCRIPTS/03.enviro.factors.R")

# trait ordination with biomass and biomass NDVI correlation
source("../SCRIPTS/04.traitNDVI.biomass.R")

# make regression summary tables for predicting biomass and cover
source("../SCRIPTS/05a.regression.summary.biomass.R")
source("../SCRIPTS/05b.regression.summary.cover.R")

# make figures: NDVI/biomass and NDVI/cover regressions, summary boxplots
source("../SCRIPTS/06.making.graphs.R")






# ISSUE not sure what this does
source("../SCRIPTS/999.gheatmap.mod.R")
