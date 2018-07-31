# master script that sources everything

# read all data
source('../SCRIPTS/00a.readBiomass.R')
source('../SCRIPTS/00b.compileData.R')
source('../SCRIPTS/00c.readTree-v3.R')
source('../SCRIPTS/00d.problemSpp.2017.R')
source('../SCRIPTS/00e.readData.traits.R')

# convert .txt file with plot coordinates to list of polygons
source("../SCRIPTS/01a.makeplotpolygons.R")

# define VI functions that will be used to calculate VI values for each plot
source("../SCRIPTS/01b.define.VI.functions.R")

# calculate VI values for each plot - this step takes a while, only run when necessary
#source("../SCRIPTS/01c.calculate.VI.values.R")
# if there is no new data, read in this csv instead of running the script:
VI <- read.csv("../DATA/VIdata.csv")

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
source("../SCRIPTS/05a.make.regression.summary.table.biomass.R")
source("../SCRIPTS/05b.make.regression.summary.table.cover.R")

# make figures: NDVI/biomass and NDVI/cover regressions, summary boxplots
source("../SCRIPTS/06.making.graphs.R")






# ISSUE not sure what this does
source("../SCRIPTS/999.gheatmap.mod.R")

# ISSUE not sure what this does
source("../SCRIPTS/07.biomass&VI.model.ahor&block.R")