# master script that sources everything

# convert .txt file with plot coordinates to list of polygons
source("../SCRIPTS/01.makeplotpolygons.R")

# compile data - ISSUE this script sources another file
source("../SCRIPTS/00a.compileData.R")

# compile more data
source("../SCRIPTS/00b.readData.biomassPlotsTree-v3.R")

# compile more data
source("../SCRIPTS/00c.problemSpp.2017.R")

# define VI functions that will be used to calculate VI values for each plot
source("../SCRIPTS/102.define.VI.functions.R")

# calculate VI values for each plot - this step takes a while, only run when necessary
#source("../SCRIPTS/103.calculate.VI.things.R")
# if there is no new data, read in this csv instead of running the script:
VI <- read.csv("../DATA/VIdata.csv")

# add VI data to all.prairie df
source("../SCRIPTS/107.add.VIdata.to.all.prairie.R")

# ISSUE not sure what this script does
#source("../SCRIPTS/03.sensitiveEnvir.R")

### at this point, all the data has been read in and formatted
### into one df called all.prairie

# ISSUE this script sources 00b again
source("../SCRIPTS/04a.phylo.v3.R")

# determines phylogenetic signal - ISSUE add cover?
source("../SCRIPTS/04b.phyloSignal.R")

# trait ordination with biomass and biomass NDVI correlation
source("../SCRIPTS/05.traitNDVI.biomass.R")

# ISSUE not sure what this does
source("../SCRIPTS/07.biomass&VI.model.ahor&block.R")

# make regression summary tables for predicting biomass
source("../SCRIPTS/104c.make.regression.summary.table.biomass.R")

# make regression summary tables for predicting cover
source("../SCRIPTS/105c.make.regrssion.summary.table.cover.R")

# make figures: NDVI/biomass and NDVI/cover regressions, summary boxplots
source("../SCRIPTS/106.making.graphs.R")

# ISSUE not sure what this does
source("../SCRIPTS/999.gheatmap.mod.R")
