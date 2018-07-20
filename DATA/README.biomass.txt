2018-02-01
Andrew Hipp (ahipp@mortonarb.org)

To get things rolling, navigate into WORKSPACE and launch R. Do:

source('../SCRIPTS/01.load.and.format.cover.R')

to get all data into R. Here are the objects you'll have with some handy notes:

blocks: This shows the blocking for each plot. The blocking we actually used was block_BS.mod.
        Also within here are the soils data collected in the summer only: AHOR_cm is depth
        of the a-horizon, TOTAL_cm is depth of the total soil core taken, REDOX_cm is depth to the
        redox layer. Only a-horizon depth seems to be useful.
data.mono: this is biomass for the monocultures
dat.plugs.mat: this is biomass for the treatment plots, plugs only, arranged as a plots x spp matrix
dat.plugs: this is a data.frame of the plug data for treatments, useful mostly b/c I've added phylogenetic
           and trait diversity (as planted) to these.

Then, do the next script:

source('../SCRIPTS/02.ndviVSbiomassPlots.R')

and you'll get 11 additional objects. Of these, probably only one is useful:

ndvi.mat: has biomass (with and without ground leaves), ndvi, and plot category (monoculture vs treatment)
          for all plug plots.

The script for doing partitioned diversity analyses (see presentation for a brief on this) is 00b.lor.hect...
this script is called from within 03.biomassStatsAndPlots.R
