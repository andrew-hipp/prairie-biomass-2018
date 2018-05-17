require(dplyr)

source("../SCRIPTS/00.readData.R")

dat.soil.cover = read.csv("../DATA/dat.blocksSoilCover.csv")
dat.compo = read.csv("../DATA/dat.composition.2017.csv")
dat.phylo.cover = read.csv("../DATA/dat.cover.diversity.2017.csv")
VI.values = read.csv("../DATA/VIvalues.csv")
source("../DATA/plot.design.r")

plot = c(1:437)

all.prairie = as.data.frame(plot)
all.prairie = merge(all.prairie, dat.soil.cover, by = "plot")
names(all.prairie)[names(all.prairie)=="block_BS.mod"] <- "block"
all.prairie$X = NULL
names(dat.compo)[names(dat.compo)=="X"] <- "plot"
all.prairie$ndvi_v1 = NULL

all.prairie = merge(all.prairie, dat.phylo.cover, all.x = T)
names(all.prairie)[names(all.prairie)=="sp"] <- "plot.ID"
all.prairie$X = NULL

all.prairie = merge(all.prairie, ndvi.mat, all.x = T)
all.prairie$as.is = NULL

VI.values$X = NULL
all.prairie = merge(all.prairie, VI.values, all.x = T)

all.prairie$TMT.use = 0
all.prairie$TMT.use[148] = 1
all.prairie$TMT.use[152] = 1
all.prairie$TMT.use[159] = 1
all.prairie$TMT.use[160] = 1
all.prairie$TMT.use[163] = 1
all.prairie$TMT.use[172] = 1
all.prairie$TMT.use[174] = 1
all.prairie$TMT.use[181] = 1
all.prairie$TMT.use[188] = 1
all.prairie$TMT.use[205] = 1
all.prairie$TMT.use[211] = 1
all.prairie$TMT.use[219] = 1
all.prairie$TMT.use[223] = 1
all.prairie$TMT.use[230] = 1
all.prairie$TMT.use[234] = 1
all.prairie$TMT.use[240] = 1
all.prairie$TMT.use[266] = 1
all.prairie$TMT.use[271] = 1
all.prairie$TMT.use[273] = 1
all.prairie$TMT.use[278] = 1
all.prairie$TMT.use[281] = 1
all.prairie$TMT.use[284] = 1
all.prairie$TMT.use[288] = 1
all.prairie$TMT.use[291] = 1
all.prairie$TMT.use[296] = 1
all.prairie$TMT.use[300] = 1
all.prairie$TMT.use[309] = 1
all.prairie$TMT.use[318] = 1
all.prairie$TMT.use[319] = 1
all.prairie$TMT.use[320] = 1
all.prairie$TMT.use[321] = 1
all.prairie$TMT.use[323] = 1
all.prairie$TMT.use[333] = 1
all.prairie$TMT.use[346] = 1
all.prairie$TMT.use[356] = 1
all.prairie$TMT.use[379] = 1
all.prairie$TMT.use[381] = 1
all.prairie$TMT.use[392] = 1
all.prairie$TMT.use[396] = 1
all.prairie$TMT.use[397] = 1
all.prairie$TMT.use[412] = 1
all.prairie$TMT.use[414] = 1
all.prairie$TMT.use[424] = 1
all.prairie$TMT.use[429] = 1
