require(dplyr)

source("../DATA/plot.design.R") # this reads in the original plot design... used at all?

plot <- c(1:437)

all.prairie <- as.data.frame(plot)
all.prairie <- merge(all.prairie, dat$blocks, by = "plot")
names(all.prairie)[names(all.prairie)=="block_BS.mod"] <- "block"
all.prairie$X = NULL
names(dat$composition)[names(dat$composition)=="X"] <- "plot"
all.prairie$ndvi_v1 <- NULL

all.prairie <- merge(all.prairie, dat$plotMeta, all.x = T)
names(all.prairie)[names(all.prairie)=="sp"] <- "plot.ID"
all.prairie$X <- NULL

all.prairie <- merge(all.prairie, biomass.mat, by = "plot", all.x = T)
all.prairie$as.is = NULL

#VI.values$X = NULL
#all.prairie = merge(all.prairie, VI.values, all.x = T)

all.prairie = merge(all.prairie, dat$flower.presence, all.x = T)
names(all.prairie)[names(all.prairie)=="flowers"] <- "flower.presence"

tmtsToUse <- c(148, 152, 159, 160, 163, 172, 174, 181, 188,205,
               211, 219, 223, 230, 234, 240, 266, 271, 273, 278,
               281, 284, 288, 291, 296, 300, 309, 318, 319, 320,
               321, 323, 333, 346, 356, 379, 381, 392, 396, 397,
               412, 414, 424, 429)

all.prairie$TMT.use = 0
all.prairie$TMT.use[tmtsToUse] = 1

# add drone cover estimates
dcover <- read.csv("../DATA/drone-cover.csv", 
                   col.names = c("plot", "dcover.obs1", "dcover.obs2"))
all.prairie <- merge(all.prairie, dcover, by = "plot", all.x = T)
all.prairie$dcover <- (all.prairie$dcover.obs1 + all.prairie$dcover.obs2)/2

