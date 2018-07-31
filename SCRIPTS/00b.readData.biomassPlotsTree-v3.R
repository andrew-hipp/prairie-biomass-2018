## read data for prairie biomass and ndvi study
## ahipp@mortonarb.org
## 2018-03-06
# this is a test

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

tr.prairie$tip.label <- gsub('[.-]', '', tr.prairie$tip.label)
