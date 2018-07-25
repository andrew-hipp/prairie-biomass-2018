# adds VIdata to all.prairie

VI <- read.csv("../DATA/VIdata.csv")
VI$X <- NULL

all.prairie <- merge(all.prairie, VI, all.x = T)
