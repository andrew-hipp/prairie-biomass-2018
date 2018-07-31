## v1: atuffin 2018-07-31
  ## -- taking important script from 03.sensitiveEnvir.R

##PCA of environmental factors

log.envir = log(all.prairie [,c("AHOR_cm","GSM","LOI","fWAS","pH","EC")])
log.envir = log.envir[-c(409), ] ##AHOR_cm missing from plot 409
blocks = all.prairie["block"]
blocks = data.frame(blocks[-409, ]) ##AHOR_cm missing from plot 409

pca.envir = prcomp(log.envir, center = TRUE, scale. = TRUE)

library(devtools)
install_github("vqv/ggbiplot")
library(plyr)
library(dplyr)

gg.pca.envir <- ggbiplot(pca.envir, obs.scale = 1, var.scale = 1, groups = blocks[,1], ellipse = TRUE, circle = TRUE)
gg.pca.envir = gg.pca.envir + scale_color_discrete(name = 'Block')

pdf('../OUT/pca.environmental.factors.pdf')
