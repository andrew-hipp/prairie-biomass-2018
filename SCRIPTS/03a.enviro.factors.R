## v1: atuffin 2018-07-31
  ## -- taking important script from 03.sensitiveEnvir.R

##PCA of environmental factors

#log.envir = log(all.prairie [c(7,11,12,13,14,15)])
log.envir = log(all.prairie [c(6,10,11,12,13,14)])
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
