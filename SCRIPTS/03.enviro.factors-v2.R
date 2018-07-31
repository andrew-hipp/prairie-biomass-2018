## v1: atuffin 2018-07-31
  ## -- taking important script from 03.sensitiveEnvir.R

##PCA of environmental factors

log.envir = log(all.prairie [,c("AHOR_cm","GSM","LOI","fWAS","pH","EC")])
log.envir = log.envir[-c(409), ] ##AHOR_cm missing from plot 409
blocks = all.prairie["block"]
blocks = data.frame(blocks[-409, ]) ##AHOR_cm missing from plot 409

use <- which(blocks[, 1] %in% LETTERS[1:6]) # eliminate plots not in blocks

blocks.use <- blocks[use, 1]
log.envir.use <- log.envir[use, ]

pca.envir = prcomp(log.envir.use, center = TRUE, scale. = TRUE)

library(devtools)
if(class(try(library(ggbiplot))) == "try-error") {
  install_github("vqv/ggbiplot")
  library(ggbiplot)
}
library(plyr)
library(dplyr)

gg.pca.envir <- ggbiplot(pca.envir, obs.scale = 1, var.scale = 1,
                         groups = blocks.use,
                         ellipse = TRUE,
                         circle = FALSE)
gg.pca.envir <- gg.pca.envir + scale_color_discrete(name = 'Block')

pdf('../OUT/pca.environmental.factors-v2.pdf')
print(gg.pca.envir)
dev.off()
