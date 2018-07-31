dat.headers.plugs <- c("plot", "type", "sp", "biomass.total", "b01", "b02", "b03", "b04",
"b05", "b06", "b07", "b08", "b09", "b10", "b11", "b12",
"b13", "b14", "b15", "groundLeaves", "collector",
"date")
dat.headers.mono <- c("plot", "type", "sp", "biomass.total",
"weightTotal", "weightBag", "gl", "glTotal", "glBag",
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
"collector",
"date")

## this is a kludge -- remove when solved
ndvi <- read.table('../DATA/plotNDVI', header = T) 

mono.scalar <- 15/4 # accounts for the fact that 4 of 15 plants were collected from each monoculture
mono.rows <- match(ndvi$plot, dat$mono$plot)
tmt.rows <- match(ndvi$plot, dat$plugs$plot)
ndvi.mat <- data.frame(plot = ndvi$plot,
              biomass.monocultures = dat$mono[mono.rows, 'biomass.total'] * mono.scalar,
              biomass.monocultures.noGL = dat$mono[mono.rows, 'noGL'] * mono.scalar,
              biomass.tmts = dat$plugs[tmt.rows, 'biomass.total'],
              biomass.tmts.noGL = dat$plugs[tmt.rows, 'biomass.total'] -
                                  dat$plugs[tmt.rows, 'groundLeaves'],
              phy.div = dat$plugs[tmt.rows, 'phy'],
              trait.div = dat$plugs[tmt.rows, 'trt'],
              ndvi = ndvi$ndvi,
              as.is = T)

ndvi.mat$biomass.all <- apply(ndvi.mat[, c('biomass.monocultures', 'biomass.tmts')],
                              1, sum, na.rm = T)
ndvi.mat$biomass.all[which(apply(head(ndvi.mat[, c('biomass.monocultures', 'biomass.tmts')], 15),1,function(x) sum(is.na(x))) == 2)] <- NA


ndvi.mat$'Plot.category' = NA
ndvi.mat$'Plot.category'[which(!is.na(tmt.rows))] = "Treatment"
ndvi.mat$'Plot.category'[which(!is.na(mono.rows))] = "Monoculture"
ndvi.mat$Plot.category <- factor(ndvi.mat$Plot.category, levels = c('Treatment',
                                                                    'Monoculture'))

ndvi.mat <- ndvi.mat[-ndvi.mat$plot[is.na(ndvi.mat$Plot.category)], ]
