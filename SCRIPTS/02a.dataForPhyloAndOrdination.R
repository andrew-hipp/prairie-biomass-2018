library(magrittr)

spp.prob.2017 <- gsub('_', ' ', spp.prob.2017, fixed = T)

vars.productivity <- c('biomass.monocultures',
                                        'NDVI.CI',
                                        'GNDVI.CI',
                                        'GDVI2.CI',
                                        'NIR.CI',
                                        'REG.CI',
                                        'RED.CI',
                                        'GRE.CI')
labels.productivity <- c('Biomass',
                              'NDVI',
                              'GNDVI',
                              'GDVI2',
                              'NIR',
                              'REG',
                              'RED',
                              'GRE')

labels.traits.continuous <- c("Seed mass", "Leaf dry matter content", "SLA",
                              "Leaf N content", "Leaf C content", "Leaf P content",
                              "Stem dry matter content", "Leaf circularity",
                              "Vegetative height", "Leaf length", "Leaf thickness", "Leaf width",
                              "Petiole length","Genome size")

labels.traits.use <- c(labels.traits.continuous[-length(labels.traits.continuous)],
                       "Photosynthetic pathway", "Rhizomes",
                       "N-fixer", "Genome size")

all.prairie.spl <- split(all.prairie, all.prairie$monoTreeName) ##changed ndvi.mat to all.prairie to complie with 01.compileData.R
all.prairie.mean <- sapply(all.prairie.spl, function(x) {
  apply(x, 2, function(y) mean(as.numeric(y), rm.na = T))
  }) %>%
  t %>%
  as.data.frame

all.prairie.small <- all.prairie.mean[vars.productivity]

names(all.prairie.small) <- labels.productivity

row.names(all.prairie.small) <- gsub('_', ' ', row.names(all.prairie.small), fixed = T)

message('CHECKING FOR 2017 PROBLEM SPECIES')
if(any(!spp.prob.2017 %in% row.names(all.prairie.small))) {
  message("... the following problem spp were not found:")
  print(setdiff(spp.prob.2017, row.names(all.prairie.small)))
  } else
  message("... and all are excluded from all.prairie.small")

all.prairie.small <- all.prairie.small[which(!row.names(all.prairie.small) %in% spp.prob.2017), ]

## remove all NAs and -INF and prune tree
problem.rows <- which(apply(all.prairie.small, 1, function(x) any(c(-Inf, NaN) %in% x)))
if(length(problem.rows) > 0) all.prairie.small <- all.prairie.small[-problem.rows, ]

all.prairie.small <- apply(all.prairie.small, 2, function(x) (x - min(x)) / max(x - min(x))) %>%
  as.data.frame
if(length(problem.rows) > 0) all.prairie.small <- all.prairie.small[-problem.rows, ]

all.prairie.ordi <- cbind(scale(all.prairie.mean[gsub(" ", "_", row.names(all.prairie.small)), vars.productivity]),
                          dat.traits.scaled[gsub(" ", "_",row.names(all.prairie.small)), dat.traits.use]
                        ) %>%
                       as.data.frame
names(all.prairie.ordi) <- c(labels.productivity, labels.traits.use)
row.names(all.prairie.ordi) <- gsub("_", " ", row.names(all.prairie.ordi))
