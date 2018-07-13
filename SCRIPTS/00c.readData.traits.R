dat.fams <- read.csv('../DATA/families.csv', row.names = 1, as.is = T)
dat.traits <- read.csv('../DATA/ImputedMiceTraits.2016-01-06.csv',
                       as.is = F, row.names = 1)

dat.traits.use <- c("seedMass", "LDMC", "SLA", "LNC", "LCC", "LPC", "SDMC", "circularity",
                       "vegetativeHeight", "leafLength", "leafThickness", "leafWidth",
                       "petioleLength", "Lifeform", "photosyntheticPathway", "rhizomes",
                       "habitClean", "nFixer",
                       "wis_IL", "genome_Use")
dat.traits.continuous <- c("seedMass", "LDMC", "SLA", "LNC", "LCC", "LPC", "SDMC", "circularity",
                                              "vegetativeHeight", "leafLength", "leafThickness", "leafWidth",
                                              "petioleLength","genome_Use")
dat.traits.scaled <- dat.traits[row.names(ndvi.mat.small), dat.traits.use]
for(i in dat.traits.continuous) dat.traits.scaled[[i]] <- as.numeric(scale(dat.traits.scaled[[i]]))
dat.fams$Family.8 <- dat.fams$family
dat.fams$Family.8[!dat.fams$family %in% names(tail(sort(table(dat.fams$family)), 7))] <- "Other"
