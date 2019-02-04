# adds VI data to all.prairie
all.prairie <- cbind(all.prairie, allRS)


# make df to use for biomass analyses
prairie.bio <- all.prairie[which(all.prairie$Plot.category == "Monoculture" |
                                   all.prairie$TMT.use == 1),]

# make df to use for analyses that do not include biomass
prairie <- all.prairie[which(all.prairie$Plot.category == "Monoculture" |
                               all.prairie$Plot.category == "Treatment"),]


# remove monocultures of problem species
'%ni%' <- Negate('%in%')
prairie.noProb <- all.prairie[which(all.prairie$monoTreeName %ni% spp.prob.2017),]

# make df for phylogenetic analysis
prairie.mono <- prairie.noProb[which(prairie.noProb$Plot.category == "Monoculture"),]



