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
all.prairie <- all.prairie[which(all.prairie$monoTreeName %ni% spp.prob.2017),]

# make df for phylogenetic analysis
prairie.mono <- all.prairie[which(all.prairie$Plot.category == "Monoculture"),]



# old code
# remove treatments with problem species
tmtcomp <- read.csv("../DATA/matrix.of.planted.species.csv", row.names = 1)
tmtCompProb <- tmtcomp[,colnames(tmtcomp) %in% spp.prob.2017]
plotTotal <- rowSums(tmtCompProb)
probSpCount <- as.data.frame(plotTotal)
probSpCount$plot <- rownames(probSpCount)
all.prairie <- merge(all.prairie, probSpCount, by = "plot", all = T)

all.prairie <- all.prairie[which(all.prairie$plotTotal < 1 |
                                   all.prairie$Plot.category == "Monoculture"),]


