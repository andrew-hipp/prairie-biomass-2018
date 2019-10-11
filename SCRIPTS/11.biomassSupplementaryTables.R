library(reshape2)

# make supplemental biomass table with biomass per species per plot

# monocultures
bioMono <- prairie.bio[which(prairie.bio$Plot.category == "Monoculture"),c("plot", "monoTreeName", "biomass.all")]

bioMono$name <- gsub("_", " ", bioMono$monoTreeName)
bioMono <- bioMono[,c("name", "biomass.all")]

nOcc <- unique(bioMono$name)
plotNum <- c(1:nrow(bioMono))
for (i in 1:nrow(bioMono)) {
  sp <- bioMono$name[i]
  tab <- table(nOcc)
  tab <- as.data.frame(tab)
  
  plotNum[i] <- as.numeric(tab$Freq[which(tab$nOcc == sp)])
  nOcc <- c(nOcc, sp)
}

bioMono$plotNum <- plotNum

bioMono <- dcast(bioMono, bioMono$name ~ bioMono$plotNum,
                 value.var = "biomass.all")

colnames(bioMono) <- c("species", "plot 1", "plot 2")


# treatments

biomass.raw <- dat$biomass.raw
rownames(biomass.raw) <- 1:nrow(biomass.raw)
inds <- biomass.raw$type
tmtInd <- inds == "Plug" | inds == ""
tmtInd <- rownames(biomass.raw$plot[which(biomass.raw$type == "Plug")])

tmt <- biomass.raw[which(biomass.raw$type == "Plug" |
                           biomass.raw$type == ""),]
tmt <- tmt[-which(tmt$species == "CONTROL"),]
keep <- c(seq(1, nrow(tmt), by = 4), seq(2, nrow(tmt), by = 4))
keep <- sort(keep)

tmt <- tmt[keep, 3:20]

colnames(tmt) <- c("species mix", "total biomass", "species 1",
                   "species 2", "species 3", "species 4", "species 5",
                   "species 6", "species 7", "species 8", "species 9",
                   "species 10", "species 11", "species 12", "species 13",
                   "sepcies 14", "species 15", "ground leaves")

tmt$total <- tmt$`total biomass`
tmt$`total biomass` <- NULL

# write .csv files
write.csv(bioMono, file = "../OUT/TABLE.monocultureSupplement.csv")
write.csv(tmt, file = "../OUT/TABLE.treatmentSupplement.csv")
