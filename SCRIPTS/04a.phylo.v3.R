## Assumes you've already compiled data (script 01)
## v1: ahipp 2018-04-12
## v2: atuffin, sometime in May
## v3: ahipp, 2018-07-12. Edits include:
  ## -- removing all species excluded from 2017 analysis - DONE
  ## -- cleaning code - DONE
  ## -- add tip labels - DONE
  ## -- coloring major clades? - DONE

library(ape)
library(ggtree)
library(magrittr)
library(picante)
library(geiger)
library(tidyverse)

relabelBranches = TRUE

source('../SCRIPTS/999.gheatmap.mod.R') # to suppress legend selectively

tr.prairie.biomassPlot <- tr.prairie
tr.prairie.biomassPlot$tip.label[which(tr.prairie.biomassPlot$tip.label == "Symphyotrichum_novaeangliae")] <-
  "Symphyotrichum_novae-angliae"
tr.prairie.biomassPlot$tip.label <- gsub('_', ' ', tr.prairie.biomassPlot$tip.label, fixed = T)

tr.prairie.biomassPlot <- drop.tip(tr.prairie.biomassPlot, which(!tr.prairie.biomassPlot$tip.label %in% row.names(all.prairie.small))) %>%
  multi2di

#stop('TREE FIXING STOP')

tr.prairie.biomassPlot$node.label[tr.prairie.biomassPlot$node.label %in% c('', 'NA')] <- NA
tr.prairie.biomassPlot$node.label <-
  c(rep(NA, length(tr.prairie.biomassPlot$tip.label)), tr.prairie.biomassPlot$node.label)

tr.mrca <- mrca(tr.prairie.biomassPlot)
tr.prairie.biomassPlot <- groupClade(tr.prairie.biomassPlot,
                                     c(tr.mrca['Helianthus occidentalis', 'Ratibida pinnata'],
                                   tr.mrca['Lespedeza capitata', 'Desmanthus illinoensis'],
                                   tr.mrca['Bromus kalmii', 'Bouteloua curtipendula'],
                                   tr.mrca['Pycnanthemum virginianum', 'Physostegia virginiana'],
                                   tr.mrca['Carex bicknellii', 'Carex blanda'],
                                   tr.mrca['Asclepias tuberosa', 'Asclepias verticillata'],
                                   tr.mrca['Rosa blanda', 'Spiraea alba']
                                   )
                                 )
if(relabelBranches) {
  attr(tr.prairie.biomassPlot, 'group') <- as.factor(c('All other lineages',
                                      'Asteraceae - Sunflower Family',
                                      'Fabaceae - Bean Family',
                                      'Poaceae - Grass Family',
                                      'Lamiaceae - Mint Family',
                                      'Cyperaceae - Sedge Family',
                                      'Apocynaceae - Milkweed Family',
                                      'Rosaceae - Rose Family')[attr(tr.prairie.biomassPlot, 'group')]
                                      )
                                    }

pdf('../OUT/FIGURE.prairie.biomass.allSpectra.withColors.pdf')
p <- ggtree(tr.prairie.biomassPlot,
            aes(color=group)
          )
#p <- p + scale_color_manual("Major plant families",
#                            values = c('black',
#                                        'orange',
#                                        'maroon1',
#                                        'lightgreen',
#                                        'blue',
#                                        '')
#                                        )
p <- p + scale_color_brewer("Plant family", type = 'qual', palette = 1)
#p <- p + geom_label(aes(x = branch), label = tr.prairie.biomassPlot$node.label, size = 2)
p <- gheatmap(p, data = all.prairie.small,
              low = 'white', high = 'black',
              colnames_angle = 315,
              font.size = 1.8,
              width = 0.2,
              hjust = 0,
              )
p <- p + geom_tiplab(fontface='italic', size = 1.7,
                      offset = 40,
                      color = 'black')
p <- p + theme(legend.position = c(0.15, 0.88),
               legend.title = element_text(size = 9.5),
               legend.text = element_text(size = 8),
               legend.key.size = unit(0.35, 'cm'),
               legend.box.background = element_rect(color = NA)
              )

p <- p + ggplot2::xlim(c(0, 280))
print(p)
dev.off()
