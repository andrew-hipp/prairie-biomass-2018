## Make a figure comparable to Schweiger et al., 2018, Nat Ec Evol

## missing:
# Achillea millefolium (Asteraceae, Anthemideae)
# Lupinus perennis
# Dalea candida
# Dalea purpurea

library(vegan)
dat.schweiger <- read.csv('../DATA/biodiv_spectra.csv', as.is = T, row.names=1)
dat.schweiger$Latin.Species <- NULL

spp.schw <- list(
  narrow = c('Amorpha canescens',
                  'Andropogon gerardii',
                  'Asclepias tuberosa',
                  'Lespedeza capitata',
                  'Liatris aspera',
                  'Monarda fistulosa',
                  'Panicum virgatum',
                  'Schizachyrium scoparium',
                  'Solidago rigida',
                  'Sorghastrum nutans'
                ),
  broad = c('Amorpha canescens',
                  'Andropogon gerardii',
                  'Asclepias tuberosa',
                  'Lespedeza capitata',
                  'Liatris aspera',
                  'Monarda fistulosa',
                  'Panicum virgatum',
                  'Schizachyrium scoparium',
                  'Solidago rigida',
                  'Sorghastrum nutans',
                  'Elymus canadensis', #for Agropyron smithii
                'Koeleria pyramidata', # for K. cristata
                'Hierochloe odorata' # for Poa pratensis
              ),
  codes.narrow = c('AMOCA',
                'ANDGE',
                'ASCTU',
                'LESCA',
                'LIAAS',
                'MONFI',
                'PANVI',
                'SCHSC',
                'SOLRI',
                'SORNU'),
  codes.broad = c('AMOCA',
                'ANDGE',
                'ASCTU',
                'LESCA',
                'LIAAS',
                'MONFI',
                'PANVI',
                'SCHSC',
                'SOLRI',
                'SORNU',
                'AGRSM',
                    'KOECR',
                    'POAPR')
)

## compare our data with schweiger; source = https://ecosis.org/#result/6b3feb89-bf62-4003-8f03-73341a4c4033
vi.pca.scher <- prcomp(all.prairie.mean[gsub(" ", "_", spp.schw$broad),
                                       c('pNIRvalues','pREGvalues','pREDvalues','pGREvalues')],
                        scale = TRUE)
vi.pca.schweiger <- prcomp(dat.schweiger[spp.schw$codes.broad, ], scale = TRUE)

vi.pca.mantel <- mantel(dist(vi.pca.scher$x[, 1:2]),
                        dist(vi.pca.schweiger$x[, 1:2]),
                        method = 'pearson')

vi.phylo.schweiger <- mantel(as.dist(cophenetic(tr.prairie.biomassPlot)[spp.schw$broad, spp.schw$broad]),
                            dist(vi.pca.schweiger$x[, 1:2]),
                            method = 'pearson')

vi.phylo.mantel <- mantel(as.dist(cophenetic(tr.prairie.biomassPlot)[spp.schw$broad, spp.schw$broad]),
                          dist(all.prairie.mean[gsub(' ', '_', spp.schw$broad), 'pNDVIvalues']),
                         method = 'pearson')

vi.biomass.mantel <- mantel(as.dist(cophenetic(tr.prairie.biomassPlot)[spp.schw$broad, spp.schw$broad]),
                          dist(all.prairie.mean[gsub(' ', '_', spp.schw$broad), 'biomass.all']),
                          method = 'pearson')

sink('../OUT/STATS.mantelVI.txt')
print('-----------------------MANTEL TEST, PCA OF SCHER VI VS Schweiger VI-----------------------')
print(vi.pca.mantel)
print('-----------------------MANTEL TEST, PCA of Schweiger VI vs phylo-----------------------')
print(vi.phylo.schweiger)
print('-----------------------MANTEL TEST, dist(SCHER NDVI) vs phylo-----------------------')
print(vi.phylo.mantel)
print('-----------------------MANTEL TEST, dist(biomass) vs phylo-----------------------')
print(vi.biomass.mantel)
#unlink('../OUT/STATS.mantelVI.txt')
