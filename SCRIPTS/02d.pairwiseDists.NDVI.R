## Make a figure comparable to Schweiger et al., 2018, Nat Ec Evol

## missing:
# Achillea millefolium (Asteraceae, Anthemideae)
# Lupinus perennis
# Dalea candida
# Dalea purpurea

library(vegan)
dat.schweiger <- read.csv('../DATA/biodiv_spectra.csv', as.is = T, row.names=1)
dat.schweiger$Latin.Species <- NULL

spp.schweiger <- list(
  identical = c('Amorpha canescens',
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
  relatives = c('Elymus canadensis', #for Agropyron smithii
                'Koeleria pyramidata', # for K. cristata
                'Hierochloe odorata' # for Poa pratensis
              ),
  schwCodes = c('AMOCA',
                'ANDGE',
                'ASCTU',
                'LESCA',
                'LIAAS',
                'MONFI',
                'PANVI',
                'SCHSC',
                'SOLRI',
                'SORNU')
)

## compare our data with schweiger; source = https://ecosis.org/#result/6b3feb89-bf62-4003-8f03-73341a4c4033
vi.pca.scher <- prcomp(all.prairie.mean[gsub(" ", "_", spp.schweiger$identical),
                                       c('pNIRvalues','pREGvalues','pREDvalues','pGREvalues')],
                        scale = TRUE)
vi.pca.schweiger <- prcomp(dat.schweiger[spp.schweiger$schwCodes, ], scale = TRUE)

vi.pca.mantel <- mantel(dist(vi.pca.scher$x[, 1:2]),
                        dist(vi.pca.schweiger$x[, 1:2]),
                        method = 'spearman')
