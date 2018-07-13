## problem spp identified in 2017 study of cover
## ahipp 2018-07-12

spp.spring <- sapply(c('Maianthemum',
                      'Polygonatum',
                      'Dodecatheon',
                      'Allium',
                      'Camassia'),
                    function(x) grep(x, tr.prairie$tip.label, value = T)) %>%
              unlist


spp.didNotEstablishWell <- c('Castilleja_coccinea',
                             'Asclepias_hirtella',
                             'Asclepias_syriaca',
                             'Dalea_purpurea',
                             "Helianthus_strumosus",
                             'Juncus_interior',
                             'Juncus_tenuis',
                             'Verbena_stricta',
                             'Asclepias_sullivantii',
                             'Baptisia_bracteata',
                             'Ceanothus_americanus',
                             'Dalea_candida',
                             'Senna_hebecarpa',
                             'Hypericum_punctatum',
                             'Lilium_michiganense',
                             'Symphyotrichum_sericeum')

spp.almostZeroBiomass <- c('Agalinis_tenuifolia', 'Senna_marilandica')

spp.prob.2017 <- unique(c(spp.spring,
                          spp.didNotEstablishWell,
                          spp.almostZeroBiomass
                          ))
