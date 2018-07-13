labels.signal <- c(labels.productivity, labels.traits.continuous)

tr.prairie.phylosig <- tr.prairie.biomassPlot
tr.prairie.phylosig$node.label <- NULL
message('Doing Blomberg\'s K')
prairie.phylosignal <- lapply(labels.signal, function(x) {
  phylosignal(structure(all.prairie.ordi[tr.prairie.phylosig$tip.label, x], names = tr.prairie.phylosig$tip.label),
              tr.prairie.phylosig)[1,]
  }) %>%
  do.call('rbind', .)
# row.names(prairie.phylosignal) <- names(all.prairie.small)

message('Doing Pagel\'s Lambda')
prairie.lambda <- list(estimated = fitContinuous(tr.prairie.phylosig,
                                                 all.prairie.ordi[, labels.signal],
                                                 model = 'lambda'),
                       zero = fitContinuous(rescale(tr.prairie.phylosig, 'lambda', 0),
                                            all.prairie.ordi[, labels.signal])
                                          )
prairie.phylosignal <- cbind(lambda = sapply(prairie.lambda$estimated, function(x) x$opt$lambda),
                              L.ratio = 2 * (sapply(prairie.lambda$estimated, function(x) x$opt$lnL) -
                                            sapply(prairie.lambda$zero, function(x) x$opt$lnL)),
                              L.ratio.p = pchisq(2 * (sapply(prairie.lambda$estimated, function(x) x$opt$lnL) -
                                                      sapply(prairie.lambda$zero, function(x) x$opt$lnL)),
                                                        df = 1,
                                                        lower.tail = F),
                              prairie.phylosignal)
prairie.phylosignal.pretty <- prairie.phylosignal[order(prairie.phylosignal$lambda, decreasing = T), ]
prairie.phylosignal.pretty<- round(prairie.phylosignal.pretty, 5)
prairie.phylosignal.pretty[prairie.phylosignal.pretty == 0] <- "< 0.00001"
prairie.phylosignal.pretty[prairie.phylosignal.pretty == 1] <- "> 0.99999"
write.csv(prairie.phylosignal.pretty, file = '../OUT/TABLE.phylosignal.csv')
