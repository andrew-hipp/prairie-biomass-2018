tr.prairie.phylosig <- tr.prairie.biomassPlot
tr.prairie.phylosig$node.label <- NULL
message('Doing Blomberg\'s K')
prairie.phylosignal <- lapply(names(all.prairie.small), function(x) {
  phylosignal(all.prairie.small[tr.prairie.phylosig$tip.label, x], tr.prairie.phylosig)[1,]
  }) %>%
  do.call('rbind', .)
# row.names(prairie.phylosignal) <- names(all.prairie.small)

message('Doing Pagel\'s Lambda')
prairie.lambda <- list(estimated = fitContinuous(tr.prairie.phylosig, all.prairie.small, model = 'lambda'),
                       zero = fitContinuous(rescale(tr.prairie.phylosig, 'lambda', 0), all.prairie.small)
                       )
prairie.phylosignal <- cbind(prairie.phylosignal,
                             lambda = sapply(prairie.lambda$estimated, function(x) x$opt$lambda),
                             L.ratio = 2 * (sapply(prairie.lambda$estimated, function(x) x$opt$lnL) -
                                           sapply(prairie.lambda$zero, function(x) x$opt$lnL)),
                             L.ratio.p = pchisq(2 * (sapply(prairie.lambda$estimated, function(x) x$opt$lnL) -
                                           sapply(prairie.lambda$zero, function(x) x$opt$lnL)),
                                           df = 1,
                                           lower.tail = F)
                                           )


write.csv(round(prairie.phylosignal, 5), file = '../OUT/TABLE.phylosignal.csv')
