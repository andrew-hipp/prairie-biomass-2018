library("qpcR")


namesL <- c("all", "mono", "tmt", "NFall", "NFmono")

scoresAllBio <- list()
scoresAllBioMixed <- list()

scoresAllCov <- list()
scoresAllCovMixed <- list()


VIs <- c( "GDVI2", "NDVI", "GRE.CI", "REG.CI", "GNDVI", "REG",
          "RED.CI", "NIR", "NIR.CI", "RED", "GRE", "GNDVI.CI",
          "GDVI2.CI", "NDVI.CI")

#### biomass fixed effects models ####
for (j in 1:length(namesL)) {
  tmp <- read.csv(paste0("C:/Users/clane_897q3pb/Documents/GitHub/prairie-biomass-2018/OUT/TABLE.biomass.regression.", namesL[j], ".csv"))
  tmp <- tmp[,-1]
  
  tmp$AICweight <- akaike.weights(tmp$AIC)[[3]]
  
  
  # make dataframe to record scores
  scores <- data.frame(covariate = NA, 
                       score = NA,
                       count = NA)
  
  # get VI scores
  for (i in 1:length(VIs)) {
    saw <- sum(tmp$AICweight[which(tmp$VI.used == VIs[i])])
    count <- length(tmp$AICweight[which(tmp$VI.used == VIs[i])])
    scores[i,1] <- VIs[i]
    scores[i,2] <- saw
    scores[i,3] <- count
  }
  
  # get cover scores
  covers <- c("planted", "total")
  for (i in 1:length(covers)){
    saw <- sum(tmp$AICweight[which(tmp$cover.used == covers[i])])
    count <- length(tmp$AICweight[which(tmp$cover.used == covers[i])])
    scores[14+i,1] <- covers[i]
    scores[14+i,2] <- saw
    scores[14+i,3] <- count
  }
  
  # get volume score
  sawVOL <- sum(tmp$AICweight[which(tmp$volume != "-")])
  countVOL <- length(tmp$AICweight[which(tmp$volume != "-")])
  scores[17,1] <- "volume"
  scores[17,2] <- sawVOL
  scores[17,3] <- countVOL
  
  # sort
  scores <- scores[order(scores$score, decreasing = T),]
  
  scoresAllBio[[j]] <- scores
}


#### biomass mixed effects models ####
for (j in 1:length(namesL)) {
  tmp <- read.csv(paste0("C:/Users/clane_897q3pb/Documents/GitHub/prairie-biomass-2018/OUT/TABLE.biomass.mixedRegression.", namesL[j], ".csv"))
  tmp <- tmp[,-1]
  
  tmp$AICweight <- akaike.weights(tmp$AIC)[[3]]
  
  
  # make dataframe to record scores
  scores <- data.frame(covariate = NA, 
                       score = NA)
  
  # get VI scores
  for (i in 1:length(VIs)) {
    saw <- sum(tmp$AICweight[which(tmp$VI.used == VIs[i])])
    count <- length(tmp$AICweight[which(tmp$VI.used == VIs[i])])
    scores[i,1] <- VIs[i]
    scores[i,2] <- saw
    scores[i,3] <- count
  }
  
  # get cover scores
  covers <- c("planted", "total")
  for (i in 1:length(covers)){
    saw <- sum(tmp$AICweight[which(tmp$cover.used == covers[i])])
    count <- length(tmp$AICweight[which(tmp$cover.used == covers[i])])
    scores[14+i,1] <- covers[i]
    scores[14+i,2] <- saw
    scores[14+i,3] <- count
  }
  
  # get volume score
  sawVOL <- sum(tmp$AICweight[which(tmp$volume != "-")])
  countVOL <- length(tmp$AICweight[which(tmp$volume != "-")])
  scores[17,1] <- "volume"
  scores[17,2] <- sawVOL
  scores[17,3] <- countVOL
  
  # sort
  scores <- scores[order(scores$score, decreasing = T),]
  
  scoresAllBioMixed[[j]] <- scores
}

#### cover fixed effects models ####
for (j in 1:length(namesL)) {
  tmp <- read.csv(paste0("C:/Users/clane_897q3pb/Documents/GitHub/prairie-biomass-2018/OUT/TABLE.cover.regression.", namesL[j], ".dcover.csv"))
  tmp <- tmp[,-1]
  
  tmp$AICweight <- akaike.weights(tmp$AIC)[[3]]
  
  
  # make dataframe to record scores
  scores <- data.frame(covariate = NA, 
                       score = NA,
                       count = NA)
  
  # get VI scores
  for (i in 1:length(VIs)) {
    saw <- sum(tmp$AICweight[which(tmp$VI.used == VIs[i])])
    count <- length(tmp$AICweight[which(tmp$VI.used == VIs[i])])
    scores[i,1] <- VIs[i]
    scores[i,2] <- saw
    scores[i,3] <- count
  }
  
  # get volume score
  sawVOL <- sum(tmp$AICweight[which(tmp$volume != "-")])
  countVOL <- length(tmp$AICweight[which(tmp$volume != "-")])
  scores[15,1] <- "volume"
  scores[15,2] <- sawVOL
  scores[15,3] <- countVOL
  
  # sort
  scores <- scores[order(scores$score, decreasing = T),]
  
  scoresAllCov[[j]] <- scores
}

#### cover mixed effects models ####
for (j in 1:length(namesL)) {
  tmp <- read.csv(paste0("C:/Users/clane_897q3pb/Documents/GitHub/prairie-biomass-2018/OUT/TABLE.cover.mixedRegression.", namesL[j], ".dcover.csv"))
  tmp <- tmp[,-1]
  
  tmp$AICweight <- akaike.weights(tmp$AIC)[[3]]
  
  
  # make dataframe to record scores
  scores <- data.frame(covariate = NA, 
                       score = NA)
  
  # get VI scores
  for (i in 1:length(VIs)) {
    saw <- sum(tmp$AICweight[which(tmp$VI.used == VIs[i])])
    count <- length(tmp$AICweight[which(tmp$VI.used == VIs[i])])
    scores[i,1] <- VIs[i]
    scores[i,2] <- saw
    scores[i,3] <- count
  }
  
  
  # get volume score
  sawVOL <- sum(tmp$AICweight[which(tmp$volume != "-")])
  countVOL <- length(tmp$AICweight[which(tmp$volume != "-")])
  scores[15,1] <- "volume"
  scores[15,2] <- sawVOL
  scores[15,3] <- countVOL
  
  # sort
  scores <- scores[order(scores$score, decreasing = T),]
  
  scoresAllCovMixed[[j]] <- scores
}

