# this script turns a txt file into a list of polygons

# read in plot coords from txt file
plotcoords <- read.csv("../DATA/prairiecoords.txt", sep = "\t", header = TRUE)

# cut out extra columns
plotswithoutNA <- plotcoords[c(5,6)]

# make new df called plotswithNA
plotswithNA <- data.frame()

# make new df with polygons
i <- 1
for (i in 1:(length(plotswithoutNA$X_est)/4)) {
  j <- ((i-1)*4)
  plotswithNA <- rbind(plotswithNA, plotswithoutNA[j+1,])
  plotswithNA <- rbind(plotswithNA, plotswithoutNA[j+2,])
  plotswithNA <- rbind(plotswithNA, plotswithoutNA[j+3,])
  plotswithNA <- rbind(plotswithNA, plotswithoutNA[j+4,])
  plotswithNA <- rbind(plotswithNA, plotswithoutNA[j+1,])
  plotswithNA <- rbind(plotswithNA, c(NA, NA))
  i <- i + 1
} 

# rename columns
names(plotswithNA)[names(plotswithNA)=="X_est"] <- "x"
names(plotswithNA)[names(plotswithNA)=="Y_est"] <- "y"

# convert df to list of polygons
i <- 1
plotlist <- list(plotswithNA[1:5,1:2])
for (i in 1:(length(plotswithNA$x)/6)) {
  start <- i * 6 + 1
  end <- (i * 6 + 1) + 4
  plotlist[[length(plotlist) + 1]] <- plotswithNA[start:end, 1:2]
  i <- i + 1
} 
plotlist[[438]] <- NULL # remove last item from list (it is empty)

