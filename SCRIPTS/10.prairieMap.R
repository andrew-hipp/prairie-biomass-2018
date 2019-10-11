library(raster)
library(ggplot2)


ndvi <- raster("C:/Users/clane_897q3pb/Documents/projects/prairie/tiffs_final/NDVI.tif")

# aggregate
ndviAgg <- aggregate(ndvi,fact=100,fun=mean)
ndviAgg10 <- aggregate(ndvi,fact=10,fun=mean)

# convert to df
ndviAgg_test <- as(ndviAgg10, "SpatialPixelsDataFrame")
ndviAgg_df <- as.data.frame(ndviAgg_test)
colnames(ndviAgg_df) <- c("value", "x", "y")

# make color scale
colScale <- c("blue", "blue", 
              "deeppink3", "red", "yellow", "yellow", 
              "green", "springgreen4")

# plot
jpeg("../OUT/FIGURE.map.jpg", width = 702, height = 440)

ggplot(ndviAgg_df, aes(x = ndviAgg_df$x,
                       y = ndviAgg_df$y,
                       fill = ndviAgg_df$value)) +
  geom_tile() +
  scale_fill_gradientn(colors = colScale,
                      limits = c(-0.25, 1),
                      name = "NDVI") +
  theme_bw() +
  coord_fixed(xlim = c(-88.09295, -88.09215), 
              ylim = c(41.82035, 41.82082)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dev.off()

