library(ggplot2)
library(dplyr)
library(caret)
library(raster)
library(sp)
library(cowplot)

# Set first example data (not Mike Trout's)
df <- data.frame("y" = c(2.594, 3.803, 3.254, 3.599, 
                          3.617, 3.297, 2.093, 3.611, 2.842, 3.316, 2.872, 3.228, 3.633, 
                          4.28, 3.309, 2.8, 2.632, 3.754, 2.207, 3.604, 3.443, 2.188, 3.452, 
                          2.553, 3.382, 3.067, 2.986, 2.785, 2.567, 3.804), 
                "x" = c(0.059, -1.596, -0.65, -0.782, -0.301, -0.104, 0.057, -0.807, 0.003, 
                          1.661, 0.088, -0.32, -1.115, -0.146, -0.364, -0.952, 0.254, 0.109, 
                          -0.671, -0.803, -0.212, -0.069, -0.09, -0.472, 0.434, 0.337, 
                          0.723, 0.508, -0.197, -0.635), 
                "z" = c(69.891, 73.352, 83.942, 85.67, 79.454, 85.277, 81.078, 73.573, 77.272, 59.263, 
                           97.343, 91.436, 76.264, 83.479, 47.576, 84.13, 60.475, 61.093, 
                          84.54, 69.959, 88.729, 88.019, 82.18, 83.684, 86.296, 90.605, 
                         79.945, 59.899, 62.522, 77.75)
                )
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.95
outKzone <- 0.95
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)
df$xy <- df$x * df$y # include interaction term

# additive model for exit velocity as function of x, y, and interaction term
train_control <- trainControl(method = "cv")
GAM_model <- train(z ~ x + y + xy ,
                   data = df,
                   method = "gamSpline",
                   trControl = train_control
)

# interpolation target grid
grid <- data.frame(expand.grid(x=seq(-1.8,2,.2), y=seq(1.5,4.5,.2)))
grid$xy <- grid$x * grid$y

#clip grid
spdf <- SpatialPointsDataFrame(coords = df[,c(2,1)], data = df) #points <- SpatialPoints( df[,c(1,2)])
for (i in 1:nrow(grid)) {
    grid$dist[i] = min(pointDistance( spdf, c(grid[i,1], grid[i,2]), lonlat=FALSE, allpairs=F))
    }
grid2 <- grid[grid$dist<1,]

# get fitted (interpolated) values
grid2$z <- predict(GAM_model, grid2)

library(RColorBrewer)
library(ggthemes)
# build color Palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

ggplot(grid2, aes(x = x, y = y)) + 
  geom_raster(aes(fill=z)) + 
  geom_path(data = kZone, aes(x,y)) +
  geom_point(data = df, aes(x,y)) +
  scale_fill_gradientn(colours = myPalette(8)) +
  coord_fixed() +
  ylab("Height") + xlab("Horizontal Location") + labs(fill='Exit Velocity') 

# now use Mike Trout's data
df <- read.csv(choose.files(), stringsAsFactors = F)
df$x <- as.numeric(df$x)
df$y <- as.numeric(df$y)
df$xy <- df$x * df$y
df <- na.omit(df)

training_control <- trainControl(method = "repeatedcv", search = "random", repeats = 5, allowParallel = FALSE)

# fit with gaussian process model
GP_model <- train(z ~ x + y + xy,
                   data = df, tuneLength =20,
                   method = "gaussprRadial", 
                   na.action = na.pass,
                   trControl = training_control
)
GP_model
ggplot(GP_model)

# interpolation target grid
grid <- data.frame(expand.grid(x=seq(-2,2,.2), y=seq(.5,4.5,.2)))
grid$xy <- grid$x * grid$y

#clip grid
spdf <- SpatialPointsDataFrame(coords = df[,c(1,2)], data = df) #points <- SpatialPoints( df[,c(1,2)])
for (i in 1:nrow(grid)) {
  grid$dist[i] = min(pointDistance( spdf, c(grid[i,1], grid[i,2]), lonlat=FALSE, allpairs=F))
}
grid2 <- grid[grid$dist<1,]

# get fitted (interpolated) values
grid2$z <- predict(GP_model, grid2)

ggplot(grid2, aes(x = x, y = y)) + 
  geom_raster(aes(fill=z)) + 
  geom_path(data = kZone, aes(x,y)) +
  geom_point(data = df, aes(x,y), size = 1) +
  scale_fill_gradientn(colours = myPalette(8)) +
  coord_fixed() +
  ylab("Height") + xlab("Horizontal Location") + labs(fill='Exit Velocity') 


