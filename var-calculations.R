# Variance calculations on three rasters
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-03-20

library(terra)
library(ggplot2)
library(tidyterra)

# Code for creating Figure Variance (tentatively figure 3)
# Panel A: A figure of entire area, showing variance calculated on three 
#          sources. Use red for *low* variance.
# Panel B: Four-panel figure, of zoomed-in area for each of the three methods
#          and the variance plot. Use grayscale for three method plots (to 
#          match earlier figures) and same scale as panel A for variance.

# Load in the three rasters
des <- terra::rast(x = "data/20240320_RasterMaps/DSGNRASTER.tif")
field <- terra::rast(x = "data/20240320_RasterMaps/Field_20240320.tif")
res <- terra::rast(x = "data/20240320_RasterMaps/Res_20240320.tif")

# Stick the three rasters together into a SpatRasterDataset so we can calculate 
# variance across them all
three_ras <- terra::sds(x = list(des, field, res))

# NOPE: Apply the var function across the rasters; takes a few minutes
# var_ras <- terra::app(three_ras, fun = var)
# plot(var_ras)

# Better to use the sd function, as that is done by C++? OMG, yes, so much 
# faster
sd_ras <- terra::app(three_ras, fun = sd)
plot(sd_ras)
var_ras <- sd_ras * sd_ras
plot(var_ras)

# Plot the three original rasters and the variance raster
par(mfrow = c(2, 2))
plot(des, main = "Designer")
plot(field, main = "Field Ecologist")
plot(res, main = "Research Ecologist")
plot(var_ras, main = "Variance")
par(mfrow = c(1, 1))

# Zoom in for an example (xmin, xmax, ymin, ymax)
# Large area, including Old Main, Student Union, and part of mall
# zoom_ext <- terra::ext(x = c(997500, 998500, 449000, 450000))
# Old Main:
# zoom_ext <- terra::ext(x = c(997700, 998000, 449300, 449600))
# A northeast spot
# zoom_ext <- terra::ext(x = c(999500, 1000000, 451100, 451500))
# Engineering, north of Old Main
# zoom_ext <- terra::ext(x = c(997500, 998100, 449500, 450000))
# Spot a little northwest of student union
zoom_ext <- terra::ext(x = c(998000, 998100, 449900, 450000))

# Create the zoomed-in rasters
des_crop <- terra::crop(x = des, y = zoom_ext)
field_crop <- terra::crop(x = field, y = zoom_ext)
res_crop <- terra::crop(x = res, y = zoom_ext)
var_crop <- terra::crop(x = var_ras, y = zoom_ext)

# Extents that we want to highlight in the Zoom-in
disagree_ext <- terra::ext(x = c(998002, 998010, 449965, 449995))
agree_ext <- terra::ext(x = c(998070, 998085, 449920, 449935))

################################################################################
# base R plots

# Plotting the rasters, then zooming in
# A four-panel figure showing three models and variance, with a black rectangle
# in each indicating area of zoom
# pdf(file = "output/large-map.pdf")
# par(mfrow = c(2, 2))
# plot(des, main = "Designer")
# lines(zoom_ext, lwd = 1, col = "black")
# plot(field, main = "Field Ecologist")
# lines(zoom_ext, lwd = 1, col = "black")
# plot(res, main = "Research Ecologist")
# lines(zoom_ext, lwd = 1, col = "black")
# plot(var_ras, main = "Variance", col = rev(heat.colors(n = 50)))
# lines(zoom_ext, lwd = 1, col = "black")
# par(mfrow = c(1, 1))
# dev.off()

pdf(file = "output/large-map.pdf")
plot(var_ras, main = "Variance", col = rev(heat.colors(n = 50)))
lines(zoom_ext, lwd = 1, col = "black")
dev.off()


pdf(file = "output/zoom-map.pdf")
par(mfrow = c(2, 2))
plot(des_crop, main = "Designer")
lines(x = disagree_ext, col = "black", lwd = 1.5)
lines(x = agree_ext, col = "royalblue1", lwd = 1.5)
plot(field_crop, main = "Field Ecologist")
lines(x = disagree_ext, col = "black", lwd = 1.5)
lines(x = agree_ext, col = "royalblue1", lwd = 1.5)
plot(res_crop, main = "Research Ecologist")
lines(x = disagree_ext, col = "black", lwd = 1.5)
lines(x = agree_ext, col = "royalblue1", lwd = 1.5)
plot(var_crop, main = "Variance", col = rev(heat.colors(n = 50)))
lines(x = disagree_ext, col = "black", lwd = 1.5)
lines(x = agree_ext, col = "royalblue1", lwd = 1.5)
par(mfrow = c(1, 1))
dev.off()

################################################################################
# The ggplot way
# Panel A (just variance)
# A plot that is pretty much all red. Tough to really see...
ggplot() +
  geom_spatraster(data = var_ras) +
  scale_fill_gradient(low = "#FF0000",
                      high = "#FFFFFF")

# For variance plot, try a purple-green gradient, green for low variance, 
# purple for high variance
# Pale green #99FFCC
# Pale violet #FFCCFF
ggplot() +
  geom_spatraster(data = var_ras) +
  scale_fill_gradient(low = "#e4fff1",
                      high = "#FF33FF") +
  geom_rect(mapping = aes(xmin = 998000, 
                          xmax = 998100,
                          ymin = 449900,
                          ymax = 450000),
            lwd = 0.5,
            color = "#000000",
            fill = NA) +
  theme_bw()


