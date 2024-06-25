# Variance calculations on three rasters
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-03-20

library(terra)     # For raster wrangling
library(ggplot2)   # Nice images
library(tidyterra) # Plotting terra objects with ggplot
library(ggpubr)    # Multi-panel figures

# Code for creating Figure Variance (tentatively figure 3)
# Panel A: A figure of entire area, showing variance calculated on three 
#          sources. Use red for *low* variance. UPDATE: red for low variance
#          does not work well. Entire map looks red.
# Panel B: Four-panel figure, of zoomed-in area for each of the three methods
#          and the variance plot. Use grayscale for three method plots (to 
#          match earlier figures) and same scale as panel A for variance.

# TODO: Need to rename things thusly:
# des(igner) -> regression
# res(earch ecologist) -> gis
# field( ecologist) -> map_layering

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

# Zoom as a SpatExtent
# zoom_ext <- terra::ext(x = c(998050, 998150, 449840, 449940))

# Extents that we want to highlight in the Zoom-in
disagree_ext <- terra::ext(x = c(998002, 998010, 449965, 449995))
agree_ext <- terra::ext(x = c(998070, 998085, 449920, 449935))
# disagree_ext <- terra::ext(x = c(998077, 998103, 449850, 449868))
# agree_ext <- terra::ext(x = c(998069, 998087, 449921, 449933))

################################################################################
# base R plots

# Plot the three original rasters and the variance raster
par(mfrow = c(2, 2))
plot(des, main = "Designer")
plot(field, main = "Field Ecologist")
plot(res, main = "Research Ecologist")
plot(var_ras, main = "Variance")
par(mfrow = c(1, 1))

# Create the zoomed-in rasters
des_crop <- terra::crop(x = des, y = zoom_ext)
field_crop <- terra::crop(x = field, y = zoom_ext)
res_crop <- terra::crop(x = res, y = zoom_ext)
var_crop <- terra::crop(x = var_ras, y = zoom_ext)

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

# Large map, just variance
pdf(file = "output/large-map.pdf")
plot(var_ras, main = "Variance", col = rev(heat.colors(n = 50)))
lines(zoom_ext, lwd = 1, col = "black")
dev.off()

# Four-panel zoom in of three methods and variance
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

# Just pull out values from the Zoom-in SpatExtent as a polygon; easier to work 
# with in ggplot + tidyterra
zoom_vect <- as.polygons(zoom_ext)
crs(zoom_vect) <- crs(var_ras)

# Panel A (just variance)
# A plot that is pretty much all red. Tough to really see...
# ggplot() +
#   geom_spatraster(data = var_ras) +
#   scale_fill_gradient(low = "#FF0000",
#                       high = "#FFFFFF")

# The greens need to be slightly different depending on if it is a fill (for 
# variance maps) or a line (for suitability maps)
agree_line <- "#84ffa1"
agree_fill <- "#e4fff1"
disagree_color <- "#FF33FF"

# TODO: For small maps (suitability and variance), need to establish endpoints 
# of palettes. These should be min/max based on larger maps.
var_limits <- minmax(var_ras)[1:2]
# Two step process because minmax gives pair of values (min/max) for each 
# raster
suit_limits <- minmax(c(des, field, res))
suit_limits <- c(min(suit_limits[1, ]), max(suit_limits[2, ]))

# For variance plot, try a purple-green gradient, green for low variance, 
# purple for high variance
# Pale green #99FFCC
# Pale violet #FFCCFF
large_var_map <- ggplot() +
  geom_spatraster(data = var_ras) +
  scale_fill_gradient(low = agree_fill,
                      high = disagree_color,
                      name = "Variance",
                      limits = var_limits) +
  geom_spatvector(data = zoom_vect, 
                  color = "#000000",
                  lwd = 0.5,
                  fill = NA) +
  theme_bw()
large_var_map
ggsave(filename = "output/figure3a.pdf", plot = large_var_map)

# Need to create three maps, with black and white color scale. Want to still 
# retain the same endpoints for the palette across each of the three zoom-in 
# maps, even though individual endpoints for each map may vary. Maybe set xlim
# and ylim through ggplot, rather than cropping rasters?
# Holy shit it actually works. But we lose a bunch of resolution...

# When we do the cropping, the resolution is maintained, but we *do* lose the 
# link across all three of the suitability map scales (one of the maps, 
# regression, only ranges from 4-5 in zoomed-in version). UPDATE: can set 
# endpoint values for cells with the limits argument of scale_fill_gradient().

# Create SpatVectors for those rectangles of interest, easier to plot in 
# ggplot+tidyterra with geom_spatvector
disagree_vect <- as.polygons(disagree_ext)
agree_vect <- as.polygons(agree_ext)

# Update CRS based on var_ras
crs(disagree_vect) <- crs(var_ras)
crs(agree_vect) <- crs(var_ras)

# Make four separate plot objects
small_des_map <- ggplot() +
  geom_spatraster(data = terra::crop(x = des, y = zoom_ext)) +
  scale_fill_gradient(low = "#FFFFFF",
                      high = "#000000",
                      name = "Suitability",
                      limits = suit_limits) +
  # xlim(c(zoom_list$xmin, zoom_list$xmax)) +
  # ylim(c(zoom_list$ymin, zoom_list$ymax)) +
  geom_spatvector(data = disagree_vect, 
                  color = disagree_color,
                  lwd = 1,
                  fill = NA) +
  geom_spatvector(data = agree_vect, 
                  color = agree_line,
                  lwd = 1,
                  fill = NA) +
  theme_bw() +
  ggtitle(label = "Map Layering") +
  theme(axis.text = element_blank())
small_des_map

small_field_map <- ggplot() +
  geom_spatraster(data = terra::crop(x = field, y = zoom_ext)) +
  scale_fill_gradient(low = "#FFFFFF",
                      high = "#000000",
                      name = "Suitability",
                      limits = suit_limits) +
  # xlim(c(zoom_list$xmin, zoom_list$xmax)) +
  # ylim(c(zoom_list$ymin, zoom_list$ymax)) +
  geom_spatvector(data = disagree_vect, 
                  color = disagree_color,
                  lwd = 1,
                  fill = NA) +
  geom_spatvector(data = agree_vect, 
                  color = agree_line,
                  lwd = 1,
                  fill = NA) +
  theme_bw() +
  ggtitle(label = "GIS") +
  theme(axis.text = element_blank())
small_field_map

small_res_map <- ggplot() +
  geom_spatraster(data = terra::crop(x = res, y = zoom_ext)) +
  scale_fill_gradient(low = "#FFFFFF",
                      high = "#000000",
                      name = "Suitability",
                      limits = suit_limits) +
  # xlim(c(zoom_list$xmin, zoom_list$xmax)) +
  # ylim(c(zoom_list$ymin, zoom_list$ymax)) +
  geom_spatvector(data = disagree_vect, 
                  color = disagree_color,
                  lwd = 1,
                  fill = NA) +
  geom_spatvector(data = agree_vect, 
                  color = agree_line,
                  lwd = 1,
                  fill = NA) +
  theme_bw() +
  ggtitle(label = "Regression") +
  theme(axis.text = element_blank())
small_res_map

small_var_map <- ggplot() +
  geom_spatraster(data = terra::crop(x = var_ras, y = zoom_ext)) +
  scale_fill_gradient(low = agree_fill,
                      high = disagree_color,
                      name = "Variance",
                      limits = var_limits) +
  # xlim(c(zoom_list$xmin, zoom_list$xmax)) +
  # ylim(c(zoom_list$ymin, zoom_list$ymax)) +
  geom_spatvector(data = disagree_vect, 
                  color = "#000000",
                  lwd = 1,
                  fill = NA) +
  geom_spatvector(data = agree_vect, 
                  color = "#000000",
                  lwd = 1,
                  fill = NA) +
  theme_bw() +
  ggtitle(label = "Variance") +
  theme(axis.text = element_blank())
small_var_map

# Stitch all four small maps together into single image

# ggarrange does a poor job with labels
# des(igner) -> map_layering
# res(earch ecologist) -> regression
# field( ecologist) -> gis
# labels_list <- list("Map layering", "Regression", "GIS", "Variance")

four_panel <- ggpubr::ggarrange(small_des_map, small_field_map,
                                small_res_map, small_var_map,
                                # labels = labels_list,
                                ncol = 2, nrow = 2)
four_panel
ggsave(filename = "output/figure3b.pdf", plot = four_panel)

# Try putting A (variance of entire area) and B (four-panel plot of the three 
# methods and variance zoomed in) into a single image
# No. This is hideous. Need to do it manually
# fig_ab <- ggpubr::ggarrange(large_var_map, four_panel,
#                             labels = c("A", "B"),
#                             ncol = 2, nrow = 1)
# fig_ab
