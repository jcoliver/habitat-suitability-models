# Early correlations among different assessments
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-03-13

library(dplyr)
library(tidyr)
library(ggplot2)

hab_table <- read.csv(file = "data/HabitatCampusMaps_AttrTable_20240309.csv")
head(hab_table)

# Hab_Field
# Hab_Res
# Hab_Des

hab_measures <- hab_table[, c("Hab_Field", "Hab_Res", "Hab_Des")]

cor(hab_measures)

hab_long <- hab_measures %>%
  pivot_longer(cols = everything())

hab_hist<- ggplot(data = hab_long,
                  mapping = aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name, ncol = 3)
hab_hist
ggsave(filename = "output/habitat-histogram.png", 
       plot = hab_hist,
       width = 6, units = "in")

# Correlation plot (oof. lots of points, takes a while)
# plot(hab_measures)
# Also see https://r-coder.com/correlation-plot-r/

# Look at variance, to see areas of high agreement and areas of high 
# disagreement; for ease of interpretation, normalize each to mean 0, 
# variance of 1
hab_measures_z <- scale(x = hab_measures)
# Takes a few seconds
hab_measures_var <- apply(X = hab_measures_z,
                          MARGIN = 1,
                          FUN = var)
hist(hab_measures_var)

# Add the variance back to hab measures and use the variance in the Y axis 
# for scatterplots
hab_measures$Variance <- hab_measures_var

hab_long <- hab_measures %>%
  pivot_longer(cols = -Variance)

pts_plot <- ggplot(data = hab_long, mapping = aes(x = value, y = Variance)) +
  geom_point(alpha = 0.05) +
  facet_wrap(~ name)

ggsave(filename = "output/varplot.png", plot = pts_plot)

# What about a density plot?
# Fails for Hab_Des?
dens_plot <- ggplot(data = hab_long, mapping = aes(x = value, y = Variance)) +
  geom_density_2d_filled() +
  facet_wrap(~ name)
ggsave(filename = "output/densplot.png", plot = dens_plot)
