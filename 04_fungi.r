#===============================================================================

# Hypothesis: In plots with a higher tree species richness, the macrofungal species richness is also higher. 

# Load dependencies ----
library("lme4")
library("tidyverse")

# Get tree and fungal data -----
source("01_read_data.R")

# Check that there are not misspelled species names ----
d_fungi$spp %>% unique() %>% sort()
d_tree$spp %>% unique() %>% sort()

# Calculate the number of macrofungal species per plot ----
tmp1 <- d_fungi %>% group_by(site, plot) %>% 
  summarize(n_fungi = n_distinct(spp, na.rm = FALSE), .groups = "drop")

# Calculate the number of tree species per plot ----
tmp2 <- d_tree %>% group_by(site, plot) %>% 
  summarize(n_trees = n_distinct(spp, na.rm = FALSE), .groups= "drop")

# Join two data sets ----
tmp <- left_join(x = tmp1, y = tmp2, by = c("site", "plot"))

# Plot macrofungal versus tree species diversity -----
plot(x = tmp$n_trees, y = tmp$n_fungi, axes = FALSE, 
     xlim = c(1, 10), ylim = c(0, 20), pch = 19, col = "#91b9a4",
     xlab = "Tree species richness (n)", ylab = "Macrofungal species richness (n)")
axis(side = 1)
axis(side = 2, las = 1)

# Fit a model 
mod <- lm(n_fungi ~ n_trees, data = tmp)
summary(mod)

abline(mod)
#======================================================================================