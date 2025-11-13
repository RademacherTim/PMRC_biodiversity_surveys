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
tmp1 <- d_fungi %>% 
  #mutate(substrate = ifelse(substrate %in% c("Snag", "Deadwood Bark", "Dead branch"), "Deadwood", substrate)) %>% 
  group_by(site, plot) %>% 
  summarize(n_fungi = n_distinct(spp, na.rm = FALSE), .groups = "drop")

# Calculate the number of tree species per plot ----
tmp2 <- d_tree %>% group_by(site, plot) %>% 
  summarize(n_trees = n_distinct(spp, na.rm = FALSE), .groups= "drop")

# Join two data sets ----
tmp <- left_join(x = tmp1, y = tmp2, by = c("site", "plot"))

# Plot macrofungal versus tree species diversity -----
par(mfrow = c(1, 1), mar = c(5, 5, 1, 1))
plot(x = tmp$n_trees, 
     y = tmp$n_fungi, 
     axes = FALSE, 
     xlim = c(1, 10), ylim = c(0, 20), pch = 19, col = "brown",
     xlab = "Tree species richness (n)", ylab = "Macrofungal species richness (n)")
plot(x = tmp$n_trees[tmp$substrate == "Deadwood"], 
     y = tmp$n_fungi[tmp$substrate == "Deadwood"], axes = FALSE, 
     xlim = c(1, 10), ylim = c(0, 20), pch = 19, col = "brown",
     xlab = "Tree species richness (n)", ylab = "Macrofungal species richness (n)")
axis(side = 1)
axis(side = 2, las = 1)
points(x = tmp$n_trees[tmp$substrate == "Soil"], 
       y = tmp$n_fungi[tmp$substrate == "Soil"],
       col = "brown1")
points(x = tmp$n_trees[tmp$substrate == "Tree"], 
       y = tmp$n_fungi[tmp$substrate == "Tree"],
       col = "darkgreen")

# Fit a model 
mod <- lmer(n_fungi ~ n_trees, data = tmp)
summary(mod)

abline(mod, col = "#91b4a9", lwd = 2)

# Compare sugared versus non-sugared plots ----
tmp1 <- d_plot %>% select(plot, sugared)
tmp2 <- d_fungi %>% group_by(site, plot) %>% 
  summarize(n_fungi = n_distinct(spp, na.rm = FALSE),
            N_fungi = sum(n_sporo, na.rm = TRUE), .groups = "drop")

# Join two data sets ----
tmp <- left_join(x = tmp1, y = tmp2, by = "plot") %>% filter(!is.na(site))

# Plot boxplots ----
par(mar = c(5, 5, 1, 1), mfrow = c(1, 2))
boxplot(n_fungi ~ sugared, data = tmp, las  = 1, xlab = "", 
        ylab = "Number of species", ylim = c(0, 25), lwd = 2,
        axes = FALSE, xlim = c(0, 2), at = c(0.5, 1.5), 
   border = c("#4a6741cc", "#ee7600cc"))
points(x = jitter(rep(0.5, 6), 10), y = tmp$n_fungi[tmp$sugared == "F"], 
       pch = 19,
       col ="#4a6741", cex = 2)
points(x = jitter(rep(1.5, 6), 10), y = tmp$n_fungi[tmp$sugared == "T"], 
       pch = 19,
       col ="#ee7600", cex = 2)
axis(side = 1, at = c(0.5, 1.5), labels = c("Non-sugared", "Sugared"))
axis(side = 2, las = 1)
par(mar = c(5, 1, 1, 5))
boxplot(N_fungi ~ sugared, data = tmp, xlab = "", ylab = "", axes = FALSE, 
        at = c(0.5, 1.5), lwd = 2,
        xlim = c(0, 2),
        ylim = c(0, 300), border = c("#4a6741cc", "#ee7600cc"))
points(x = jitter(rep(0.5, 6), 10), y = tmp$N_fungi[tmp$sugared == "F"], pch = 19,
       col ="#4a6741", cex = 2)
points(x = jitter(rep(1.5, 6), 10), y = tmp$N_fungi[tmp$sugared == "T"], pch = 19,
       col ="#ee7600", cex = 2)
axis(side = 4, las = 1)
mtext(side = 4, line = 3, text = "Number of sporocarps")
axis(side = 1, at = c(0.5, 1.5), labels = c("Non-sugared", "Sugared"))

# Two-sample t-test to see whether the number of species/sporocarps is different 
# between sugared and non-sugared plots ----
t.test(n_fungi ~ sugared, data = tmp)
t.test(N_fungi ~ sugared, data = tmp)
#======================================================================================

