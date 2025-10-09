# Script to do some statistical analysis on the Proctor plot data
#-------------------------------------------------------------------------------

# source data ----
source('01_read_data.R')

# load dependency ----
library('lme4')

# plot of the number of TreMs versus DBH ----
par(mar = c(5, 5, 1, 1), mfrow = c(1, 2))
plot (x = d_tree$dbh[d_tree$alive == "T"], 
      y = d_tree$n_TreMs[d_tree$alive == "T"], 
      xlim = c(0, 100), axes = FALSE, ylim = c(0, 15),
      xlab = "Diameter at breast height (cm)",
      ylab = "Number of TreMs", pch = 19, col = "#91b9a499")
text(x = 100, y = 15, pos = 2, labels = "Alive")
axis(side = 1)
axis(side = 2, las = 1)

fit_alive <- lm(n_TreMs ~ dbh, data = d_tree[d_tree$alive == "T", ])
abline(fit_alive, col = "#91b9a4", lty = 2, lwd = 2)

par(mar = c (5, 1, 1, 1))
plot (x = d_tree$dbh[d_tree$alive == "F"], 
      y = d_tree$n_TreMs[d_tree$alive == "F"], 
      xlim = c(0, 100), axes = FALSE, ylim = c(0, 15), 
      xlab = "Diameter at breast height (cm)",
      ylab = "", pch = 19, col = "#C46F33")
axis(side = 1)
axis(side = 2, las = 1)
text(x = 100, y = 15, pos = 2, labels = "Dead")

fit_dead <- lm(n_TreMs ~ dbh, data = d_tree[d_tree$alive == "F", ])
abline(fit_dead, col = "#C46F33", lty = 2, lwd = 2)


fit2 <- lmer(n_TreMs ~ dbh + (1 | spp), data = d_tree)
coef(fit2)
abline(fit2)

# 
d_tree %>% 
  mutate(TreM_1 = strsplit(TreMs, ",")) %>%  
  mutate(TreM_1 = lapply(TreM_1, as.numeric)) %>% 
  select(TreM_1)

strsplit(d_tree$TreMs[1:10], ",")[[1]]
