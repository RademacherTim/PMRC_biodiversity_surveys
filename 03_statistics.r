# Script to do some statistical analysis on the Proctor plot data
#-------------------------------------------------------------------------------

# source data ----
source('01_read_data.R')

# load dependency ----
library('lme4')

# making a plot ----
par(mar = c(5, 5, 1, 1))
plot (d_tree$dbh, d_tree$n_TreMs, xlim = c(0, 100), axes = FALSE, 
      xlab = "Diameter at breast height (cm)",
      ylab = "Number of TreMs", pch = 19, col = "#91b9a499")
axis(side = 1)
axis(side = 2, las = 1)

fit <- lm(n_TreMs ~ dbh, data = d_tree)

abline(fit, col = "red")

fit2 <- lmer (n_TreMs ~ dbh + (1 | spp), data = d_tree)
coef(fit2)
abline()

# 
d_tree %>% 
  mutate(TreM_1 = strsplit(TreMs, ",")) %>%  
  mutate(TreM_1 = lapply(TreM_1, as.numeric)) %>% 
  select(TreM_1)

strsplit(d_tree$TreMs[1:10], ",")[[1]]
