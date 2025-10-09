# Script to do some statistical analysis on the Proctor plot data
#-------------------------------------------------------------------------------

# source data ----
source('01_read_data.R')

# load dependency ----
library('lme4')

# making a plot
plot (d_tree$dbh, d_tree$n_TreMs, xlim = c(0, 100))

fit <- lm(n_TreMs ~ dbh, data = d_tree)

abline(fit2, col = "red")

fit2 <- lmer (n_TreMs ~ dbh + (1 | spp), data = d_tree)
coef(fit2)

# 
d_tree %>% 
  mutate(TreM_1 = strsplit(TreMs, ",")) %>%  
  mutate(TreM_1 = lapply(TreM_1, as.numeric)) %>% 
  select(TreM_1)

strsplit(d_tree$TreMs[1:10], ",")[[1]]
