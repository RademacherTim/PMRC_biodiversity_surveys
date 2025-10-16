# Script to do some statistical analysis on the Proctor plot data
#-------------------------------------------------------------------------------

# source data ----
source('01_read_data.R')

# load dependency ----
library('lme4')

# remove the data for trees that we do not have at least data for 20 
# individuals from that species
d_tmp <- d_tree %>% filter(spp %in% c("American Beech", 
                                      "Red Maple", 
                                      "Red Spruce",
                                      "Sugar Maple",
                                      "White Ash",
                                      "Yellow Birch"))

# plot of the number of TreMs versus DBH for six species with more than 20 
# individuals ----
par(mfrow = c(6, 2))
for (species in c("American Beech", "Red Maple", "Red Spruce", "Sugar Maple", "White Ash", "Yellow Birch")){
  par(mar = c(5, 5, 1, 1))
  con <- d_tmp$alive & d_tmp$spp == species
  plot (x = d_tmp$dbh[con], 
        y = d_tmp$n_TreMs[con], 
        xlim = c(0, 100), axes = FALSE, ylim = c(0, 16),
        xlab = "Diameter at breast height (cm)",
        ylab = "Number of TreMs", pch = 19, col = "#91b9a499")
  text(x = 20, y = 16, pos = 2, labels = "Alive")
  axis(side = 1)
  axis(side = 2, las = 1)
  
  fit_alive <- lm(n_TreMs ~ dbh, data = d_tmp[d_tmp$alive, ])
  abline(fit_alive, col = "#91b9a4", lty = 2, lwd = 2)
  
  par(mar = c (5, 1, 1, 1))
  con <- !d_tmp$alive & d_tmp$spp == species
  plot (x = d_tmp$dbh[con], 
        y = d_tmp$n_TreMs[con], 
        xlim = c(0, 100), axes = FALSE, ylim = c(0, 16), 
        xlab = "Diameter at breast height (cm)",
        ylab = "", pch = 19, col = "#C46F33")
  axis(side = 1)
  axis(side = 2, las = 1)
  text(x = 20, y = 16, pos = 2, labels = "Dead")
  
  fit_dead <- lm(n_TreMs ~ dbh, data = d_tmp[!d_tmp$alive, ])
  abline(fit_dead, col = "#C46F33", lty = 2, lwd = 2)

}

# Red Maple ----
par (mar = c(5, 5, 1, 1))
con <- d_tmp$alive & d_tmp$spp == "Red Maple"
plot (x = d_tmp$dbh[con], 
      y = d_tmp$n_TreMs[con], 
      xlim = c(0, 100), axes = FALSE, ylim = c(0, 16),
      xlab = "Diameter at breast height (cm)",
      ylab = "Number of TreMs", pch = 19, col = "#91b9a499")
text(x = 20, y = 16, pos = 2, labels = "Alive")
axis(side = 1)
axis(side = 2, las = 1)

fit_alive <- lm(n_TreMs ~ dbh, data = d_tmp[d_tmp$alive, ])
abline(fit_alive, col = "#91b9a4", lty = 2, lwd = 2)

par(mar = c (5, 1, 1, 1))
con <- !d_tmp$alive & d_tmp$spp == "Red Maple"
plot (x = d_tmp$dbh[con], 
      y = d_tmp$n_TreMs[con], 
      xlim = c(0, 100), axes = FALSE, ylim = c(0, 16), 
      xlab = "Diameter at breast height (cm)",
      ylab = "", pch = 19, col = "#C46F33")
axis(side = 1)
axis(side = 2, las = 1)
text(x = 20, y = 16, pos = 2, labels = "Dead")

fit_dead <- lm(n_TreMs ~ dbh, data = d_tmp[!d_tmp$alive, ])
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
