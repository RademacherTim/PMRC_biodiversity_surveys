#===============================================================================
# Script to read the Proctor Maple Research Center file
#-------------------------------------------------------------------------------

# load dependencies ----
#-------------------------------------------------------------------------------
if(!existsFunction("read_excel")) library("readxl")
library("dplyr")

# read plot data ----
d_plot <- read_excel(path = "../Plot-level_data_sheet.xlsx", 
                     sheet = "Plot")

# read tree data ----
d_tree <- read_excel(path = "../Plot-level_data_sheet.xlsx", 
                     sheet = "Trees")

# rename dbh column ----
d_tree <- d_tree %>% rename(dbh = `dbh (cm)`)

hist(d_tree$dbh)
d_tree[d_tree$dbh > 300, ]
which(d_tree$dbh > 300)
d_tree$dbh[161]

hist(d_tree$dbh[d_tree$dbh < 400], 
     col = "olivedrab",
     xlim = c(0, 100), 
     breaks = seq(0, 400, by = 5))

par(mar = c(5, 5, 1, 1))
plot(x = d_tree$`Distance (m)`,
     y = d_tree$dbh,
     ylim = c(0, 100),
     xlab = "Distance (m)",
     ylab = "dbh (cm)",
     col = "#91a4b966",
     las = 1, pch = 19, axes = F)
axis(side = 1)
axis(side = 2, las = 1)


#===============================================================================