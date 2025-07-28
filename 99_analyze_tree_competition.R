###
# PMRC Tree Competition Analysis Script
# Requires installation of the "useful" and "dplyr" package
#
# Last updated 28JUL2025
###

### Import data with external script; key output is the `d_tree` dataframe
source("01_read_data.R")

### Use external script to calculate total basal area surrounding our trees.
## This script depends on the `d_tree` dataframe created by "01_read_data.R"
## Primary output is the `d_tree_competition` dataframe.
source("02_calculate_basal_area_buffer.R")

# Plot the results.
plot(d_tree_competition$dbh, d_tree_competition$compensated_buffer_area, xlab="DBH, cm", ylab="Basal area (sq. m) within 5m radius of tree", main="PMRC Tree Competitiveness")

