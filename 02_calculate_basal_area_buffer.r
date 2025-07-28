##########
#
# 02_calculate_basal_area_buffer.R
#
# Using the `d_tree` output dataframe from the `01_read_data.R` script,
#   which must contain `plot`, `tree_ID`, `distance` (meters), `grid_angle` (degrees), and `dbh` (cm) columns,
#   this script calculates the total basal area within a 5-m radius of the tree, not including the tree itself.
#   
# The primary output is the `d_tree_competition` dataframe, which contains a subset of `d_tree` columns, plus:
#  - `basal_area`: basal area of the given tree in meters squared.
#  - `x`, `y`: Cartesian coordinates of the tree in the plot, where plot center is (0,0)
#  - `overlap_fraction`: fraction of the tree's 5-m buffer circle that overlaps with the plot circle (between 0 and 1)
#  - `buffer_basal_area`: total basal area (m) of other trees centered within a given tree's 5-m buffer radius
#  - `compensated_basal_area`: the `buffer_basal_area` column divided by the `overlap_fraction`.
#		-- **The purpose of this column is to add some "imagined" basal area for trees at the edge
#			of the plot circle, whose buffer radius likely includes trees not captured in measurements.
#
# Requires installation of the "useful" and "dplyr" packages
#
##########


### Make a copy of d_tree for processing within this script
d_tree_copy <- d_tree

### Data Pre-Processing for distance calculations using radial coordinates.

d_tree_copy$dbh <- as.numeric(d_tree_copy$dbh)

# Subset to relevant columns
d_tree_copy <- select(d_tree_copy, c('plot', 'tree_ID', 'distance', 'grid_angle', 'dbh'))

# Drop rows with DBH greater than 100cm
d_tree_copy <- d_tree_copy[d_tree_copy$dbh <= 100,]

#Drop rows with missing values
d_tree_copy <- na.omit(d_tree_copy)

#calculate basal area (meters squared) from the DBH (cm)
d_tree_copy$basal_area <- (pi /4) * (d_tree_copy$dbh)^2 * 0.0001

# Convert polar coordinates (r, theta) to cartesian (x,y) [meters]
library ("useful")
library ("dplyr")
d_xy_coords <- pol2cart(d_tree_copy$distance, d_tree_copy$grid_angle, degrees=TRUE)
d_tree_copy$x <- d_xy_coords$x
d_tree_copy$y <- d_xy_coords$y

# For each tree, calculate the areal overlap between a 5-m radius circle 
# about its center and the 11.3-m radius plot circle.
# Convert the output as a percentage (decimal) between 0 and 1, 
# then store the percentage decimal output is stored as column name `overlap_fraction`
circle_overlap <- function(row, tree_radius=5, plot_radius=11.3) {
  #### Define a function to calculate overlap fraction (between 0 and 1)
  #### Assume x and y are your 1st and 2nd columns
  # Calculate the distance between the centers of the circles
  x <- row[1]
  y <- row[2]
  d <- sqrt(x^2 + y^2)
  max_fully_enclosed_radius <- abs(plot_radius - tree_radius)
  tree_buffer_area <- pi * tree_radius^2
  
  # If fully enclosed in outer circle, return 1
  ifelse(d <= max_fully_enclosed_radius, return(1), 0)
  
  # Otherwise, calculate the overlap and return the overlap fraction
  phi <- (acos((tree_radius^2 + d^2 - plot_radius^2) / (2 * tree_radius * d))) * 2
  theta <- (acos((plot_radius^2 + d^2 - tree_radius^2) / (2 * plot_radius * d))) * 2
  area2 <- 0.5 * theta * plot_radius^2 - 0.5 * plot_radius^2 * sin(theta)
  area1 <- 0.5 * phi * tree_radius^2 - 0.5 * tree_radius^2 * sin(phi)
  return((area1 + area2)/tree_buffer_area)
  }

d_tree_copy$overlap_fraction <- apply(d_xy_coords, MARGIN=1, FUN=circle_overlap)

##### Find basal area within tree buffer radius
## I'll use a for loop. Probably not the most efficient, but it's small.

# Split up `d_tree` into a list of dataframes based on `plot`
plot_df_list <- split(d_tree_copy, d_tree$plot)
# Create an dataframe to populate with updated dataframes per plot
updated_d_tree <- tibble(plot=numeric(), tree_ID=character(), distance=numeric(), grid_angle=numeric(), dbh=numeric(), basal_area=numeric(), x=numeric(), y=numeric(), overlap_fraction=numeric(), buffer_basal_area=numeric())
for (plot in plot_df_list) {
  # For each tree in the plot, find the distance to all other trees,
  # and if the distance is less than the specified radius (use 5m),
  # then add the other tree's basal area to the buffer_basal_area variable
  #print(str(plot))
  x_list <- plot$x
  y_list <- plot$y
  
  basal_area_list <- plot$basal_area
  total_trees <- nrow(plot)
  buffer_basal_area_list <- list()
  for (tree_index in seq(1,total_trees)) {
    x1 <- x_list[[tree_index]]
    y1 <- y_list[[tree_index]]
    buffer_basal_area <- 0
    for (other_tree_index in seq(1, total_trees)) {
      other_x = x_list[[other_tree_index]]
      other_y = y_list[[other_tree_index]]
      tree2tree_dist <- sqrt( (other_x - x1)^2 + (other_y - y1)^2 )
      # Only add in if not zero (the tree distanced to itself) and less than our basal area buffer radius in meters
      if(tree2tree_dist != 0 && tree2tree_dist <= 5) {
        buffer_basal_area <- buffer_basal_area + basal_area_list[[other_tree_index]]
      }
    }
    plot[tree_index, "buffer_basal_area"] <- buffer_basal_area
  }
  updated_d_tree <- full_join(updated_d_tree, plot)
}

# Use the `overlap_fraction` to compensate for trees at the boundary (low overlap) versus those at the center (high overlap).
# We'll divide the `buffer_basal_area` (m) column by the overlap fraction and store as `compensated_buffer_area`
updated_d_tree$compensated_buffer_area <- updated_d_tree$buffer_basal_area / updated_d_tree$overlap_fraction

# The final output will be `d_tree_competition`, for a neater naming convention and to indicate that it's the final output.
d_tree_competition <- updated_d_tree
#These variables are no longer needed. Delete them to minimize clutter in the global environment.
rm(updated_d_tree, d_tree_copy, plot, plot_df_list, d_xy_coords, basal_area_list, buffer_basal_area, buffer_basal_area_list, other_tree_index, other_x, other_y, total_trees, tree_index, tree2tree_dist, x_list, x1, y_list, y1, circle_overlap)
