#===============================================================================
# Script to read the Proctor Maple Research Center file
#-------------------------------------------------------------------------------

# load dependencies ----
#-------------------------------------------------------------------------------
if(!existsFunction("read_excel")) library("readxl")

# read data file ----
d_plot <- read_excel(path = "../Plot-level_data_sheet.xlsx", 
                     sheet = "Plot")

#===============================================================================