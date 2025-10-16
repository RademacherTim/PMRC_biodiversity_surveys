#===============================================================================
# Script to read the Proctor Maple Research Center file
#-------------------------------------------------------------------------------

# load dependencies ----
library ("readxl")
library ("dplyr")

# directory variable ----
dir <- "../"

# read plot data including metadata, tree, tubing, understory, fungi, coarse 
# woody material, and wildlife data ----
file_name <- "Plot-level_data_sheet-UPDATED-TreM-TM_Copy.xlsx"
d_plot <- read_excel(path = paste0(dir, file_name), 
                     sheet = "Plot")
d_tree <- read_excel(path = paste0(dir, file_name), 
                     sheet = "Trees",
                     range = "A1:Q411") # TR - removed the last 
# five columns as they are irrelevant, as the only exist because of link to 
# additional TreMs notes. Also, removed the tap columns, as they are read in 
# separately below.
# TR - I also currently hardcoded the last row index, which will have to change 
# with varying file length. 
d_tap <- read_excel(path = paste0(dir, file_name), 
                    sheet = "Trees",
                    range = "A1:AL342", 
                    col_names = 
                      c("site", "plot", "tree_ID", "colD", "colE", "colF", 
                        "colG", "colH", "colI", "colJ", "colK", "colL", "colM", 
                        "colN", "colO", "colP", "colQ", "tap_ID1", "w1", "h1", 
                        "d1", "comments", "tap_ID2", "w2", "h2", "d2", 
                        "tap_ID3", "w3", "h3", "d3", "tap_ID4", "w4", "h4", 
                        "d4", "tap_ID5", "w5", "h5", "d5")
                      ) [, c(1:3, 18:38)]
# TR - I also currently hardcoded the last row index, which will have to change 
# with varying file length. 
d_under <- read_excel(path = paste0(dir, file_name), 
                      sheet = "Understory")
d_CWD <- read_excel(path = paste0(dir, file_name), 
                    sheet = "Coarse woody debris")
d_fungi <- read_excel(path = paste0(dir,file_name), 
                      sheet = "Fungi")
d_tubing <- read_excel(path = paste0(dir, file_name), 
                       sheet = "Tubing")
d_fauna <- read_excel(path = paste0(dir, file_name), 
                      sheet = "Wildlife")

# rename columns for code efficiency (i.e., "dbh (cm)" -> dbh) ----
d_plot <- d_plot %>% 
  rename(site = "Site #", site_name = "Site name", plot = "Plot #", 
         personnel = "Personnel", veg_date = "Veg Done Date (yyyy-mm-dd)",           
         fauna_date = "Wildlife/Tubing Done Date (yyyy-mm-dd)",
         s_time = "Start time (HH:MM)", e_time = "End time (HH:MM)", 
         FDC = "Forest development class (visual)", 
         lat = "Latitude (decimal °N)", lon = "Longitude (decimal °W)",
         ele = "Elevation (m)", asp = "Aspect (°)", slo = "Slope (°)", 
         sugared =  "Sugaring (T/F)", drainage = "Soil drainage (visual)",
         l_depth = "Litter depth (cm)", 
         rho_deer_tracks = "Density of deer tracks (N/L/M/H)",      
         rho_moose_tracks = "Density of moose tracks (N/L/M/H)",     
         rho_bear_tracks = "Density of bear tracks (N/L/M/H)",     
         comments = "Comments")
d_tree <- d_tree %>% 
  rename(site = "Site #", plot = "Plot #", tree_ID = "Tree ID", 
         alive = "Alive (T/F)", distance = "Distance (m)", 
         mag_angle = "Magnetic Angle", grid_angle = "Grid Angle (°)", 
         spp = "Species", dbh = "dbh (cm)", h ="Height (m)", 
         canopy_status = "Canopy status", canopy_damage = "Canopy damage (1-5)",
         cored = "Cored (T/F)", n_TreMs = "# of TreMs", h_TreMs = "TreMs Height",
         tapped = "Tapped (T/F)") %>%
  mutate(alive = as.logical(alive))
d_under <- d_under %>%
  rename(site = "Site #", plot = "Plot #", subplot = "Subplot", 
         name = "Common Name", spp = "Species", group = "Group", 
         substrate = "Substrate", submerged = "Submerged", 
         tree_spp = "Tree Species", on_tree = "Location on Tree", # TR - Talk to Walt as this seems redundant with substrate, which can be on tree.
         cover = "Cover (%)", gr = "Growth", GIV = "Global Importance Value",
         comments = "Comments")
d_CWD <- d_CWD %>%
  rename(site = "Site #", plot = "Plot #", transect = "Transect", 
         azimuth = "Azimuth (°)", CWD_ID = "CWD ID", dist = "Distance (m)", 
         l = "Length (m)", dia = "Diameter (cm)", 
         decomp = "Decomposition class (1-5)", spp = "Species", 
         comments = "Comments")
d_fungi <- d_fungi %>%
  rename(site = "Site #", plot = "Plot #",  date = "Date", 
         sample_ID = "Sample ID", spp = "Species", name = "Common Name", 
         substrate = "Substrate/Host", n_sporo = "Number of Sporocarps", 
         life_stage = "Observed life stages", 
         site_descrip = "Site description/conditions", dia = "Diameter (mm)",
         h = "Height/Length (mm)", stipe_morph = "Stipe Morphology", 
         gill_morph = "Gill Morphology", pileus_morph = "Pileus Morphology", 
         spore_print = "Spore Print", eco_role = "Ecological Role", 
         notes = "Notes")
d_tubing <- d_tubing %>%
  rename(site = "Site #", plot = "Plot #", tubing_ID = "Tubing ID", 
         tree_ID1 = "Tree1", tree_ID2 = "Tree2", b2 = "Boundary2", 
         l = "Length (cm)", h1 = "H1 (cm)", h2 = "H2 (cm)", h_max = "Max (cm)",
         h_min = "Min (cm)", typ = "Type", dia = "D (\")", col = "Color", 
         comments = "Comments")
d_fauna <- d_fauna %>%
  rename(site = "Site #", site_name = "Site name", # TR - Redundant, as it can be derived form site ID
         plot = "Plot #", sign = "Type of sign", spp = "Species",
         crossed = "Trail crossed tubing (Y/N)", 
         hei_t = "Tubing height at crossing (cm)",
         dia_t = "Tubing size (in)", col_t = "Tubing color", 
         comments = "Comments")
#===============================================================================

