# Title: Congressional District Templates
# Author: Yui Fujii
# Start Date: 10/26/2022
# Most Recent Update Date: 011/21/2024
# First, load the pacman package


# Define the list of packages to load
pacman::p_load(
  rio,                                     # to import data
  here,                                    # to locate files 
  skimr,                                   # Provides summary statistics, Data type info, visual summaries
  dplyr,                                   # Data manipulation > Filtering, Transformation, Joining, etc. 
  tidyverse,                               # to clean, handle, and plot the data (includes ggplot2 package)
  sf,                                      # to manage spatial data using a Simple Feature format
  tmap,                                    # to produce simple maps, works for both interactive and static maps
  xlsx,
  RColorBrewer,                            # Provides a collection of color palettes 
  ggborderline,                            # Swaps out ggplot2 lines with 'border' equivalents
  tigris,                                  # Provides access to spatial data from the U.S. Census Bureau's TIGER/Line shape files and other geo data
  cartogram,                               # Transforms shape files into the designated Projection
  janitor,                                 # to clean column names
  #png,                                    # Creates png files
  #jpeg,                                   # Creates jpeg files
  haven,                                   # Can read SAS, SPSS and Stata files. Data Import/Export
  sas7bdat,                                # Can read data files in the SAS binary data format
  spdep,                                   # (Possibly not useful) Conducts spatial statistics
  ggplot2,                                 # Supports the creation of various visuals including maps
  rgdal,                                   # Can read, manipulated and export geo-spatial data like shape files
  rlang,                                   # (Possibly not useful) Provides tools and utilities for working with expressions, symbols, and language objects
  wesanderson,                             # (Not used but may be useful in the Future) Provides a collection of color palettes inspired by the distinctive color schemes used in the films of the American filmmaker Wes Anderson    
  stringi,                                 # (Possibly not useful) Provides a comprehensive set of functions for working with character strings
  cowplot,                                 # Allows us to create and customized plots and data visualizations by combining and arranging multiple ggplot2 plots
  magick,                                  # Can handle and work with multiple types of images including jpeg and png 
  rgeos,                                   # (Possibly not useful) Provides a set of functions for working with vectorized geospatial operations
  gridExtra,                               # (Possibly not useful) Provides functions and tools for arranging and combining multiple grid-based plots or graphical objects into a single composite plot
  grid,                                    # (Possibly not useful) Provides low-level tools for creating and customizing data visualizations, graphical displays, and plots
  patchwork,                               # (Possibly not useful) provides a simple and flexible way to combine and arrange multiple ggplot2 plots into a single composite plot
  groundhog,                                # Loads packages & their dependencies as available on chosen date on CRAN to create reproducability without worrying about updates
  gtable,
  extrafont)







# Function -------------------------------------------------------------------------
# Run this function to make the maps. 


quick.maps <- function(point, congress, state, outcome, mapvar, ages, races, path, ht, wd) { 
  # These variables (in the function) are defined in the bottom of the script
  # when you call the maps to be made into a png file.
  # The variables only exists within this function.
  # If you want to define the variables without calling for a map, 
  # please go to line: 4688  
  
  
# Data Loading -------------------------------------------------------------
  
# Here we import all the files necessary to create the map.
# Import csv files Point level data.
  
ctny_sf <- st_read(here("Data", "Interactive_Atlas_Counties_2018", "ATLAS_COUNTIES_TERRITORIES_2018.shp"))                 # County Boundary
st_sf <- st_read(here("Data", "quick_map_project-2019_data.gdb"))               # State Boundary
cg_sf <- st_read(here("Data", "119th", "USA 119th.shp"))                  # Congressional Boundary
high_sf <- st_read(here("Data", "USA_Freeway", "USA_Freeway_System.shp"))       # Highway Layer
fq_sf <- import(here("Data", "FQHC", "FQHC_2024.csv"))                          # Federally qualified health center Point
str_sf <- import(here("Data", "Certification", "Total_2023_Clean_Final.csv"))   # Total Stroke Center data

act_sf <- str_sf %>%
  filter(CertificationProgram == "Acute Stroke Ready Hospital")                 # Acute Stroke Center
prm_sf <- str_sf %>%
  filter(CertificationProgram == "Primary Stroke Center")                       # Primary Stroke Center
prm_pls_sf  <- str_sf %>%
  filter(CertificationProgram == "Thrombectomy Capable Stroke Center")          # Thrombectomy Stroke Center 
comp_sf  <- str_sf %>%
  filter(CertificationProgram == "Comprehensive Stroke Center")                 # Comprehensive Stroke Center
st_inf <- import(here("Data", "State Map Projections.csv"))                     # State Projection

  
# Import image
img <- png::readPNG(here("Data", "R_IMG", "logo2.png"))                         # CDC Logo
img.hdr <- png::readPNG(here("Data", "R_IMG", paste0(state, ".png")))           # Template Header
lg.cong <- png::readPNG(here("Data", "R_IMG", "cong.dist.png"))                 # Congressional district legend (Yellow)
hw.img <- png::readPNG(here("Data", "R_IMG", "Hawaii2.png"))                    # Hawaii Template Header
hw.cong2.img <- png::readPNG(here("Data", "R_IMG", "Hawaii_cong_2.png"))        # Image of State Hawaii (Yellow)
qr2 <- png::readPNG(here("Data", "R_IMG", "atlas_qr.png"))                      # QR Code for Atlas


 
# Identify and assign directory to raw HD & Stroke data.
#\\cdc.gov\project\NCCD_DHDSP_DHDSP-SAA\Group\Interactive Atlas\IAAP\IAAP - Mortality\output\2022 data
if (outcome == "hd_mort") {
  st_file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Mortality/output/2022 data/s_r_allhd_mort_20_22_load.sas7bdat"
  n_file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Mortality/output/2022 data/n_r_allhd_mort_20_22_load.sas7bdat"
} else if (outcome == "str_mort") {
  st_file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Mortality/output/2022 data/s_r_allstr_mort_20_22_load.sas7bdat"
  n_file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Mortality/output/2022 data/n_r_allstr_mort_20_22_load.sas7bdat"
}


if (outcome=="hd_mort") { file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Mortality/output/2022 data/c_s_allhd_mort_20_22_load.sas7bdat"
} else if (outcome=="hd_hosp") {file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Medicare/Output/2019_21 data/c_s_allhd_19_21_load.sas7bdat"
} else if (outcome=="str_mort") {file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Mortality/output/2022 data/c_s_allstr_mort_20_22_load.sas7bdat"
} else if (outcome=="str_hosp") {file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Medicare/Output/2019_21 data/c_s_allstr_19_21_load.sas7bdat"
} else if (outcome=="soc_det") {file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/Data/Risk Factors/Aug 2022/social_determinants_update_ord.sas7bdat"
} else if (outcome=="soc_det") {file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Medicare/Output/CardRehab_2019_20/c_s_cardrehab_19_20_load.sas7bdat"
} else if (outcome=="soc_det") {file_path <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Group/Interactive Atlas/IAAP/IAAP - Medicare/Output/CardRehab_2019_20/c_s_cardrehab_hom_19_20_load.sas7bdat"
}
 
  # Import HD Stroke Outcome data (SAS)
    # comb.raw      <-- Combined HD and Stroke data (Mortality and Hosp for All races and ages)
comb.raw <- read_sas(file_path)



# Avg. --------------------------------------------------------------------

avg.st.raw <- read_sas(st_file_path)
avg.st.raw <- avg.st.raw %>%
  mutate(st_fips = sub("^0+", "", st_fips))

avg.n.raw <- read_sas(n_file_path)

# Cleaning and Managing Data ----------------------------------------------

comb.raw <- read_sas(file_path)
comb.raw$id <- as.numeric(as.character(comb.raw$stcty_fips))                    # Change stcty_fips as numeric
comb.raw[,mapvar] <- round(comb.raw[,mapvar], 1)                                # Round the whole table to .1
comb <- comb.raw %>% 
  replace(comb.raw==-1.0, NA) %>%                                               # Remove -1
  replace(comb.raw==-9999, NA)                                                  # Remove -9999


# Clean and Transform -----------------------------------------------------

crs_st <- st_inf[state == st_inf$Name, "EPSG"]                                  # Assign the specific state for each projections
 
  # For the Next few codes we are cleaning each shape file. We remove leading 0s and mutate columns to a specific class.
  # We are ultimately filtering the specific state of interest using projections table for assistance. 
  
##### State Level #####
fips_code <- st_inf[ state == st_inf$Name, "FIPS"]
st_sf <- st_sf %>%
  mutate(state_fips = sub("^0+", "", state_fips)) %>%
  filter(state_fips == fips_code) %>%
  st_transform(crs = st_crs(crs_st))

##### County Level #####
fips_code <- st_inf[ state == st_inf$Name, "FIPS"]
ctny_sf <- ctny_sf %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  mutate(state_fips = as.character(state_fips)) %>%
  mutate(state_fips = sub("^0+", "", state_fips)) %>%
  filter(fips_code == state_fips) %>%
  st_transform(crs = st_crs(crs_st))

ct_sf <- ctny_sf %>% 
  left_join(comb, by = c("cnty_fips" = "stcty_fips"))                           # Combine County Shapefile with Mortality data


##### Congressional Level #####
fips_code <- st_inf[ state == st_inf$Name, "FIPS"]
cg_sf <- cg_sf %>%
  mutate(CDFIPS = str_remove(CDFIPS, "^0+")) %>% 
  mutate(CDFIPS = replace(CDFIPS, CDFIPS == "", 1)) %>%
  mutate(STFIPS = sub("^0+", "", STFIPS)) %>%
  filter(fips_code == STFIPS) %>%
  st_transform(crs = st_crs(crs_st))

# Isolate the congressional file from the US to the specific district of interest
cg_sf2 <- cg_sf[ congress == cg_sf$CDFIPS, "CDFIPS"]


  
##### Highway Level #####
st_high_sf <- high_sf %>%
  st_transform(crs = crs_st)

##### FQHC Level #####
fq_point_sf <- fq_sf %>% 
 st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(st_crs(4326))
fq_point_sf <-  st_transform(fq_point_sf, st_crs(crs_st))

##### Acute Stroke Level #####
act_point_sf <- act_sf %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(st_crs(4326))
act_point_sf <-  st_transform(act_point_sf, st_crs(crs_st))

##### Comprehensive Stroke Lvel
comp_point_sf <- comp_sf %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(st_crs(4326))
comp_point_sf <-  st_transform(comp_point_sf, st_crs(crs_st))

##### Primary Stroke Level #####
prm_point_sf <- prm_sf %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(st_crs(4326))
prm_point_sf <-  st_transform(prm_point_sf, st_crs(crs_st))

##### Thrombectomy Stroke Level
prm_pls_point_sf <- prm_pls_sf %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(st_crs(4326))
prm_pls_point_sf <-  st_transform(prm_pls_point_sf, st_crs(crs_st))
 
 
# Congressional District Analysis (Counties) --------------------------------------
  # We use this section to analyse what counties are present in this district.

intersect_sf <- st_intersection(ctny_sf, cg_sf) # Perform a spatial intersection between county and congressional district boundaries

intersect_sf <- intersect_sf %>%
  mutate(overlap_area = st_area(.))             # Add a new column 'overlap_area' that calculates the area of the intersected regions

ctny_sf <- ctny_sf %>%
  mutate(county_area = st_area(.))              # Calculate the area of each county and add it as a new column 'county_area'

ct_df <- st_drop_geometry(ctny_sf) %>%
  select(cnty_fips, county_area)                # Drop the geometry column from the counties dataframe and keep only the county FIPS code and county area

intersect_sf <- intersect_sf %>%    
  left_join(ct_df, by = "cnty_fips")            # Perform a left join to add the county area data to the intersected regions based on the county FIPS code

threshold <- 0.0005                             # Define a threshold for the minimum significant overlap area as a ratio of the county area
significant_overlap_sf <- intersect_sf %>%
  filter(as.numeric(overlap_area) / as.numeric(county_area) > threshold) # Filter the intersected regions to retain only those with a significant overlap
                                                                         # The overlap must be greater than the defined threshold (0.0005) of the total county area  
ctcg <- significant_overlap_sf[significant_overlap_sf$CDFIPS == congress, ] # Further filter the data to retain only rows that belong to the specified congressional district (CD119FP)


# -------------Analysis--------------

ctcg <- ctcg %>% 
  left_join(comb, by = c("cnty_fips" = "stcty_fips"))                           # Perform a left join between 'ctcg' and 'comb' dataframes
  
low_ct <- round(min(ctcg[[mapvar]], na.rm = TRUE))                                     # Find the minimum mortality rate
high_ct <- round(max(ctcg[[mapvar]], na.rm = TRUE))                                    # Find the maximum mortality rate
single_ct <- round(ctcg[[mapvar]], 0)                                                     # Extract the lone mortality rate 
tot_ct <- nrow(ctcg)                                                            # Count the total number of rows in 'ctcg', which represents the total number of counties

  
# Quantile and Cut Points Set up -----------------------------------------------------

# We are creating quantile and tertile cutpoints for all states.
# For Delaware and heart disease mortality (hd_mort) outcome:
if (state=="Delaware" & outcome=="hd_mort") {                                    # Get the unique rounded values of 'mapvar' and sort them
  unique.values <- unique(round(ct_sf[[mapvar]], 0))
  sorted_values <- sort(unique.values)                                           # Set the number of cut points to 3 and calculate quantile breakpoints
  num_cut_points <- 3
  cat.limits <- quantile(ct_sf[[mapvar]],
                         probs = seq(0, 1, length.out = num_cut_points + 1))     # Store the sorted unique values as labels for cuts
  cut.labels <<- sort(unique.values)                                             # Create a 'quintile' column by cutting 'mapvar' into intervals based on the calculated limits
  ct_sf$quintile <- cut(ct_sf[[mapvar]], cat.limits, 
                        include.lowest = TRUE, labels = cut.labels)              # For Delaware and stroke mortality (str_mort) outcome:
} else if (state=="Delaware" & outcome=="str_mort") {                            # Get and sort unique rounded values of 'mapvar'
  unique.values <- unique(round(ct_sf[[mapvar]], 0))
  sorted_values <- sort(unique.values)                                           # Calculate quantile breakpoints for 3 cut points based on rounded 'mapvar'
  num_cut_points <- 3
  cat.limits <- quantile(round(ct_sf[[mapvar]], 0), 
                         probs = seq(0, 1, length.out = num_cut_points + 1))     # Store the sorted unique values as cut labels
  cut.labels <<- sort(unique.values)                                             # Create 'quintile' column by cutting rounded 'mapvar' into intervals based on calculated limits
  ct_sf$quintile <- cut(round(ct_sf[[mapvar]], digits = 0), breaks = cat.limits, 
                        include.lowest = TRUE, labels = cut.labels)              # For Hawaii and stroke mortality (str_mort) outcome:
} else if (state=="Hawaii" & outcome=="str_mort") {                              # Get and sort unique rounded values of 'mapvar'
  unique.values <- unique(round(ct_sf[[mapvar]], 0))
  sorted_values <- sort(unique.values)                                           # Find the median of the sorted unique values
  separator_value <- median(sorted_values)                                       # Calculate quantile breakpoints using 0.51 increments, removing NAs
  cat.limits <- quantile(round(ct_sf[[mapvar]], 0), 
                        probs=seq(0, 1, by=.51), type=3, na.rm = TRUE)           # If it's congressional district 1, assign rounded 'mapvar' values directly as quintile levels
  #if (congress == 1){
    ct_sf$quintile <- as.factor(round(ct_sf[[mapvar]], 0))                       # If it's congressional district 2, assign "Level 1" or "Level 2" based on comparison to median value
  #} else if (congress == 2) {
   # ct_sf$quintile <- factor(ifelse(ct_sf[[mapvar]] <= separator_value, "Level 1", "Level 2"))
  #}
  # For Hawaii and heart disease mortality (hd_mort) outcome:
} else if (state == "Hawaii" & outcome=="hd_mort") {                             # Calculate quantile breakpoints into thirds for the rounded 'mapvar'
  cat.limits <- quantile(round(ct_sf[[mapvar]], 0),
                         probs=seq(0, 1, by=0.333), type=3, na.rm = TRUE)        # Create 'quintile' column by cutting 'mapvar' into thirds based on calculated limits
  ct_sf$quintile <- cut(as_vector(ct_sf[[mapvar]]),
                        quantile(as_vector(ct_sf[[mapvar]]),
                                 probs=seq(0, 1, by=0.333), type=3, na.rm = TRUE),
                        include.lowest=TRUE)
  # For Rhode Island, Connecticut, or New Hampshire:
} else if (state == "Rhode Island"|
           state == "Connecticut" | 
           state == "New Hampshire") {                                           # Calculate quantile breakpoints into thirds for the rounded 'mapvar'
  cat.limits <- quantile(round(ct_sf[[mapvar]], digits = 0),
                         probs=seq(0, 1, by=0.333), type=3, na.rm = TRUE)        # Create 'quintile' column by cutting 'mapvar' into thirds based on calculated limits
  ct_sf$quintile <- cut(as_vector(ct_sf[[mapvar]]),
                        quantile(as_vector(ct_sf[[mapvar]]),
                                 probs=seq(0, 1, by=0.333), type=3, na.rm = TRUE),
                        include.lowest=TRUE)
# For any other state except Delaware:
} else if (state == "Vermont") {                                           # Calculate quantile breakpoints into thirds for the rounded 'mapvar'
  maj.cat.limits <- unique(quantile(round(ct_sf[[mapvar]], digits = 0),
                                    probs = seq(0, 1, by = 0.2), 
                                    type = 3, 
                                    na.rm = TRUE)) 
  
  ct_sf$quintile <- cut(round(ct_sf[[mapvar]], digits = 0),
                        breaks = maj.cat.limits,
                        include.lowest = TRUE,
                        right = FALSE)
  # For any other state except Delaware:
} else if (state!="Delaware") {                                                  # Calculate quantile breakpoints into quintiles for the rounded 'mapvar'
  maj.cat.limits <- quantile(round(ct_sf[[mapvar]],digits = 0),
                                 probs=seq(0, 1, by=0.2), type=3, na.rm = TRUE)  # Create 'quintile' column by cutting 'mapvar' into quintiles based on calculated limits
  ct_sf$quintile <- cut(round(ct_sf[[mapvar]],digits = 0),
                        quantile(round(ct_sf[[mapvar]],digits = 0),
                                 probs=seq(0, 1, by=0.2), type=3, na.rm = TRUE),
                        include.lowest=TRUE,
                        right = FALSE)
}
  

# Creating Labels for cut points
  # If the state is Delaware:
if (state == "Delaware" ) {                               # Set 'cut.labels' to the sorted unique values
  cut.labels <- sort(unique.values)                       # If the state is Hawaii and the outcome is stroke mortality (str_mort):
} else if (state == "Hawaii" & outcome == "str_mort") {   # Set 'cut.labels' to the sorted unique values
  cut.labels <- sort(unique.values)                       # If the state is Rhode Island, Hawaii, Connecticut, or New Hampshire:
 } else if (state == "Rhode Island" | state == "Hawaii" 
            | state == "Connecticut" | state == "New Hampshire") {   # Create 'cut.labels' as a vector of strings, representing ranges between calculated limits
   cut.labels <<- c(                                                 # Add 0.0 to the second and third cut points to handle edge cases
       paste(cat.limits[1], cat.limits[2], sep = " - "),
       paste(cat.limits[2]+0.0, cat.limits[3], sep = " - "),
       paste(cat.limits[3]+0.0, cat.limits[4], sep = " - "),
       "Insufficient Data")                               # If the state is not Delaware (handling all other states):
 } else if (state!="Delaware") {                          # Create 'cut.labels' as a vector of strings representing ranges based on major cut points
   cut.labels <<- c(
   paste(maj.cat.limits[1], maj.cat.limits[2],sep = " - "),
   paste(maj.cat.limits[2]+0.0, maj.cat.limits[3], sep = " - "),
   paste(maj.cat.limits[3]+0.0, maj.cat.limits[4], sep = " - "),
   paste(maj.cat.limits[4]+0.0, maj.cat.limits[5], sep = " - "),
   paste(maj.cat.limits[5]+0.0, maj.cat.limits[6], sep = " - "),
   "Insufficient Data")
  }
  

# Congressional District Analysis (FQHC, Highway, Stroke Center)----------------------------------------------------------------

# Function to ensure that two spatial data frames have the same CRS (coordinate reference system)
ensure_same_crs <- function(sf1, sf2) {            # Check if the CRS of sf1 is different from the CRS of sf2
  if (st_crs(sf1) != st_crs(sf2)) {                # If they differ, transform sf1 to the CRS of sf2
    sf1 <- st_transform(sf1, st_crs(sf2))          # Return the potentially transformed sf1
  }
  return(sf1)
}

# Ensure CRS and perform intersections
# Perform intersection between congressional district (cg_sf2) and county data (ct_sf)
at <- st_intersection(cg_sf2, ct_sf)
# Perform intersection between Hawaii stroke centers (st_high_sf) and congressional districts (cg_sf2),
# ensuring that 'st_high_sf' has the same CRS as 'cg_sf2'
hi <- st_intersection(ensure_same_crs(st_high_sf, cg_sf2), cg_sf2)
# Perform intersection between FQHC (fq_point_sf) and congressional districts (cg_sf2),
# ensuring that 'fq_point_sf' has the same CRS as 'cg_sf2'
fq <- st_intersection(ensure_same_crs(fq_point_sf, cg_sf2), cg_sf2)
# Perform intersection between acute stroke centers (act_point_sf) and congressional districts (cg_sf2),
# ensuring that 'act_point_sf' has the same CRS as 'cg_sf2'
act <- st_intersection(ensure_same_crs(act_point_sf, cg_sf2), cg_sf2)
# Perform intersection between comprehensive stroke centers (comp_point_sf) and congressional districts (cg_sf2),
# ensuring that 'comp_point_sf' has the same CRS as 'cg_sf2'
comp <- st_intersection(ensure_same_crs(comp_point_sf, cg_sf2), cg_sf2)
# Perform intersection between primary stroke centers (prm_point_sf) and congressional districts (cg_sf2),
# ensuring that 'prm_point_sf' has the same CRS as 'cg_sf2'
prm <- st_intersection(ensure_same_crs(prm_point_sf, cg_sf2), cg_sf2)
# Perform intersection between primary plus stroke centers (prm_pls_point_sf) and congressional districts (cg_sf2),
# ensuring that 'prm_pls_point_sf' has the same CRS as 'cg_sf2'
prm_pls <- st_intersection(ensure_same_crs(prm_pls_point_sf, cg_sf2), cg_sf2)

#####Analysis#####
# Here we measure the number of stroke center within each district
tot.fqhc <- nrow(fq)            # Count the total number of FQHC centers (Federally Qualified Health Centers) within the intersection
tot_act <- nrow(act)            # Count the total number of acute stroke centers within the intersection
tot_comp <- nrow(comp)          # Count the total number of comprehensive stroke centers within the intersection
tot_prm <- nrow(prm)            # Count the total number of primary stroke centers within the intersection
tot_prm_pls <- nrow(prm_pls)    # Count the total number of primary plus stroke centers within the intersection

# Sum the total number of stroke centers across all categories (acute, comprehensive, primary, primary plus)
tot.str <- tot_act + tot_comp + tot_prm + tot_prm_pls

# Color Ramp and Title Configuration ----------------------------------------------------------

# Set color schemes based on the state and outcome, assigning to 'map.colors'
if ((state == "Rhode Island") & (outcome == "hd_mort")) {                           # Colors for heart disease mortality in Rhode Island
  map.colors <<- c("#FEE5D9", "#FB6A4A", "#A50F15", "#DCDCDC")
} else if ((state == "Connecticut") & (outcome == "hd_mort")) {                     # Colors for heart disease mortality in Connecticut
  map.colors <<- c("#FEE5D9", "#FB6A4A", "#A50F15", "#DCDCDC")
} else if ((state == "New Hampshire") & (outcome == "hd_mort")) {                   # Colors for heart disease mortality in New Hampshire
  map.colors <<- c("#FEE5D9", "#FB6A4A", "#A50F15", "#DCDCDC")
} else if (state == "Hawaii" & outcome == "hd_mort") {                              # Colors for heart disease mortality in Hawaii
  map.colors <<- c("#FEE5D9", "#FB6A4A", "#A50F15")
} else if (state == "Delaware" & outcome == "hd_mort") {                            # Colors for heart disease mortality in Delaware
  map.colors <<- c("#FEE5D9", "#FB6A4A", "#A50F15")
} else if (state == "Vermont" & outcome == "str_mort") {                             # Colors for stroke mortality in Hawaii
  num_levels <- length(unique(ct_sf$quintile))
  # Define the full ramp for Hawaii
  vt_colors <- c("#F1DFF2", "#AD66B3", "#84308A", "#631069")                    # Select only as many colors as needed
  map.colors <- vt_colors[1:num_levels] 
} else if (state == "Hawaii" & outcome == "str_mort") {                             # Colors for stroke mortality in Hawaii
  num_levels <- length(unique(ct_sf$quintile))
  # Define the full ramp for Hawaii
  hawaii_colors <- c("#F1DFF2", "#AD66B3", "#84308A", "#631069")                    # Select only as many colors as needed
  map.colors <- hawaii_colors[1:num_levels] 
} else if (state == "Delaware" & outcome == "str_mort") {                           # Colors for stroke mortality in Delaware
  map.colors <<- c("#F1DFF2",  "#AD66B3", "#631069")
} else if ((state == "Rhode Island"| state == "Connecticut" | state == "New Hampshire") &
           (outcome == "str_mort")) {                                                 # Colors for stroke mortality in Rhode Island, Connecticut, or New Hampshire
  map.colors <<- c("#F1DFF2", "#AD66B3", "#631069", "#DCDCDC")
} else if (outcome == "hd_mort" | outcome == "hd_hosp") {                             # Colors for heart disease mortality or hospitalization nationwide
  map.colors <<- c("#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15", "#DCDCDC")
} else if (outcome == "str_mort" | outcome == "str_hosp") {                           # Colors for stroke mortality or hospitalization nationwide
  map.colors <<- c("#F1DFF2", "#D3A7D6", "#AD66B3", "#84308A", "#631069", "#DCDCDC")
} else if (outcome == "soc_det" & mapvar == "povpct" | mapvar == "unemploy" |
           mapvar == "no_hsdip" | mapvar == "pctui") {                                # Colors for social determinants of health related to poverty, unemployment, or education level
  map.colors <<- c("#FFD1BA", "#FDBE85", "#FD8D3C", "#E6550D", "#A63603", "#DCDCDC")
} else if (outcome == "soc_det" & outcome == "soc_det" & mapvar == "total_inc" |
           mapvar == "inp_inc" | mapvar == "ou_inc" | mapvar == "pac_inc" | 
           mapvar == "total_cost" | mapvar == "inp_cost" | mapvar == "ou_cost" | 
           mapvar == "pac_cost") {                                                    # Colors for social determinants of health related to income or healthcare costs
  map.colors <<- c("#DDF59B", "#9FD444", "#3CAF28", "#097609", "#014C00", "#DCDCDC")
}

#map_color <- dplyr::case_when(
#  (state == "Rhode Island") & (outcome == "hd_mort") ~ c("#FEE5D9", "#FB6A4A", "#A50F15", "#DCDCDC"),
#  (outcome == "soc_det" & outcome == "soc_det" & mapvar == "total_inc" |
#     mapvar == "inp_inc" | mapvar == "ou_inc" | mapvar == "pac_inc" | 
#     mapvar == "total_cost" | mapvar == "inp_cost" | mapvar == "ou_cost" | 
#     mapvar == "pac_cost") ~ c("#DDF59B", "#9FD444", "#3CAF28", "#097609", "#014C00", "#DCDCDC")
#)


# Text Annotation on Maps ----------------------------------------------------------------
  # The text is written on the condition based on the disease of interest
  # The code is categorized based on the different groups of text that occupies
  # the template.
  # I placed the order of the text based on text appearing from top to bottom.

dz.text <- ifelse(outcome == "hd_mort", "heart disease death rates",
           ifelse(outcome == "hd_hosp", "heart disease hopitalization rates",
           ifelse(outcome == "str_mort", "stroke death rates",
           ifelse(outcome == "str_hosp", "stroke hospitalzation rates",
           ifelse(outcome == "soc_det" & mapvar == "povpct","percent of population living in poverty",
           ifelse(outcome == "soc_det" & mapvar == "unemploy", "unemployment rate",
           ifelse(outcome == "soc_det" & mapvar == "no_hsdip", "percent without high school diploma",
           ifelse(outcome == "soc_det" & mapvar == "pctui", "percent of population under age 65 without health insurance",
           ifelse(outcome == "soc_det" & mapvar == "total_inc" | mapvar == "inp_inc" | mapvar == "ou_inc" | mapvar == "pac_inc", "incremental cost of care for medicare beneficiaries\n diagnosed with heart disease",
           ifelse(outcome == "soc_det" & mapvar == "total_cost" | mapvar == "inp_cost" | mapvar == "ou_cost" | mapvar == "pac_cost", "cost of care for medicare beneficiaries\n diagnosed with heart disease", "zzz"))))))))))

dz.text.2 <- ifelse(outcome == "hd_mort", "Heart disease death rates",
             ifelse(outcome == "hd_hosp", "Heart disease hopitalization rates",
             ifelse(outcome == "str_mort", "Stroke death rates",
             ifelse(outcome == "str_hosp", "Stroke hospitalzation rates",
             ifelse(outcome == "soc_det" & mapvar == "povpct","percent of population living in poverty",
             ifelse(outcome == "soc_det" & mapvar == "unemploy", "unemployment rate",
             ifelse(outcome == "soc_det" & mapvar == "no_hsdip", "percent without high school diploma",
             ifelse(outcome == "soc_det" & mapvar == "pctui", "percent of population under age 65 without health insurance",
             ifelse(outcome == "soc_det" & mapvar == "total_inc" | mapvar == "inp_inc" | mapvar == "ou_inc" | mapvar == "pac_inc", "incremental cost of care for medicare beneficiaries\n diagnosed with heart disease",
             ifelse(outcome == "soc_det" & mapvar == "total_cost" | mapvar == "inp_cost" | mapvar == "ou_cost" | mapvar == "pac_cost", "cost of care for medicare beneficiaries\n diagnosed with heart disease", "zzz"))))))))))

dz.text.3 <- ifelse(outcome == "hd_mort", "Heart Disease",
             ifelse(outcome == "hd_hosp", "Heart Disease Hospitalization",
             ifelse(outcome == "str_mort", "Stroke",
             ifelse(outcome == "str_hosp", "Stroke Hospitalzation",
             ifelse(outcome == "soc_det" & mapvar == "povpct","Percent of Population Living in Poverty",
             ifelse(outcome == "soc_det" & mapvar == "unemploy", "Unemployment Rate",
             ifelse(outcome == "soc_det" & mapvar == "no_hsdip", "Percent without High School Diploma",
             ifelse(outcome == "soc_det" & mapvar == "pctui", "Percent of Population Under Age 65 without Health Insurance",
             ifelse(outcome == "soc_det" & mapvar == "total_inc" | mapvar == "inp_inc" | mapvar == "ou_inc" | mapvar == "pac_inc", "Incremental Cost of Care for Medicare Beneficiaries\n Diagnosed with Heart Disease",
             ifelse(outcome == "soc_det" & mapvar == "total_cost" | mapvar == "inp_cost" | mapvar == "ou_cost" | mapvar == "pac_cost", "Cost of Care for Medicare Beneficiaries\n Diagnosed with Heart Disease", "zzz"))))))))))

dz.text.4 <- ifelse(outcome == "hd_mort", "Heart Disease Death Rate",
             ifelse(outcome == "hd_hosp", "Heart Disease Hospitalization Rate",
             ifelse(outcome == "str_mort", "Stroke Death Rate",
             ifelse(outcome == "str_hosp", "Stroke Hospitalzation Rate",
             ifelse(outcome == "soc_det" & mapvar == "povpct","Percent of Population Living in Poverty",
             ifelse(outcome == "soc_det" & mapvar == "unemploy", "Unemployment Rate",
             ifelse(outcome == "soc_det" & mapvar == "no_hsdip", "Percent without High School Diploma",
             ifelse(outcome == "soc_det" & mapvar == "pctui", "Percent of Population Under Age 65 without Health Insurance",
             ifelse(outcome == "soc_det" & mapvar == "total_inc" | mapvar == "inp_inc" | mapvar == "ou_inc" | mapvar == "pac_inc", "Incremental Cost of Care for Medicare Beneficiaries\n Diagnosed with Heart Disease",
             ifelse(outcome == "soc_det" & mapvar == "total_cost" | mapvar == "inp_cost" | mapvar == "ou_cost" | mapvar == "pac_cost", "Cost of Care for Medicare Beneficiaries\n Diagnosed with Heart Disease", "zzz"))))))))))

legend.title <- ifelse(outcome == "hd_mort", "Death Rates per 100,000",
                ifelse(outcome == "hd_hosp", "Hospitalization Rates per 100,000",
                ifelse(outcome == "str_mort", "Death Rates per 100,000",
                ifelse(outcome == "str_hosp", "Hospitalization Rates per 100,000",
                ifelse(outcome == "soc_det" & mapvar == "povpct","Percent Living\nin Poverty (%)",
                ifelse(outcome == "soc_det" & mapvar == "unemploy", "Percent Unemployed (%)",
                ifelse(outcome == "soc_det" & mapvar == "no_hsdip", "Percent without\nHigh School Diploma (%)",
                ifelse(outcome == "soc_det" & mapvar == "pctui", "Percent Uninsured (%)",
                ifelse(outcome == "soc_det" & mapvar == "total_inc" | mapvar == "inp_inc" | 
                       mapvar == "ou_inc" | mapvar == "pac_inc", "Cost of Care ($)", #"Cost of [type of care] Care, $
                ifelse(outcome == "soc_det" & mapvar == "total_cost" | 
                       mapvar == "inp_cost" | mapvar == "ou_cost" | mapvar == "pac_cost", "Cost of Care ($)", "zzz"))))))))))


#dplyr::case_when(
#  outcome == "hd_mort" ~ "Death Rates per 100,000",
#  outcome == "hd_hosp" & mapvar == "povpct" ~ "Hospitalization Rates per 100,000",
#  
#)

#case_match(
#  outcome,
#  "hd_mort" ~ "hi",
#  "hd_hosp" ~ "hi"
#)


# Stroke Naming convention in FAQ Descriptions.
if (tot_act >= 1 & tot_prm >= 1 & tot_prm_pls >= 1 & tot_comp  >= 1) {
  str.text.desc <- paste(tot_act, "Acute,", tot_prm, "Primary,", tot_prm_pls, "Thrombectomy,", tot_comp, "Comprehensive")
} else if (tot_act == 0 & tot_prm >= 1 & tot_prm_pls >= 1 & tot_comp  >= 1) {
  str.text.desc <- paste(tot_prm, "Primary,", tot_prm_pls, "Thrombectomy,", tot_comp, "Comprehensive")
} else if (tot_act >= 1 & tot_prm == 0 & tot_prm_pls >= 1 & tot_comp  >= 1) {
  str.text.desc <- paste(tot_act, "Acute,", tot_prm_pls, "Thrombectomy,", tot_comp, "Comprehensive")
} else if (tot_act >= 1 & tot_prm >= 1 & tot_prm_pls == 0 & tot_comp  >= 1) {
  str.text.desc <- paste(tot_act, "Acute,", tot_prm, "Primary,", tot_comp, "Comprehensive")
} else if (tot_act >= 1 & tot_prm >= 1 & tot_prm_pls >= 1 & tot_comp  == 0) {
  str.text.desc <- paste(tot_act, "Acute,", tot_prm, "Primary,", tot_prm_pls, "Thrombectomy")
} else if (tot_act == 0 & tot_prm == 0 & tot_prm_pls >= 1 & tot_comp  >= 1) {
  str.text.desc <- paste(tot_prm_pls, "Thrombectomy,", tot_comp, "Comprehensive")
} else if (tot_act == 0 & tot_prm >= 1 & tot_prm_pls >= 1 & tot_comp  == 0) {
  str.text.desc <- paste(tot_prm, "Primary,", tot_prm_pls, "Thrombectomy")
} else if (tot_act == 0 & tot_prm >= 1 & tot_prm_pls == 0 & tot_comp  >= 1) {
  str.text.desc <- paste(tot_prm, "Primary,", tot_comp, "Comprehensive")
} else if (tot_act >= 1 & tot_prm == 0 & tot_prm_pls == 0 & tot_comp  >= 1) {
  str.text.desc <- paste(tot_act, "Acute,", tot_comp, "Comprehensive")
} else if (tot_act >= 1 & tot_prm == 0 & tot_prm_pls >= 1 & tot_comp  == 0) {
  str.text.desc <- paste(tot_act, "Acute,", tot_prm_pls, "Thrombectomy")
} else if (tot_act >= 1 & tot_prm >= 1 & tot_prm_pls == 0 & tot_comp  == 0) {
  str.text.desc <- paste(tot_act, "Acute,", tot_prm, "Primary")
} else if (tot_act == 0 & tot_prm == 0 & tot_prm_pls == 0 & tot_comp  >= 1) {
  str.text.desc <- paste(tot_comp, "Comprehensive")
} else if (tot_act == 0 & tot_prm == 0 & tot_prm_pls >= 1 & tot_comp  == 0) {
  str.text.desc <- paste(tot_prm_pls, "Thrombectomy")
} else if (tot_act == 0 & tot_prm >= 1 & tot_prm_pls == 0 & tot_comp  == 0) {
  str.text.desc <- paste(tot_prm, "Primary")
} else if (tot_act >= 1 & tot_prm == 0 & tot_prm_pls == 0 & tot_comp  == 0) {
  str.text.desc <- paste(tot_act, " Acute")
} else if (tot_act == 0 & tot_prm == 0 & tot_prm_pls == 0 & tot_comp  == 0) {
  str.text.desc <- paste("There are no certified stroke centers in this district.")
}


# Top Right Outcome Header
FAQ <- "Frequently Asked Questions"
dz.head <- paste("Map of ", dz.text.3, " and", sep = "")
dz.head2 <- ifelse(point == "fqhc", "Federally Qualified Health Centers",
            ifelse(point == "centers", "Stroke Centers", "zzz"))
dz.head3 <- "BY CONGRESSIONAL DISTRICT & COUNTY"


description <- ifelse(point == "fqhc",
                      paste(dz.text.2, " can differ considerably \nwithin a congressional district.",
                      "\n\nThese maps highlight the disparities in county−level", "\n", dz.text,  
                      " within your congressional district.", sep = ""), 
               ifelse(point == "centers",
                      paste("Timely access to stroke care is critical to save lives and",
                      "\nreduce stroke-related disabilities. However, stroke centers", 
                      "\nare not evenly distributed throughout the United States.",
                      "\n\nThis map highlights county-level disparities in stroke death rates",
                      "\nand stroke centers in your congressional district and can be used to",
                      "\ninform policies that improve timely access to stroke care.", sep = ""),
               ifelse(outcome == "soc_det" & mapvar == "povpct" | mapvar == "unemploy" | mapvar == "pctui", 
                      paste(dz.text.2, " can differ considerably \nwithin a congressional district.",
                      "\n\nThese maps highlight the disparities in county−level", "\n", dz.text,  
                      " within your congressional district.", sep = ""),
               ifelse(outcome == "soc_det" & mapvar == "no_hsdip",
                      paste(dz.text.2, " can differ considerably \nwithin a congressional district.",
                      "\n\nThese maps highlight the disparities in county−level", "\n", dz.text,  
                      " within your congressional district.", sep = ""), 
               ifelse (outcome == "soc_det" & mapvar == "total_inc" | 
                      mapvar == "inp_inc" | mapvar == "ou_inc" | 
                      mapvar == "pac_inc" | mapvar == "total_cost" | 
                      mapvar == "inp_cost" | mapvar == "ou_cost" | 
                      mapvar == "pac_cost",
                      paste(dz.text,",\n",date, ", ",cost, sep = ""), "zzz")))))


# District Title
district.title <-  paste("DISTRICT ", congress, sep = " ")
str.cnt.title <- paste("Stroke Death Rates")
str.cnt.sub <- paste("By county, per 100,000 persons")
legend.title.str <- paste("Stroke Centers", sep = " ")
Sub.title.str <- paste("Level of Stroke Services", sep = " ")


# Sub-Legend Title
  # Legend title underneath main title
age <- ifelse(strsplit(ages, split = "_")[[1]][1] == "35+", "Ages 35 years and older",
       ifelse(strsplit(ages, split = "_")[[1]][1] == "25+", "Ages 25+",
       ifelse(strsplit(ages, split = "_")[[1]][1] == "65+", "Ages 65+",
       ifelse(strsplit(ages, split = "_")[[1]][1] == "all", "All Ages", "zzz"))))

race <- ifelse(strsplit(races, split = "_")[[1]][1] == "all", "All Races",
        ifelse(strsplit(races, split = "_")[[1]][1] == "whites", "White (Non-Hispanic)",
        ifelse(strsplit(races, split = "_")[[1]][1] == "blacks", "Black (Non-Hispanic)",
        ifelse(strsplit(races, split = "_")[[1]][1] == "hispanics", "Hispanic", "zzz"))))

cost <- ifelse(outcome == "soc_det" & mapvar == "total_inc" | mapvar == "total_cost", "Total Cost by County",
        ifelse(outcome == "soc_det" & mapvar == "inp_inc" | mapvar == "inp_cost", "Inpatient Costs",
        ifelse(outcome == "soc_det" & mapvar == "ou_inc" | mapvar == "ou_cost", "Outpatient Costs",
        ifelse(outcome == "soc_det" & mapvar == "pac_inc" | mapvar == "pac_cost", "Post-Acute Care Costs", "ZZZ"))))

date <- ifelse(outcome == "hd_mort", "2020 - 2022",
        ifelse(outcome == "hd_hosp", "2020 - 2022",
        ifelse(outcome == "str_mort", "2020 - 2022",
        ifelse(outcome == "str_hosp", "2020 - 2022",
        ifelse(outcome == "soc_det" & mapvar == "povpct",map.dates$povpct,
        ifelse(outcome == "soc_det" & mapvar == "unemploy", map.dates$unemploy,
        ifelse(outcome == "soc_det" & mapvar == "no_hsdip", map.dates$no_hsdip,
        ifelse(outcome == "soc_det" & mapvar == "pctui", map.dates$pctui,
        ifelse(outcome == "soc_det" & mapvar == "total_inc" | 
               mapvar == "inp_inc" | mapvar == "ou_inc" | 
               mapvar == "pac_inc"| mapvar == "total_cost" | 
               mapvar == "inp_cost" | mapvar == "ou_cost" | 
               mapvar == "pac_cost", map.dates$cost, "zzz")))))))))

legend.title.sub <- ifelse(point == "fqhc"| point == "centers", paste(age, ", ", date, sep = ""),
                                  ifelse( races == "whites" | races == "blacks" | races == "hispanics", paste(age, ", ", race, ", ", "(2020-2022)", sep = ""),"zzz"))
  
fqhc.tx <- "Federally Qualified\nHealth Centers"
act.tx <- "Acute Stroke Ready Hospital"
act.tx.1 <- "(ASRH)"
act.tx.2 <- "Rapid Stroke Assessment, CT Scans,\n Stroke Drugs (IV tPA)"
comp.tx <- "Comprehensive Stroke Center"
comp.tx.1 <- "(CSC)"
comp.tx.2 <- "TC + Neurological ICU, Treats Multiple\n Complex Stroke Patients, Research"
prm.tx <- "Primary Stroke Center"
prm.tx.1 <- "(PSC)"
prm.tx.2 <- "ASRH + Stroke Unit Coordinator,\n Designated Stroke Beds"
prm_pls.tx <- "Thrombectomy-Capable"
prm_pls.tx.1  <- "(TC)"
prm_pls.tx.2 <- "PSC + Designated ICU Beds,\n Mechanical Thrombectomy"



   
# State average text and legend
state_fips <- st_inf %>%
  filter(Name == state) %>%
  pull(FIPS)

# Use `state_fips` to get the correct average from `avg.st.raw`
avg <- avg.st.raw %>%
  filter(st_fips == as.character(state_fips)) %>% 
  pull(!!sym(mapvar)) %>%
  round()

us.avg <- avg.n.raw[[mapvar]][1] %>%
  round()

state.mean <- paste(dz.text.4, " | ", state, sep = "")
state.mean2 <- paste(avg, " per 100,000 persons", sep = "")
st.leg <- paste("Congressional District", congress, sep = " ") 
 
# Atlas Description
atlas.des <- paste("FOR MORE DETAILS", sep = "")
atlas.des2 <- paste("Visit Frequently", sep = "")
atlas.des3 <- paste("Asked Questions", sep = "")
atlas.des4 <- paste("FOR MORE MAPS", sep = "")
atlas.des5 <- paste("Visit CDC's Atlas of", sep = "")
atlas.des6 <- paste("Heart Disease and Stroke", sep = "")
cnty.desc <- paste(
  "Congressional district boundaries do not align\n
with county boundaries. Therefore, some\n
counties may be partially included in this district.")
  
# FAQ Description Text
if (tot_ct == 1) {
  county_desc1 <- paste(ctcg$cnty_name, " County, the single county represented in the district, is ", sep = "")
} else if (tot_ct >= 1) {
  county_desc1 <- paste("counties (or partial counties) in the district", sep = "")
} 


if (tot_ct == 1) {
  county_desc2 <- paste(single_ct, sep = "")
} else if (tot_ct >= 1) {
  county_desc2 <- paste(" range from ", low_ct, " - ", high_ct, sep = "")
} 


if (tot.str == 1) {
  str_desc1 <- paste("\nThere is only ", tot.str, " stroke center in District ", sep = "")
} else if (tot.str >= 1) {
  str_desc1 <- paste( "\nThere are a total of ", tot.str, " stroke centers in District ", sep = "")
} else if (tot.str == 0) {
  str_desc1 <- paste( "\nThere are a total of ", tot.str, " stroke centers in District ", sep = "")
} 



if (tot.fqhc == 1) {
  fq_desc1 <- paste("\nThere is only ", tot.fqhc, " Federally Qualified Health Center in District ", sep = "")
} else if (tot.fqhc >= 1) {
  fq_desc1 <- paste("\nThere are a total of ", tot.fqhc, " Federally Qualified Health Centers in District ", sep = "")
} else if (tot.fqhc == 0) {
  fq_desc1 <- paste("\nThere are a total of ", tot.fqhc, " Federally Qualified Health Centers in District ", sep = "")
} 

# Description
mp.desc.tit <- paste("Map Description:", state, "Congressional District", congress, sep = " ") 
if (point == "centers") {
  mp.desc <- paste("The map for Congressional District ", congress, " in ", state, " shows that ", dz.text, " (ages 35+,",
                   "\n2020 - 2022) for ", county_desc1, county_desc2, " per 100,000.",
                   "\nFor ", state, " and the United States, ", dz.text, " are ", avg, " and ", us.avg, " per 100,000, respectively.",
                   str_desc1, congress, " (", str.text.desc, ").", sep = "")
  } else if (point == "fqhc") {
    mp.desc <- paste("The map for Congressional District ", congress, " in ", state, " shows that ", dz.text, " (ages 35+,",
                     "\n2020 - 2022) for ", county_desc1, county_desc2, " per 100,000.",
                     "\nFor ", state, " and the United States, ", dz.text, " are ", avg, " and ", us.avg, " per 100,000, respectively.",
                     fq_desc1, congress, "." , sep = "")
} 

congress.desc <- paste("The Congressional Districts represent the 119th Congress of the United States. These\n
congressional boundaries are in effect from January 2025 until the new congress begins.", sep = "")


# Map Plotting -----------------------------------------------------------
    
  # The first plot will produce the main map which is of the cong. dist.
  # This map consists of the counties, highway, and fqhc in the cong. dist.
  
if (point == "fqhc") {cg <- ggplot(at) +
   geom_sf(aes(fill = quintile), color = "grey", size = 0, lwd = 0.2) +
   geom_sf(data = cg_sf2, fill = NA, color = NA, lwd = .5) +
   geom_sf(data = hi, fill = NA, color = "grey", lwd = 1.5) +
  geom_sf(data = fq, aes(), fill = "#43A6C6", color = "black", size = 2, shape = 24) +
   theme_bw() +
   theme(plot.title = element_text(), text = element_text(size=9),
         axis.line = element_blank(), axis.text.x = element_blank(),
         axis.text.y = element_blank(), axis.ticks = element_blank(),
         axis.title.x = element_blank(), axis.title.y = element_blank(), 
         legend.position="none", panel.border = element_blank(),
         panel.background = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
   scale_fill_manual(values = map.colors, limits = levels(ct_sf$quintile), na.value = "#DCDCDC")
} else if (point == "centers") {cg <- ggplot(at) +
  geom_sf(aes(fill = quintile), color = "grey", size = 0, lwd = 0.2) +
  geom_sf(data = cg_sf2, fill = NA, color = NA, lwd = .5) +
  geom_sf(data = hi, fill = NA, color = "grey", lwd = 1.5) +
  geom_sf(data = comp, aes(), fill = wes_palette("GrandBudapest1")[2], color = "black", size = 5.0, shape = 21) +
  geom_sf(data = prm_pls, aes(), fill = wes_palette("Darjeeling2")[4], color = "black", size = 4.0, shape = 21) +
  geom_sf(data = prm, aes(), fill = wes_palette("FantasticFox1")[2], color = "black", size = 3.0, shape = 21) +
  geom_sf(data = act, aes(), fill = "#2ecc71", color = "black", size = 2.0, shape = 21) +
  theme_bw() +
  theme(plot.title = element_text(), text = element_text(size=9),
        axis.line = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.position="none", panel.border = element_blank(),
        panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = map.colors, limits = levels(ct_sf$quintile), na.value = "#DCDCDC")
}
  
  
  # This plot will produce the state map
  # It will consist of the state, counties, Cong. Dist, boundary layers, and
  # a yellow mark indicating which congressional district is being displayed


if (state == "Hawaii" & congress == 1) {
st_cg <- ggplot(ct_sf, fill = "white") + 
geom_sf(aes(fill=quintile), size = 0, color = "grey", lwd = 0.09) +
geom_sf(data = cg_sf2, fill = "yellow", color = "black", lwd =.5) +
labs(x='Longitude', y='Latitude') +
theme_bw() +
theme(plot.title = element_text(),
      text = element_text(),
      axis.line = element_blank(), axis.text.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(), 
      legend.position="none", panel.border = element_blank(),
      panel.background = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
scale_fill_manual(values = map.colors, limits = levels(ct_sf$quintile), na.value = "#DCDCDC")
} else if (point == "fqhc" | point == "centers") {
st_cg <- ggplot(ct_sf) + 
  geom_sf(aes(fill=quintile), size = 0, color = "grey", lwd = 0.09) +
  geom_sf(data = st_sf, fill = NA, color = "black", size = 0, lwd = 0.3) +
  geom_sf(data = st_sf, fill = NA) +
  geom_sf(data = cg_sf, fill = NA, color = "black", lwd =.3) +
  geom_sf(data = cg_sf2, fill = "black", color = "black", lwd = 1.5) +
  geom_sf(data = cg_sf2, fill = "yellow", color = "yellow", lwd =.5) +
  labs(x='Longitude', y='Latitude') +
  theme_bw() +
  theme(plot.title = element_text(), text = element_text(),
        axis.line = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.position="none", panel.border = element_blank(),
        panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = map.colors, limits = levels(ct_sf$quintile), na.value = "#DCDCDC")
  }
  

# Legend Customization ------------------------------------------------------------------

  # Let's start off by saying creating a legend is a difficult task
    # Understand that I am trying to make a legend that is independent
    # from the plot that we made above. Basically, we need to have the legend
    # and the map plotted separately because if they exist in the same plot
    # it reduces our ability to place them in any x and y coordinate on the template
    # If they are separate, then we can place the map on one end of the template
    # and the legend on the other end. 
    # SO, how do we do that?
      # Step 1: Plot the layer you want to create a legend for
      # Step 2: Extract the legend from the plot 
  
  # Plot the legend for FQHC
    # You may notice that it is weird that color = "Federally Qualified Health Centers"
    # This is done purposefully. For some reason, we are calling the column
    # when we make color equal to something. 
    # WARNING: If you were to change the Color and Value to something other than
    # "Federally Qualified Health Centers" this function will give an error. 
l.fqhc <- 
  ggplot() +
  geom_sf(data = fq, aes(fill = "Federally Qualified Health Centers"), color = "black", size = 6, shape = 24) +
  theme(legend.key = element_rect(colour = NA, fill = "white", linewidth = .07),
      legend.spacing.x = unit(.15, 'cm')) +
  scale_fill_manual(values = c("Federally Qualified Health Centers" = "#43A6C6"))

  # Plot the legend for Highways  
l.hwy <- 
  ggplot() +
  geom_sf(data = hi, aes(color = "Highway"), lwd =1.7, key_glyph = draw_key_path) +
  theme(legend.key = element_rect(colour = NA, fill = "white", linewidth = .5),
      legend.title = element_blank(),
      legend.spacing.x = unit(.18, 'cm'),
      legend.spacing.y = unit(0, 'cm'),
      legend.key.height = unit(0, 'cm'), #change legend key height
      legend.key.width = unit(1, 'cm'), #change legend key width,
      legend.spacing = unit(0, 'cm')) +
 guides(fill = guide_legend(byrow = TRUE)) +
 scale_color_manual(
   values = c("Congressional District" = "yellow", "State" = "black", "Highway" = "grey"),
  )

  # Plot of the legend for counties
l.cnty <- 
  ggplot() +
    geom_sf(data = st_sf,aes(color = "County"), lwd = .6, key_glyph = draw_key_path) +
    theme(legend.key = element_rect(colour = NA, fill = "yellow", linewidth = .4),
          legend.title = element_blank(),
          legend.spacing.x = unit(.18, 'cm'),
          legend.spacing.y = unit(0, 'cm'),
          legend.key.height = unit(0, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width,
          legend.spacing = unit(0, 'cm')) +
    guides(fill = guide_legend(byrow = TRUE)) +
  scale_color_manual(
      values = c("County" = "grey"))

l.act <- 
  ggplot() +
  geom_sf(data = act, aes(fill = "Acute"), color = "black", size = 3.0, shape = 21) +
  theme(legend.key = element_rect(colour = NA, fill = "white", linewidth = .2),
        legend.spacing.x = unit(.15, 'cm')) +
  scale_fill_manual(values = c("Acute" = "#2ecc71"))

 l.comp <- 
  ggplot() +
  geom_sf(data = comp, aes(fill = "Comprehensive"), color = "black", size = 12.0, shape = 21) +
  theme(legend.key = element_rect(colour = NA, fill = "white", linewidth = .2),
        legend.spacing.x = unit(.15, 'cm')) +
  scale_fill_manual(values = c("Comprehensive" = wes_palette("GrandBudapest1")[2]))
 
l.prm <- 
  ggplot() +
  geom_sf(data = prm, aes(fill = "Primary"), color = "black", size = 6.0, shape = 21) +
  theme(legend.key = element_rect(colour = NA, fill = "white", linewidth = .2),
        legend.spacing.x = unit(.15, 'cm')) +
  scale_fill_manual(values = c("Primary" = wes_palette("FantasticFox1")[2]),)

l.prm_pls <- 
  ggplot() +
  geom_sf(data = prm_pls, aes(fill = "Primary Plus - Thrombectomy Capable"), color = "black", size = 9.0, shape = 21) +
  theme(legend.key = element_rect(colour = NA, fill = "white", linewidth = .2),
        legend.spacing.x = unit(.15, 'cm')) +
  scale_fill_manual(values = c("Primary Plus - Thrombectomy Capable" = wes_palette("Darjeeling2")[4]))
  

  # This is where we begin using the plot we just made and extracting the
  # legend from them.
      # We begin by extracting the legend for cg (Congressional District [main map]).
      # This is the legend for the quantile classifications for the outcomes.
        # For some reason Delaware and Hawaii are both different when legends are created.
        # Please be aware that county quantile legend and county legend are two different things

if (state=="Delaware" | state=="Hawaii" & outcome == "str_mort") {
  if (point == "fqhc" | point == "centers") {
  legend <- get_legend(st_cg + theme(legend.key = element_rect(colour = "white", linewidth = 0),
                                  legend.box.margin = margin(t=12, r=0, b=0, l=20),
                                  legend.key.size = unit(1, 'inch'), #change legend key size
                                  legend.key.height = unit(.61, 'cm'), #change legend key height
                                  legend.key.width = unit(.61, 'cm'), #change legend key width
                                  legend.title = element_blank(), #change legend title font size
                                  legend.text = element_text(size=12.5),
                                  legend.position = "right",
                                  legend.spacing = unit(5, 'cm'),
                                  legend.spacing.x = unit(.52, 'cm'),
                                  legend.spacing.y = unit(.9, 'cm')) +
                         guides(fill = guide_legend(ncol = 1)) +
                         scale_fill_manual(values= map.colors, labels = cut.labels, 
                                           name = legend.title, limits = levels(ct_sf$quintile)))
  }} else if (state=="Delaware" | state=="Hawaii") {
    legend <- get_legend(st_cg + theme(legend.key = element_rect(colour = "white", linewidth = 0),
                                    legend.box.margin = margin(t=12, r=0, b=0, l=20),
                                    legend.key.size = unit(1, 'inch'), #change legend key size
                                    legend.key.height = unit(.61, 'cm'), #change legend key height
                                    legend.key.width = unit(.61, 'cm'), #change legend key width
                                    legend.title = element_blank(), #change legend title font size
                                    legend.text = element_text(size=12.5),
                                    legend.position = "right",
                                    legend.spacing = unit(5, 'cm'),
                                    legend.spacing.x = unit(.52, 'cm'),
                                    legend.spacing.y = unit(1.1, 'cm')) +
                           guides(fill = guide_legend(ncol = 1)) +
                           scale_fill_manual(values= map.colors, labels = cut.labels, 
                                             name = legend.title, limits = levels(ct_sf$quintile)))
  } else if (point == "centers" | point == "fqhc") {
    legend <- get_legend(st_cg + theme(legend.key = element_rect(colour = "white", fill = NA, linewidth = 0),
                                    legend.box.margin = margin(t=12, r=0, b=0, l=20),
                                    legend.key.size = unit(1, 'inch'), #change legend key size
                                    legend.key.height = unit(.61, 'cm'), #change legend key height
                                    legend.key.width = unit(.61, 'cm'), #change legend key width
                                    legend.title = element_blank(), #change legend title font size
                                    legend.text = element_text(size=10.5),
                                    legend.position = "right",
                                    legend.spacing = unit(5, 'cm'),
                                    legend.spacing.x = unit(.42, 'cm'),
                                    legend.spacing.y = unit(.7, 'cm')) +
                           guides(fill = guide_legend(ncol = 1)) +
                           scale_fill_manual(values= map.colors, labels = cut.labels, 
                                             name = legend.title, limits = levels(ct_sf$quintile)))
  } 
  

legend.2.2 <- get_legend(l.fqhc + theme(legend.key = element_rect(fill = NA, linewidth = 5),
                                      legend.box.margin = margin(t=12, r=0, b=, l=5),
                                      legend.key.size = unit(.5, 'inch'), #change legend key size
                                      legend.key.height = unit(.04, 'cm'), #change legend key height
                                      legend.title = element_blank(), #change legend title font size
                                      legend.text = element_blank()))
                       
legend.3 <- get_legend(l.hwy + theme(legend.key = element_rect(fill = NA),
                                      legend.box.margin = margin(t=12, r=0, b=, l=1),
                                      legend.key.size = unit(.5, 'inch'), #change legend key size
                                      legend.key.height = unit(0, 'cm'), #change legend key height
                                      legend.title = element_blank(), #change legend title font size
                                      legend.text = element_text(size=9.5)))

legend.4 <- get_legend(l.cnty + theme(legend.key = element_rect(fill = NA),
                                       legend.box.margin = margin(t=12, r=0, b=, l=1),
                                       legend.key.size = unit(.5, 'inch'), #change legend key size
                                       legend.key.height = unit(0, 'cm'), #change legend key height
                                       legend.title = element_blank(), #change legend title font size
                                       legend.text = element_text(size=9.5)))

legend.5 <- get_legend(l.act + theme(legend.key = element_rect(fill = NA),
                                      legend.box.margin = margin(t=12, r=0, b=, l=5),
                                      legend.key.size = unit(.5, 'inch'), #change legend key size
                                      legend.key.height = unit(.04, 'cm'), #change legend key height
                                      legend.title = element_blank(), #change legend title font size
                                      legend.text = element_blank()))

legend.6 <- get_legend(l.comp + theme(legend.key = element_rect(fill = NA),
                                      legend.box.margin = margin(t=12, r=0, b=, l=5),
                                      legend.key.size = unit(.5, 'inch'), #change legend key size
                                      legend.key.height = unit(.04, 'cm'), #change legend key height
                                      legend.title = element_blank(), #change legend title font size
                                      legend.text = element_blank()))

legend.7 <- get_legend(l.prm + theme(legend.key = element_rect(fill = NA),
                                      legend.box.margin = margin(t=12, r=0, b=, l=5),
                                      legend.key.size = unit(.5, 'inch'), #change legend key size
                                      legend.key.height = unit(.04, 'cm'), #change legend key height
                                      legend.title = element_blank(), #change legend title font size
                                      legend.text = element_blank()))

legend.8 <- get_legend(l.prm_pls + theme(legend.key = element_rect(fill = NA),
                                      legend.box.margin = margin(t=12, r=0, b=, l=5),
                                      legend.key.size = unit(.5, 'inch'), #change legend key size
                                      legend.key.height = unit(.04, 'cm'), #change legend key height
                                      legend.title = element_blank(), #change legend title font size
                                      legend.text = element_blank()))


path.img=here("Data", "R_IMG")

l.fqhc.img <- png::readPNG(here("Data", "R_IMG", "l.fqhc.png")) 
l.act.img <- png::readPNG(here("Data", "R_IMG", "l.act.png"))
l.comp.img <- png::readPNG(here("Data", "R_IMG", "l.comp.png"))
l.prm.img <- png::readPNG(here("Data", "R_IMG", "l.prm.png"))
l.prm_pls.img <- png::readPNG(here("Data", "R_IMG", "l.prm_pls.png"))

  # Combining Plots and Saving  ------------------------------------------------
  
  # After we get all the plots we then use the cowplot package to combine them into one layout
  # Use the function ggdraw to locate where you would like the plot to go
  # Notice that we use draw_image to plot the .png.
  # Be aware that there are multiple variations of the template because Hawaii, Delaware, and the small states require their own unique template.
  # Finally, we use the variables in the function to save our maps in here:
  # NCCDPHP DHDSP ESB - Small Area Analysis Team\On-line Tools\Quick Maps\R Maps\Quick Map Data\Final Product

base.map <- ggdraw() +
  draw_image(img.hdr, x = -0.01, y = -0.07, width = 1.05, height = 1.05) +
  draw_image(img, x = 0.85, y = 0.03, width = 0.13, height = 0.12) +
  draw_label(atlas.des4, hjust = 0, x = .746, y = .058, fontface = "bold", size = 8, fontfamily = "Arial", color = "#093542") +
  draw_label(atlas.des5, hjust = 0, x = .75, y = .045, size = 7, fontfamily = "Helvetica", color = "#093542") +
  draw_label(atlas.des6, hjust = 0, x = .738, y = .033, size = 7, fontfamily = "Helvetica", color = "#093542") +
  draw_label(cnty.desc, hjust = 1, x = .698, y = .049, lineheight = .5, size = 7, fontfamily = "Helvetica", color = "#093542") +
  draw_label(paste("*"), hjust = 1, x = .518, y = .066, lineheight = .5, size = 11, fontfamily = "Helvetica", color = "#093542") +
  draw_image(qr2, x = 0.757, y = 0.077, width = 0.06, height = 0.06) 

base.map2 <- ggdraw() +
  draw_label(paste(dz.text.3, "and"), fontface = "bold", hjust = 1, size = 16, x = 0.97, y = .939, color = "#093542") +
  draw_label(paste(dz.head2), fontface = "bold", hjust = 1, size = 16, x = 0.97, y = .91, color = "#093542") +
  draw_label(dz.head3, hjust = 1, size = 12, x = 0.97, y = .88, color = "#093542") +
  draw_label(district.title, fontface = "bold", hjust = 1, size = 24, x = 0.97, y = .78, color = "#093542") +
  draw_label(dz.text.4, hjust = 0, x = .049, y = .625, size = 14.5, fontface = "bold", fontfamily = "Aharoni", color = "#093542") +
  draw_label(str.cnt.sub, hjust = 0, x = .05, y = .601, size = 7, fontfamily = "Helvetica", color = "#093542") +
  draw_label(legend.title.sub, hjust = 0, x = .05, y = .587, size = 7, fontfamily = "Helvetica", color = "#093542") +
  draw_label(description, hjust = 0, size = 13, x = 0.05, y = .75, fontfamily = "Helvetica") +
  draw_label(state.mean, hjust = 1, x = .49, y = .35, fontfamily = "Helvetica", color = "#093542", fontface = "bold", size = 10) +
  draw_label(state.mean2, hjust = 1, x = .49, y = .33, fontfamily = "Helvetica", color = "#093542",  size = 9) +
  draw_line(    x = c(0.04,.49), y = c(.66,.66), color = "#093542", size = .3) +
  draw_line(    x = c(0.04,.49), y = c(.38,.38), color = "#093542", size = .3) +
  draw_line(    x = c(0.55,.97), y = c(.74,.74), color = "#093542", size = .3) +
  draw_line(    x = c(0.71,.71), y = c(.027,.078), color = "#093542", size = .1)

base.map3 <- ggdraw() +
  draw_label(paste(dz.text.3, "and"), fontface = "bold", hjust = 1, size = 16, x = 0.97, y = .939, color = "#093542") +
  draw_label(paste(dz.head2), fontface = "bold", hjust = 1, size = 16, x = 0.97, y = .91, color = "#093542") +
  draw_label(dz.head3, hjust = 1, size = 11.7, x = 0.97, y = .88, color = "#093542") +
  draw_label(district.title, fontface = "bold", hjust = 1, size = 24, x = 0.97, y = .78, color = "#093542") +
  draw_label(dz.text.4, hjust = 0, x = .049, y = .639, size = 14.5, fontface = "bold", fontfamily = "Aharoni", color = "#093542") +
  draw_label(str.cnt.sub, hjust = 0, x = .05, y = .62, size = 7, fontfamily = "Helvetica", color = "#093542") +
  draw_label(description, hjust = 0, size = 10.5, x = 0.05, y = .76, fontfamily = "Helvetica") +
  draw_label(state.mean, hjust = 1, x = .49, y = .31, fontfamily = "Helvetica", color = "#093542", fontface = "bold", size = 10) +
  draw_label(state.mean2, hjust = 1, x = .49, y = .29, fontfamily = "Helvetica", color = "#093542",  size = 9) +
  draw_image(l.comp.img, width = .078, height = .078, x = .248, y = .365) +
  draw_image(l.prm_pls.img, width = .07, height = .07, x = .252, y = .435) +
  draw_image(l.prm.img, width = .06, height = .06, x = .257, y = .5) +
  draw_image(l.act.img, width = .061, height = .061, x = .2566, y = .56) +
  draw_label(legend.title.str, hjust = 0, x = .3, y = .64, size = 14.5, fontfamily = "Helvetica", color = "#093542", fontface = "bold") +
  draw_label(Sub.title.str,  hjust = 0, x = .3, y = .62, size = 7, fontfamily = "Helvetica", color = "#093542") +
  draw_label(act.tx, hjust = 0, x = .32, y = .59, size = 8.9, fontface = "bold", fontfamily = "Helvetica") +
  draw_label(act.tx.1, hjust = 0, x = .478, y = .59, size = 7.5, fontfamily = "Helvetica") +
  draw_label(act.tx.2, hjust = 0, x = .32, y = .567, size = 7, fontfamily = "Helvetica") +
  draw_label(prm_pls.tx, hjust = 0, x = .32, y = .47, size = 8.9, fontface = "bold", fontfamily = "Helvetica") +
  draw_label(prm_pls.tx.1, hjust = 0, x = .453, y = .47, size = 7.5, fontfamily = "Helvetica") +
  draw_label(prm_pls.tx.2, hjust = 0, x = .32, y = .445, size = 7, fontfamily = "Helvetica") +
  draw_label(prm.tx, hjust = 0, x = .32, y = .53, size = 8.9, fontface = "bold", fontfamily = "Helvetica") +
  draw_label(prm.tx.1, hjust = 0, x = .443, y = .53, size = 7.5, fontfamily = "Helvetica") +
  draw_label(prm.tx.2, hjust = 0, x = .32, y = .505, size = 7, fontfamily = "Helvetica") +
  draw_label(comp.tx, hjust = 0, x = .32, y = .41, size = 8.9, fontface = "bold", fontfamily = "Helvetica") +
  draw_label(comp.tx.1, hjust = 0, x = .485, y = .41, size = 7.5, fontfamily = "Helvetica") +
  draw_label(comp.tx.2, hjust = 0, x = .32, y = .384, size = 7, fontfamily = "Helvetica") +
  draw_label(legend.title.sub, hjust = 0, x = .05, y = .606, size = 7, fontfamily = "Helvetica", color = "#093542") +
  draw_line(    x = c(0.04,.5), y = c(.675,.675), color = "#093542", size = .3) +
  draw_line(    x = c(0.04,.5), y = c(.335,.335), color = "#093542", size = .3) +
  draw_line(    x = c(0.55,.97), y = c(.74,.74), color = "#093542", size = .3) +
  draw_line(    x = c(0.71,.71), y = c(.027,.078), color = "#093542", size = .1)


if (state == "Delaware") {
  if (point == "fqhc") {
    map <- ggdraw() +
      draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
      draw_plot(cg, x = 0.57, y = .72, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
      draw_label(fqhc.tx, hjust = 0, x = .335, y = .49, size = 9.5, fontfamily = "Helvetica") +
      draw_image(l.fqhc.img, width = .03, height = .03, x = .299, y = .48) +
      draw_grob(legend, x = .02, y = .52, width = 0.10, height = 0.01) +
      draw_grob(legend.3, x = -.159, y = -0.04) +
      draw_grob(legend.4, x = -.162, y = -0.07) +
      draw_grob(as_grob(base.map2), x = 0, y = 0, width = 1, height = 1) 
    } else if (point == "centers") {
    map <- ggdraw() +
      draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
      draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
      draw_grob(legend, x = .02, y = .54, width = 0.10, height = 0.01) +
      draw_grob(legend.3, x = -.409, y = -.11) +
      draw_grob(legend.4, x = -.412, y = -.08) +
      draw_grob(as_grob(base.map3), x = 0, y = 0, width = 1, height = 1) 
    }}  else if (state == "Connecticut" | state == "New Hampshire" | state == "Rhode Island") {
      if (point == "fqhc") {
    map <- ggdraw() +
      draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
      draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
      draw_label(fqhc.tx, hjust = 0, x = .335, y = .49, size = 9.5, fontfamily = "Helvetica") +
      draw_image(l.fqhc.img, width = .03, height = .03, x = .299, y = .48) +
      draw_grob(legend, x = .035, y = .52, width = 0.10, height = 0.01) +
      draw_grob(legend.3, x = -.159, y = -0.04) +
      draw_grob(legend.4, x = -.162, y = -0.07) +
      draw_plot(st_cg, x = .025, y = .0, width = .28, height = 0.32, vjust = 0, hjust = 0) +
      draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
      draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
      draw_grob(as_grob(base.map2), x = 0, y = 0, width = 1, height = 1)
    }  else if (point == "centers") {
    map <- ggdraw() +
      draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
      draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
      draw_grob(legend, x = .025, y = .54, width = 0.10, height = 0.01) +
      draw_grob(legend.3, x = -.409, y = -.11) +
      draw_grob(legend.4, x = -.412, y = -.08) +
      draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
      draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
      draw_plot(st_cg, x = .31, y = .32, width = .28, height = 0.32, vjust = 1, hjust = 1) +
      draw_grob(as_grob(base.map3), x = 0, y = 0, width = 1, height = 1) 
  }}  else if (state == "Hawaii" & congress == 1) {
      if (outcome == "hd_mort") {
        map <- ggdraw() +
          draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
          draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
          draw_label(fqhc.tx, hjust = 0, x = .335, y = .49, size = 9.5, fontfamily = "Helvetica") +
          draw_image(l.fqhc.img, width = .03, height = .03, x = .299, y = .48) +
          draw_grob(legend, x = .04, y = .52, width = 0.10, height = 0.01) +
          draw_grob(legend.3, x = -.159, y = -0.04) +
          draw_grob(legend.4, x = -.162, y = -0.07) +
          draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
          draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
          draw_plot(st_cg, x = .25, y = .26, width = .2, height = 0.2, vjust = 1, hjust = 1) +
          draw_grob(as_grob(base.map2), x = 0, y = 0, width = 1, height = 1) 
    } else if (outcome == "str_mort") {
      if (point == "fqhc") {
        map <- ggdraw() +
          draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
          draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
          draw_label(fqhc.tx, hjust = 0, x = .335, y = .49, size = 9.5, fontfamily = "Helvetica") +
          draw_image(l.fqhc.img, width = .03, height = .03, x = .299, y = .48) +
          draw_grob(legend, x = .01, y = .505, width = 0.10, height = 0.01) +
          draw_grob(legend.3, x = -.159, y = -0.04) +
          draw_grob(legend.4, x = -.162, y = -0.07) +
          draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
          draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
          draw_plot(st_cg, x = .25, y = .26, width = .2, height = 0.2, vjust = 1, hjust = 1) +
          draw_grob(as_grob(base.map2), x = 0, y = 0, width = 1, height = 1) 
        } else if (point == "centers") {
        map <- ggdraw() +
          draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
          draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
          draw_grob(legend, x = .01, y = .52, width = 0.10, height = 0.01) +
          draw_grob(legend.3, x = -.409, y = -.11) +
          draw_grob(legend.4, x = -.412, y = -.08) +
          draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
          draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
          draw_plot(st_cg, x = .31, y = .32, width = .28, height = 0.32, vjust = 1, hjust = 1) +
          draw_grob(as_grob(base.map3), x = 0, y = 0, width = 1, height = 1) 
    }}} else if (state == "Hawaii" & congress == 2) {
      if (outcome == "hd_mort") {
        map <- ggdraw() +
          draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
          draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
          draw_image(hw.img, x = 0.02, y = .415,  width = 1, height = 1) +
          draw_label(fqhc.tx, hjust = 0, x = .335, y = .49, size = 9.5, fontfamily = "Helvetica") +
          draw_image(l.fqhc.img, width = .03, height = .03, x = .299, y = .48) +
          draw_grob(legend, x = .04, y = .52, width = 0.10, height = 0.01) +
          draw_grob(legend.3, x = -.159, y = -0.04) +
          draw_grob(legend.4, x = -.162, y = -0.07) +
          draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
          draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
          draw_image(hw.cong2.img, x = .3, y = .32, width = .25, height = 0.25, vjust = 1, hjust = 1) +
          draw_grob(as_grob(base.map2), x = 0, y = 0, width = 1, height = 1) 
  } else if (outcome == "str_mort") {
    if (point == "fqhc") {
        map <- ggdraw() +
          draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
          draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
          draw_image(hw.img, x = 0.02, y = .415,  width = 1, height = 1) +
          draw_label(fqhc.tx, hjust = 0, x = .335, y = .49, size = 9.5, fontfamily = "Helvetica") +
          draw_image(l.fqhc.img, width = .03, height = .03, x = .299, y = .48) +
          draw_grob(legend, x = .01, y = .505, width = 0.10, height = 0.01) +
          draw_grob(legend.3, x = -.159, y = -0.04) +
          draw_grob(legend.4, x = -.162, y = -0.07) +
          draw_image(hw.cong2.img, x = .3, y = .32, width = .25, height = 0.25, vjust = 1, hjust = 1) +
          draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
          draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
          draw_grob(as_grob(base.map2), x = 0, y = 0, width = 1, height = 1) 
  } else if (point == "centers") {
        map <- ggdraw() +
          draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
          draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
          draw_image(hw.img, x = 0.02, y = .415,  width = 1, height = 1) +
          draw_grob(legend, x = .01, y = .52, width = 0.10, height = 0.01) +
          draw_grob(legend.3, x = -.409, y = -.11) +
          draw_grob(legend.4, x = -.412, y = -.08) +
          draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
          draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
          draw_image(hw.cong2.img, x = .3, y = .3, width = .25, height = 0.25, vjust = 1, hjust = 1) +
          draw_grob(as_grob(base.map3), x = 0, y = 0, width = 1, height = 1) 
}}} else if (state=="Alaska" | state == "North Dakota" | 
                state == "South Dakota" | state == "Vermont" | state == "Wyoming") {
      if (point == "fqhc") {
    map <- ggdraw() +
      draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
      draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
      draw_label(fqhc.tx, hjust = 0, x = .335, y = .49, size = 9.5, fontfamily = "Helvetica") +
      draw_image(l.fqhc.img, width = .03, height = .03, x = .299, y = .48) +
      draw_grob(legend, x = .032, y = .495, width = 0.10, height = 0.01) +
      draw_grob(legend.3, x = -.159, y = -0.04) +
      draw_grob(legend.4, x = -.162, y = -0.07) +
      draw_grob(as_grob(base.map2), x = 0, y = 0, width = 1, height = 1)
} else if (point == "centers") {
    map <- ggdraw() +
      draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
      draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
      draw_grob(legend, x = .031, y = .515, width = 0.10, height = 0.01) +
      draw_grob(legend.3, x = -.409, y = -.11) +
      draw_grob(legend.4, x = -.412, y = -.08) +
      draw_grob(as_grob(base.map3), x = 0, y = 0, width = 1, height = 1) 
}}  else if (point == "centers") {  
    map <- ggdraw() +
     draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
     draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
     draw_grob(legend, x = .032, y = .52, width = 0.10, height = 0.01) + 
     draw_grob(legend.3, x = -.409, y = -.11) +
     draw_grob(legend.4, x = -.412, y = -.08) +
     draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
     draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
     draw_plot(st_cg, x = .31, y = .32, width = .28, height = 0.32, vjust = 1, hjust = 1) +
     draw_grob(as_grob(base.map3), x = 0, y = 0, width = 1, height = 1) 
}  else if (point == "fqhc") {  
   map <- ggdraw() +
     draw_grob(as_grob(base.map), x = 0, y = 0, width = 1, height = 1) +
     draw_plot(cg, x = 0.57, y = .7, width = 0.45, height = 0.46, vjust = 1, hjust = .1) +
     draw_label(fqhc.tx, hjust = 0, x = .335, y = .49, size = 9.5, fontfamily = "Helvetica") +
     draw_image(l.fqhc.img, width = .03, height = .03, x = .299, y = .48) +
     draw_grob(legend, x = .032, y = .495, width = 0.10, height = 0.01) +
     draw_grob(legend.3, x = -.159, y = -0.04) +
     draw_grob(legend.4, x = -.162, y = -0.07) +
     draw_label("*", hjust = 1, size = 15, x = 0.39, y = .42, color = "#093542") +
     draw_plot(st_cg, x = .025, y = .02, width = .28, height = 0.32, vjust = 0, hjust = 0) +
     draw_image(lg.cong,  x = 0.32, y = 0.175, width = 0.044, height = 0.044) +
     draw_label(st.leg, hjust = 1, size = 9, x = 0.49, y = .2, color = "#093542") +
     draw_grob(as_grob(base.map2), x = 0, y = 0, width = 1, height = 1)
} 



if (point == "centers") {
description <- ggdraw() + 
  draw_image(img.hdr, x = -0.01, y = -0.07, width = 1.05, height = 1.05) +
  draw_image(img, x = 0.85, y = 0.03, width = 0.13, height = 0.12) +
  draw_label(FAQ, hjust = 1, size = 13, x = 0.97, y = .955, color = "#093542") +
  draw_label(dz.head, fontface = "bold", hjust = 1, size = 16, x = 0.97, y = .924, color = "#093542") +
  draw_label(paste(dz.head2), fontface = "bold", hjust = 1, size = 16, x = 0.97, y = .895, color = "#093542") +
  draw_label(dz.head3, hjust = 1, size = 12, x = 0.97, y = .865, color = "#093542") +
  draw_label(mp.desc.tit, size = 12, x = 0.05, y = 0.175, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold", fontfamily = "Helvetica", color = "#093542") + 
  draw_label(mp.desc, size = 9, x = 0.05, y = 0.1, hjust = 0, lineheight = 1.25, fontfamily = "Helvetica", color = "#093542") +
  draw_label("Congressional Districts", size = 12, x = 0.05, y = 0.8, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold", fontfamily = "Helvetica", color = "#093542") +
  draw_label("Which congressional boundaries are used to define\nthe Congressional Districts on the map?", size = 10, x = 0.05, y = 0.77, hjust = 0, vjust = 1, lineheight = 1, fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
  draw_label(congress.desc, size = 8.5, x = 0.05, y = 0.705, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
  draw_label("Stroke Centers", size = 12, x = 0.05, y = 0.65, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold", fontfamily = "Helvetica", color = "#093542") +
  draw_label("The four major types of stroke centers provide increasingly\nadvanced levels of stroke care. ", size = 8.5, x = 0.05, y = 0.605,  hjust = 0, lineheight = 1, fontfamily = "Helvetica", color = "#093542") +
  draw_label("What is an Acute Stroke Ready Hospital (ASRH)?", size = 10, x = 0.05, y = 0.575, hjust = 0, vjust = 1, lineheight = 1, fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
  draw_label(
    "An ASRH is a hospital or emergency center with a dedicated stroke-focused program.\n
An Acute Stroke Team is available 24/7 and at the bedside within 15 minutes. Computed\n
tomography (CT) and labs are available 24/7. A neurologist is available at all times\n
(in person or via telemedicine), and neurosurgical services can be provided within\n
3 hours via transfer. IV thrombolytics can be administered with a plan to transfer\n
patients to a higher level stroke center.", size = 8.5, x = 0.05, y = 0.49, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
  draw_label("What is a Primary Stroke Center (PSC)?", size = 10, x = 0.05, y = 0.405, hjust = 0, vjust = 1, lineheight = 1, fontface = "bold",  fontfamily = "Helvetica", color = "#006884") + 
  draw_label(
    "A PSC is a hospital that meets all requirements of an Acute Stroke Ready Hospital\n
and offers more advanced services. PSCs have a stroke unit coordinator and\n
designated beds for the acute care of stroke patients. Computed tomography\n
angiography (CTA), an advanced imaging method is also available. Neurosurgical\n
services can be provided within hospital or within 2 hours with transfer.\n
IV thrombolytics and medical management of stroke is available with transfers for\n
neurosurgical emergencies.", size = 8.5, x = 0.05, y = 0.31, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
  draw_label("What is a Thrombectomy-Capable (TC) Stroke Center?", size = 10, x = 0.49, y = 0.805, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
  draw_label(
    "A TC Stroke Center is a hospital that meets all requirements of a Primary Stroke Center\n
and has the ability to perform mechanical thrombectomy and meet the minimum required\n
number of procedures annually. A neurointensive care unit or designated intensive care beds\n
are present for complex stroke patients and on-site critical care coverage 24/7. MRI, CTA,\n
and magnetic resonance angiography are available 24/7.",
    size = 8.5, x = 0.49, y = 0.725, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
  draw_label("What is a Comprehensive Stroke Center (CSC)?", size = 10, x = 0.49, y = 0.65, hjust = 0, vjust = 1, lineheight = 1.2,fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
  draw_label(
"A CSC is a hospital that meets all requirements of a Thrombectomy-Capable Stroke Center and\n
has the ability to treat the most complex stroke cases and meet requirements for number of\n
advanced stroke treatment procedures performed. There is 24/7 availability for a neurointerventionist,\n
neuroradiologist, neurologist, and neurosurgeon. Hospitals are able to treat multiple complex stroke\n
patients at once and have on-site 24/7 neurointerventionist coverage in the stroke unit. These\n
hospitals also participate in patient-centered research.", size = 8.5, x = 0.49, y = 0.56, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
  draw_label("Stroke Death Rates", size = 12, x = 0.49, y = 0.465, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold", fontfamily = "Helvetica", color = "#093542") +
  draw_label("What is the geographic unit for the stroke death rates?", size = 10, x = 0.49, y = 0.435, hjust = 0, vjust = 1, lineheight = 1.2,fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
  draw_label(
"Stroke death rates are mapped for the counties within each congressional district. When county\n
boundaries overlap congressional districts, any portion outside the boundary of\n
the congressional district is not displayed. ", size = 8.5, x = 0.49, y = 0.38, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
  draw_label("How are the stroke death rates calculated?", size = 10, x = 0.49, y = 0.33, hjust = 0, vjust = 1, lineheight = 1.2,fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
  draw_label(
"For each county, stroke death rates are calculated per 100,000 adults,\n
ages 35 and older, averaged over 3 years (2020-2022) and displayed by\n
quintile. For more details, see CDC’s Atlas of Heart Disease and Stroke ", size = 8.5, x = 0.49, y = 0.28, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
  draw_line(x = c(0.05,.75), y = c(.205,.205), color = "#093542", size = .3)
} else if (point == "fqhc") {
  description <- ggdraw() + 
    draw_image(img.hdr, x = -0.01, y = -0.07, width = 1.05, height = 1.05) +
    draw_image(img, x = 0.85, y = 0.03, width = 0.13, height = 0.12) +
    draw_label(FAQ, hjust = 1, size = 13, x = 0.97, y = .955, color = "#093542") +
    draw_label(dz.head, fontface = "bold", hjust = 1, size = 16, x = 0.97, y = .924, color = "#093542") +
    draw_label(paste(dz.head2), fontface = "bold", hjust = 1, size = 16, x = 0.97, y = .895, color = "#093542") +
    draw_label(dz.head3, hjust = 1, size = 12, x = 0.97, y = .865, color = "#093542") +
    draw_label(mp.desc.tit, size = 12, x = 0.05, y = 0.175, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold", fontfamily = "Helvetica", color = "#093542") + 
    draw_label(mp.desc ,size = 9, x = 0.05, y = 0.1, hjust = 0, lineheight = 1.3, fontfamily = "Helvetica", color = "#093542") +
    draw_label("Congressional Districts", size = 12, x = 0.05, y = 0.75, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold", fontfamily = "Helvetica", color = "#093542") +
    draw_label("Which congressional boundaries are used to define\nthe Congressional Districts on the map?", size = 10, x = 0.05, y = 0.72, hjust = 0, vjust = 1, lineheight = 1, fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
    draw_label(congress.desc, size = 8.5, x = 0.05, y = 0.655, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
    draw_label("Federally Qualified Health Centers (FQHCs)", size = 12, x = 0.05, y = 0.59, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold", fontfamily = "Helvetica", color = "#093542") +
    draw_label(
"Community-based outpatient clinics providing access to health and wellness\n
services to areas or populations that lack access to primary care.", size = 8.5, x = 0.05, y = 0.545, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
    draw_label("Where did we obtain FQHC data?", size = 10, x = 0.05, y = 0.515, hjust = 0, vjust = 1, lineheight = 1, fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
    draw_label(
"We accessed FQHC data (2024) from the Health Resource and Service Administration (HRSA).\n
We included FQHCs that met the following criteria: a) Service delivery site = ‘Hospital’\n
or ‘All Other Clinic Types’, b) Active status as of the date data were downloaded,\n
c) Location = ‘Permanent’, d) Health center type = ’Service Delivery Site’\n
or Administrative/Service Delivery Site’.", size = 8.5, x = 0.05, y = 0.44, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
    draw_label("Heart Disease and Stroke Death Rates", size = 12, x = 0.51, y = 0.75, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold",  fontfamily = "Helvetica", color = "#093542") +
    draw_label(
"What is the geographic unit for Heart Disease\n
and Stroke death rates on this map?", size = 10, x = 0.51, y = 0.72, hjust = 0,lineheight = .6, vjust = 1, fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
    draw_label(
"Death rates are mapped for the counties within each congressional district.\n
When county boundaries overlap congressional districts, any portion outside the\n
boundary of the congressional district is not displayed.", size = 8.5, x = 0.51, y = 0.64, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
    draw_label("How are the Heart Disease and Stroke death rates calculated?", size = 10, x = 0.51, y = 0.585, hjust = 0, vjust = 1, lineheight = 1.2, fontface = "bold",  fontfamily = "Helvetica", color = "#006884") +
    draw_label(
"For each county, death rates are calculated per 100,000 adults, ages 35\n
and older, averaged over 3 years (2020-2022) and displayed by quintile.\n
For more details, see CDC’s Atlas of Heart Disease and Stroke", size = 8.5, x = 0.51, y = 0.53, hjust = 0, lineheight = .6, fontfamily = "Helvetica", color = "#093542") +
    draw_line(x = c(0.05,.75), y = c(.205,.205), color = "#093542", size = .3)
}

  
# Convert the first plot to a PNG in memory
png_map <- image_graph(width = 3300, height = 2400, res = 300)
print(map)
dev.off()

png_description <- image_graph(width = 3300, height = 2400, res = 300)
print(description)
dev.off()

# Combine the in-memory PNGs into a single PDF
combined_pdf <- image_convert(c(png_map, png_description), format = "pdf")


# Save the second page (description) as a PNG file

if (point == "fqhc" & outcome == "hd_mort") {
  image_write(combined_pdf, path = here("Final Product", "2020-2022", "Heart Disease - FQHCS",
                                        paste(outcome, "_", state, "_", congress, "_", ".pdf", sep = "")))
} else if (point == "fqhc" & outcome == "str_mort") {
  image_write(combined_pdf, path = here("Final Product", "2020-2022", "Stroke - FQHC", 
                                        paste(outcome, "_", state, "_", congress, "_", ".pdf", sep = "")))
} else if (point == "centers" & outcome == "str_mort") {
  image_write(combined_pdf, path = here("Final Product", "2020-2022", "Stroke - Stroke Centers", 
                                        paste(outcome, "_", state, "_", congress, "_", ".pdf", sep = "")))
}
  
  # End of state.maps function
}

 # Code Finalization and Map Generation ------------------------------------------------------

# We are defining our variables in the function here. When we run these individual codes we are selecting a specific variable of a specific dataframe
# AND telling it where it should be saved.
 
# Generate maps for all states and congressional districts
#for (oc in c("hd_mort", "str_mort")) {  # Iterate over outcomes
#  for (state_name in st_inf$name) {  # Iterate over state names
#    for (cd in 1:n_congress) {  # Iterate over congressional districts
      # Generate FQHC maps
#      quick.maps(point = "fqhc", congress = cd, state = state_name, outcome = oc, 
#                 mapvar = "all_all_35up", races = "all", ages = "35+", 
#                 path = here("Final Product", ifelse(oc == "hd_mort", "Heart Disease", "Stroke"), "FQHC"), 
#                 ht = 8, wd = 11)
      
      # Generate Stroke Centers maps only for stroke mortality
#      if (oc == "str_mort") {
#        quick.maps(point = "centers", congress = cd, state = state_name, outcome = oc, 
#                   mapvar = "all_all_35up", races = "all", ages = "35+", 
#                   path = here("Final Product", "Stroke", "Centers"), ht = 8, wd = 11)
#      }
#    }
#  }
#}




# Alabama -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Alabama", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Alabama", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Alabama", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Alabama", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Alabama", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Alabama", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Alabama", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - Centers
quick.maps(point = "centers", congress = 1, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Alabama", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Alaska ------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Alaska", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR
quick.maps(point = "fqhc", congress = 1, state = "Alaska", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - Centers
quick.maps(point = "centers", congress = 1, state = "Alaska", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Arizona -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Arizona", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - Centers
quick.maps(point = "centers", congress = 1, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 9, state = "Arizona", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# Arkansas ----------------------------------------------------------------
# HD 
quick.maps(point = "fqhc", congress = 1, state = "Arkansas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Arkansas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Arkansas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Arkansas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Arkansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Arkansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Arkansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Arkansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - Centers
quick.maps(point = "centers", congress = 1, state = "Arkansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Arkansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Arkansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Arkansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# California --------------------------------------------------------------
#HD
quick.maps(point = "fqhc", congress = 1, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 13, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 14, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 15, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 16, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 17, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 18, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 19, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 20, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 21, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 22, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 23, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 24, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 25, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 26, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 27, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 28, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 29, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 30, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 31, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 32, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 33, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 34, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 35, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 36, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 37, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 38, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 39, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 40, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 41, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 42, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 43, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 44, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 45, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 46, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 47, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 48, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 49, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 50, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 51, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 52, state = "California", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 13, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 14, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 15, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 16, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 17, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 18, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 19, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 20, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 21, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 22, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 23, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 24, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 25, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 26, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 27, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 28, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 29, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 30, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 31, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 32, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 33, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 34, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 35, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 36, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 37, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 38, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 39, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 40, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 41, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 42, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 43, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 44, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 45, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 46, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 47, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 48, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 49, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 50, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 51, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 52, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 9, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 10, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 11, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 12, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 13, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 14, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 15, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 16, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 17, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 18, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 19, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 20, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 21, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 22, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 23, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 24, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 25, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 26, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 27, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 28, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 29, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 30, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 31, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 32, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 33, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 34, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 35, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 36, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 37, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 38, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 39, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 40, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 41, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 42, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 43, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 44, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 45, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 46, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 47, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 48, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 49, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 50, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 51, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 52, state = "California", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Colorado ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Colorado", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Colorado", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Colorado", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Colorado", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Colorado", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Colorado", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Colorado", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Colorado", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - Centers
quick.maps(point = "centers", congress = 1, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "Colorado", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Connecticut -------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Connecticut", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Connecticut", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Connecticut", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Connecticut", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Connecticut", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Connecticut", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Delaware ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Delaware", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Delaware", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Delaware", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Florida -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 13, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 14, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 15, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 16, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 17, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 18, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 19, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 20, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 21, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 22, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 23, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 24, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 25, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 26, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 27, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 28, state = "Florida", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 13, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 14, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 15, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 16, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 17, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 18, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 19, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 20, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 21, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 22, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 23, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 24, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 25, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 26, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 27, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 28, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 9, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 10, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 11, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 12, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 13, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 14, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 15, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 16, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 17, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 18, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 19, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 20, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 21, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 22, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 23, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 24, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 25, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 26, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 27, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 28, state = "Florida", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Georgia -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 13, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 14, state = "Georgia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 13, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 14, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 9, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 10, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 11, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 12, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 13, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 14, state = "Georgia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Hawaii ------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Hawaii", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Hawaii", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR -FQHC
quick.maps(point = "fqhc", congress = 1, state = "Hawaii", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Hawaii", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Hawaii", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Hawaii", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Idaho -------------------------------------------------------------------
#HD
quick.maps(point = "fqhc", congress = 1, state = "Idaho", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Idaho", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Idaho", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Idaho", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Idaho", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Idaho", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Illinois ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 14, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 15, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 16, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 17, state = "Illinois", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 14, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 15, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 16, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 17, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 10, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 11, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 12, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 13, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 14, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 15, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 16, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 17, state = "Illinois", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Indiana -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Indiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Indiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Iowa --------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Iowa", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Iowa", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Iowa", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Iowa", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Iowa", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Iowa", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Iowa", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Iowa", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Iowa", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Iowa", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Iowa", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Iowa", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Kansas -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Kansas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Kansas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Kansas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Kansas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Kansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Kansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Kansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Kansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Kansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Kansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Kansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Kansas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Kentucky ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Kentucky", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Kentucky", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Kentucky",outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Kentucky", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Kentucky", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Kentucky", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Kentucky", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Louisiana ---------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Louisiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Louisiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Louisiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Louisiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Louisiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Louisiana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Louisiana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Maine -------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Maine", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Maine", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Maine", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Maine", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Maine", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Maine", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Maryland ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Maryland", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Maryland", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Maryland", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Maryland", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Maryland", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Maryland", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Maryland", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Maryland", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Maryland", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 



# Massachusetts -----------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Massachusetts", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Massachusetts", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Michigan ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "Michigan", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 10, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 11, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 12, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 13, state = "Michigan", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Minnesota ---------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Minnesota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Minnesota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Minnesota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Minnesota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Minnesota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Minnesota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Minnesota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Minnesota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "Minnesota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)



# Mississippi -------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Mississippi", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Mississippi", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Mississippi", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Mississippi", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Mississippi", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Mississippi", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Mississippi", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Mississippi", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Mississippi", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Mississippi", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Mississippi", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Mississippi", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Missouri ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Missouri", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Missouri", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Missouri", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Missouri", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Missouri", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Missouri", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Missouri", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Missouri", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Missouri", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Montana -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Montana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Montana", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Montana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Montana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Montana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Montana", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Nebraska ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Nebraska", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 2, state = "Nebraska", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Nebraska", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Nebraska", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 2, state = "Nebraska", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Nebraska", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Nebraska", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 2, state = "Nebraska", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Nebraska", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Nevada ------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Nevada", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Nevada", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Nevada", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Nevada", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Nevada", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Nevada", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Nevada", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Nevada", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Nevada", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Nevada", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Nevada", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Nevada", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# New Hampshire -----------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "New Hampshire", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "New Hampshire", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "New Hampshire", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "New Hampshire", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "New Hampshire", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "New Hampshire", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# New Jersey --------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "New Jersey", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - Centers
quick.maps(point = "centers", congress = 1, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 9, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 10, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 11, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 12, state = "New Jersey", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# New Mexico --------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "New Mexico", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "New Mexico", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "New Mexico", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "New Mexico", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "New Mexico", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "New Mexico", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - Centers
quick.maps(point = "centers", congress = 1, state = "New Mexico", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "New Mexico", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "New Mexico", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# New York ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 14, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 15, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 16, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 17, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 18, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 19, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 20, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 21, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 22, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 23, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 24, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 25, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 26, state = "New York", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 14, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 15, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 16, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 17, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 18, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 19, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 20, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 21, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 22, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 23, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 24, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 25, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 26, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 10, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 11, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 12, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 13, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 14, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 15, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 16, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 17, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 18, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 19, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 20, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 21, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 22, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 23, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 24, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 25, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 26, state = "New York", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# North Carolina ----------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 3, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 4, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 5, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 6, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 7, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 8, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 9, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 10, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 11, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 12, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 13, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 14, state = "North Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 3, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 4, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 5, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 6, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 7, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 8, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 9, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 10, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 11, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 12, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 13, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)  
quick.maps(point = "fqhc", congress = 14, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 3, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 4, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 5, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 6, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 7, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 8, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 9, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 10, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 11, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 12, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 13, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)  
quick.maps(point = "centers", congress = 14, state = "North Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# North Dakota ------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "North Dakota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "North Dakota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "North Dakota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Ohio --------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 13, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 14, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 15, state = "Ohio", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 9, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 10, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 11, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 12, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 13, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 14, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 15, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 9, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 10, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 11, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 12, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 13, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 14, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 15, state = "Ohio", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Oklahoma ----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Oklahoma", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Oklahoma", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Oklahoma", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Oklahoma", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Oklahoma", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 2, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 2, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Oklahoma", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# Oregon ------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Oregon", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Oregon", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Oregon", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Oregon", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Oregon", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Oregon", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "Oregon", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)



# Pennsylvania ------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 14, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 15, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 16, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 17, state = "Pennsylvania", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 14, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 15, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 16, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 17, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 10, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 11, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 12, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 13, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 14, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 15, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 16, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 17, state = "Pennsylvania", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

 

# Rhode Island ------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Rhode Island", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Rhode Island", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Rhode Island", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Rhode Island", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Rhode Island", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Rhode Island", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)


# South Carolina ----------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "South Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "South Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "South Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "South Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "South Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "South Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "South Carolina", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "South Carolina", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 



# South Dakota ------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "South Dakota", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "South Dakota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "South Dakota", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Tennessee ---------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Tennessee", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Tennessee", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Texas -------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 14, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 15, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 16, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 17, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 18, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 19, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 20, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 21, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 22, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 23, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 24, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 25, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 26, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 27, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 28, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 29, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 30, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 31, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 32, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 33, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 34, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 35, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 36, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 37, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 38, state = "Texas", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 12, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 13, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 14, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 15, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 16, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 17, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 18, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 19, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 20, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 21, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 22, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 23, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 24, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 25, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 26, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 27, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 28, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 29, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 30, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 31, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 32, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 33, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 34, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 35, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 36, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 37, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 38, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 10, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 11, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 12, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 13, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 14, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 15, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 16, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 17, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 18, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 19, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 20, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 21, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 22, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 23, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 24, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 25, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 26, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 27, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 28, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 29, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 30, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 31, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 32, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 33, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 34, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 35, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 36, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 37, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 38, state = "Texas", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 



# Utah --------------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Utah", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Utah", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Utah", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Utah", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Utah", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Utah", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Utah", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Utah", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Utah", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Utah", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Utah", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Utah", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Vermont -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Vermont", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Vermont", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", path= here("Final Product", "Stroke" , "FQHC"), ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Vermont", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", path= here("Final Product", "Stroke" , "centers"), ht = 8, wd = 11)


# Virginia -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 11, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 10, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 11, state = "Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Washington --------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Washington", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 3, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 4, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 5, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 6, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 7, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 8, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 9, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 10, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 3, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 4, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 5, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 6, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 7, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 8, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 9, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 10, state = "Washington", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# West Virginia -----------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "West Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "West Virginia", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "West Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "West Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 

# STR - centers
quick.maps(point = "centers", congress = 1, state = "West Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "West Virginia", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 


# Wisconsin ---------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Wisconsin", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Wisconsin", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Wisconsin", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Wisconsin", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Wisconsin", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Wisconsin", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Wisconsin", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Wisconsin", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11) 
quick.maps(point = "fqhc", congress = 2, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 3, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 4, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 5, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 6, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 7, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)
quick.maps(point = "fqhc", congress = 8, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11) 
quick.maps(point = "centers", congress = 2, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 3, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 4, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 5, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 6, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 7, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)
quick.maps(point = "centers", congress = 8, state = "Wisconsin", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)



# Wyoming -----------------------------------------------------------------
# HD
quick.maps(point = "fqhc", congress = 1, state = "Wyoming", outcome = "hd_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)

# STR - FQHC
quick.maps(point = "fqhc", congress = 1, state = "Wyoming", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+",ht = 8, wd = 11)

# STR - centers
quick.maps(point = "centers", congress = 1, state = "Wyoming", outcome = "str_mort", mapvar = "all_all_35up", 
           races = "all", ages = "35+", ht = 8, wd = 11)




# Tools Just in case ------------------------------------------------------

# If there are any issues with the code, you can define the variables here 
# and still run the function row by row without activating the whole function

races = "all"
ages = "all"
mapvar = "all_all_35up"
path="NCCDPHP DHDSP ESB - Small Area Analysis Team/On-line Tools/Quick Maps/
           R Maps/Quick Map Data/Final Product/Heart Disease Hospitalization"
outcome = "hd_mort"
ht = 12
wd = 8.5
state = "Hawaii"
point = "fqhc"
congress = 1


