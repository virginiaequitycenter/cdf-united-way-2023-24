
# Set working directory, this is the folder where you will be working and where all your files used in this script are located

setwd("C:/Users/affes/OneDrive/Documents/UVA SDS Fall 2023/Other Events/Community Data Fellowship") 
# Can also go to tabs and select Session -> Set Working Directory -> Folder


# Load library packages here (these are essentially tool suites that provide additional functionality within R)
# When first installing, need to enter: install.packages("package_name")
# After first install, can just load the library (don't include quotes like above): library(package_name)

library(tidyverse) # Always want to load this package (very foundational)
library(tidycensus)


# Since we are working with census API, will need to set API key (should have gotten from their website)
dotenv::load_dot_env()
census_api <- Sys.getenv("censusapi") # Access the API key


# Now we want to practice using the tidycensus package

# A function that will pull the DP05 data we are interested in a given county 
dp05_longtbl <- function(fips5, endyr, keyval){
  # Split the fips key into state and county
  state_code <- as.numeric(substr(as.character(fips5), 1, 2))
  county_code <- as.numeric(substr(as.character(fips5), 3, 5))
  
  # Gets variable names
  titl <- load_variables(endyr, "acs5/profile")
  # Gets the table as requested
  tbl <- get_acs(geography="county", 
                 table = "DP05", 
                 state = state_code, 
                 county = county_code, 
                 output = "tidy", 
                 year = endyr, 
                 survey = "acs5", 
                 key = keyval)
  # Combine table of names with table of values 
  comb_tbl <- merge(x = tbl, y = titl, by.x = "variable", by.y = "name", all.x = T)
  
  return(comb_tbl) # Returns table
}

# Create a store for location codes
gca <- c(51003, 51540, 51065, 51079, 51109, 51125)

# Create empty table of values
gca_tbl <- tibble(variable=NULL, GEOID=NULL, NAME=NULL, estimate=NULL, moe=NULL, label=NULL, concept=NULL)

# Create loop to go through all locations in Greater Charlottesville area and combine to single table of all DP05
for (i in gca){
  
  # Run the code for the tables
  loc_tbl <- dp05_longtbl(i, 2021, census_api)
  
  # Append new rows to table
  gca_tbl <- bind_rows(gca_tbl, loc_tbl)
}


# Group by variables and aggregate across all county in Greater Charlottesville area
# Approximate MOE when aggregating (square each and take sum, then square root sum)
gca_agg <- gca_tbl %>% 
  group_by(variable) %>% 
  summarise(label = head(label, 1), 
            concept = head(concept, 1), 
            agg_estimate = sum(estimate), 
            approx_moe = sqrt(sum(moe^2)))




# Create function to create aggregated data when given a collection of fips codes
agg_table <- function(fips5){
  
  # Create a store for location codes
  codes <- fips5
  
  # Create empty table of values
  gca_tbl <- tibble(variable=NULL, GEOID=NULL, NAME=NULL, estimate=NULL, moe=NULL, label=NULL, concept=NULL)
  
  # Runs loop
  for (i in codes){
    
    # Run the code for the tables
    loc_tbl <- dp05_longtbl(i, 2021, census_api)
    
    # Append new rows to table
    all_loc <- bind_rows(gca_tbl, loc_tbl)
  }
  
  # Group by variables and aggregate across all county in Greater Charlottesville area
  # Approximate MOE when aggregating (square each and take sum, then square root sum)
  agg_tbl <- all_loc %>% 
    group_by(variable) %>% 
    summarise(label = head(label, 1), 
              concept = head(concept, 1), 
              agg_estimate = sum(estimate), 
              approx_moe = sqrt(sum(moe^2)))
  
  return(agg_tbl)
}


# Test with Staunton/Augusta/Waynesboro
saw <- c(51790, 51015, 51820)

saw_agg <- agg_table(saw)
