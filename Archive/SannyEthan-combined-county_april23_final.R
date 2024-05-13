## Tidycensus code for United Way Data
## Edits from original file made by Ethan and Sanny

## This script can be used to create tables for ONE COUNTY or ONE COMBINED REGION (Example: Charlottesville-Albermarle)
## Data Request - THESE ARE FOR 2022 ACS 5-YEAR EST - TABLE CODES CAN BE DIFFERENT FOR DIFFERENT YEARS

## County FIP codes
# 003 -- Albemarle
# 540 -- Charlottesville
# 065 -- Fluvanna
# 079 -- Greene
# 109 -- Louisa
# 125 -- Nelson
# 790 -- Staunton
# 015 -- Augusta
# 820 -- Waynesboro
# 660 -- Harrisonburg
# 029 -- Buckingham
# 137 -- Orange
# 165 -- Rockingham
# 113 -- Madison
# 139 -- Page
# 157 -- Rappahannock
# 171 -- Shenandoah
# 187 -- Warren
# 840 -- Winchester
# 043 -- Clarke
# 069 -- Frederick

# Packages
library(tidyverse)
library(tidycensus)

# Census API key
census_api <- Sys.getenv("CENSUS_API_KEY")

# Year for acs5 data pull
year <- 2022

# County FIP codes and name
county_codes <- c("003", "540") # locality FIPS codes desired
name <- "Charlottesville-Albermarle" # name of locality or combined region

# Variable view helper
all_acs_meta <- function(){
  # Gets the list of all variables from all acs5 metadata tables
  vars1 <- load_variables(year, "acs5") %>% select(-geography) # Remove the geography column
  vars2 <- load_variables(year, "acs5/profile")
  vars3 <- load_variables(year, "acs5/subject")
  vars4 <- load_variables(year, "acs5/cprofile")

  # Provides column with specific lookup
  vars1$dataset_table <- "acs5"
  vars2$dataset_table  <- "acs5/profile"
  vars3$dataset_table  <- "acs5/subject"
  vars4$dataset_table  <- "acs5/cprofile"

  # Combine all table rows
  all_vars_meta <- rbind(vars1, vars2, vars3, vars4)

  return(all_vars_meta)
}

# Pull all variable names from metadata
metadata_var <- all_acs_meta()

# View acs metadata tables
view(metadata_var)

####################################
#### Race and Ethnicity -- DP05 ####
####################################
# Race and Ethnicity - these are counts
var_DP05 <- list(
  "RaceEthnic_%_Black" = "DP05_0038", # Black/AA
  "RaceEthnic_%_AmerIndian" = "DP05_0039", # American Indian/Alaska Native
  "RaceEthnic_%_Asian" = "DP05_0044", # Asian
  "RaceEthnic_%_PacifIslan" = "DP05_0052", # Native Hawaiian/Pacific Islander
  "RaceEthnic_%_HispanLatin" = "DP05_0073", # Hispanic/Latino (any race)
  "RaceEthnic_%_White" = "DP05_0079" # White alone (not Hispanic/Latino)
)

# Get ACS data
acs_data_DP05 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_codes,
                    variables = var_DP05,
                    summary_var = "DP05_0033", # this provides the total (summary table) we need for creating percents
                    year = year, 
                    survey = "acs5",
                    key = census_api
                    ) 

# Create summary variables for the combined counties
# Run for single locality, will lead to no changes in the data table
acs_data_DP05_summarize <- acs_data_DP05 %>% 
  group_by(variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate),
            sum_all = sum(summary_est))

# Create percentages from estimates
acs_data_DP05_summarize <- acs_data_DP05_summarize %>% 
  mutate(value = round(((sum_est / sum_all) * 100), digits = 2),
         name = name) %>% 
  select(name, variable, value)

# Save table as csv
write.csv(acs_data_DP05_summarize, paste0(as.character(year), "-", name, "-DP05.csv"), row.names=FALSE)

##############################################################
#### Race and Ethnicity of Children % -- B01001-I tables ####
##############################################################
# Using American Community Survey, 5-year estimates
# Table B01001
# American Community Survey table B01001A-I "Sex by Age, by Race"
# https://www.socialexplorer.com/data/ACS2022_5yr/metadata/?ds=ACS22_5yr&table=B01001A

## black ----
vars_B01001B <- c(popm_u5 = "B01001B_003", # Male under 5yrs
          popm_5to9 = "B01001B_004", # Male ages 5 to 9yrs
          popm_10to14 = "B01001B_005", # Male ages 10 to 14yrs
          popm_15to17 = "B01001B_006", # Male ages 15 to 17yrs
          popf_u5 = "B01001B_018", # Female under 5yrs
          popf_5to9 = "B01001B_019", # Female ages 5 to 9
          popf_10to14 = "B01001B_020", # Female ages 10 to 14yrs
          popf_15to17 = "B01001B_021") # Female ages 15 to 17yrs

pop_child_black <- get_acs(geography = "county",
                           state = "51",
                           county = county_codes,
                           var = vars_B01001B,
                           year = year,
                           survey = "acs5")

pop_child_black <- pop_child_black %>% 
  group_by(GEOID, NAME) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = "u18_Black")

# American Indian/Alaska Native
vars_B01001C <- c(popm_u5 = "B01001C_003", 
          popm_5to9 = "B01001C_004", 
          popm_10to14 = "B01001C_005",
          popm_15to17 = "B01001C_006",
          popf_u5 = "B01001C_018", 
          popf_5to9 = "B01001C_019", 
          popf_10to14 = "B01001C_020",
          popf_15to17 = "B01001C_021")

pop_child_aian <- get_acs(geography = "county",
                          state = "51",
                          county = county_codes,
                          var = vars_B01001C,
                          year = year,
                          survey = "acs5")

pop_child_aian <- pop_child_aian %>% 
  group_by(GEOID, NAME) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = "u18_AmerIndian")

# Asian
vars_B01001D <- c(popm_u5 = "B01001D_003", 
          popm_5to9 = "B01001D_004", 
          popm_10to14 = "B01001D_005",
          popm_15to17 = "B01001D_006",
          popf_u5 = "B01001D_018", 
          popf_5to9 = "B01001D_019", 
          popf_10to14 = "B01001D_020",
          popf_15to17 = "B01001D_021")

pop_child_asian <- get_acs(geography = "county",
                           state = "51",
                           county = county_codes,
                           var = vars_B01001D,
                           year = year,
                           survey = "acs5")

pop_child_asian <- pop_child_asian %>% 
  group_by(GEOID, NAME) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>%
  mutate(race_ethn = "u18_Asian")

# Native Hawaiian/Pacific Islander
vars_B01001E <- c(popm_u5 = "B01001E_003", 
          popm_5to9 = "B01001E_004", 
          popm_10to14 = "B01001E_005",
          popm_15to17 = "B01001E_006",
          popf_u5 = "B01001E_018", 
          popf_5to9 = "B01001E_019", 
          popf_10to14 = "B01001E_020",
          popf_15to17 = "B01001E_021")

pop_child_nhpi <- get_acs(geography = "county",
                          state = "51",
                          county = county_codes,
                          var = vars_B01001E,
                          year = year,
                          survey = "acs5")

pop_child_nhpi <- pop_child_nhpi %>% 
  group_by(GEOID, NAME) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = "u18_PacifIslan")

# Hispanic/Latino (any race)
vars_B01001I <- c(popm_u5 = "B01001I_003", 
          popm_5to9 = "B01001I_004", 
          popm_10to14 = "B01001I_005",
          popm_15to17 = "B01001I_006",
          popf_u5 = "B01001I_018", 
          popf_5to9 = "B01001I_019", 
          popf_10to14 = "B01001I_020",
          popf_15to17 = "B01001I_021")

pop_child_hisp <- get_acs(geography = "county",
                          state = "51",
                          county = county_codes,
                          var = vars_B01001I,
                          year = year,
                          survey = "acs5")

pop_child_hisp <- pop_child_hisp %>% 
  group_by(GEOID, NAME) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>%  
  mutate(race_ethn = "u18_HispanLatin")

# White alone (not Hispanic/Latino)
vars_B01001H <- c(popm_u5 = "B01001H_003", 
          popm_5to9 = "B01001H_004", 
          popm_10to14 = "B01001H_005",
          popm_15to17 = "B01001H_006",
          popf_u5 = "B01001H_018", 
          popf_5to9 = "B01001H_019", 
          popf_10to14 = "B01001H_020",
          popf_15to17 = "B01001H_021")

pop_child_nhwhite <- get_acs(geography = "county",
                             state = "51",
                             county = county_codes,
                             var = vars_B01001H,
                             year = year,
                             survey = "acs5")

pop_child_nhwhite <- pop_child_nhwhite %>% 
  group_by(GEOID, NAME) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = "u18_White")

# Combine child_pop_race ----
pop_child_race <- bind_rows(pop_child_black, pop_child_aian, 
                            pop_child_asian, pop_child_nhpi, 
                            pop_child_hisp, pop_child_nhwhite) 

# NEEDS FIXING
# Need to be reconsidered (See totals from B01001 tables)
pop_child_race <- pop_child_race %>% 
  group_by(GEOID, NAME) %>% 
  mutate(total_count = sum(sum_est)) # this total count should come from a total of under 18, table B01001

# Create summary variables for the combined counties
# Run for single locality, will lead to no changes in the data table
pop_child_race_summarize <- pop_child_race %>% 
  group_by(race_ethn) %>% 
  summarize(sum_est = sum(sum_est),
            sum_moe = moe_sum(moe = sum_moe, estimate = sum_est),
            sum_all = sum(total_count))

# Create percentages from estimates
pop_child_race_summarize <- pop_child_race_summarize %>% 
  mutate(value = round(((sum_est / sum_all) * 100), digits = 2),
         name = name,
         variable = paste0("ChildPopRace_", race_ethn)) %>% 
  select(name, variable, value)

# Save table as csv
write.csv(pop_child_race_summarize, paste0(as.character(year), "-", name, "-B01001.csv"), row.names=FALSE)

################################
#### Poverty Rates -- S1701 ####
################################

var_S1701 <- c(
  "Poverty_%_Black" = "S1701_C02_014", # Black/AA
  "Poverty_%_AmerIndian" = "S1701_C02_015", # American Indian/Alaska Native
  "Poverty_%_Asian" = "S1701_C02_016", # Asian
  "Poverty_%_PacifIslan" = "S1701_C02_017", # Native Hawaiian/Pacific Islander
  "Poverty_%_HispanLatin" = "S1701_C02_020", # Hispanic/Latino (any race)
  "Poverty_%_White" = "S1701_C02_021", # White alone (not Hispanic/Latino)
  "Poverty_%_All" = "S1701_C02_001" # All
)
var_S1701_sum <- c(
  "Poverty_%_Black" = "S1701_C01_014", # Black/AA
  "Poverty_%_AmerIndian" = "S1701_C01_015", # American Indian/Alaska Native
  "Poverty_%_Asian" = "S1701_C01_016", # Asian
  "Poverty_%_PacifIslan" = "S1701_C01_017", # Native Hawaiian/Pacific Islander
  "Poverty_%_HispanLatin" = "S1701_C01_020", # Hispanic/Latino (any race)
  "Poverty_%_White" = "S1701_C01_021", # White alone (not Hispanic/Latino)
  "Poverty_%_All" = "S1701_C01_001" # All
)

# Get ACS data
acs_data_S1701 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_codes,
                    variables = var_S1701,
                    year = year, 
                    survey = "acs5",
                    key = census_api) 

acs_data_S1701_sum <-  get_acs(geography = "county",
                               state = "VA",
                               county = county_codes,
                               variables = var_S1701_sum,
                               year = year, 
                               survey = "acs5",
                               key = census_api) 

acs_data_S1701 <- acs_data_S1701 %>% 
  left_join(acs_data_S1701_sum %>% rename(summary_est = estimate,
                                          summary_moe = moe),
            by = c("GEOID", "NAME", "variable"))

# Create summary variables for the combined counties
# Run for single locality, will lead to no changes in the data table
acs_data_S1701_summarize <- acs_data_S1701 %>% 
  group_by(variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate),
            sum_all = sum(summary_est))

# Create percentages from estimates
acs_data_S1701_summarize <- acs_data_S1701_summarize %>% 
  mutate(value = round(((sum_est / sum_all) * 100), digits = 2),
         name = name) %>% 
  select(name, variable, value)

# Save table as csv
write.csv(acs_data_S1701_summarize, paste0(as.character(year), "-", name, "-S1701.csv"), row.names=FALSE)

###############################################################################################
#### Median Household Income -- S1903 (using B19001 to provide aggregate median estimates) ####
###############################################################################################
## Needs more complex computations for accurate combined values
## Instructions for aggregating medians (https://dof.ca.gov/wp-content/uploads/sites/352/Forecasting/Demographics/Documents/How_to_Recalculate_a_Median.pdf)

# Function for pulling all income range categories for the given race/ethnicity(s)
pull_B19001 <- function(county_codes, year, census_api, race_ethnicity = c("Black", "AmerIndian", "Asian", "PacifIslan", "HispanLatin", "White", "All")){
  
  # Mapping letter code in census to corresponding race/ethnicity group
  racethn_map = list("Black" = "B19001B", 
                     "AmerIndian" = "B19001C", 
                     "Asian" = "B19001D", 
                     "PacifIslan" = "B19001E", 
                     "HispanLatin" = "B19001I", 
                     "White" = "B19001H", 
                     "All" = "B19001")
  
  # Loops through all provided race/ethnicity groups and creates mappings/pull data
  for (racethn in race_ethnicity){
    
    # Creates variable mapping for the income ranges
    range_map <- list("2500-9999" = paste0(racethn_map[racethn], "_002"),
                      "10000-14999" = paste0(racethn_map[racethn], "_003"),
                      "15000-19999" = paste0(racethn_map[racethn], "_004"),
                      "20000-24999" = paste0(racethn_map[racethn], "_005"),
                      "25000-29999" = paste0(racethn_map[racethn], "_006"),
                      "30000-34999" = paste0(racethn_map[racethn], "_007"),
                      "35000-39999" = paste0(racethn_map[racethn], "_008"),
                      "40000-44999" = paste0(racethn_map[racethn], "_009"),
                      "45000-49999" = paste0(racethn_map[racethn], "_010"),
                      "50000-59999" = paste0(racethn_map[racethn], "_011"),
                      "60000-74999" = paste0(racethn_map[racethn], "_012"),
                      "75000-99999" = paste0(racethn_map[racethn], "_013"),
                      "100000-124999" = paste0(racethn_map[racethn], "_014"),
                      "125000-149999" = paste0(racethn_map[racethn], "_015"),
                      "150000-199999" = paste0(racethn_map[racethn], "_016"),
                      "200000-300000" = paste0(racethn_map[racethn], "_017"))
    
    # Creates table name
    tbl_name <- paste0("acs_B19001_", racethn)
    
    # Pulls data from acs
    tbl <- get_acs(geography = "county",
                   state = "VA",
                   county = county_codes,
                   variables = range_map, 
                   summary_var = "B19001_001",
                   year = year,
                   survey = "acs5", 
                   key = census_api)
    
    # Assigns table name to pulled data for given iteration of loop
    assign(tbl_name, tbl, envir = .GlobalEnv)
  }
}

# Function for calculating standard error (SE) of a 50 percent proportion
se_50per <- function(total){
  # standard error (SE) Formula
  se_val = 1.5 * sqrt(((99/total)*(50^2)))
  
  # Check for edge cases where outside of percent bounds
  se_val <- max(se_val, 0) # Ensure SE is not negative
  se_val <- min(se_val, 100) # Ensure SE is not greater than 100
  return(se_val)
}

# Function that determines which tables are in the environment to plug into agg_measures function
find_tbls <- function(){
  
  # Get a list of objects in the global environment
  objects <- ls(.GlobalEnv)
  
  # Filter objects that start with "acs_B19001"
  acs_tables <- objects[grep("^acs_B19001", objects, perl = TRUE)]
  
  # Check if any acs tables are found
  if (length(acs_tables) > 0) {
    
    # Create a list to store the datasets
    dataset_list <- list()
    
    # Add datasets to the list
    for (table_name in acs_tables) {
      dataset <- get(table_name)
      race_ethnicity <- sub("^acs_B19001_+(.*)", "\\1", table_name, perl = TRUE)
      dataset_list[[table_name]] <- list(data = dataset, race_ethnicity = race_ethnicity)
    }
    
    return(dataset_list)
  } else {
    # Return an error message if no datasets are found
    stop("No datasets starting with 'acs_B19001' found in the environment.")
  }
}

# Function that determines aggregate measures and returns table for all race groups
agg_aprrox <- function(name){
  
  # Finds all objects
  tables <- find_tbls()
  
  # Create empty dataframe to hold the aggregate values
  agg_vals <- tibble(RacEthn = NULL, MedianHHIncome_value = NULL, MedianHHIncome_moe = NULL)
  
  # Loop through the tables provided (all race/ethnicity groups by default) and aggregates medians
  for (t in tables){
    
    # Gets just the data
    t_dat <- t$data
    
    # Check if the dataframe is empty or if all estimates are zero
    if (nrow(t_dat) == 0 || all(t_dat$estimate == 0)) {
      
      new_vals <- tibble(RacEthn = t$race_ethnicity, MedianHHIncome_value = NA, MedianHHIncome_moe = NA)
      agg_vals <- rbind(agg_vals, new_vals)
      # Skip calculations if all estimates are zero
      next()
    }
    
    # Transforms table into format needed for finding aggregate median and moe
    t_dat <- t_dat %>% rename(income_bracket = variable) %>% 
      separate(income_bracket, into = c("bin_start", "bin_end"), 
               sep = "-", remove = FALSE) %>% 
      mutate(across(starts_with("bin"), as.numeric)) %>%
      group_by(bin_start, bin_end,income_bracket) %>%
      summarise(estimate=sum(estimate), moe=sum(moe)) %>%
      ungroup() %>% 
      mutate(Cummul_estimate = cumsum(estimate), Cummul_perc = (Cummul_estimate / max(Cummul_estimate)) * 100) 
    
    
    #### Calculate aggregate median ####
    
    # Gets the total number of households
    total_est <- max(t_dat$Cummul_estimate)
    
    # Finds the middle value
    midpoint_est <- round(total_est/2)
    
    # Gives index value for the row containing the middle value
    midpoint_index <- max(which(t_dat$Cummul_estimate < midpoint_est))
    
    # Calculate proportion of households
    prop_val <- (midpoint_est - t_dat$Cummul_estimate[midpoint_index-1])/t_dat$Cummul_estimate[midpoint_index]
    
    # Gives the range for midrange
    midrange <- (t_dat$bin_end[midpoint_index] - t_dat$bin_start[midpoint_index]) + 1
    
    # Gets median by adding the product of the proportion of households and the range 
    aggregate_median <- t_dat$bin_start[midpoint_index] + (midrange * prop_val)
    
    #### Calculate aggregate moe ####
    
    # Calculates lower and upper percent bounds around the median
    p_low <- max(50 - se_50per(total_est), 0) # Ensure lower percent bound is not negative
    p_upr <- 50 + se_50per(total_est)
    
    # Gives index value for row containing lower percent bound and upper percent bound
    p_low_index <- max(which(t_dat$Cummul_perc < p_low), 1)  # Ensure lower percent bound is not negative
    p_upr_index <- max(which(t_dat$Cummul_perc < p_upr), na.rm = TRUE)
    
    # Conditional checks and computations for median and margin of error
    if (p_low_index != p_upr_index) {
      
      # Handling for different ranges...
      # If not same range; need two sets of A1, A2, C1, C2
      p_low_A1 <- t_dat$bin_start[p_low_index] # Smallest value in the range
      p_low_A2 <- t_dat$bin_start[p_low_index+1] # Smallest value in next highest range
      
      # Adjust for edge case where lower bound index is the first row in data
      if (p_low_index != 1){
        p_low_C1 <- t_dat$Cummul_perc[p_low_index-1] # Cumulative percent of units strictly less than A1 (nothing less than it)
      } else{
        p_low_C1 <- 0 # Set as 0 because there are no units strictly less than A1
      }
      
      p_low_C2 <- t_dat$Cummul_perc[p_low_index] # Cumulative percent of units strictly less than A2
      
      p_upr_A1 <- t_dat$bin_start[p_upr_index] # Smallest value in the range
      p_upr_A2 <- t_dat$bin_start[p_upr_index+1] # Smallest value in next highest range
      p_upr_C1 <- t_dat$Cummul_perc[p_upr_index-1] # Cumulative percent of units strictly less than A1
      p_upr_C2 <- t_dat$Cummul_perc[p_upr_index] # Cumulative percent of units strictly less than A2
      
      # Approximate the lower and upper bounds for a confidence interval around the median
      low_bound <- ((p_low - p_low_C1) / (p_low_C2 - p_low_C1)) * (p_low_A2 - p_low_A1) + p_low_A1
      upr_bound <- ((p_upr - p_upr_C1) / (p_upr_C2 - p_upr_C1)) * (p_upr_A2 - p_upr_A1) + p_upr_A1
      
      # Calculate standard error (SE) of the median
      se_median <- 0.5 * (upr_bound - low_bound)
      
      # Calculate the margin of error at the 90% confidence interval
      MoErr <- 1.645 * se_median
      
    } else {
      
      # Handling for the same range...
      # If same range; only need one set of A1, A2, C1, C2
      A1 <- t_dat$bin_start[p_low_index] # Smallest value in the range
      A2 <- t_dat$bin_start[p_low_index+1] # Smallest value in next highest range
      
      # Adjust for edge case where lower bound index is the first row in data
      if (p_low_index != 1){
        C1 <- t_dat$Cummul_perc[p_low_index-1] # Cumulative percent of units strictly less than A1 (nothing less than it)
      } else{
        C1 <- 0 # Set as 0 because there are no units strictly less than A1
      }
      
      C2 <- t_dat$Cummul_perc[p_low_index] # Cumulative percent of units strictly less than A2
      
      # Approximate the lower and upper bounds for a confidence interval around the median (same values plugged in for both)
      low_bound <- ((p_low - C1) / (C2 - C1)) * (A2 - A1) + A1
      upr_bound <- ((p_upr - C1) / (C2 - C1)) * (A2 - A1) + A1
      
      # Calculate standard error (SE) of the median
      se_median <- 0.5 * (upr_bound - low_bound)
      
      # Calculate the margin of error at the 90% confidence interval
      MoErr <- 1.645 * se_median
      
    }
    
    #### Combines results and appends to agg_vals ####
    new_vals <- tibble(RacEthn = t$race_ethnicity, MedianHHIncome_value = aggregate_median, MedianHHIncome_moe = MoErr)
    agg_vals <- rbind(agg_vals, new_vals)
    
  }
  
  # Creates format with naming convention of script
  agg_vals <- agg_vals %>% 
    pivot_longer(
      cols = -RacEthn,  # Exclude the race/ethnicity column from being pivoted
      names_to = "variable", 
      values_to = "value",
    ) %>%
    mutate(
      name = name,
      variable = paste(variable, RacEthn, sep = "_")  # Format names as required
    ) %>%
    select(-RacEthn) %>% 
    select(name, variable, value)
  
  return(agg_vals)
}


# Pulls the median household income data from the ACS
pull_B19001(county_codes=county_codes, year=year, census_api=census_api) 

# Approximates median household income aggregated across the counties of interest
median_HH_Incomes <- agg_aprrox(name)

# Save table as csv
write.csv(median_HH_Incomes, paste0(as.character(year), "-", name, "-B19001.csv"), row.names=FALSE)

#####################################################
#### Without Health Insurance -- S2701 -- counts ####
#####################################################

var_S2701 <- c(
  "NoHealthInsur_%_Black" = "S2701_C04_017", # Black/AA
  "NoHealthInsur_%_AmerIndian" = "S2701_C04_018", # American Indian/Alaska Native
  "NoHealthInsur_%_Asian" = "S2701_C04_019", # Asian
  "NoHealthInsur_%_PacifIslan" = "S2701_C04_020", # Native Hawaiian/Pacific Islander
  "NoHealthInsur_%_HispanLatin" = "S2701_C04_023", # Hispanic/Latino (any race)
  "NoHealthInsur_%_White" = "S2701_C04_024", # White alone (not Hispanic/Latino)
  "NoHealthInsur_%_All" = "S2701_C04_001" # All
)
var_S2701_sum <- c(
  "NoHealthInsur_%_Black" = "S2701_C01_017", # Black/AA
  "NoHealthInsur_%_AmerIndian" = "S2701_C01_018", # American Indian/Alaska Native
  "NoHealthInsur_%_Asian" = "S2701_C01_019", # Asian
  "NoHealthInsur_%_PacifIslan" = "S2701_C01_020", # Native Hawaiian/Pacific Islander
  "NoHealthInsur_%_HispanLatin" = "S2701_C01_023", # Hispanic/Latino (any race)
  "NoHealthInsur_%_White" = "S2701_C01_024", # White alone (not Hispanic/Latino)
  "NoHealthInsur_%_All" = "S2701_C01_001" # All
)

# Get ACS data
acs_data_S2701 <- get_acs(geography = "county",
                          state = "VA",
                          county = county_codes,
                          variables = var_S2701,
                          year = year, 
                          survey = "acs5",
                          key = census_api) 

acs_data_S2701_sum <-  get_acs(geography = "county",
                               state = "VA",
                               county = county_codes,
                               variables = var_S2701_sum,
                               year = year, 
                               survey = "acs5",
                               key = census_api) 

acs_data_S2701 <- acs_data_S2701 %>% 
  left_join(acs_data_S2701_sum %>% rename(summary_est = estimate,
                                          summary_moe = moe),
            by = c("GEOID", "NAME", "variable"))

# Create summary variables for the combined counties
# Run for single locality, will lead to no changes in the data table
acs_data_S2701_summarize <- acs_data_S2701 %>% 
  group_by(variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate),
            sum_all = sum(summary_est))

# Create percentages from estimates
acs_data_S2701_summarize <- acs_data_S2701_summarize %>% 
  mutate(value = round(((sum_est / sum_all) * 100), digits = 2),
         name = name) %>% 
  select(name, variable, value)

# Save table as csv
write.csv(acs_data_S2701_summarize, paste0(as.character(year), "-", name, "-S2701.csv"), row.names=FALSE)

#####################################################
#### Children Without Health Insurance -- B27001 ####
#####################################################
# Use tables B27001A-I tables for age/race
# https://censusreporter.org/tables/B27001/
# This process will be similar to "Race and Ethnicity of Children % -- B01001A-I tables" above 
# These are ACS 1-yr surveys, will need to adjust get_acs()

# Variable view helper for ACS 1-year surveys
#acs_var <- load_variables(year, "acs1", cache = TRUE)
#view(acs_var)

############
# PROBLEM: when pulling data, received NAs for all estimate and moe; warning stated "The 1-year ACS provides data for geographies with populations of 65,000 and greater."
############

# MPC NOTE: yes, on data.census.gov it looks like for
# B27001B - both values are 0
# B27001C-B27001E - no results are found
# B27001I - the first value is 0, the second is not
# B27001H - both values are non-zero and returns the estimates
# given Albemarle is the only locality for which ACS-1 estimates are available
# and that the results for all subgroups except white are either 0 or missing
# I don't think you can generate any useful information for this;
# this is, though, partly a story of success -- the ACA and expansion
# of medicaid (finally, in VA) has reduced the number of uninsured children
# low enough for to be not easily estimable
# and even among white-non-hispanic children, the estimated number 
# without insurance is exceeded by the margin of error -- these are
# very unstable estimates.

############
# DETERMINATION: cannot implement "children without health insurance" for local counties as is, can pull for just Charlottesville MSA (metropolitan statistical areas)
############

# Function for pulling children without insurance by race for charlotteville metro statistical area (MSA)
pull_B27001_cvilleMSA <- function(year, census_api, race_ethnicity = c("Black", "AmerIndian", "Asian", "PacifIslan", "HispanLatin", "White")){
  
  # Mapping letter code in census to corresponding race/ethnicity group
  racethn_map = list("Black" = "B27001B", 
                     "AmerIndian" = "B27001C", 
                     "Asian" = "B27001D", 
                     "PacifIslan" = "B27001E", 
                     "HispanLatin" = "B27001I", 
                     "White" = "B27001H")
  
  # Create empty table to hold values
  cville_B27001 <- tibble()
  
  # Gives name
  name = "Charlottesville Metro Area"
  
  # Loops through all provided race/ethnicity groups and creates mappings/pull data
  for (racethn in race_ethnicity){
    
    # Creates variable mapping for the income ranges
    range_map <- list("pop_u6" = paste0(racethn_map[racethn], "_004"), # Without health insur under 6yrs
                      "pop_6to18" = paste0(racethn_map[racethn], "_007")) # Without health insur ages 6 to 18yrs
    
    # Pulls data from acs
    racetbl <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                       variables = range_map,
                       year = year,
                       survey = "acs1", 
                       key = census_api) 
    # Select only the Charlotesville MSA
    racetbl <- racetbl %>% 
      filter(NAME == "Charlottesville, VA Metro Area") 
    
    # Group together for each 
    racetbl <- racetbl %>% 
      summarise(value = sum(estimate),
                moe = moe_sum(moe = moe, estimate = estimate)) %>% 
      mutate(name = name, variable = paste0("u18_", racethn)) %>% 
      select(name, variable, value, moe)
    
    
    # Assigns table name to pulled data for given iteration of loop
    cville_B27001 <- rbind(cville_B27001, racetbl)
  }
  
  # Returns in long format with script naming convention
  cville_B27001 <- cville_B27001 %>% 
    pivot_longer(cols = c("value", "moe"),
                 names_to = "type") %>% 
    mutate(variable = paste0("ChildNoInsur_", type, "_", variable)) %>% 
    select(-type)
  
  return(cville_B27001)
}

# For Charlottesville MSA
child_NoHealthInsur_summarize <- pull_B27001_cvilleMSA(year, census_api)

# Save table as csv
write.csv(child_NoHealthInsur_summarize, paste0(as.character(year), "-", name, "-B27001.csv"), row.names=FALSE) 

#########################################################
#### Unemployment Rate % -- B23002, changed to S2301 ####
#########################################################
# Use tables B23002A-I tables for age/race
# https://censusreporter.org/tables/B23002A/
# This process will be similar to "Race and Ethnicity of Children % -- B01001A-I tables" above 
# These are ACS 1-yr surveys, will need to adjust get_acs() (see above)
# Only have data disaggregated by race and sex; need to bring in separate and group manually

############
# PROBLEM: when pulling data, received NAs for all estimate and moe; warning stated "The 1-year ACS provides data for geographies with populations of 65,000 and greater."
############

# MPC NOTE: here again, it's only available for Albemarle and only for
# white/white-non-hispanic (tables A and H); so probably not able to
# generate the answers folks want.
# There is a Charlottesville MSA (metropolitan statistical areas) version
# available -- essentially for the whole region rather than by county
# but only tables A/B/H (Black and white)
# Actually, children without health insurance could also be pulled/provided
# for the whole MSA as well, for tables A/B/D/H (again, some pretty 
# large moe's relative to the estimates for several of these)

############
# DETERMINATION: cannot implement "unemployment rate %" from the B23002, instead use Table S2301 to pull the % labor participation rate by race along with the total civilian population aged 16-64 by race
############

# Create function to generate unemployed counts based on labor participation rate estimates and total pop counts
calculate_unemp_counts <- function(labor_rate, total_counts){
  
  # Calculate estimated employed count
  employed_count = round(labor_rate * total_counts)
  
  # Calculate estimated unemployed count
  unemployed_counts = total_counts - employed_count
  
  return(unemployed_counts)
}

# Create function to generate unemployed moe counts based on labor participation rate moe and total pop counts
calculate_moe_counts <- function(labor_moe, total_counts){
  
  # Calculate estimated count moe
  moe_count = round((labor_moe/100) * total_counts)
  
  return(moe_count)
}

# Function for pulling all-age unemployment counts for the given race/ethnicity(s)
pull_S2301 <- function(county_codes, year, census_api, name){
  
  # Mapping letter code in census for labor participation rate estimate to corresponding race/ethnicity group
  racethn_laborpartic_map = list("Black" = "S2301_C02_013", 
                                 "AmerIndian" = "S2301_C02_014", 
                                 "Asian" = "S2301_C02_015", 
                                 "PacifIslan" = "S2301_C02_016", 
                                 "HispanLatin" = "S2301_C02_019", 
                                 "White" = "S2301_C02_020")
  
  # Mapping letter code in census for total estimate to corresponding race/ethnicity group
  racethn_totals_map = list("Black" = "S2301_C01_013", 
                            "AmerIndian" = "S2301_C01_014", 
                            "Asian" = "S2301_C01_015", 
                            "PacifIslan" = "S2301_C02_016", 
                            "HispanLatin" = "S2301_C01_019", 
                            "White" = "S2301_C01_020")

  # Pulls data from acs
  # For labor participation rate
  laborpartic_tbl <- get_acs(geography = "county",
                             state = "VA",
                             county = county_codes,
                             variables = racethn_laborpartic_map,
                             year = year,
                             survey = "acs5", 
                             key = census_api)
  # For totals
  totals_tbl <- get_acs(geography = "county",
                        state = "VA",
                        county = county_codes,
                        variables = racethn_totals_map,
                        year = year,
                        survey = "acs5", 
                        key = census_api)
  
  # Joins the two tables together, providing both labor participation rate (relative prop.) and totals (absolute count)
  joined_data <- inner_join(laborpartic_tbl, 
                            totals_tbl %>% select(-GEOID), 
                            by=c("NAME", "variable"),
                            suffix = c("_laborpartic", "_totals"))
  
  # Takes joined table and calculates aggregated unemployed % by race using totals/rates
  final_data <- joined_data %>%
    # Provides the 
    mutate(unemployed_count = calculate_unemp_counts(labor_rate = estimate_laborpartic, total_counts = estimate_totals),
           unemployed_moe = calculate_moe_counts(labor_moe = moe_laborpartic, total_counts = estimate_totals)) %>% 
    # Select variables of interest
    select(GEOID, NAME, variable, unemployed_count, unemployed_moe, estimate_totals) %>% 
    # Groups by race across counties
    group_by(variable) %>% 
    # Sums up unemployment counts and moe
    summarize(sum_est = sum(unemployed_count),
              sum_totals = sum(estimate_totals),
              sum_moe = moe_sum(moe = unemployed_moe, estimate = unemployed_count)) %>% 
    # Converts back to percentage, now aggregated
    mutate(name = name,
           variable = paste0("Unemploy_%_", variable),
           value = round(sum_est/sum_totals, 4)*100,
           moe = round(sum_moe/sum_totals, 4)*100) %>% 
    # Selects only columns of interest, matching the naming convention for tables in script
    select(name, variable, value)
  
  return(final_data)
}

unemp_race_summarize <- pull_S2301(county_codes=county_codes, year=year, census_api=census_api, name=name)

# Save table as csv
write.csv(unemp_race_summarize, paste0(as.character(year), "-", name, "-S2301.csv"), row.names=FALSE)

####################################################
#### Homeownership % -- S2502 ####
####################################################
# process is similar to "Race and Ethnicity" and "Poverty Rates" above
# be sure to use count estimates to derive percents, for Owner-occupied housing unit

var_S2502 <- c(
  "Homeownership_%_Black" = "S2502_C03_003", # Black/AA
  "Homeownership_%_AmerIndian" = "S2502_C03_004", # American Indian/Alaska Native
  "Homeownership_%_Asian" = "S2502_C03_005", #Asian
  "Homeownership_%_PacifIslan" = "S2502_C03_006", # Native Hawaiian/Pacific Islander
  "Homeownership_%_HispanLatin" = "S2502_C03_009", # Hispanic/Latino (any race)
  "Homeownership_%_White" = "S2502_C03_010", # White alone (not Hispanic/Latino)
  "Homeownership_%_All" = "S2502_C03_001" # All
)
var_S2502_sum <- c(
  "Homeownership_%_Black" = "S2502_C01_003", # Black/AA
  "Homeownership_%_AmerIndian" = "S2502_C01_004", # American Indian/Alaska Native
  "Homeownership_%_Asian" = "S2502_C01_005", # Asian
  "Homeownership_%_PacifIslan" = "S2502_C01_006", # Native Hawaiian/Pacific Islander
  "Homeownership_%_HispanLatin" = "S2502_C01_009", # Hispanic/Latino (any race)
  "Homeownership_%_White" = "S2502_C01_010", # White alone (not Hispanic/Latino)
  "Homeownership_%_All" = "S2502_C01_001" # All
)

# Get ACS data
acs_data_S2502 <- get_acs(geography = "county",
                          state = "VA",
                          county = county_codes,
                          variables = var_S2502,
                          year = year,
                          survey = "acs5",
                          key = census_api)

acs_data_S2502_sum <-  get_acs(geography = "county",
                               state = "VA",
                               county = county_codes,
                               variables = var_S2502_sum,
                               year = year, 
                               survey = "acs5",
                               key = census_api) 

acs_data_S2502 <- acs_data_S2502 %>% 
  left_join(acs_data_S2502_sum %>% rename(summary_est = estimate,
                                          summary_moe = moe),
            by = c("GEOID", "NAME", "variable"))

# Create summary variables for the combined counties
# Run for single locality, will lead to no changes in the data table
acs_data_S2502_summarize <- acs_data_S2502 %>%
  group_by(variable) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate),
            sum_all = sum(summary_est)) 

# Create percentages frome estimates
acs_data_S2502_summarize <- acs_data_S2502_summarize %>%
  mutate(value = round(((sum_est / sum_all) * 100), digits = 2),
         name = name) %>%
  select(name, variable, value)

# Save tale as csv
write.csv(acs_data_S2502_summarize, paste0(as.character(year), "-", name, "-S2502.csv"), row.names=FALSE)

###################################
#### Combine tables if desired ####
###################################

# If tables are long
acs_data_combined <- rbind(acs_data_DP05_summarize, pop_child_race_summarize, acs_data_S1701_summarize, median_HH_Incomes, 
                           acs_data_S2701_summarize, child_NoHealthInsur_summarize, unemp_race_summarize, 
                           acs_data_S2502_summarize)
  
# Save table as csv
write.csv(acs_data_combined, paste0(as.character(year), "-", "acs-data-combined.csv"), row.names=FALSE)
