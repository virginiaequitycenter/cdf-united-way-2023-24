## Tidycensus code for United Way Data

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
  "RaceEtnic_%_Black" = "DP05_0038", # Black/AA
  "RaceEtnic_%_AmerIndian" = "DP05_0039", # American Indian/Alaska Native
  "RaceEtnic_%_Asian" = "DP05_0044", # Asian
  "RaceEtnic_%_PacifIslan" = "DP05_0052", # Native Hawaiian/Pacific Islander
  "RaceEtnic_%_HispanLatin" = "DP05_0073", # Hispanic/Latino (any race)
  "RaceEtnic_%_White" = "DP05_0079" # White alone (not Hispanic/Latino)
)

# Get ACS data
acs_data_DP05 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_codes,
                    variables = var_DP05,
                    summary_var = "DP05_0033", # this provides the total (summary table) we need for creating percents
                    year = year, 
                    survey = "acs5",
                    key = census_api) 

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

###################################################
#### Race and Ethnicity of Children % -- B01001A-I tables ####
###################################################
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
  mutate(race_ethn = "black_u18")

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
  mutate(race_ethn = "aian_u18")

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
  mutate(race_ethn = "asian_u18")

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
  mutate(race_ethn = "nhpi_u18")

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
  mutate(race_ethn = "hisp_u18")

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
  mutate(race_ethn = "white_nh_u18")

# Combine child_pop_race ----
pop_child_race <- bind_rows(pop_child_black, pop_child_aian, 
                            pop_child_asian, pop_child_nhpi, 
                            pop_child_hisp, pop_child_nhwhite) 

pop_child_race <- pop_child_race %>% 
  group_by(GEOID, NAME) %>% 
  mutate(total_count = sum(sum_est))

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
         variable = race_ethn) %>% 
  select(name, variable, value)

# Save table as csv
write.csv(pop_child_race_summarize, paste0(as.character(year), "-", name, "-B01001.csv"), row.names=FALSE)


################################
#### Poverty Rates -- S1701 ####
################################

var_S1701 <- list(
  "Poverty_%_Black" = "S1701_C02_014", # Black/AA
  "Poverty_%_AmerIndian" = "S1701_C02_015", # American Indian/Alaska Native
  "Poverty_%_Asian" = "S1701_C02_016", # Asian
  "Poverty_%_PacifIslan" = "S1701_C02_017", # Native Hawaiian/Pacific Islander
  "Poverty_%_HispanLatin" = "S1701_C02_020", # Hispanic/Latino (any race)
  "Poverty_%_White" = "S1701_C02_021", # White alone (not Hispanic/Latino)
  "Poverty_%_All" = "S1701_C02_001" # All
)

# Get ACS data
acs_data_S1701 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_codes,
                    variables = var_S1701,
                    summary_var = "S0901_C01_001", # this provides the total (summary table) we need for creating percents
                    year = year, 
                    survey = "acs5",
                    key = census_api) 

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

##########################################
#### Median Household Income -- S1903 ####
##########################################
## Needs more complex computations for accurate combined values
## WILL SKIP FOR NOW

####################################################
#### Without Health Insurance -- S2701 -- counts ####
####################################################
var_S2701 <- list(
  "NoHealthInsur_%_Black" = "S2701_C04_017", # Black/AA
  "NoHealthInsur_%_AmerIndian" = "S2701_C04_018", # American Indian/Alaska Native
  "NoHealthInsur_%_Asian" = "S2701_C04_019", # Asian
  "NoHealthInsur_%_PacifIslan" = "S2701_C04_020", # Native Hawaiian/Pacific Islander
  "NoHealthInsur_%_HispanLatin" = "S2701_C04_023", # Hispanic/Latino (any race)
  "NoHealthInsur_%_White" = "S2701_C04_024", # White alone (not Hispanic/Latino)
  "NoHealthInsur_%_All" = "S2701_C04_001" # All
)
# Get ACS data
acs_data_S2701 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_codes,
                    variables = var_S2701,
                    summary_var = "S2701_C01_001", # Estimate!!Total!!Civilian noninstitutionalized population
                    year = year, 
                    survey = "acs5",
                    key = census_api) 

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

###################################
#### Combine tables if desired ####
###################################

# If tables are long
acs_data_combined <- rbind(acs_data_DP05_summarize, pop_child_race_summarize, acs_data_S1701_summarize, acs_data_S2701_summarize)
  
# Save table as csv
write.csv(acs_data_combined, paste0(as.character(year), "-", "acs-data-combined.csv"), row.names=FALSE)
