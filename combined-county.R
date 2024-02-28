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
county_codes <- c("003", "540") # county FIPS codes desired
name <- "Charlottesville-Albermarle" # name of county or combined region

# Variable view helper
# all_acs_meta <- function(){
#   # Gets the list of all variables from all acs5 metadata tables
#   vars1 <- load_variables(year, "acs5") %>% select(-geography) # Remove the geography column
#   vars2 <- load_variables(year, "acs5/profile")
#   vars3 <- load_variables(year, "acs5/subject")
#   vars4 <- load_variables(year, "acs5/cprofile")
#   
#   # Provides column with specific lookup
#   vars1$dataset_table <- "acs5"
#   vars2$dataset_table  <- "acs5/profile"
#   vars3$dataset_table  <- "acs5/subject"
#   vars4$dataset_table  <- "acs5/cprofile"
#   
#   # Combine all table rows
#   all_vars_meta <- rbind(vars1, vars2, vars3, vars4)
#   
#   return(all_vars_meta)
# }
# 
# # Pull all variable names from metadata
# metadata_var <- all_acs_meta()
# 
# # View acs metadata tables
# view(metadata_var)

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

# When tables have COUNTS use below
# Create summary variables for the combined counties
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

# Pivot table (if desired)
# acs_data_DP05_summarize <- acs_data_DP05_summarize %>% 
#   pivot_wider(names_from = variable, values_from = value)

# Save table as csv
write.csv(acs_data_DP05_summarize, paste0(as.character(year), "-", name, "-DP05.csv"), row.names=FALSE)

###################################################
#### Race and Ethnicity of Children % -- S0901 ####
###################################################
# These tables only have percents - 
# We should search for the corresponding B Table with count values - cannot accutately aggregate with just percents 
var_S0901 <- list(
  "RaceEthnicChild_%_Black" = "S0901_C01_007", # Black/AA
  "RaceEthnicChild_%_AmerIndian" = "S0901_C01_008", # American Indian/Alaska Native
  "RaceEthnicChild_%_Asian" = "S0901_C01_009", # Asian
  "RaceEthnicChild_%_PacifIslan" = "S0901_C01_010", # Native Hawaiian/Pacific Islander
  "RaceEthnicChild_%_HispanLatin" = "S0901_C01_013", # Hispanic/Latino (any race)
  "RaceEthnicChild_%_White" = "S0901_C01_014" # White alone (not Hispanic/Latino)
)

# Get ACS data
acs_data_S0901 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_codes,
                    variables = var_S0901,
                    summary_var = "S0901_C01_001", # this provides the total (summary table) we need for creating percents
                    year = year, 
                    survey = "acs5",
                    key = census_api) 

# When tables have PERCENTS use below
acs_data_S0901_summarize <- acs_data_S0901 %>% 
  mutate(estimate_count = (estimate / 100) * summary_est,
         moe_count = (moe / 100) * summary_est)

# Create summary variables for the combined counties
acs_data_S0901_summarize <- acs_data_S0901_summarize %>% 
  group_by(variable) %>% 
  summarize(sum_est = sum(estimate_count),
            sum_moe = moe_sum(moe = moe_count, estimate = estimate_count),
            sum_all = sum(summary_est))

# Create percentages from estimates
acs_data_S0901_summarize <- acs_data_S0901_summarize %>% 
  mutate(value = round(((sum_est / sum_all) * 100), digits = 2),
         name = name) %>% 
  select(name, variable, value)

# Pivot table (if desired)
# acs_data_S0901_summarize <- acs_data_S0901_summarize %>% 
#   pivot_wider(names_from = variable, values_from = value)

# Save table as csv
write.csv(acs_data_S0901_summarize, paste0(as.character(year), "-", name, "-S0901.csv"), row.names=FALSE)

################################
#### Poverty Rates -- S1701 ####
################################
# these variables are counts
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

# When tables have COUNTS use below
# Create summary variables for the combined counties
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

# Pivot table (if desired)
# acs_data_S1701_summarize <- acs_data_S1701_summarize %>% 
#   pivot_wider(names_from = variable, values_from = value)

# Save table as csv
write.csv(acs_data_S1701_summarize, paste0(as.character(year), "-", name, "-S1701.csv"), row.names=FALSE)

##########################################
#### Median Household Income -- S1903 ####
##########################################
## Needs more complex computations for accurate combined values

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

# When tables have COUNTS use below
# Create summary variables for the combined counties
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

# Pivot table (if desired)
# acs_data_S2701_summarize <- acs_S2701_data_summarize %>% 
#   pivot_wider(names_from = variable, values_from = value)

# Save table as csv
write.csv(acs_data_S2701_summarize, paste0(as.character(year), "-", name, "-S2701.csv"), row.names=FALSE)


###################################
#### Combine tables if desired ####
###################################

# If tables are long (no pivot_wider)
acs_data_combined <- rbind(acs_data_DP05_summarize, acs_data_S0901_summarize, acs_data_S1701_summarize, acs_data_S2701_summarize)

# If tables are wide (yes pivot_wider)
# acs_data_combined <- acs_data_DP05_summarize %>% 
#   left_join(acs_data_S0901_summarize) %>% 
#   left_join(acs_data_S1701_summarize) %>%
#   left_join(acs_data_S2701_summarize)
  
# Save table as csv
write.csv(acs_data_combined, paste0(as.character(year), "-", "acs-data-combined.csv"), row.names=FALSE)
