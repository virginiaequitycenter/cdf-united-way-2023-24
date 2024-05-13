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

# Need to be reconsidered (See totals from B01001 tables)
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

####################################################
#### Children Without Health Insurance -- B27001 ####
####################################################
# Use tables B27001A-I tables for age/race
# https://censusreporter.org/tables/B27001/
# This process will be similar to "Race and Ethnicity of Children % -- B01001A-I tables" above 
# These are ACS 1-yr surveys, will need to adjust get_acs()

# Variable view helper for ACS 1-year surveys
acs_var <- load_variables(year, "acs1", cache = TRUE)
view(acs_var)

############
# PROBLEM: when pulling data, received NAs for all estimate and moe; warning stated "The 1-year ACS provides data for geographies with populations of 65,000 and greater."
############


## Black/AA
# -----------------------------------------------------------------------

vars_B27001B <- c(pop_u6 = "B27001B_004", # Total under 6yrs
                  pop_6to18 = "B27001B_007") # Total ages 6 to 18yrs

insur_child_black <- get_acs(geography = "county",
                           state = "51", 
                           county = county_codes,
                           var = vars_B27001B,
                           year = year,
                           survey = "acs1",
                           key = census_api)

insur_child_black <- insur_child_black %>%
  group_by(GEOID, NAME) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>%
  mutate(NoHealthInsur = "black_u18") 



## American Indian/Alaska Native
# -----------------------------------------------------------------------

vars_B27001C <- c(pop_u6 = "B27001C_004", # Total under 6yrs
                  pop_6to18 = "B27001C_004") # Total ages 6 to 18yrs

insur_child_aian <- get_acs(geography = "county",
                          state = "51", 
                          county = county_codes,
                          var = vars_B27001C,
                          year = year,
                          survey = "acs1",
                          key = census_api)

insur_child_aian <- insur_child_aian %>%
  group_by(GEOID, NAME) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>%
  mutate(NoHealthInsur = "aian_u18")



## Asian
# -----------------------------------------------------------------------

vars_B27001D <- c(pop_u6 = "B27001D_003", # Total under 6yrs
                  pop_6to18 = "B27001D_007") # Total ages 6 to 18yrs

insur_child_asian <- get_acs(geography = "county",
                           state = "51", 
                           county = county_codes,
                           var = vars_B27001D,
                           year = year,
                           survey = "acs1",
                           key = census_api)

insur_child_asian <- insur_child_asian %>%
  group_by(GEOID, NAME) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>%
  mutate(NoHealthInsur = "asian_u18")



## Native Hawaiian/Pacific Islander
# -----------------------------------------------------------------------

vars_B27001E <- c(pop_u6 = "B27001E_003", # Total under 6yrs
                  pop_6to18 = "B27001E_007") # Total ages 6 to 18yrs

insur_child_nhpi <- get_acs(geography = "county",
                          state = "51", 
                          county = county_codes,
                          var = vars_B27001E,
                          year = year,
                          survey = "acs1",
                          key = census_api)

insur_child_nhpi <- insur_child_nhpi %>%
  group_by(GEOID, NAME) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>%
  mutate(NoHealthInsur = "nhpi_u18")



## Hispanic/Latino (any race)
# -----------------------------------------------------------------------

vars_B27001I <- c(pop_u6 = "B27001I_003", # Total under 6yrs
                  pop_6to18 = "B27001I_007") # Total ages 6 to 18yrs

insur_child_hisp <- get_acs(geography = "county",
                          state = "51", 
                          county = county_codes,
                          var = vars_B27001I,
                          year = year,
                          survey = "acs1",
                          key = census_api)

insur_child_hisp <- insur_child_hisp %>%
  group_by(GEOID, NAME) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>%
  mutate(NoHealthInsur = "hisp_u18")



## White alone (not Hispanic/Latino)
# -----------------------------------------------------------------------
vars_B27001H <- c(pop_u6 = "B27001H_003", # Total under 6yrs
                  pop_6to18 = "B27001H_007") # Total ages 6 to 18yrs

insur_child_nhwhite <- get_acs(geography = "county",
                             state = "51", 
                             county = county_codes,
                             var = vars_B27001H,
                             year = year,
                             survey = "acs1",
                             key = census_api)

insur_child_nhwhite <- insur_child_nhwhite %>%
  group_by(GEOID, NAME) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>%
  mutate(NoHealthInsur = "white_nh_u18")



## Combine child_pop_NoHealthInsur
# -----------------------------------------------------------------------

child_NoHealthInsur <- bind_rows(insur_child_black, insur_child_aian,
                                     insur_child_asian, insur_child_nhpi,
                                     insur_child_hisp, insur_child_nhwhite)

child_NoHealthInsur <- child_NoHealthInsur %>%
  group_by(GEOID, NAME) %>%
  mutate(total_count = sum(sum_est))

# Create summary variables for the combined counties
# Run for single locality, will lead to no changes in the data table
child_NoHealthInsur_summarize <- child_NoHealthInsur %>%
  group_by(NoHealthInsur) %>%
  summarize(sum_est = sum(sum_est),
            sum_moe = moe_sum(moe = sum_moe, estimate = sum_est),
            sum_all = sum(total_count))

# Create percentages from estimates
child_NoHealthInsur_summarize <- child_NoHealthInsur_summarize %>%
  mutate(value = round(((sum_est / sum_all) * 100), digits = 2),
         name = name,
         variable = NoHealthInsur) %>%
  select(name, variable, value)

# Save table as csv
write.csv(child_NoHealthInsur_summarize, paste0(as.character(year), "-", name, "-B27001.csv"), row.names=FALSE) 


####################################################
#### Unemployment Rate % -- B23002 ####
####################################################
# Use tables B23002A-I tables for age/race
# https://censusreporter.org/tables/B23002A/
# This process will be similar to "Race and Ethnicity of Children % -- B01001A-I tables" above 
# These are ACS 1-yr surveys, will need to adjust get_acs() (see above)

# Only have data disaggregated by race and sex; need to bring in separate and group manually

############
# QUESTION: are we including non-civilian? These counts (employed and totals) are only civilian and those in labor force
############
############
# PROBLEM: when pulling data, received NAs for all estimate and moe; warning stated "The 1-year ACS provides data for geographies with populations of 65,000 and greater."
############


## Black/AA
# -----------------------------------------------------------------------

# Gets unemployment counts (civilian only/labor force only)
vars_B23002B <- c(unempM_16to19 = "B23002B_008", # Unemployed males aged 16-19 yrs 
                  unempM_20to24 = "B23002B_015", # Unemployed males aged 20-24 yrs
                  unempM_25to54 = "B23002B_022", # Unemployed males aged 25-54 yrs
                  unempM_55to64 = "B23002B_029", # Unemployed males aged 55-64 yrs
                  unempM_65to69 = "B23002B_034", # Unemployed males aged 65-69 yrs
                  unempM_70plus = "B23002B_039", # Unemployed males aged 70+ yrs
                  unempF_16to19 = "B23002B_047", # Unemployed female aged 16-19 yrs 
                  unempF_20to24 = "B23002B_054", # Unemployed female aged 20-24 yrs
                  unempF_25to54 = "B23002B_061", # Unemployed female aged 25-54 yrs
                  unempF_55to64 = "B23002B_068", # Unemployed female aged 55-64 yrs
                  unempF_65to69 = "B23002B_073", # Unemployed female aged 65-69 yrs
                  unempF_70plus = "B23002B_078" # Unemployed female aged 70+ yrs
                  )

# Gets total counts (civilian only/labor force only)
totals_B23002B <- c(totalM_16to19 = "B23002B_006", # Male aged 16-19 yrs 
                    totalM_20to24 = "B23002B_013", # Male aged 20-24 yrs
                    totalM_25to54 = "B23002B_020", # Male aged 25-54 yrs
                    totalM_55to64 = "B23002B_027", # Male aged 55-64 yrs
                    totalM_65to69 = "B23002B_032", # Male aged 65-69 yrs
                    totalM_70plus = "B23002B_037", # Male aged 70+ yrs
                    totalF_16to19 = "B23002B_045", # Female aged 16-19 yrs 
                    totalF_20to24 = "B23002B_052", # Female aged 20-24 yrs
                    totalF_25to54 = "B23002B_059", # Female aged 25-54 yrs
                    totalF_55to64 = "B23002B_066", # Female aged 55-64 yrs
                    totalF_65to69 = "B23002B_071", # Female aged 65-69 yrs
                    totalF_70plus = "B23002B_076" # Female aged 70+ yrs
                    )
  
# Pulls unemployment counts from the ACS
unemp_black <- get_acs(geography = "county",
                           state = "51",
                           county = county_codes,
                           var = vars_B23002B,
                           year = year,
                           survey = "acs1")

# Pulls total counts from the ACS
total_black <- get_acs(geography = "county",
                       state = "51",
                       county = county_codes,
                       var = totals_B23002B,
                       year = year,
                       survey = "acs1")

# Groups sex categories to form values for Black/AA unemployment across age
unemp_black <- unemp_black %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>% 
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("black_", variable)) %>% 
  select(-variable)

# Groups sex categories to form values for Black/AA totals across age
total_black <- total_black %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>%
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("black_", variable)) %>% 
  select(-variable)

# Combines totals and unemp into single table
unemp_total_black <- rbind(unemp_black,total_black)



## American Indian/Alaska Native
# -----------------------------------------------------------------------

# Gets unemployment counts (civilian only/labor force only)
vars_B23002C <- c(unempM_16to19 = "B23002C_008", # Unemployed males aged 16-19 yrs 
                  unempM_20to24 = "B23002C_015", # Unemployed males aged 20-24 yrs
                  unempM_25to54 = "B23002C_022", # Unemployed males aged 25-54 yrs
                  unempM_55to64 = "B23002C_029", # Unemployed males aged 55-64 yrs
                  unempM_65to69 = "B23002C_034", # Unemployed males aged 65-69 yrs
                  unempM_70plus = "B23002C_039", # Unemployed males aged 70+ yrs
                  unempF_16to19 = "B23002C_047", # Unemployed female aged 16-19 yrs 
                  unempF_20to24 = "B23002C_054", # Unemployed female aged 20-24 yrs
                  unempF_25to54 = "B23002C_061", # Unemployed female aged 25-54 yrs
                  unempF_55to64 = "B23002C_068", # Unemployed female aged 55-64 yrs
                  unempF_65to69 = "B23002C_073", # Unemployed female aged 65-69 yrs
                  unempF_70plus = "B23002C_078" # Unemployed female aged 70+ yrs
)

# Gets total counts (civilian only/labor force only)
totals_B23002C <- c(totalM_16to19 = "B23002C_006", # Male aged 16-19 yrs 
                    totalM_20to24 = "B23002C_013", # Male aged 20-24 yrs
                    totalM_25to54 = "B23002C_020", # Male aged 25-54 yrs
                    totalM_55to64 = "B23002C_027", # Male aged 55-64 yrs
                    totalM_65to69 = "B23002C_032", # Male aged 65-69 yrs
                    totalM_70plus = "B23002C_037", # Male aged 70+ yrs
                    totalF_16to19 = "B23002C_045", # Female aged 16-19 yrs 
                    totalF_20to24 = "B23002C_052", # Female aged 20-24 yrs
                    totalF_25to54 = "B23002C_059", # Female aged 25-54 yrs
                    totalF_55to64 = "B23002C_066", # Female aged 55-64 yrs
                    totalF_65to69 = "B23002C_071", # Female aged 65-69 yrs
                    totalF_70plus = "B23002C_076" # Female aged 70+ yrs
)

# Pulls unemployment counts from the ACS
unemp_aian <- get_acs(geography = "county",
                       state = "51",
                       county = county_codes,
                       var = vars_B23002C,
                       year = year,
                       survey = "acs1")

# Pulls total counts from the ACS
total_aian <- get_acs(geography = "county",
                             state = "51",
                             county = county_codes,
                             var = totals_B23002C,
                             year = year,
                             survey = "acs1")

# Groups sex categories to form values for aian unemployment across age
unemp_aian <- unemp_aian %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>% 
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("aian_", variable)) %>% 
  select(-variable)

# Groups sex categories to form values for aian totals across age
total_aian <- total_aian %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>%
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("aian_", variable)) %>% 
  select(-variable)

# Combines totals and unemployed into single table
unemp_total_aian <- rbind(unemp_aian,total_aian)



## Asian
# -----------------------------------------------------------------------

# Gets unemployment counts (civilian only/labor force only)
vars_B23002D <- c(unempM_16to19 = "B23002D_008", # Unemployed males aged 16-19 yrs 
                  unempM_20to24 = "B23002D_015", # Unemployed males aged 20-24 yrs
                  unempM_25to54 = "B23002D_022", # Unemployed males aged 25-54 yrs
                  unempM_55to64 = "B23002D_029", # Unemployed males aged 55-64 yrs
                  unempM_65to69 = "B23002D_034", # Unemployed males aged 65-69 yrs
                  unempM_70plus = "B23002D_039", # Unemployed males aged 70+ yrs
                  unempF_16to19 = "B23002D_047", # Unemployed female aged 16-19 yrs 
                  unempF_20to24 = "B23002D_054", # Unemployed female aged 20-24 yrs
                  unempF_25to54 = "B23002D_061", # Unemployed female aged 25-54 yrs
                  unempF_55to64 = "B23002D_068", # Unemployed female aged 55-64 yrs
                  unempF_65to69 = "B23002D_073", # Unemployed female aged 65-69 yrs
                  unempF_70plus = "B23002D_078" # Unemployed female aged 70+ yrs
)

# Gets total counts (civilian only/labor force only)
totals_B23002D <- c(totalM_16to19 = "B23002D_006", # Male aged 16-19 yrs 
                    totalM_20to24 = "B23002D_013", # Male aged 20-24 yrs
                    totalM_25to54 = "B23002D_020", # Male aged 25-54 yrs
                    totalM_55to64 = "B23002D_027", # Male aged 55-64 yrs
                    totalM_65to69 = "B23002D_032", # Male aged 65-69 yrs
                    totalM_70plus = "B23002D_037", # Male aged 70+ yrs
                    totalF_16to19 = "B23002D_045", # Female aged 16-19 yrs 
                    totalF_20to24 = "B23002D_052", # Female aged 20-24 yrs
                    totalF_25to54 = "B23002D_059", # Female aged 25-54 yrs
                    totalF_55to64 = "B23002D_066", # Female aged 55-64 yrs
                    totalF_65to69 = "B23002D_071", # Female aged 65-69 yrs
                    totalF_70plus = "B23002D_076" # Female aged 70+ yrs
)

# Pulls unemployment counts from the ACS
unemp_asian <- get_acs(geography = "county",
                      state = "51",
                      county = county_codes,
                      var = vars_B23002D,
                      year = year,
                      survey = "acs1")

# Pulls total counts from the ACS
total_asian <- get_acs(geography = "county",
                            state = "51",
                            county = county_codes,
                            var = totals_B23002D,
                            year = year,
                            survey = "acs1")

# Groups sex categories to form values for asian unemployment across age
unemp_asian <- unemp_asian %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>% 
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("asian_", variable)) %>% 
  select(-variable)

# Groups sex categories to form values for asian totals across age
total_asian <- total_asian %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>%
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("asian_", variable)) %>% 
  select(-variable)

# Combines totals and unemployed into single table
unemp_total_asian <- rbind(unemp_asian,total_asian)



## Native Hawaiian/Pacific Islander
# -----------------------------------------------------------------------

# Gets unemployment counts (civilian only/labor force only)
vars_B23002E <- c(unempM_16to19 = "B23002E_008", # Unemployed males aged 16-19 yrs 
                  unempM_20to24 = "B23002E_015", # Unemployed males aged 20-24 yrs
                  unempM_25to54 = "B23002E_022", # Unemployed males aged 25-54 yrs
                  unempM_55to64 = "B23002E_029", # Unemployed males aged 55-64 yrs
                  unempM_65to69 = "B23002E_034", # Unemployed males aged 65-69 yrs
                  unempM_70plus = "B23002E_039", # Unemployed males aged 70+ yrs
                  unempF_16to19 = "B23002E_047", # Unemployed female aged 16-19 yrs 
                  unempF_20to24 = "B23002E_054", # Unemployed female aged 20-24 yrs
                  unempF_25to54 = "B23002E_061", # Unemployed female aged 25-54 yrs
                  unempF_55to64 = "B23002E_068", # Unemployed female aged 55-64 yrs
                  unempF_65to69 = "B23002E_073", # Unemployed female aged 65-69 yrs
                  unempF_70plus = "B23002E_078" # Unemployed female aged 70+ yrs
)

# Gets total counts (civilian only/labor force only)
totals_B23002E <- c(totalM_16to19 = "B23002E_006", # Male aged 16-19 yrs 
                    totalM_20to24 = "B23002E_013", # Male aged 20-24 yrs
                    totalM_25to54 = "B23002E_020", # Male aged 25-54 yrs
                    totalM_55to64 = "B23002E_027", # Male aged 55-64 yrs
                    totalM_65to69 = "B23002E_032", # Male aged 65-69 yrs
                    totalM_70plus = "B23002E_037", # Male aged 70+ yrs
                    totalF_16to19 = "B23002E_045", # Female aged 16-19 yrs 
                    totalF_20to24 = "B23002E_052", # Female aged 20-24 yrs
                    totalF_25to54 = "B23002E_059", # Female aged 25-54 yrs
                    totalF_55to64 = "B23002E_066", # Female aged 55-64 yrs
                    totalF_65to69 = "B23002E_071", # Female aged 65-69 yrs
                    totalF_70plus = "B23002E_076" # Female aged 70+ yrs
)

# Pulls unemployment counts from the ACS
unemp_nhpi <- get_acs(geography = "county",
                       state = "51",
                       county = county_codes,
                       var = vars_B23002E,
                       year = year,
                       survey = "acs1")

# Pulls total counts from the ACS
total_nhpi <- get_acs(geography = "county",
                             state = "51",
                             county = county_codes,
                             var = totals_B23002E,
                             year = year,
                             survey = "acs1")

# Groups sex categories to form values for nhpi unemployment across age
unemp_nhpi <- unemp_nhpi %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>% 
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("nhpi_", variable)) %>% 
  select(-variable)

# Groups sex categories to form values for nhpi totals across age
total_nhpi <- total_nhpi %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>%
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("nhpi_", variable)) %>% 
  select(-variable)

# Combines totals and unemployed into single table
unemp_total_nhpi <- rbind(unemp_nhpi,total_nhpi)



## Hispanic/Latino (any race)
# -----------------------------------------------------------------------

# Gets unemployment counts (civilian only/labor force only)
vars_B23002I <- c(unempM_16to19 = "B23002I_008", # Unemployed males aged 16-19 yrs 
                  unempM_20to24 = "B23002I_015", # Unemployed males aged 20-24 yrs
                  unempM_25to54 = "B23002I_022", # Unemployed males aged 25-54 yrs
                  unempM_55to64 = "B23002I_029", # Unemployed males aged 55-64 yrs
                  unempM_65to69 = "B23002I_034", # Unemployed males aged 65-69 yrs
                  unempM_70plus = "B23002I_039", # Unemployed males aged 70+ yrs
                  unempF_16to19 = "B23002I_047", # Unemployed female aged 16-19 yrs 
                  unempF_20to24 = "B23002I_054", # Unemployed female aged 20-24 yrs
                  unempF_25to54 = "B23002I_061", # Unemployed female aged 25-54 yrs
                  unempF_55to64 = "B23002I_068", # Unemployed female aged 55-64 yrs
                  unempF_65to69 = "B23002I_073", # Unemployed female aged 65-69 yrs
                  unempF_70plus = "B23002I_078" # Unemployed female aged 70+ yrs
)

# Gets total counts (civilian only/labor force only)
totals_B23002I <- c(totalM_16to19 = "B23002I_006", # Male aged 16-19 yrs 
                    totalM_20to24 = "B23002I_013", # Male aged 20-24 yrs
                    totalM_25to54 = "B23002I_020", # Male aged 25-54 yrs
                    totalM_55to64 = "B23002I_027", # Male aged 55-64 yrs
                    totalM_65to69 = "B23002I_032", # Male aged 65-69 yrs
                    totalM_70plus = "B23002I_037", # Male aged 70+ yrs
                    totalF_16to19 = "B23002I_045", # Female aged 16-19 yrs 
                    totalF_20to24 = "B23002I_052", # Female aged 20-24 yrs
                    totalF_25to54 = "B23002I_059", # Female aged 25-54 yrs
                    totalF_55to64 = "B23002I_066", # Female aged 55-64 yrs
                    totalF_65to69 = "B23002I_071", # Female aged 65-69 yrs
                    totalF_70plus = "B23002I_076" # Female aged 70+ yrs
)

# Pulls unemployment counts from the ACS
unemp_hisp <- get_acs(geography = "county",
                      state = "51",
                      county = county_codes,
                      var = vars_B23002I,
                      year = year,
                      survey = "acs1")

# Pulls total counts from the ACS
total_hisp <- get_acs(geography = "county",
                      state = "51",
                      county = county_codes,
                      var = totals_B23002I,
                      year = year,
                      survey = "acs1")

# Groups sex categories to form values for hisp unemployment across age
unemp_hisp <- unemp_hisp %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>% 
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("hisp_", variable))  %>% 
  select(-variable)

# Groups sex categories to form values for hisp totals across age
total_hisp <- total_hisp %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>%
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("hisp_", variable))  %>% 
  select(-variable)

# Combines totals and unemployed into single table
unemp_total_hisp <- rbind(unemp_hisp,total_hisp)



## White alone (not Hispanic/Latino)
# -----------------------------------------------------------------------

# Gets unemployment counts (civilian only/labor force only)
vars_B23002H <- c(unempM_16to19 = "B23002H_008", # Unemployed males aged 16-19 yrs 
                  unempM_20to24 = "B23002H_015", # Unemployed males aged 20-24 yrs
                  unempM_25to54 = "B23002H_022", # Unemployed males aged 25-54 yrs
                  unempM_55to64 = "B23002H_029", # Unemployed males aged 55-64 yrs
                  unempM_65to69 = "B23002H_034", # Unemployed males aged 65-69 yrs
                  unempM_70plus = "B23002H_039", # Unemployed males aged 70+ yrs
                  unempF_16to19 = "B23002H_047", # Unemployed female aged 16-19 yrs 
                  unempF_20to24 = "B23002H_054", # Unemployed female aged 20-24 yrs
                  unempF_25to54 = "B23002H_061", # Unemployed female aged 25-54 yrs
                  unempF_55to64 = "B23002H_068", # Unemployed female aged 55-64 yrs
                  unempF_65to69 = "B23002H_073", # Unemployed female aged 65-69 yrs
                  unempF_70plus = "B23002H_078" # Unemployed female aged 70+ yrs
)

# Gets total counts (civilian only/labor force only)
totals_B23002H <- c(totalM_16to19 = "B23002H_006", # Male aged 16-19 yrs 
                    totalM_20to24 = "B23002H_013", # Male aged 20-24 yrs
                    totalM_25to54 = "B23002H_020", # Male aged 25-54 yrs
                    totalM_55to64 = "B23002H_027", # Male aged 55-64 yrs
                    totalM_65to69 = "B23002H_032", # Male aged 65-69 yrs
                    totalM_70plus = "B23002H_037", # Male aged 70+ yrs
                    totalF_16to19 = "B23002H_045", # Female aged 16-19 yrs 
                    totalF_20to24 = "B23002H_052", # Female aged 20-24 yrs
                    totalF_25to54 = "B23002H_059", # Female aged 25-54 yrs
                    totalF_55to64 = "B23002H_066", # Female aged 55-64 yrs
                    totalF_65to69 = "B23002H_071", # Female aged 65-69 yrs
                    totalF_70plus = "B23002H_076" # Female aged 70+ yrs
)

# Pulls unemployment counts from the ACS
unemp_nhwhite <- get_acs(geography = "county",
                      state = "51",
                      county = county_codes,
                      var = vars_B23002H,
                      year = year,
                      survey = "acs1")

# Pulls total counts from the ACS
total_nhwhite <- get_acs(geography = "county",
                      state = "51",
                      county = county_codes,
                      var = totals_B23002H,
                      year = year,
                      survey = "acs1")

# Groups sex categories to form values for nhwhite unemployment across age
unemp_nhwhite <- unemp_nhwhite %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>% 
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("nhwhite_", variable))  %>% 
  select(-variable)

# Groups sex categories to form values for nhwhite totals across age
total_nhwhite <- total_nhwhite %>% 
  mutate(variable = str_sub(variable, 1, 5) %>% 
           paste0(str_sub(variable, 7, nchar(variable)))) %>%
  group_by(GEOID, NAME, variable) %>% 
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(race_ethn = paste0("nhwhite_", variable))  %>% 
  select(-variable)

# Combines totals and unemployed into single table
unemp_total_nhwhite <- rbind(unemp_nhwhite,total_nhwhite)



# Combine all races unemployment data into one table
# -----------------------------------------------------------------------

# Combine all races in one table
unemp_age_race <- bind_rows(unemp_total_black, unemp_total_aian, unemp_total_asian, unemp_total_nhpi, unemp_total_hisp, unemp_total_nhwhite) 

# Seperate out race_ethn and totals column and pivot wider
unemp_age_race <- unemp_age_race %>% 
  separate(col = race_ethn, 
           into = c("race_ethn", "total_flag", "age"), sep = "_") %>% # Separates out the single column containing race_ethn and total flag
  pivot_wider(
    names_from = total_flag,
    values_from = c(3, 4)
  )

# Create summary variables for the combined counties
# Run for single locality, will lead to no changes in the data table
unemp_age_race_summarize <- unemp_age_race %>% 
  group_by(race_ethn) %>% # Will group by just race_ethn; add age group if wanting grouped by both
  summarize(sum_est = sum(sum_est_unemp),
            sum_moe = moe_sum(moe = sum_moe_unemp, estimate = sum_est_unemp),
            sum_all = sum(sum_est_total))

# Create percentages from estimates
unemp_age_race_summarize <- unemp_age_race_summarize %>% 
  mutate(value = round(((sum_est / sum_all) * 100), digits = 2),
         name = name,
         variable = race_ethn) %>% 
  select(name, variable, value)

# Save table as csv
write.csv(unemp_age_race_summarize, paste0(as.character(year), "-", name, "-B23002.csv"), row.names=FALSE)


####################################################
#### Homeownership % -- S2502 ####
####################################################
# process is similar to "Race and Ethnicity" and "Poverty Rates" above
# be sure to use count estimates to derive percents, for Owner-occupied housing unit

var_S2502 <- list(
  "Homeownership_%_Black" = "S2502_C03_003", # Black/AA
  "Homeownership_%_AmerIndian" = "S2502_C03_004", # American Indian/Alaska Native
  "Homeownership_%_Asian" = "S2502_C03_005", #Asian
  "Homeownership_%_PacifIslan" = "S2502_C03_006", # Native Hawaiian/Pacific Islander
  "Homeownership_%_HispanLatin" = "S2502_C03_009", # Hispanic/Latino (any race)
  "Homeownership_%_White" = "S2502_C03_010", # White alone (not Hispanic/Latino)
  "Homeownership_%_All" = "S2502_C03_001" # All
)

# Get ACS data
acs_data_S2502 <- get_acs(geography = "county",
                          state = "VA",
                          county = county_codes,
                          variables = var_S2502,
                          summary_var = "S2502_C03_001", # this provides the total (summary table) we need for creating percents
                          year = year,
                          survey = "acs5",
                          key = census_api)

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
acs_data_combined <- rbind(acs_data_DP05_summarize, pop_child_race_summarize, acs_data_S1701_summarize, acs_data_S2701_summarize, child_NoHealthInsur_summarize, unemp_age_race_summarize, acs_data_S2502_summarize)
  
# Save table as csv
write.csv(acs_data_combined, paste0(as.character(year), "-", "acs-data-combined.csv"), row.names=FALSE)
