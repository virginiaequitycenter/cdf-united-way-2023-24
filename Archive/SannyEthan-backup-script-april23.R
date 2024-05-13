## Backup Script: Dropped code for United Way Project
## Edits file made by Ethan and Sanny

# Needed to drop the youth without health insurance by race and the unemployment by race 
# Code is contained here in backup script to keep note of the work; removed from the working file

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

vars_B27001I <- c(pop_u6 = "B27001I_004", # Total under 6yrs
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
vars_B27001H <- c(pop_u6 = "B27001H_004", # Total under 6yrs
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
# DETERMINATION: cannot implement "unemployment rate" from the B23002, can use Table S2301 to pull the % unemployed by race along with the total civilian population aged 16-64 by race for local counties as is, can 
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

