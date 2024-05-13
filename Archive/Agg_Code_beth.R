

# Census API key
census_api <- "9f1daffae1a3171fcf4bcec45d7bae5823bcf48b"

# Load R environment
load()

#### List all variable of interest ####

# Create variable list
varlist <- c("DP05", # Race and Ethnicity % 
             "S0901", # Race and Ethnicity of Children %
             "S1701", # Poverty Rates
             "S1903", # Median Household Income 
             "S2701", # Without Health Insurance % 
             "S2301", # Unemployment Rate %
             "S2502") # Homeownership %

# Create greater charlottesville area zip codes
gca <- c("51003", # Albemarle
         "51540", # Charlottesville
         "51065", # Fluvanna
         "51079", # Greene
         "51109", # Louisa
         "51125") # Nelson

# Create year list
times <- c(2022)

#### Find variables of interest in metadata ####

# Pull all variable names from metadata
metadata_var <- all_acs_meta()

# Get variable names of interest from all metadata
all_vars <- get_all_vars(varlist, metadata_var)


#### Pull data for variable of interest ####
varlist_s = c("S1701_C03_001", # povrate
              "S1701_C03_002",   # cpovrate
              "S1901_C01_012")

dat <- pull_single_locality_year(varlist_s, gca, times, census_api)
dat3 <- all_years_localities(all_vars[["no_child_insur"]][["var_DP05"]][["name"]], gca, times, census_api)

dat2 <- get_acs(geography = "county", 
                variables = varlist_s, # Variables list provided in the all_acs5_data function wrapping this one
                state = "51", # Virginia code
                county = c("003","540","065","079","109","125"), 
                year = 2022, 
                survey = "acs5", 
                key = census_api,
                output = "wide")

# Pulls all the dpo5 variables for all the localities and years
dpo5_tbl <- all_years_localities(all_vars[["no_child_insur"]][["var_DP05"]][["name"]], gca, times, census_api)

# Filter to only the rows with percentages
dpo5_tbl_percents <- dpo5_tbl %>% 
  filter(str_detect(variable, "P$"))


# Group by variable and calculate new values
dpo5_tbl_aggregated <- dpo5_tbl_percents %>% 
  group_by(variable) %>% 
  summarise(agg_e = sum(estimate), agg_moe = moe_sum(moe, estimate = dpo5_tbl_percents$estimate, na.rm=T))






