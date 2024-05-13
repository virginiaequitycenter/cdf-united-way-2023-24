
################################## Packages ##################################

library(tidyverse)
library(tidycensus)
library(purrr)

###############################################################################


###############################################################################
############################# Variable Acquisition ############################
################################## Functions ##################################
###############################################################################


# Create function to get all metadata in single file
all_acs_meta <- function(){
  # Gets the list of all variables from all acs5 metadata tables
  vars1 <- load_variables(2021, "acs5") %>% select(-geography) # Remove the geography column
  vars2 <- load_variables(2021, "acs5/profile")
  vars3 <- load_variables(2021, "acs5/subject")
  vars4 <- load_variables(2021, "acs5/cprofile")
  
  # Provides column with specific lookup
  vars1$dataset_table <- "acs5"
  vars2$dataset_table  <- "acs5/profile"
  vars3$dataset_table  <- "acs5/subject"
  vars4$dataset_table  <- "acs5/cprofile"
  
  # Combine all table rows
  all_vars_meta <- rbind(vars1, vars2, vars3, vars4)
  
  return(all_vars_meta)
}

###############################################################################

# Create function to get full names of single census variable based on code
get_single_var <- function(variable, metadata){
  
  # Create list of variables that we only want percents
  precent_vars <- c("DP05","S0901","S2701","S2301","S2502")
  
  # Convert to uppercase
  uppercase_variable <- toupper(variable)
  uppercase_df <- data.frame(lapply(metadata, toupper))
  
  # First filtering to select only for single race variables or total our race/ethnicity groups of interest
  # Phrases to remove if part is contained within 
  filter_out <- c("MORE RACE", "OTHER RACE", "TWO")
  # Phrases to keep if part is contained within
  filter_in <- c("ALONE", "TOTAL", "ONE")
  
  # Second filtering for specific race/ethnicity groups
  race_ethn <- c("BLACK", # Black or African American
                 "INDIAN", # American Indian and Alaska Native
                 "ASIAN", # Asian
                 "HAWAIIAN", # Native Hawaiian and Other Pacific Islander
                 "HISPANIC", # Hispanic or Latino (of any race)
                 "WHITE") # White alone, not Hispanic or Latino
  
  # Checks if variable is in the list of percent variables, only keeps variables ending with P if so
  
  # Checks if any of the variable prefixes are contained in table and pulls matches (not case-sensitive)
  vars_filtered <- uppercase_df %>%
    # Filters to variable codes that contain provided prefixes
    filter(grepl(uppercase_variable, name, fixed = TRUE)) %>%
    # First Filter: Filters to variables totals or single race 
    filter(str_detect(label, paste(filter_in, collapse = "|"))) %>%
    filter(!str_detect(label, paste(filter_out, collapse = "|"))) %>%
    # Second Filter: Filters only to the race groups of interest
    filter(str_detect(label, paste(race_ethn, collapse = "|")))
  
  return(vars_filtered)
}

###############################################################################

# Need function just to get values for Children without Health Insurance %
# NOTE: cannot find 2701 variable from project desc
get_child_insur <- function(metadata){
  
  # Convert to uppercase
  uppercase_df <- data.frame(lapply(metadata, toupper))
  
  # First filtering to select only for single race variables or total our race/ethnicity groups of interest
  # Phrases to remove if part is contained within 
  filter_out <- c("MORE RACE", "OTHER RACE", "TWO")
  # Phrases to keep if part is contained within
  filter_in <- c("ALONE", "TOTAL", "ONE", "HISPANIC")
  
  # Second filtering for specific race/ethnicity groups
  race_ethn <- c("BLACK", # Black or African American
                 "INDIAN", # American Indian and Alaska Native
                 "ASIAN", # Asian
                 "HAWAIIAN", # Native Hawaiian and Other Pacific Islander
                 "(HISPANIC OR LATINO)", # Hispanic or Latino (of any race)
                 "WHITE") # White alone, not Hispanic or Latino
  
  
  # Checks if any of the variable prefixes are contained in table and pulls matches (not case-sensitive)
  vars_filtered <- uppercase_df %>%
    # Filters to variable codes that contain provided prefixes
    filter(grepl("C27001", name, fixed = TRUE)) %>%
    # First Filter: Filters to variables totals or single race 
    filter(str_detect(concept, paste(filter_in, collapse = "|"))) %>%
    filter(!str_detect(concept, paste(filter_out, collapse = "|"))) %>%
    # Second Filter: Filters only to the race groups of interest
    filter(str_detect(concept, paste(race_ethn, collapse = "|"))) %>% 
    # Third Filter: Filters only to those with no health insurance
    filter(grepl("NO HEALTH INSURANCE", label, fixed = TRUE)) %>% 
    # Fourth Filter: Filters to those under 19 yo
    filter(grepl("UNDER 19", label, fixed = TRUE))
  
  # To get totals need to pull another variable
  vars_filtered_total <- uppercase_df %>%
    # Filters to total variable code
    filter(name == "B18135_007" | name ==  "B18135_012") # Under 19 yo, no insurance [1st code is disability, 2nd is no disability]
  
  # Groups the two tables together
  vars_filtered <- rbind(vars_filtered_total, vars_filtered)
  
  return(vars_filtered)
  
}

###############################################################################

# Create function to use get_single_var to get entire list of functions
get_all_vars <- function(variables, metadata){
  
  # Creates empty dataframe to store data (previously combined all variables into single dataset)
  #var_all <- tibble()
  
  # Removes the child insurance variable
  variables_nochild <- variables[!str_detect(variables, paste(c("B18135", "C27001"), collapse = "|"))]
  # Places child insurance into own container
  variables_child <- variables[str_detect(variables, paste(c("B18135", "C27001"), collapse = "|"))]
  
  # Apply the get_single_var function to the no child variable list
  no_child_results <- map(variables_nochild, get_single_var, metadata)
  
  # Gets the output for child insurance variables
  var_B18135_C27001 <- get_child_insur(metadata_var)
  
  # Create list of the two combined
  outlist <- list(no_child_insur = no_child_results, var_B18135_C27001 = var_B18135_C27001)
  
  # Create empty vector for dataset names
  df_names <- c()
  
  # Cycle through creating unique name
  for (i in seq_along(no_child_results)) {
    # Create a unique variable name
    df_names <- c(df_names, paste0("var_", varlist[i]))
  }
  
  # Change datasets inside the list names
  names(outlist[["no_child_insur"]]) <- df_names
  
  # Add the child insurance variables to our empty df (previously combined all variables into single dataset)
  #var_all <- rbind(var_all, var_child)
  
  return(outlist)

}

###############################################################################


###############################################################################
############################### Data Acquisition ##############################
################################## Functions ##################################
###############################################################################


# Create function for pulling variables directly from single locality for a single year
pull_single_locality_year <- function(variables, county_code, year, api_key) {
  
  # Get the county code from the overall zip by taking last three digits
  county_code <- substr(county_code, nchar(county_code) - 2, nchar(county_code))
  
  # Pulls from census API using tidycensus
  get_acs(geography = "county", 
          variables = variables, # Variables list provided in the all_acs5_data function wrapping this one
          state = "51", # Virginia code
          county = county_code, 
          year = year, 
          survey = "acs5", 
          key = api_key) # API key provided in the all_acs5_data function
}

###############################################################################

# Pull all the data for single var_table for all years and localities
all_years_localities <- function(variables, regions, years, api_key){
  
  # Define the variables you want to pull
  #variables <- unique(variable)
  
  # Create all combinations of variables, regions, and times
  all_combos <- expand.grid(variables = variables, regions = regions, time = years, stringsAsFactors =F)
  
  # Run the function over all combinations
  data <- pmap(all_combos, ~pull_single_locality_year(as.character(..1), ..2, as.numeric(..3), api_key))
  
  # Combine the list of lists into a single dataframe
  combined_df <- do.call(rbind, data)
  
  return(combined_df)
}

###############################################################################

# Function that allows list of var_tables and pull data for each for given years/regions



###############################################################################

# Function that only keeps percentage data for variables where we only care about percents
perc_only <- function(input_list){
  
}

###############################################################################


###############################################################################
############################### Data Aggregation ##############################
################################## Functions ##################################
###############################################################################


# Function that can take dataset of variable and combines only the given localities
single_aggregator <- function(){}

###############################################################################

# Function creates combined table of all variables for single locality 

locality_all_var <- function(){}

###############################################################################
