#############
## Process eVars
#############

# Set a counter for messaging out progress
i <- 0

# Filter to just the eVars, sort them numerically, and flag which ones are classifications
df_evars <- df_dims %>% 
  # filter(grepl("(^evar)|(^listvariable).*", id)) %>% 
  filter(grepl("(^evar).*", id)) %>% 
  mutate(evar_num = as.numeric(gsub("evar","", id)),
         classification = if_else(!is.na(parent), TRUE, FALSE),
         # We'll need this for joining with the total instances
         base_evar = if_else(is.na(parent), id, gsub("variables/", "", parent))) %>% 
  arrange(evar_num) %>% 
  select(id, name, description, classification, base_evar)

# Get the total instances for each eVar (don't need to do this for classified values). This is
# just so we can do a check to make sure the eVar has *some* data in the period so we don't try
# to pull eVar values, as that will return an error.
df_evar_totals <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                                    dimensions = "daterangeyear",   # Hack to get a single row
                                    metrics = df_evars %>% filter(classification == FALSE) %>%
                                      mutate(inst = paste0(id,"instances")) %>% pull(inst)) %>%
  pivot_longer(-daterangeyear) %>%
  # Just in case this spanned multiple years
  group_by(name) %>%
  summarise(total_instances = sum(value)) %>%
  ungroup() %>%
  # Create a joinable name
  mutate(id = gsub("instances", "", name)) %>%
  select(id, total_instances)

# Function to get top 5 eVar values. Get the top 50,000 in order to sum up the total occurrences 
# (approximately), but only return the top 5.
get_evar_vals <- function(evar_id){
  
  # If the eVar is getting NO traffic, then the aw_freeform_table call for instances will return
  # an error. So, check for that and return a blank row before trying to pull that as data.
  if(df_evar_totals %>% filter(id == gsub("\\..*$", "", evar_id)) %>% pull(total_instances) == 0){
    df_evar_top <- data.frame(id = evar_id,
                              assessment = "No Data",
                              other_issues = "None",
                              total_instances = 0,
                              unique_vals = 0,
                              value_1 = NA,
                              value_2 = NA,
                              value_3 = NA,
                              value_4 = NA,
                              value_5 = NA)
  } else {
    
    # Get the top values for the eVar. We'll get up to 50000 just to get a count of
    # unique values, but we'll only retain the top 5
    df_evar_top <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                                     dimensions = evar_id,
                                     # Get the specific eVar instances. If it's a classification, it's the
                                     # instances of the parent
                                     metrics = paste0(gsub("\\..*$", "", evar_id), "instances"),
                                     top = 50000)
    
    # Standardize the name, count how many unique values there are and assess the evar
    names(df_evar_top) <- c("value", "instances")
    
    # Remove the "Unspecified." These are, technically, values, but...not really
    df_evar_top <- df_evar_top %>% filter(value != "Unspecified")
    
    # Estimate and assess the total instances & number of unique values
    total_instances <- sum(df_evar_top$instances)
    assessment <- case_when(
      # Classification with no values
      grepl("^evar.*\\..*$", evar_id) & nrow(df_evar_top) <= 1 ~ "No Classifications Data",
      total_instances == 0 ~ "No Data",
      total_instances <= min_instances ~ "Minimal Data",
      TRUE ~ "Has Data")
    unique_vals <- nrow(df_evar_top)
    
    # Check if there is possibly a Launch data element not evaluating. We may add other issues here
    other_issues <- if_else(grepl("^\\%.*\\%$", evar_id), "Possible Launch data element not evaluating", "None")
       
    # Check whether to write a progress message to the console and write it out
    i <<- i + 1
    if(i %% 5 == 0){message(i, " of ", nrow(df_evars), " eVars/classifications processed.")}
    
    # Turn it into a single row
    df_evar_top <- df_evar_top[1:5,] %>% # Just including the top 5; not using top_n because fine to break ties arbitrarily
      mutate(id = evar_id,
             val_label = paste0("value_", row_number()),
             value = ifelse(is.na(value), NA, paste0(value, " (", format(instances, big.mark = ",", trim = TRUE), ")"))) %>% 
      select(id, val_label, value) %>% 
      pivot_wider(names_from = val_label, values_from = value) %>% 
      mutate(assessment = assessment,
             other_issues = other_issues,
             total_instances = total_instances,
             unique_vals = unique_vals) %>% 
      relocate(c(assessment, other_issues, total_instances, unique_vals), .after = id)
       }
}

message("Starting processing of ", nrow(df_evars), " eVars/classifications.")

# Get the top values for all eVars
df_evar_vals <- map_dfr(df_evars$id, get_evar_vals)

# Join the top values
df_evars <- df_evars %>% 
  left_join(df_evar_vals, by = c(id = "id")) %>% 
  select(-base_evar)

# Write out to a .csv for use if the script craps out and put a message to the console
write_rds(df_evars, "output/df_evars.rds")
message("eVar processing completed.")

# Cleanup
rm(df_evar_totals, df_evar_vals, get_evar_vals)

