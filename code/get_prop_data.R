#############
## Process props
#############

# Set a counter for messaging out progress
i <- 0

# Filter to just the props, sort them numerically, and flag which ones are classifications
df_props <- df_dims %>% 
  filter(grepl("^prop.*", id)) %>% 
  mutate(prop_num = as.numeric(gsub("prop","", id)),
         classification = if_else(!is.na(parent), TRUE, FALSE),
         # We'll need this for joining with the total instances
         base_prop = if_else(is.na(parent), id, gsub("variables/", "", parent))) %>% 
  arrange(prop_num) %>% 
  select(id, name, description, classification, base_prop)

# Function to get top 5 prop values. Get the top 50,000 in order to sum up the total occurrences 
# (approximately), but only return the top 5.
get_prop_vals <- function(prop_id){
  
  # Get the top values for the eVar
  df_prop_top <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                                   dimensions = prop_id,
                                   metrics = "occurrences",
                                   top = 50000)
  
  # Standardize the name
  names(df_prop_top) <- c("value", "occurrences")
  
  # Remove the "Unspecified." Since these are traffic variables, occurrences will be recorded 
  # as "Unspecified" when there is just nothing populating. There are generally props that ONLY
  # have "Unspecified," which means this zeros out the data frame. We correct for this at the
  # end of the script by flagging these as "No Data"
  df_prop_top <- df_prop_top %>% filter(value != "Unspecified")
  
  # Estimate and assess the total occurrences & number of unique values
  total_occurrences <- sum(df_prop_top$occurrences)
  assessment <- case_when(
    #  # Classification with no values
    # grepl("^prop.*\\..*$", prop_id) & nrow(df_prop_top) <= 1 ~ "No Classifications Data",
    total_occurrences == 0 ~ "No Data",
    total_occurrences <= min_instances ~ "Minimal Data",
    TRUE ~ "Has Data")
  unique_vals <- nrow(df_prop_top)
  
  # Check if there is possibly a Launch data element not evaluating. We may add other issues here
  other_issues <- if_else(grepl("^\\%.*\\%$", prop_id), "Possible Launch data element not evaluating", "None")
  
  # Check whether to write a progress message to the console and write it out
  i <<- i + 1
  if(i %% 5 == 0){message(i, " of ", nrow(df_props), " props/classifications processed.")}
  
  # Turn it into a single row
  df_prop_top <- df_prop_top[1:5,] %>% # Just including the top 5; not using top_n because fine to break ties arbitrarily
    mutate(id = prop_id,
           val_label = paste0("value_", row_number()),
           value = ifelse(is.na(value), NA, paste0(value, " (", format(occurrences, big.mark = ",", trim = TRUE), ")"))) %>% 
    select(id, val_label, value) %>% 
    pivot_wider(names_from = val_label, values_from = value) %>% 
    mutate(assessment = assessment,
           other_issues = other_issues,
           total_occurrences = total_occurrences,
           unique_vals = unique_vals) %>% 
    relocate(c(assessment, other_issues, total_occurrences, unique_vals), .after = id)
}

message("Starting processing of ", nrow(df_props), " props/classifications.")

# Get the top values and total occurrences for all props
df_prop_vals <- map_dfr(df_props$id, get_prop_vals)

# Join the top values
df_props <- df_props %>% 
  left_join(df_prop_vals, by = c(id = "id"))

# Do some classification cleanup. For every value that is a classification, we need to get the
# assessment of its parent. If the *parent* has "No Data", then we want to keep the classification
# as "No Data". If the parent *does* have data, then that means there are simply no classifications,
# so we want to change the status to "No Classifications Data".
df_base_prop_assessment <- df_props %>% 
  filter(classification == FALSE) %>% 
  select(id, parent_assessment = assessment)

# This is where the lookups to the parent happen to determine if a classification has no data
# partly because the parent eVar has no data (keep as "No Data"), or if the parent eVar has data,
# but the classification does not ("No Classifications Data")
df_props <- df_props %>%
  left_join(df_base_prop_assessment, by = c(base_prop = "id")) %>% 
  mutate(assessment = case_when(
    classification == FALSE ~ assessment,
    assessment != "No Data" ~ assessment,
    parent_assessment != "No Data" ~ "No Classifications Data",
    TRUE ~ assessment
  )) %>% 
  select(-base_prop, -parent_assessment)

# Write out to a .csv for use if the script craps out and put a message to the console
write_rds(df_props, "output/df_props.rds")
message("prop processing completed.")

# Cleanup
rm(df_prop_vals, get_prop_vals, df_base_prop_assessment)
