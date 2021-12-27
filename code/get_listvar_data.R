#############
## Process listvars
#############

# Set a counter for messaging out progress
i <- 0

# Filter to just the listvars, sort them numerically, and flag which ones are classifications
df_listvars <- df_dims %>% 
  filter(grepl("(^listvariable).*", id)) %>% 
  mutate(listvar_num = as.numeric(gsub("listvariable","", id)),
         classification = if_else(!is.na(parent), TRUE, FALSE),
         # We'll need this for joining with the total occurrences
         base_listvar = if_else(is.na(parent), id, gsub("variables/", "", parent))) %>% 
  arrange(listvar_num) %>% 
  select(id, name, description, classification, base_listvar)

# Function to get top 5 listvar values. Get the top 50,000 in order to sum up the total occurrences 
# (approximately), but only return the top 5.
get_listvar_vals <- function(listvar_id){
  
  # Get the top values for the listvar. We'll get up to 50000 just to get a count of
  # unique values, but we'll only retain the top 5
  df_listvar_top <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                                      dimensions = listvar_id,
                                      # Get the specific listvar occurrences. If it's a classification, it's the
                                      # occurrences of the parent
                                      metrics = "occurrences",
                                      top = 50000)
  
  # Standardize the name, count how many unique values there are and assess the listvar
  names(df_listvar_top) <- c("value", "occurrences")
  
  # Remove the "Unspecified." These are, technically, values, but...not really
  df_listvar_top <- df_listvar_top %>% filter(value != "Unspecified")
  
  # Estimate and assess the total occurrences & number of unique values
  total_occurrences <- sum(df_listvar_top$occurrences)
  assessment <- case_when(
    # Classification with no values
    grepl("^listvar.*\\..*$", listvar_id) & nrow(df_listvar_top) <= 1 ~ "No Classifications Data",
    total_occurrences == 0 ~ "No Data",
    total_occurrences <= min_instances ~ "Minimal Data",
    TRUE ~ "Has Data")
  unique_vals <- nrow(df_listvar_top)
  
  # Check if there is possibly a Launch data element not evaluating. We may add other issues here
  other_issues <- if_else(grepl("^\\%.*\\%$", listvar_id), "Possible Launch data element not evaluating", "None")

  # Check whether to write a progress message to the console and write it out
  i <<- i + 1
  if(i %% 5 == 0){message(i, " of ", nrow(df_listvars), " listvars/classifications processed.")}
    
  # Turn it into a single row
  df_listvar_top <- df_listvar_top[1:5,] %>% # Just including the top 5; not using top_n because fine to break ties arbitrarily
    mutate(id = listvar_id,
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

# There are only 3 listvars possible, so some organizations use none of them. Account for that here

if(nrow(df_listvars) > 0){
  
  message("Starting processing of ", nrow(df_listvars), " listvars/classifications.")
  
  # Get the top values for all listvars
  df_listvar_vals <- map_dfr(df_listvars$id, get_listvar_vals)
  
  # Join the top values
  df_listvars <- df_listvars %>% 
    left_join(df_listvar_vals, by = c(id = "id")) 
  
  # Write out to a .csv for use if the script craps out and put a message to the console
  write_rds(df_listvars, "output/df_listvars.rds")
  message("listvar processing completed.")
  
  # Cleanup
  rm(df_listvar_vals, get_listvar_vals)
  
} else {
  
  # We'll need to add dummy "assessment" and "other_issues" columns just so later logic doesn't break
 df_listvars <- df_listvars %>% add_column(assessment = NA) %>% 
   add_column(other_issues = NA)
}
