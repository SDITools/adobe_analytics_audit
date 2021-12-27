#############
## Process Traffic Sources
#############

# This includes both "campaign" (and classifications of it), which are handled pretty much just
# like the eVars, and then also Marketing Channel and Marketing Channel Details, which are
# really just more to get some quick visuals and values

# Set a counter for messaging out progress
i <- 0

##############
# campaign, campaign classifications
##############

# Filter to just the campaigns, sort them numerically, and flag which ones are classifications
df_campaign <- df_dims %>% 
  filter(grepl("^campaign.*", id)) %>% 
  mutate(id = if_else(id == "campaign", "campaign.0", id)) %>% 
  mutate(campaign_num = as.numeric(gsub("campaign.","", id)),
         classification = if_else(!is.na(parent), TRUE, FALSE),
         id = ifelse(id == "campaign.0", "campaign", id),
         # We'll need this for joining with the total visits
         base_campaign = if_else(is.na(parent), id, gsub("variables/", "", parent))) %>% 
  arrange(campaign_num) %>% 
  select(id, name, description, classification, base_campaign)

##############
# Probably need to add a check here to make sure campaign (with campaigninstances) is being used
# and getting some data. Will add that once I find an account where it doesn't exist and this
# errors out.
##############

# Function to get top 5 campaign values. Get the top 50,000 in order to sum up the total visits 
# (approximately), but only return the top 5.
get_campaign_vals <- function(campaign_id){
  
  # Get the top values for the campaign. We'll get up to 50000 just to get a count of
  # unique values, but we'll only retain the top 5
  df_campaign_top <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                                       dimensions = campaign_id,
                                       metrics = "visits",
                                       top = 50000)
  
  # Standardize the name, count how many unique values there are and assess the campaign
  names(df_campaign_top) <- c("value", "visits")
  
  # Remove the "Unspecified." These are, technically, values, but...not really
  df_campaign_top <- df_campaign_top %>% filter(value != "Unspecified")
  
  # Estimate and assess the total visits & number of unique values
  total_visits <- sum(df_campaign_top$visits)
  assessment <- case_when(
    # Classification with no values
    grepl("^campaign.*\\..*$", campaign_id) & nrow(df_campaign_top) <= 1 ~ "No Classifications Data",
    total_visits == 0 ~ "No Data",
    total_visits <= min_instances ~ "Minimal Data",
    TRUE ~ "Has Data")
  unique_vals <- nrow(df_campaign_top)
  
  # Check whether to write a progress message to the console and write it out
  i <<- i + 1
  if(i %% 5 == 0){message(i, " of ", nrow(df_campaign), " campaign/classifications processed.")}
  
  # Turn it into a single row
  df_campaign_top <- df_campaign_top[1:5,] %>% # Just including the top 5; not using top_n because fine to break ties arbitrarily
    mutate(id = campaign_id,
           val_label = paste0("value_", row_number()),
           value = ifelse(is.na(value), NA, paste0(value, " (", format(visits, big.mark = ",", trim = TRUE), ")"))) %>% 
    select(id, val_label, value) %>% 
    pivot_wider(names_from = val_label, values_from = value) %>% 
    mutate(assessment = assessment,
           total_visits = total_visits,
           unique_vals = unique_vals) %>% 
    relocate(assessment, .after = id) %>% 
    relocate(total_visits, .after = assessment) %>% 
    relocate(unique_vals, .after = total_visits)
}

message("Starting processing of ", nrow(df_campaign), " campaign/classifications.")

# Get the top values for all campaigns
df_campaign_vals <- map_dfr(df_campaign$id, get_campaign_vals)

# Join the top values
df_campaign <- df_campaign %>% 
  left_join(df_campaign_vals, by = c(id = "id")) %>% 
  select(-base_campaign)

# Create the statements about campaigns
cmp_ttl <- df_campaign %>% filter(id == "campaign") %>% pull(unique_vals)
cmp_ttl <- if_else(cmp_ttl == 50000,
                   "There were **>50,000 unique values** for the campaign variable (tracking code)",
                   paste0("There were **", format(cmp_ttl, big.mark = ",", trim = TRUE), " unique values** for the campaign variable (tracking code)."))

cmp_class_ttl <- nrow(df_campaign) - 1
cmp_class_ttl <- paste0("The campaign variable (tracking code) has **", cmp_class_ttl, " classifications configured**.")

cmp_class_w_data <- df_campaign %>% filter(id != "campaign" & assessment != "No Data" &
                                             assessment != "No Classifications Data") %>% nrow()
cmp_class_w_data <- paste0("Of those, **", cmp_class_w_data, " of the classifications are being populated with values**.")

# Write out to a .csv for use if the script craps out and put a message to the console
write_rds(df_campaign, "output/df_campaign.rds")

message("campaign processing completed.")

# Cleanup
rm(df_campaign_vals, get_campaign_vals)

##############
# Marketing Channels
##############

# Get the total visits. We'll need this for some % calculations, since a single visit can have multiple
# marketing channels.
total_visits <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                                  dimensions = "daterangeyear",
                                  metrics = "visits") %>% 
  pull(visits) %>% sum()

# Get the marketing channel data
df_marketing_channels <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                                           dimensions = "marketingchannel",
                                           metrics = "visits",
                                           top = 100)

# Calculate the % of total visits
df_marketing_channels <- df_marketing_channels %>% 
  mutate(pct_visits = round(visits/total_visits, 4))

rm(total_visits)

##############
# Marketing Channel Details
##############

message("Starting the pull of Marketing Channel / details data.")

# Get the marketing channel details data
df_marketing_channel_details <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                                                  dimensions = c("marketingchannel", "marketingchanneldetail"),
                                                  metrics = "visits",
                                                  top = c(100, 50000))

# Unique marketing channel details by channel. We'll add these counts to the marketing channels summary
df_marketing_channels <- df_marketing_channel_details %>% 
  group_by(marketingchannel) %>% summarize(unique_detail_vals = n()) %>% 
  right_join(df_marketing_channels) %>% 
  relocate(unique_detail_vals, .after = pct_visits) %>% 
  arrange(-visits)

# Clip to the top 5 detail values and then spread out. There would be ties, so slice_max doesn't
# quite work—-using rank() and filter() and an arbitrary method of breaking ties (which is fine)
df_marketing_channels <- df_marketing_channel_details %>% 
  group_by(marketingchannel) %>% 
  mutate(rank_val = rank(-visits, ties.method = "first")) %>% 
  filter(rank_val <= 5) %>% 
  arrange(marketingchannel, -visits) %>% 
  mutate(val_label = paste0("value_", rank_val),
         value = paste0(marketingchanneldetail, " (", format(visits, big.mark = ",", trim = TRUE), ")")) %>% 
  ungroup() %>% 
  select(marketingchannel, val_label, value) %>% 
  pivot_wider(names_from = val_label, values_from = value) %>% 
  right_join(df_marketing_channels) %>% 
  relocate(c(visits, pct_visits, unique_detail_vals), .after = marketingchannel) %>% 
  arrange(-visits) %>% 
  # Convert to a factor for ordering in the plot
  mutate(marketingchannel = factor(marketingchannel, levels = rev(marketingchannel)))

# Write out to a .csv for use if the script craps out and put a message to the console
write_rds(df_marketing_channels, "output/df_marketing_channels.rds")

message("Marketing Channel / details completed.")

rm(df_marketing_channel_details)

# Set values for the summary

# How many channels have pretty low traffic to the site
mc_low <- paste0("**", df_marketing_channels %>% filter(pct_visits < 0.01) %>% nrow(), " channels** each account for **less than 1% of total visits** to the site.")

# Does "None" occur and, if so, how much?
mc_none <- case_when(
  df_marketing_channels %>% filter(marketingchannel == "None") %>% nrow() == 0 ~ 
    "**None** does not show up at all for marketing channels. This is great!",
  df_marketing_channels %>% filter(marketingchannel == "None") %>% pull(pct_visits) < 0.01 ~ 
    "**None** appears as a marketing channel, but it is **less than 1% of total traffic**, so this is of low concern.",
  df_marketing_channels %>% filter(marketingchannel == "None") %>% pull(pct_visits) < 0.05 ~ 
    paste0("**None** appears as a marketing channel accounting for **", 
           df_marketing_channels %>% filter(marketingchannel == "None") %>% pull(pct_visits) %>% percent(accuracy = 0.1),
           " of traffic**, which is **somewhat concerning**."),
  TRUE ~ paste0("**None** appears as a marketing channel accounting for **", 
                df_marketing_channels %>% filter(marketingchannel == "None") %>% pull(pct_visits) %>% percent(accuracy = 0.1),
                " of traffic**, which is **very concerning**."))

# Does "Session Refresh" (or under another name) occur and, if so, how much? 
# There could be a channel that is, essentially "Session Refresh", but that isn't 
# in this list because it is just configured with a different name, so this is 
# by no means bulletproof

# A few different names get used, so add to this if you come across another one
session_refresh_names <- c("Session Refresh", "Internal", "Internal Refresh", "Session Timeout",
                           "Browser left open", "Internal Referrer")

# Find "the one" that is the name used in this case. Get a df with just that one row
df_session_refresh <- df_marketing_channels %>% 
  filter(marketingchannel %in% session_refresh_names)

session_refresh_name <- case_when(
  nrow(df_session_refresh) == 0 ~ "None Found",
  nrow(df_session_refresh) == 1 ~ as.character(df_session_refresh$marketingchannel[1]),
  # Having 2 or more rows match would be a surprise. For now, just using the first
  # row that appears if that happens, but could change later to get fancier
  TRUE ~ as.character(df_session_refresh$marketingchannel[1])   
)

mc_session_refresh <- if(session_refresh_name == "None Found"){ 
  paste("**Session Refresh** does not appear to show up at all for marketing channels. If such a channel exists and",
        "is properly configured, this is great! If the channel exists, but with a slightly different name that we",
        "were not able to detect, check the chart below to confirm that it is not showing up more than expected.")
} else {
  case_when(
    df_marketing_channels %>% filter(marketingchannel == session_refresh_name) %>% pull(pct_visits) < 0.01 ~
      paste0("**", session_refresh_name,"** appears as a marketing channel, but it is **less than 1% of total traffic**, so this is of low concern."),
    
    df_marketing_channels %>% filter(marketingchannel == session_refresh_name) %>% pull(pct_visits) < 0.05 ~
      paste0("**", session_refresh_name,"** appears as a marketing channel accounting for **",
             df_marketing_channels %>% filter(marketingchannel == session_refresh_name) %>% 
               pull(pct_visits) %>% percent(accuracy = 0.1),
             " of traffic**, which is **somewhat concerning**."),
    TRUE ~ paste0("**", session_refresh_name,"** appears as a marketing channel accounting for **", 
                  df_marketing_channels %>% filter(marketingchannel == session_refresh_name) %>% 
                    pull(pct_visits) %>% percent(accuracy = 0.1),
                  " of traffic**, which is **very concerning**."))
}

# Write out the various campaign / marketing channels, too. We'll use these if the writing
# to Google Sheets craps out
write_rds(list(cmp_ttl = cmp_ttl,
               cmp_class_ttl = cmp_class_ttl,
               cmp_class_w_data = cmp_class_w_data,
               mc_low = mc_low,
               mc_none = mc_none,
               mc_session_refresh = mc_session_refresh),
          "output/marketing_channel_messages.rds")
