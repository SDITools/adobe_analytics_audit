#############
## Process events
#############

# Filter to just the event & sort them numerically
df_events <- df_metrics %>% 
  filter(grepl("^event.*", id)) %>% 
  mutate(event_num = as.numeric(gsub("event","", id))) %>% 
  arrange(event_num) %>% 
  select(id, name, description, type)

# Get the the daily event total for each event. There is potentially a limit as to the 
# number of events allowed, so this is done as a function that pulls the events 100 at
# a time
get_events_daily <- function(event_ids){
  df <- aw_freeform_table(company_id = company_id, rsid = rsid, date_range = date_range,
                          dimensions = "daterangeday",
                          metrics = event_ids) %>%
    pivot_longer(-daterangeday) %>%
    arrange(daterangeday, name) %>% 
    select(day = daterangeday, id = name, value)
}

# Split the events up into chunks of events
event_blocks <- split(df_events$id, ceiling(seq_along(df_events$id)/100))

message("Starting processing of ", nrow(df_events), " events.")

df_events_daily <- map_dfr(event_blocks, get_events_daily)

# Make a totals version. This will be total events for counter events and just the total/sum
# for numeric/currency events. So it goes.
df_event_totals <- df_events_daily %>% 
  group_by(id) %>%
  summarise(event_total = sum(value)) %>% 
  ungroup() 

# Join the event totals
df_events <- df_events %>% 
  left_join(df_event_totals, by = c(id = "id")) %>% 
  mutate(assessment = case_when(
    event_total == 0 ~ "No Data",
    event_total <= min_instances ~ "Minimal Data",
    TRUE ~ "Has Data")) %>% 
  relocate(assessment, .after = type)

# Remove the events from the daily data that had no data
df_events_daily <- df_events_daily %>% 
  left_join(df_events, by = c(id = "id")) %>% 
  filter(assessment != "No Data") %>% 
  select(day, id, name, value)

# ############
# # Use the anomalize package to add an anomaly_val column. This will be the value if it's detected
# # as an anomaly, and NA otherwise
# 
# # Function to get the anomaly values
# get_anomalies <- function(event_id){
# 
# df <- df_events_daily %>% 
#   filter(id == event_id) %>% 
#   arrange(day) %>% 
#   time_decompose(target = value, method = "stl", frequency = "1 week", merge = TRUE) %>% 
#   anomalize(target = remainder, method = "iqr", max_anoms = 0.5) %>% 
#   time_recompose() %>% 
#   mutate(anomaly_val_anomalize = ifelse(anomaly == "Yes", value, NA)) %>% 
#   select(day, id, anomaly_val_anomalize)
# 
# }
# 
# df_event_anomalies <- map_dfr(df_events %>% filter(assessment != "No Data") %>% pull(id), get_anomalies)
# 
# df_events_daily <- df_events_daily %>% 
#   left_join(df_event_anomalies, by = c(day = "day", id = "id"))
# 
# 
# ###############

# Write out to a .csv for use if the script craps out and put a message to the console
write_rds(df_events, "output/df_events.rds")
write_rds(df_events_daily, "output/df_events_daily.rds")
message("event processing completed.")

# Cleanup
rm(df_event_totals)

