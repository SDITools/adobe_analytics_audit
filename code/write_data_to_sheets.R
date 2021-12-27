# Create a new Google Sheet
gs <- gs4_create(name = paste("Adobe Analytics Audit:", rsid, "-", start_date, "to", end_date),
                 sheets = "Overview")

# Populate the Overview tab with an overview
tibble(Overview = c(paste("Adobe Analytics audit for report suite:", rsid),
                    paste("Audit script completed at:", Sys.time()),
                    " ",
                    paste("Date Range Included:", start_date, "to", end_date),
                    " ",
                    "'Unspecified' values have been removed from eVar and prop calculation totals.",
                    "eVar and prop totals are based on the total instances (eVars) and occurrences (props) for the top 50,000 values for each variable.")) %>% 
  sheet_write(gs, sheet = "Overview")

Sys.sleep(2)
range_autofit(gs, sheet = "Overview", range = "A:A")

# Write the data frames to it
df_evars %>% sheet_write(gs, sheet = "eVars")
df_listvars %>% sheet_write(gs, sheet = "listvars")
df_events %>% sheet_write(gs, sheet = "Events")
df_props %>% sheet_write(gs, sheet = "props")
df_marketing_channels %>% sheet_write(gs, sheet = "Marketing Channels")
df_campaign %>% sheet_write(gs, sheet = "Campaign Classifications")
