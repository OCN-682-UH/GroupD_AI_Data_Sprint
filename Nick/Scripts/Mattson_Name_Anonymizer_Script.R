#####################################

library(tidyverse)
library(here)
library(ggplot2)
library(kableExtra)
library(lubridate)

# --- CONFIGURATION ---
INPUT_FILE             <- here("Data","Master_Output_Dataframe.csv")
ANONYMIZED_OUTPUT_FILE <- here("Data","Master_Output_Dataframe_anonymized.csv")
MAPPING_KEY_FILE       <- here("Data","OPIHI_mapping_key.csv")

# List the exact column names you want to mask
COLUMNS_TO_ANONYMIZE   <- c("team_members","team_lead_or_recorder","data_entry_by") 


# --- CORE LOGIC ---
# 1. Load the original CSV
df <- read_csv(INPUT_FILE)

# Initialize an empty tibble to hold the master lookup map
master_mapping <- tibble(
  Column_Source  = character(),
  Original_Value = character(),
  Randomized_ID  = character()
)

# 2. Loop through each column to generate random maps
for (col in COLUMNS_TO_ANONYMIZE) {
  if (!col %in% colnames(df)) {
    warning(paste("Column '", col, "' not found in CSV. Skipping.", sep=""))
    next
  }
  
  # Extract unique, non-NA values from the targeted column
  unique_vals <- df %>% 
    pull(!!sym(col)) %>% 
    na.omit() %>% 
    unique()
  
  # Generate short, 8-character random alphanumeric IDs
  # e.g., "ID_A1B2C3D4"
  random_ids <- replicate(
    length(unique_vals), 
    paste0("ID_", toupper(paste(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = "")))
  )
  
  # Ensure IDs are perfectly unique inside this loop iteration
  while(any(duplicated(random_ids))) {
    random_ids[duplicated(random_ids)] <- replicate(
      sum(duplicated(random_ids)), 
      paste0("ID_", toupper(paste(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = "")))
    )
  }
  
  # Construct temporary map for this specific column
  col_map <- tibble(
    Column_Source  = col,
    Original_Value = as.character(unique_vals),
    Randomized_ID  = random_ids
  )
  
  # Append to master mapping key
  master_mapping <- bind_rows(master_mapping, col_map)
  
  # Replace the original column values in the dataframe with their new IDs
  df <- df %>%
    left_join(col_map %>% select(Original_Value, Randomized_ID), by = setNames("Original_Value", col)) %>%
    mutate(!!sym(col) := ifelse(is.na(Randomized_ID), !!sym(col), Randomized_ID)) %>%
    select(-Randomized_ID)
}

# 3. Export the processed files
write_csv(df, ANONYMIZED_OUTPUT_FILE)
write_csv(master_mapping, MAPPING_KEY_FILE)

cat("Success! Files generated successfully in your working directory.")
