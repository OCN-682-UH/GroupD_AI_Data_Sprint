###Mattson Year Changer Script###

library(tidyverse)
library(here)

# 1. Load the data
OPIHI_data <- read_csv(here("Data", "Master_Output_Dataframe_anonymized.csv"))

# 2. Fix the character type and fill the years
OPIHI_data <- OPIHI_data |>
  mutate(
    # Force the text "NA" into a true R missing value (NA)
    year = as.numeric(year),
    
    # Extract the 4-digit year from the date string
    extracted_year = as.numeric(str_extract(date, "\\d{4}")),
    
    # Fill in the true NAs with the extracted years
    year = coalesce(year, extracted_year)
  ) |>
  # Remove the temporary column
  select(-extracted_year)

# 3. Save the updated data back to your CSV
write_csv(OPIHI_data, file = here("Data", "OPIHI_Master_Output_cleaned_anonymized.csv"))


                              