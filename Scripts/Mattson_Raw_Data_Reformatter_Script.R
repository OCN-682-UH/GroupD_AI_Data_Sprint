###Making sense of bad data###
###Created by Nick Mattson on 11/5/2026###
###used Google Gemini as AI model###

# --- REQUIRED LIBRARIES ---
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(here)

# --- CONFIGURATION ---
# here() locates your R Project root folder automatically.
FILE_PATH       <- here::here("Data", "OPIHI_data_2018.xlsx")   
SHEET_NAME      <- "OPIHI data 2018"               
MASTER_CSV_PATH <- here::here("Outputs","output_data_2018_sorted.csv")     

# Row definitions inside your Excel sheet
METADATA_ANCHOR_ROW <- 1    # The exact row where the word "Date" sits as a label
ORGANISM_HEADER_ROW <- 13   # Row containing "transect-1 quadrat-1..."
ORGANISM_START_ROW  <- 14   # First row of organism names
ORGANISM_END_ROW    <- 60   # Safe buffer row past your last organism row

# Mapping metadata row numbers (assuming METADATA_ANCHOR_ROW is row 1)
METADATA_ROWS <- list(
  "date"                     = 1,
  "start_time"               = 2,
  "end_time"                 = 3,
  "x_t_lines"                = 4,
  "longest_t_m"              = 5,
  "dist_between_t_pts_for_q" = 6,
  "x_quad_t"                 = 7,
  "location"                 = 8,
  "latitude"                 = 9,
  "longitude"                = 10,
  "school"                   = 11   
)

# ----------------------------------------------------------------------------
# 1. DYNAMICALLY DETECT FORM START COLUMNS
# ----------------------------------------------------------------------------
wb <- loadWorkbook(FILE_PATH)

entire_anchor_row <- readWorkbook(wb, sheet = SHEET_NAME, rows = METADATA_ANCHOR_ROW, 
                                  colNames = FALSE, skipEmptyCols = FALSE)

form_label_cols <- which(as.character(entire_anchor_row[1, ]) == "Date")

if (length(form_label_cols) == 0) {
  stop("Could not find the word 'Date' in the anchor row. Please check METADATA_ANCHOR_ROW.")
}

start_columns <- form_label_cols + 1
cat(sprintf("Found %d data forms dynamically based on 'Date' locations.\n", length(start_columns)))

# ----------------------------------------------------------------------------
# 2. RUN EXTRACTION LOOP
# ----------------------------------------------------------------------------
all_quadrats_list <- list()

for (idx in 1:length(start_columns)) {
  
  current_form_start_col <- start_columns[idx]
  
  if (idx < length(start_columns)) {
    next_form_label_col <- form_label_cols[idx + 1]
    end_col <- next_form_label_col - 1 
  } else {
    header_row_vals <- readWorkbook(wb, sheet = SHEET_NAME, rows = ORGANISM_HEADER_ROW, 
                                    colNames = FALSE, skipEmptyCols = FALSE)
    end_col <- max(which(!is.na(header_row_vals[1, ]) & header_row_vals[1, ] != ""))
  }
  
  matrix_cols <- (current_form_start_col - 1):end_col
  
  # --- EXTRACT METADATA ---
  meta_data <- list()
  for (meta_field in names(METADATA_ROWS)) {
    val <- suppressWarnings(readWorkbook(wb, sheet = SHEET_NAME, 
                                         rows = METADATA_ROWS[[meta_field]], cols = current_form_start_col, 
                                         colNames = FALSE, rowNames = FALSE))
    meta_data[[meta_field]] <- if(length(val) == 0 || is.na(val)) NA else as.character(val)
  }
  meta_df <- as.data.frame(meta_data, stringsAsFactors = FALSE)
  
  # --- EXTRACT SPECIES GRIDS ---
  raw_grid <- suppressWarnings(readWorkbook(wb, sheet = SHEET_NAME, 
                                            rows = ORGANISM_HEADER_ROW:ORGANISM_END_ROW, 
                                            cols = matrix_cols, 
                                            colNames = FALSE, rowNames = FALSE))
  
  if (is.null(raw_grid) || length(raw_grid) == 0 || nrow(raw_grid) <= 1) {
    next 
  }
  
  # Grab raw column headers
  raw_headers <- as.character(raw_grid[1, ])
  raw_headers[1] <- "Organism" # Only rename the FIRST column, keep the quadrat names!
  
  # Clean and isolate ONLY columns that have real names
  valid_col_indices <- which(!is.na(raw_headers) & raw_headers != "" & raw_headers != "NA")
  
  if (length(valid_col_indices) <= 1) {
    next 
  }
  
  counts_df <- raw_grid[-1, valid_col_indices, drop = FALSE]
  colnames(counts_df) <- raw_headers[valid_col_indices]
  
  # Filter out rows where the Organism name is missing
  counts_df <- counts_df %>% filter(!is.na(Organism) & Organism != "")
  
  if (nrow(counts_df) == 0) {
    next
  }
  
  # --- RESHAPE WIDE GRID TO LONG ROWS ---
  long_counts <- counts_df %>%
    pivot_longer(cols = -Organism, names_to = "quadrat_raw", values_to = "count") %>%
    mutate(
      count = ifelse(is.na(count) | count == "", NA, as.numeric(count)),
      clean_organism = str_to_lower(Organism) %>% 
        str_replace_all(" ", "_") %>% 
        str_replace_all("-", "_") %>%
        str_replace_all("spp", "sp"),
      
      # Standardize strings to lowercase dashes to make parsing bulletproof
      normalized_quadrat = str_to_lower(quadrat_raw) %>% str_replace_all(" ", "-"),
      
      # Strict Look-behind Regex matching to keep column extraction separate
      transect_number = str_extract(normalized_quadrat, "(?<=transect-)\\d+"),
      position_on_transect_m = str_extract(normalized_quadrat, "(?<=point-)\\d+\\.\\d+|(?<=point-)\\d+")
    )
  
  # Group duplicates and summarize counts
  long_counts_clean <- long_counts %>%
    filter(!is.na(transect_number) & !is.na(position_on_transect_m)) %>%
    group_by(transect_number, position_on_transect_m, clean_organism) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop')
  
  if (nrow(long_counts_clean) == 0) next
  
  # Pivot organisms back out horizontally into row columns
  wide_quadrats <- long_counts_clean %>%
    pivot_wider(names_from = clean_organism, values_from = count)
  
  # Merge metadata onto the rows
  combined_df <- cbind(meta_df, wide_quadrats)
  all_quadrats_list[[length(all_quadrats_list) + 1]] = combined_df
}

# Bind all variable forms into one vertical table frame
if (length(all_quadrats_list) == 0) {
  stop("The script skipped all forms. Please double-check your row indices and worksheet layout.")
}

# Coerce columns to character format uniformly before stacking to avoid bind errors
df_extracted <- bind_rows(map(all_quadrats_list, ~ .x %>% mutate(across(everything(), as.character))))

# ----------------------------------------------------------------------------
# 3. ALIGN AND FORMAT TO MATCH YOUR EXACT MASTER CSV SCHEMA
# ----------------------------------------------------------------------------
# This matches your layout specification down to the exact order provided.
master_headers <- c(
  "date", "location", "data_entry_by", "school", "transect_number", "position_on_transect_m",
  "acanthophora_spicifera", "amansia_sp", "articulated_coralline", "asparagopsis_sp",
  "astronema_breviarticulatum", "astronema_sp", "ahnfeltiopsis_sp", "avrainvillea_sp",
  "avrainvillea_lacerata", "bryopsis_sp", "bornatella_sphaerica", "caulerpa_racemosa",
  "cca", "cerramium_sp", "cerracia", "chaetomorpha_basiretrorsa", "chnoospora_sp",
  "cladophora_sp", "codium_sp", "codium_arabicum", "codium_edule", "codium_reediae",
  "colpomenia_sp", "colpomenia_sinuosa", "cyanobacteria", "dasya_iridescens",
  "dichotomaria_marginata", "dictyosphaeria_cavernosa", "dictyosphaeria_versluysii",
  "dictyosphaeria_sp", "dictyota_sp", "dictyota_acutiloba", "dictyota_sandvicensis",
  "dichtyopterous", "gelidiella_sp", "gracilaria_sp", "gracilaria_salicronia",
  "halimeda_sp", "hydroclathrus_clathratus", "ulva", "hypnea_sp", "jania_pumila",
  "jania_sp", "laurencia_sp", "laurencia_majuscula", "laurencia_mcdermidiae",
  "laurencia_nidifica", "liagora_sp", "lobophora_sp", "lobophora_obscura",
  "lyngbya_sp", "microdictyon_sp", "neomeris_sp", "portieria", "pterocladiella",
  "padina_sp", "sargassum_sp", "sargassum_aquifolium", "sargassum_polyphyllum",
  "sphaecelaria_sp", "symploca_hypnoides", "trichogloea_sp", "turbinaria_ornata",
  "valonia_sp", "turf", "cucumber_actinopyga", "hermet_crab", "crab", "echinometra",
  "snail_dendropoma_gregarium", "urchin", "cucumber_holothuria", "haminoea_cymbalum_green_snail",
  "pennaria_disticha_christmas_tree_hydroid", "pocillopora_damicornis", "sea_star",
  "snail_littoraria", "snail_nerite", "snail_serpulorbis", "snail_siphonaria_normalis",
  "sponge", "sand", "limestone", "bare_rock", "rubble", "pebble", "hole",
  "brown_crust", "unknown", "total_points", "x_x",
  "within_2m_proximity_of_attached_coral_colony_y_n_mamala_bay_only", "sediment_depth",
  "unknown_bulbous_algae", "team_leader", "team_members", "stressful_events",
  "low_tide_time", "low_tide_height", "x_in_field_group", "x_t_lines", "longest_t_m",
  "dist_between_t_pts_for_q", "x_quad_t", "start_time", "end_time", "weather",
  "rain_events", "wave_height", "wind_speed", "x_visitors", "x_fishing",
  "x_collecting", "what_collecting", "notes"
)

# Create the clean skeleton structural dataframe matching your headers explicitly
df_template <- as.data.frame(matrix(ncol = length(master_headers), nrow = 0), stringsAsFactors = FALSE)
colnames(df_template) <- master_headers
df_template[] <- lapply(df_template, as.character)

# Keep only columns from your raw data that actually belong in the master schema
matching_raw_cols <- intersect(names(df_extracted), master_headers)
df_extracted_clean <- df_extracted %>% select(all_of(matching_raw_cols))

# Inject data frames into template structure cleanly
df_final_master <- bind_rows(df_template, df_extracted_clean) %>%
  select(all_of(master_headers)) # Forces columns into your EXACT master schema blueprint order

# --- FIX EXCEL SERIAL FORMATS (DATES AND TIMES) ---
df_final_master <- df_final_master %>%
  mutate(
    # Fix the Excel Date serial numbers (e.g. 42911 -> 2017-06-25)
    date = if_else(str_detect(date, "^\\d+$"), 
                   as.character(openxlsx::convertToDate(as.numeric(date))), 
                   date),
    
    # FIX: Changed \. to \\. here
    start_time = if_else(str_detect(start_time, "^0\\.\\d+|^\\d+\\.\\d+"),
                         format(openxlsx::convertToDateTime(as.numeric(start_time)), "%H:%M:%S"),
                         start_time),
    
    # FIX: Changed \. to \\. here
    end_time = if_else(str_detect(end_time, "^0\\.\\d+|^\\d+\\.\\d+"),
                       format(openxlsx::convertToDateTime(as.numeric(end_time)), "%H:%M:%S"),
                       end_time)
  )
# --------------------------------------------------------

#Change zero species counts to NA
df_final_master <- df_final_master|>
  mutate(across(everything(), ~ ifelse(. == 0, NA, .)))

write.csv(df_final_master, MASTER_CSV_PATH, row.names = FALSE)
cat(sprintf("Success! Complete layout compiled. Master CSV written to: %s\n", MASTER_CSV_PATH))
