###2025 Data###

library(readxl) 
library(dplyr) 
library(tidyr) 
library(stringr) 
library(readr) 
library(here) 
library(janitor)

# 1. File Paths Setup
opihi_file_in  <- here("Nick", "Data", "OPIHI_data_2025.xlsx") 
opihi_file_out <- here("Nick", "Outputs", "OPIHI_data_2025_cleaned.xlsx") 

tab_names <- as.character(excel_sheets(opihi_file_in)) 

find_sheet <- function(kind, p) { 
  up <- str_to_upper(tab_names) 
  hit <- tab_names[str_detect(up, paste0("^", kind)) & str_detect(up, p)] 
  if (length(hit) != 1) {
    stop("Expected exactly 1 match for kind: ", kind, ", pattern: ", p, 
         ". Tabs in file: ", paste(tab_names, collapse = " | "))
  }
  hit 
} 

sh_t1 <- find_sheet("TRANSECT", "P1") 
sh_t2 <- find_sheet("TRANSECT", "P2") 
sh_q1 <- find_sheet("QUADRAT", "P1")  
sh_q2 <- find_sheet("QUADRAT", "P2")  


# 2. Parsing Functions

read_transect <- function(sheet) { 
  read_excel(opihi_file_in, sheet = sheet, range = "A4:I11") |> 
    rename(Point = 1, Location_m = 2) |> 
    mutate(across(everything(), as.character)) |>
    pivot_longer(-c(Point, Location_m), names_to = "Group", values_to = "Original") |> 
    filter(!is.na(Original)) |> 
    filter(!str_detect(str_squish(Original), "^[0-9.]+$")) |> 
    mutate(
      Point = as.character(Point),
      Location_m = suppressWarnings(as.numeric(Location_m)),
      Original = as.character(Original), 
      Method = "Transect", 
      Site = str_sub(Group, 1, 2), 
      Points = 1
    ) 
} 

read_quadrat <- function(sheet) { 
  read_excel(opihi_file_in, sheet = sheet, range = "A5:H40", 
             col_types = c("text", rep("guess", 7))) |> 
    rename(Original = 1) |> 
    filter(!str_detect(str_to_upper(Original), "TOTAL|PERCENT|%|CHECKER")) |>
    filter(!is.na(Original)) |> 
    filter(!str_detect(str_squish(Original), "^[0-9.]+$")) |> 
    mutate(across(-Original, as.character)) |> 
    pivot_longer(-Original, names_to = "Group", values_to = "Points") |> 
    mutate(
      Points = suppressWarnings(as.numeric(Points)),
      Points = replace_na(Points, 0)
    ) |> 
    filter(Points > 0) |> 
    mutate(
      Point = NA_character_,
      Method = "Quadrat", 
      Site = str_sub(Group, 1, 2), 
      Location_m = NA_real_
    ) 
} 


# 3. Process and Compile Data
stacked <- bind_rows( 
  read_transect(sh_t1), 
  read_transect(sh_t2), 
  read_quadrat(sh_q1), 
  read_quadrat(sh_q2) 
) 


# 4. Metadata Parsing (Names & Groups)
read_names_transect <- function(sheet) { 
  nm <- read_excel(opihi_file_in, sheet = sheet, range = "C3:I4", col_names = FALSE, col_types = "text") 
  tibble(
    Method = "Transect", 
    Group = as.character(unlist(nm)), 
    Recorder = as.character(unlist(nm)), 
    Checker_1 = NA_character_, 
    Checker_2 = NA_character_
  ) 
} 

read_names_quadrat <- function(sheet) { 
  nm <- read_excel(opihi_file_in, sheet = sheet, range = "B2:H5", col_names = FALSE, col_types = "text") 
  tibble(
    Method = "Quadrat", 
    Group = as.character(unlist(nm)), 
    Recorder = as.character(unlist(nm)), 
    Checker_1 = as.character(unlist(nm)), 
    Checker_2 = as.character(unlist(nm))) 
} 

names_tbl <- bind_rows( 
  read_names_transect(sh_t1), 
  read_names_transect(sh_t2), 
  read_names_quadrat(sh_q1), 
  read_names_quadrat(sh_q2) 
) |> 
  distinct(Method, Group, .keep_all = TRUE) |> 
  mutate(across(c(Recorder, Checker_1, Checker_2), str_squish)) 


# 5. Taxonomic Group Mapping & Label Cleaning
clean_map <- c( 
  "rock" = "Rock", "sand" = "Sand", "turf algae" = "Turf algae", "turf" = "Turf algae", 
  "padina spp" = "Padina spp.", "padina spp." = "Padina spp.", 
  "acanthophora spicifera" = "Acanthophora spicifera", "dendropoma gregarium" = "Dendropoma gregarium", 
  "sargassum polyphylum" = "Sargassum polyphyllum", "sargassum polyphyllum" = "Sargassum polyphyllum", 
  "saragassum polyphylum" = "Sargassum polyphyllum", "sargassum aquifolium" = "Sargassum aquifolium", 
  "sr aquifolium" = "Sargassum aquifolium", "turbinaria ornata" = "Turbinaria ornata", 
  "turbinania ornata" = "Turbinaria ornata", "neomeris" = "Neomeris spp.", 
  "neomeris spp." = "Neomeris spp.", "beomeris spp" = "Neomeris spp.", 
  "bornatella sphaerica" = "Bornetella sphaerica", "bornatella spaerica" = "Bornetella sphaerica", 
  "byropsis pennata" = "Bryopsis pennata", "caiclmus laevimanus" = "Calcinus laevimanus", 
  "halimeda discoidea" = "Halimeda discoidea", "halimeda discodia" = "Halimeda discoidea", 
  "avrainvilea sp." = "Avrainvillea sp.", "avrainvillea spp." = "Avrainvillea sp.", 
  "unknown snail" = "Unknown snail", "unknown fish" = "Unknown fish", "y" = "Unidentified" 
) 

substrates <- c("Rock", "Sand") 

clean_label <- function(x) { 
  key <- str_to_lower(str_squish(x)) 
  out <- unname(clean_map[key]) 
  if_else(is.na(out), str_squish(x), out) 
} 

stacked <- stacked |> 
  mutate(
    Label = clean_label(Original), 
    Type = if_else(Label %in% substrates, "Substrate", "Organism")
  ) |> 
  filter(!str_detect(str_squish(Label), "^[0-9.]+$"))


# 6. Aggregating and Mapping to requested Master File layout
opihi_long <- bind_rows( 
  stacked |> filter(Method == "Transect") |> mutate(Count = 1), 
  stacked |> filter(Method == "Quadrat") |> 
    group_by(Method, Site, Group, Label, Type) |> 
    summarise(
      Count = sum(Points), 
      Original = paste(unique(Original), collapse = "; "), 
      Point = NA_character_,
      Location_m = NA_real_, 
      .groups = "drop"
    ) 
) |> 
  left_join(names_tbl, by = c("Method", "Group")) |> 
  arrange(Method, Site, Group, Location_m, Label) |> 
  
  # Map variables to match Master File exactly
  mutate(
    date               = "2025-01-01",  
    year               = "2025",
    start_time         = NA_character_,
    stop_time          = NA_character_,
    site_name          = "Diamond Head",
    latitude           = NA_real_,
    longitude          = NA_real_,
    name_of_school     = "X high school",
    in_field_group     = NA_character_,
    group              = Group,
    team_members       = NA_character_,
    team_lead_or_recorder = Recorder,
    data_entry_by      = NA_character_,
    low_tide_time      = NA_character_,
    low_tide_height    = NA_character_,
    weather            = NA_character_,
    rain_events        = NA_character_,
    wave_height        = NA_character_,
    wind_speed         = NA_character_,
    stressful_events   = NA_character_,
    visitors           = NA_character_,
    fishing            = NA_character_,
    collecting         = NA_character_,
    what_collecting    = NA_character_,
    method             = Method,
    total_number_of_transect_lines  = NA_real_,
    length_of_transect_line         = NA_real_,
    longest_t_m                     = NA_real_,
    total_number_of_transect_points = NA_real_,
    dist_between_t_pts_for_q        = NA_character_,
    quad_t             = NA_real_,
    n_quadrats_per_transect         = NA_real_,
    point_spacing      = NA_character_,
    transect_id        = NA_character_,
    quadrat_id         = NA_character_,
    point_id           = Point,
    quadrat_location_m = Location_m,
    quadrat_total_points = NA_real_,
    p1_or_p2           = str_sub(Group, 1, 2), 
    organism_or_substrate_type = Label,
    category           = Type,
    count              = Count,
    within_2m_proximity_of_attached_coral_colony_y_n_mamala_bay_only = NA_character_,
    sediment_depth     = NA_real_,
    notes              = paste0("original ID = ", Original)
  ) |> 
  
  # NEW FILTER RULES
  # 1. Drop rows where organism type is missing or literally text "N/A"
  filter(!is.na(organism_or_substrate_type), 
         str_to_upper(str_squish(organism_or_substrate_type)) != "N/A") |> 
  # 2. Drop rows where the organism entry starts with "P2"
  filter(!str_detect(organism_or_substrate_type, "^P2")) |> 
  
  select(
    date, year, start_time, stop_time, site_name, latitude, longitude, 
    name_of_school, in_field_group, group, team_members, team_lead_or_recorder, 
    data_entry_by, low_tide_time, low_tide_height, weather, rain_events, 
    wave_height, wind_speed, stressful_events, visitors, fishing, collecting, 
    what_collecting, method, total_number_of_transect_lines, length_of_transect_line, 
    longest_t_m, total_number_of_transect_points, dist_between_t_pts_for_q, 
    quad_t, n_quadrats_per_transect, point_spacing, transect_id, quadrat_id, 
    point_id, quadrat_location_m, quadrat_total_points, p1_or_p2, 
    organism_or_substrate_type, category, count, 
    within_2m_proximity_of_attached_coral_colony_y_n_mamala_bay_only, 
    sediment_depth, notes
  )

glimpse(opihi_long) 
write_csv(opihi_long, here("Nick", "Data", "OPIHI_2025_long.csv"), na = "")

#bind 2025 data to master


# UPDATED: Targets your exact master file path
master_file_path <- here("Data", "Master_Output_Dataframe.csv")

if (file.exists(master_file_path)) {
  print("Master file found! Preserving original master column structures...")
  
  # 1. Read master file letting R detect its native types automatically
  master_df <- read_csv(master_file_path, show_col_types = FALSE)
  
  # 2. Match your new 2025 data types to match the master file exactly, column by column
  opihi_long_aligned <- opihi_long |> 
    mutate(across(everything(), as.character)) # Start as character to allow clean type conversion
  
  # Loop through columns and match types based on the master file profile
  for (col_name in names(master_df)) {
    if (col_name %in% names(opihi_long_aligned)) {
      
      # Detect master classes
      master_classes <- class(master_df[[col_name]])
      
      if (any(c("numeric", "integer", "double") %in% master_classes)) {
        opihi_long_aligned[[col_name]] <- suppressWarnings(as.numeric(opihi_long_aligned[[col_name]]))
      } else if ("logical" %in% master_classes) {
        opihi_long_aligned[[col_name]] <- suppressWarnings(as.logical(opihi_long_aligned[[col_name]]))
      } else if ("Date" %in% master_classes) {
        opihi_long_aligned[[col_name]] <- suppressWarnings(as.Date(opihi_long_aligned[[col_name]]))
      } else if (any(c("time", "hms", "difftime") %in% master_classes)) {
        # Force empty/character time placeholders to match the master's time class structure safely
        opihi_long_aligned[[col_name]] <- hms::as_hms(suppressWarnings(as.numeric(opihi_long_aligned[[col_name]])))
      } else {
        opihi_long_aligned[[col_name]] <- as.character(opihi_long_aligned[[col_name]])
      }
    }
  }
  
  # 3. Stack datasets together safely 
  combined_master <- bind_rows(master_df, opihi_long_aligned)
  
  # 4. Save the combined master tracking workbook
  # UPDATED: Writes output file using na = "NA" so missing blocks preserve explicit literal "NA" text strings
  write_csv(combined_master, here("Data", "Master_Output_Dataframe.csv"), na = "NA")
  print("Success! Combined master file written with structural types and NA values perfectly preserved.")
  
} else {
  warning("Could not find the master file at the specified path. Please check the file location.")
}

