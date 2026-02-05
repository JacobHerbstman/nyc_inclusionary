# ============================================================================
# lottery_descriptives.R
# Purpose: Produce descriptive statistics for NYC Housing Connect lotteries
#          - Income distribution of units
#          - Income distribution of applicants vs. winners (via LL217)
#          - Geographic distribution of affordable housing
#          - Competition ratios by income band
# ============================================================================

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  readxl,
  scales,
  knitr,
  kableExtra,
  ggplot2,
  viridis
)

# Set theme for plots
theme_set(theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  ))

cat("=== NYC Housing Connect Lottery Descriptives ===\n\n")

# ============================================================================
# 1. LOAD DATA
# ============================================================================

cat("Loading data...\n")

# HPD Advertised Lotteries (July 2020+)
lotteries_by_lottery <- read_csv("../input/advertised_lotteries_by_lottery.csv", 
                                  show_col_types = FALSE)
lotteries_by_building <- read_csv("../input/advertised_lotteries_by_building.csv",
                                   show_col_types = FALSE)

# Affordable Housing Production (2014+)
production <- read_csv("../input/affordable_housing_production_by_building.csv",
                       show_col_types = FALSE)

# The City NY data (2014-2019)
thecity_lotteries <- read_csv("../input/thecity_housing_lotteries.csv",
                               show_col_types = FALSE)

# Check if applications file exists and isn't too large
if (file.exists("../input/thecity_lottery_applications.csv")) {
  apps_info <- file.info("../input/thecity_lottery_applications.csv")
  cat(sprintf("  Applications file: %.1f MB\n", apps_info$size / 1e6))
  
  # If file is reasonable size, load it; otherwise sample
  if (apps_info$size < 500e6) {  # Less than 500MB
    thecity_apps <- read_csv("../input/thecity_lottery_applications.csv",
                              show_col_types = FALSE)
  } else {
    cat("  Applications file too large, loading sample...\n")
    thecity_apps <- read_csv("../input/thecity_lottery_applications.csv",
                              n_max = 1e6, show_col_types = FALSE)
  }
}

# Local Law 217 report
ll217_path <- "../input/local_law_217_report_2024.xlsx"
if (file.exists(ll217_path)) {
  ll217_sheets <- excel_sheets(ll217_path)
  cat("  LL217 sheets:", paste(ll217_sheets, collapse = ", "), "\n")
}

# Shapefiles
if (file.exists("../input/nyc_census_tracts_2020.gpkg")) {
  census_tracts <- st_read("../input/nyc_census_tracts_2020.gpkg", quiet = TRUE)
}
if (file.exists("../input/nyc_boroughs.gpkg")) {
  boroughs <- st_read("../input/nyc_boroughs.gpkg", quiet = TRUE)
}

cat("\nData loaded successfully.\n\n")

# ============================================================================
# 2. EXPLORE DATA STRUCTURE
# ============================================================================

cat("=== Data Structure ===\n\n")

cat("Lotteries by Lottery (HPD Open Data, July 2020+):\n")
cat(sprintf("  Rows: %s, Columns: %d\n", comma(nrow(lotteries_by_lottery)), ncol(lotteries_by_lottery)))
cat("  Columns:", paste(names(lotteries_by_lottery), collapse = ", "), "\n\n")

cat("Lotteries by Building (HPD Open Data, July 2020+):\n")
cat(sprintf("  Rows: %s, Columns: %d\n", comma(nrow(lotteries_by_building)), ncol(lotteries_by_building)))
cat("  Columns:", paste(names(lotteries_by_building), collapse = ", "), "\n\n")

cat("Affordable Housing Production (HPD, 2014+):\n")
cat(sprintf("  Rows: %s, Columns: %d\n", comma(nrow(production)), ncol(production)))
cat("  Columns:", paste(names(production), collapse = ", "), "\n\n")

cat("The City NY Lotteries (2014-2019):\n")
cat(sprintf("  Rows: %s, Columns: %d\n", comma(nrow(thecity_lotteries)), ncol(thecity_lotteries)))
cat("  Columns:", paste(names(thecity_lotteries), collapse = ", "), "\n\n")

if (exists("thecity_apps")) {
  cat("The City NY Applications:\n")
  cat(sprintf("  Rows: %s, Columns: %d\n", comma(nrow(thecity_apps)), ncol(thecity_apps)))
  cat("  Columns:", paste(names(thecity_apps), collapse = ", "), "\n\n")
}

# Save column names for reference
sink("../output/data_columns.txt")
cat("=== Column Names by Dataset ===\n\n")
cat("lotteries_by_lottery:\n")
cat(paste("  -", names(lotteries_by_lottery)), sep = "\n")
cat("\n\nlotteries_by_building:\n")
cat(paste("  -", names(lotteries_by_building)), sep = "\n")
cat("\n\nproduction:\n")
cat(paste("  -", names(production)), sep = "\n")
cat("\n\nthecity_lotteries:\n")
cat(paste("  -", names(thecity_lotteries)), sep = "\n")
if (exists("thecity_apps")) {
  cat("\n\nthecity_apps:\n")
  cat(paste("  -", names(thecity_apps)), sep = "\n")
}
sink()

# ============================================================================
# 3. INCOME DISTRIBUTION OF LOTTERY UNITS (HPD Data)
# ============================================================================

cat("=== Income Distribution of Lottery Units ===\n\n")

# Look for income-related columns in the HPD data
income_cols <- names(lotteries_by_lottery)[grepl("income|ami|eli|vli|li|mi", 
                                                   names(lotteries_by_lottery), 
                                                   ignore.case = TRUE)]
cat("Income-related columns in lotteries_by_lottery:\n")
cat(paste("  -", income_cols), sep = "\n")
cat("\n")

# Try to identify unit counts by income band
# Common column patterns: "Extremely_Low_Income_Units", "Very_Low_Income_Units", etc.
unit_cols <- names(lotteries_by_lottery)[grepl("unit", names(lotteries_by_lottery), ignore.case = TRUE)]
cat("Unit-related columns:\n")
cat(paste("  -", unit_cols), sep = "\n")
cat("\n")

# Attempt to summarize units by income band if columns exist
# This will be dataset-specific - adjust based on actual column names
tryCatch({
  # Check what we actually have
  head(lotteries_by_lottery) %>% 
    select(any_of(c(income_cols, unit_cols))) %>%
    print()
  
  # If we have income band columns, summarize
  if (length(income_cols) > 0) {
    income_summary <- lotteries_by_lottery %>%
      summarise(across(any_of(income_cols), ~sum(.x, na.rm = TRUE)))
    
    cat("\nTotal units by income category (if available):\n")
    print(income_summary)
  }
}, error = function(e) {
  cat("  Note: Income column structure may differ from expected.\n")
  cat("  Error:", e$message, "\n")
})

# ============================================================================
# 4. GEOGRAPHIC DISTRIBUTION (using production data)
# ============================================================================

cat("\n=== Geographic Distribution of Affordable Housing ===\n\n")

# Summarize by borough
if ("Borough" %in% names(production) | "borough" %in% names(production)) {
  boro_col <- ifelse("Borough" %in% names(production), "Borough", "borough")
  
  boro_summary <- production %>%
    group_by(!!sym(boro_col)) %>%
    summarise(
      n_buildings = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(n_buildings))
  
  cat("Buildings by Borough:\n")
  print(boro_summary)
  
  write_csv(boro_summary, "../output/production_by_borough.csv")
}

# Summarize by community district if available
cd_col <- names(production)[grepl("community", names(production), ignore.case = TRUE)][1]
if (!is.na(cd_col)) {
  cd_summary <- production %>%
    group_by(!!sym(cd_col)) %>%
    summarise(
      n_buildings = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(n_buildings)) %>%
    head(20)
  
  cat("\nTop 20 Community Districts by Building Count:\n")
  print(cd_summary)
  
  write_csv(cd_summary, "../output/production_by_community_district.csv")
}

# ============================================================================
# 5. THE CITY NY DATA: Competition Ratios by Income Band
# ============================================================================

cat("\n=== Competition Analysis (The City NY Data 2014-2019) ===\n\n")

cat("Lottery data columns:\n")
print(names(thecity_lotteries))
cat("\n")

# Preview the lottery data
cat("Sample of lottery data:\n")
print(head(thecity_lotteries, 10))

# If we have applications data, analyze competition
if (exists("thecity_apps")) {
  cat("\nApplications data columns:\n")
  print(names(thecity_apps))
  
  cat("\nSample of applications:\n")
  print(head(thecity_apps, 10))
  
  # Basic application statistics
  app_stats <- thecity_apps %>%
    summarise(
      total_applications = n(),
      unique_lotteries = n_distinct(across(any_of(c("lottery_id", "project_id", "lottery"))))
    )
  
  cat("\nApplication Statistics:\n")
  print(app_stats)
}

# ============================================================================
# 6. LOCAL LAW 217: Applications and Leases by Demographics
# ============================================================================

cat("\n=== Local Law 217 Report Analysis ===\n\n")

if (file.exists(ll217_path)) {
  # Read each sheet and explore
  for (sheet in ll217_sheets) {
    cat(sprintf("\n--- Sheet: %s ---\n", sheet))
    tryCatch({
      df <- read_excel(ll217_path, sheet = sheet)
      cat(sprintf("  Dimensions: %d rows x %d columns\n", nrow(df), ncol(df)))
      cat("  Columns:", paste(head(names(df), 10), collapse = ", "), "...\n")
      
      # Save each sheet as CSV for easier inspection
      safe_name <- gsub("[^A-Za-z0-9]", "_", sheet)
      write_csv(df, sprintf("../output/ll217_%s.csv", safe_name))
    }, error = function(e) {
      cat("  Could not read sheet:", e$message, "\n")
    })
  }
}

# ============================================================================
# 7. CREATE SUMMARY TABLES
# ============================================================================

cat("\n=== Creating Summary Tables ===\n\n")

# Summary of all data sources
data_summary <- tibble(
  Dataset = c(
    "HPD Lotteries by Lottery",
    "HPD Lotteries by Building", 
    "Affordable Housing Production",
    "The City NY Lotteries",
    if(exists("thecity_apps")) "The City NY Applications" else NULL
  ),
  Rows = c(
    nrow(lotteries_by_lottery),
    nrow(lotteries_by_building),
    nrow(production),
    nrow(thecity_lotteries),
    if(exists("thecity_apps")) nrow(thecity_apps) else NULL
  ),
  Columns = c(
    ncol(lotteries_by_lottery),
    ncol(lotteries_by_building),
    ncol(production),
    ncol(thecity_lotteries),
    if(exists("thecity_apps")) ncol(thecity_apps) else NULL
  ),
  Time_Period = c(
    "July 2020+",
    "July 2020+",
    "Jan 2014+",
    "2014-2019",
    if(exists("thecity_apps")) "2014-2019" else NULL
  )
)

write_csv(data_summary, "../output/data_summary.csv")

cat("Data Summary:\n")
print(data_summary)

# ============================================================================
# 8. QUICK VISUALIZATION: Map of Affordable Housing
# ============================================================================

cat("\n=== Creating Visualizations ===\n\n")

# Check if we have coordinates in production data
coord_cols <- names(production)[grepl("lat|lon|latitude|longitude", 
                                       names(production), ignore.case = TRUE)]
cat("Coordinate columns found:", paste(coord_cols, collapse = ", "), "\n")

if (length(coord_cols) >= 2) {
  lat_col <- coord_cols[grepl("lat", coord_cols, ignore.case = TRUE)][1]
  lon_col <- coord_cols[grepl("lon", coord_cols, ignore.case = TRUE)][1]
  
  if (!is.na(lat_col) & !is.na(lon_col)) {
    # Create simple map
    production_sf <- production %>%
      filter(!is.na(!!sym(lat_col)) & !is.na(!!sym(lon_col))) %>%
      st_as_sf(coords = c(lon_col, lat_col), crs = 4326)
    
    if (exists("boroughs")) {
      p <- ggplot() +
        geom_sf(data = boroughs %>% st_transform(4326), 
                fill = "gray95", color = "gray50") +
        geom_sf(data = production_sf, alpha = 0.3, size = 0.5, color = "steelblue") +
        labs(
          title = "Affordable Housing Production in NYC",
          subtitle = "Housing New York & Housing Our Neighbors (2014+)",
          caption = "Source: NYC HPD Open Data"
        ) +
        theme_void() +
        theme(plot.title = element_text(face = "bold"))
      
      ggsave("../output/map_affordable_housing.png", p, width = 10, height = 12, dpi = 150)
      cat("  Saved: map_affordable_housing.png\n")
    }
  }
}

# ============================================================================
# 9. FINAL OUTPUT
# ============================================================================

cat("\n=== Analysis Complete ===\n\n")

cat("Output files created:\n")
output_files <- list.files("../output", full.names = FALSE)
for (f in output_files) {
  info <- file.info(file.path("../output", f))
  cat(sprintf("  %s (%.1f KB)\n", f, info$size / 1e3))
}

cat("\nNext steps for full analysis:\n")
cat("  1. Review data_columns.txt to understand variable structure\n")
cat("  2. Examine ll217_*.csv files for income/race breakdowns\n")
cat("  3. Link lottery buildings to census tract characteristics\n")
cat("  4. Calculate neighborhood quality metrics for treatment buildings\n")

cat("\nDone!\n")
