# ============================================================================
# download_lottery_data.R
# Purpose: Download NYC HPD Housing Connect lottery data from NYC Open Data
# ============================================================================

# Load packages (install if needed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  httr,
  jsonlite,
  readxl,
  curl
)

cat("Downloading NYC Housing Connect lottery data...\n")

# ----------------------------------------------------------------------------
# 1. Advertised Lotteries on Housing Connect by Lottery
#    Dataset ID: vy5i-a666
#    Contains: lottery status, preferences, income levels, unit sizes
# ----------------------------------------------------------------------------

cat("  1/6: Downloading lotteries by lottery...\n")

lotteries_by_lottery_url <- "https://data.cityofnewyork.us/api/views/vy5i-a666/rows.csv?accessType=DOWNLOAD"
download.file(
  lotteries_by_lottery_url,
  destfile = "../output/advertised_lotteries_by_lottery.csv",
  mode = "wb"
)

# ----------------------------------------------------------------------------
# 2. Advertised Lotteries on Housing Connect by Building
#    Dataset ID: nibs-na6y
#    Contains: address, BBL, census tract, council district, income levels
# ----------------------------------------------------------------------------

cat("  2/6: Downloading lotteries by building...\n")

lotteries_by_building_url <- "https://data.cityofnewyork.us/api/views/nibs-na6y/rows.csv?accessType=DOWNLOAD"
download.file(
  lotteries_by_building_url,
  destfile = "../output/advertised_lotteries_by_building.csv",
  mode = "wb"
)

# ----------------------------------------------------------------------------
# 3. Affordable Housing Production by Building
#    Dataset ID: hg8x-zxpr
#    Contains: all Housing New York units with BBL, census tract, coordinates
# ----------------------------------------------------------------------------

cat("  3/6: Downloading affordable housing production by building...\n")

production_by_building_url <- "https://data.cityofnewyork.us/api/views/hg8x-zxpr/rows.csv?accessType=DOWNLOAD"
download.file(
  production_by_building_url,
  destfile = "../output/affordable_housing_production_by_building.csv",
  mode = "wb"
)

# ----------------------------------------------------------------------------
# 4. Affordable Housing Production by Project
#    Dataset ID: hq68-rnsi
#    Contains: project-level data including senior units
# ----------------------------------------------------------------------------

cat("  4/6: Downloading affordable housing production by project...\n")

production_by_project_url <- "https://data.cityofnewyork.us/api/views/hq68-rnsi/rows.csv?accessType=DOWNLOAD"
download.file(
  production_by_project_url,
  destfile = "../output/affordable_housing_production_by_project.csv",
  mode = "wb"
)

# ----------------------------------------------------------------------------
# 5. Local Law 217 Report (applications and leases by income/race/borough)
#    Note: This is an Excel file from HPD website
# ----------------------------------------------------------------------------

cat("  5/6: Downloading Local Law 217 report...\n")

# 2024 report
ll217_2024_url <- "https://www.nyc.gov/assets/hpd/downloads/excel/ll217-report.xlsx"
tryCatch({
  download.file(
    ll217_2024_url,
    destfile = "../output/local_law_217_report_2024.xlsx",
    mode = "wb"
  )
}, error = function(e) {
  cat("    Warning: Could not download 2024 LL217 report. Trying alternative URL...\n")
})

# 2021 report (backup)
ll217_2021_url <- "https://www.nyc.gov/assets/hpd/downloads/excel/local-law-217.xlsx"
tryCatch({
  download.file(
    ll217_2021_url,
    destfile = "../output/local_law_217_report_2021.xlsx",
    mode = "wb"
  )
}, error = function(e) {
  cat("    Warning: Could not download 2021 LL217 report.\n")
})

# ----------------------------------------------------------------------------
# 6. CDBG Eligibility by Census Tract
#    Contains tract-level low/mod income share and eligibility status
# ----------------------------------------------------------------------------

cat("  6/6: Downloading CDBG eligibility by census tract...\n")

cdbg_url <- "https://data.cityofnewyork.us/api/views/qmcw-ur37/rows.csv?accessType=DOWNLOAD"
tryCatch({
  download.file(
    cdbg_url,
    destfile = "../output/cdbg_eligibility_by_census_tract.csv",
    mode = "wb"
  )
}, error = function(e) {
  cat("    Warning: Could not download CDBG eligibility data.\n")
  cat("    Error: ", e$message, "\n")
})

cat("Lottery data download complete!\n")

# Print summary of downloaded files
cat("\nDownloaded files:\n")
files <- list.files("../output", pattern = "\\.(csv|xlsx)$", full.names = TRUE)
for (f in files) {
  info <- file.info(f)
  cat(sprintf("  %s (%.1f MB)\n", basename(f), info$size / 1e6))
}
