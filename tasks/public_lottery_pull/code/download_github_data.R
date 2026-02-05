# ============================================================================
# download_github_data.R
# Purpose: Download The City NY lottery analysis data from GitHub
#          This contains 426 lotteries (2014-2019) and 20M+ applications
# ============================================================================

# Load packages (install if needed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  httr
)

cat("Downloading The City NY lottery data from GitHub...\n")

# Base URL for raw GitHub content
base_url <- "https://raw.githubusercontent.com/thecityny/housing-lottery-data/master/"

# ----------------------------------------------------------------------------
# 1. Housing Lotteries (426 lotteries, 2014-2019)
#    Contains: 1,888 unit groups at different income levels
# ----------------------------------------------------------------------------

cat("  1/3: Downloading housing lotteries...\n")

lotteries_url <- paste0(base_url, "housing-lotteries.csv")
tryCatch({
  download.file(
    lotteries_url,
    destfile = "../output/thecity_housing_lotteries.csv",
    mode = "wb"
  )
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download housing lotteries.\n")
  cat("    Error: ", e$message, "\n")
})

# ----------------------------------------------------------------------------
# 2. Lottery Applications (20M+ applications)
#    WARNING: This file may be very large
# ----------------------------------------------------------------------------

cat("  2/3: Downloading lottery applications (may take a while)...\n")

applications_url <- paste0(base_url, "lottery-applications.csv")
tryCatch({
  download.file(
    applications_url,
    destfile = "../output/thecity_lottery_applications.csv",
    mode = "wb"
  )
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download lottery applications.\n")
  cat("    Error: ", e$message, "\n")
})

# ----------------------------------------------------------------------------
# 3. README/Methodology
# ----------------------------------------------------------------------------

cat("  3/3: Downloading methodology documentation...\n")

readme_url <- paste0(base_url, "README.md")
tryCatch({
  download.file(
    readme_url,
    destfile = "../output/thecity_methodology.md",
    mode = "wb"
  )
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download methodology.\n")
})

cat("GitHub data download complete!\n")

# Print summary
cat("\nDownloaded files:\n")
files <- list.files("../output", pattern = "thecity", full.names = TRUE)
for (f in files) {
  info <- file.info(f)
  cat(sprintf("  %s (%.1f MB)\n", basename(f), info$size / 1e6))
}

# Quick preview of the data
cat("\n--- Quick Data Preview ---\n")

if (file.exists("../output/thecity_housing_lotteries.csv")) {
  lotteries <- read_csv("../output/thecity_housing_lotteries.csv", show_col_types = FALSE)
  cat(sprintf("\nHousing Lotteries: %d rows, %d columns\n", nrow(lotteries), ncol(lotteries)))
  cat("Columns:", paste(names(lotteries), collapse = ", "), "\n")
}

if (file.exists("../output/thecity_lottery_applications.csv")) {
  # Just read first few rows to check structure
  apps_preview <- read_csv("../output/thecity_lottery_applications.csv", 
                           n_max = 100, show_col_types = FALSE)
  cat(sprintf("\nLottery Applications: ~20M rows, %d columns\n", ncol(apps_preview)))
  cat("Columns:", paste(names(apps_preview), collapse = ", "), "\n")
}
