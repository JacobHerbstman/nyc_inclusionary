# ============================================================================
# download_shapefiles.R
# Purpose: Download NYC geographic shapefiles for spatial analysis
# ============================================================================

# Load packages (install if needed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  httr,
  jsonlite
)

cat("Downloading NYC shapefiles...\n")

# ----------------------------------------------------------------------------
# 1. NYC Community Districts
#    From NYC Open Data
# ----------------------------------------------------------------------------

cat("  1/6: Downloading community districts...\n")

cd_url <- "https://data.cityofnewyork.us/api/geospatial/yfnk-k7r4?method=export&format=GeoJSON"
tryCatch({
  cd <- st_read(cd_url, quiet = TRUE)
  cd <- cd %>%
    st_transform(2263) %>%  # NY State Plane (feet)
    st_make_valid()
  st_write(cd, "../output/nyc_community_districts.gpkg", delete_dsn = TRUE, quiet = TRUE)
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download community districts.\n")
  cat("    Error: ", e$message, "\n")
})

# ----------------------------------------------------------------------------
# 2. NYC Census Tracts (2020)
#    From NYC Open Data
# ----------------------------------------------------------------------------

cat("  2/6: Downloading census tracts (2020)...\n")

ct_url <- "https://data.cityofnewyork.us/api/geospatial/63ge-mke6?method=export&format=GeoJSON"
tryCatch({
  ct <- st_read(ct_url, quiet = TRUE)
  ct <- ct %>%
    st_transform(2263) %>%
    st_make_valid()
  st_write(ct, "../output/nyc_census_tracts_2020.gpkg", delete_dsn = TRUE, quiet = TRUE)
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download census tracts.\n")
  cat("    Error: ", e$message, "\n")
})

# ----------------------------------------------------------------------------
# 3. NYC Neighborhood Tabulation Areas (NTAs) 2020
#    From NYC Open Data
# ----------------------------------------------------------------------------

cat("  3/6: Downloading NTAs (2020)...\n")

nta_url <- "https://data.cityofnewyork.us/api/geospatial/9nt8-h7nd?method=export&format=GeoJSON"
tryCatch({
  nta <- st_read(nta_url, quiet = TRUE)
  nta <- nta %>%
    st_transform(2263) %>%
    st_make_valid()
  st_write(nta, "../output/nyc_ntas_2020.gpkg", delete_dsn = TRUE, quiet = TRUE)
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download NTAs.\n")
  cat("    Error: ", e$message, "\n")
})

# ----------------------------------------------------------------------------
# 4. NYC Borough Boundaries
#    From NYC Open Data
# ----------------------------------------------------------------------------

cat("  4/6: Downloading borough boundaries...\n")

boro_url <- "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON"
tryCatch({
  boro <- st_read(boro_url, quiet = TRUE)
  boro <- boro %>%
    st_transform(2263) %>%
    st_make_valid()
  st_write(boro, "../output/nyc_boroughs.gpkg", delete_dsn = TRUE, quiet = TRUE)
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download borough boundaries.\n")
  cat("    Error: ", e$message, "\n")
})

# ----------------------------------------------------------------------------
# 5. NYC City Council Districts
#    From NYC Open Data
# ----------------------------------------------------------------------------

cat("  5/6: Downloading city council districts...\n")

cc_url <- "https://data.cityofnewyork.us/api/geospatial/yusd-j4xi?method=export&format=GeoJSON"
tryCatch({
  cc <- st_read(cc_url, quiet = TRUE)
  cc <- cc %>%
    st_transform(2263) %>%
    st_make_valid()
  st_write(cc, "../output/nyc_city_council_districts.gpkg", delete_dsn = TRUE, quiet = TRUE)
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download city council districts.\n")
  cat("    Error: ", e$message, "\n")
})

# ----------------------------------------------------------------------------
# 6. NYC PUMA boundaries (for linking to ACS)
#    From Census Bureau via tigris
# ----------------------------------------------------------------------------

cat("  6/6: Downloading PUMAs...\n")

tryCatch({
  if (!require("tigris")) install.packages("tigris")
  library(tigris)
  options(tigris_use_cache = TRUE)
  
  pumas <- pumas(state = "NY", year = 2020, cb = TRUE)
  # Filter to NYC counties (Bronx=005, Kings=047, NY=061, Queens=081, Richmond=085)
  nyc_counties <- c("005", "047", "061", "081", "085")
  pumas_nyc <- pumas %>%
    filter(STATEFP20 == "36" & COUNTYFP20 %in% nyc_counties) %>%
    st_transform(2263) %>%
    st_make_valid()
  st_write(pumas_nyc, "../output/nyc_pumas_2020.gpkg", delete_dsn = TRUE, quiet = TRUE)
  cat("    Success!\n")
}, error = function(e) {
  cat("    Warning: Could not download PUMAs.\n")
  cat("    Error: ", e$message, "\n")
})

cat("Shapefile download complete!\n")

# Print summary
cat("\nDownloaded shapefiles:\n")
files <- list.files("../output", pattern = "\\.gpkg$", full.names = TRUE)
for (f in files) {
  info <- file.info(f)
  cat(sprintf("  %s (%.1f MB)\n", basename(f), info$size / 1e6))
}
