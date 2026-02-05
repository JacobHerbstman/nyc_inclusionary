# ============================================================================
# lottery_descriptives.R
# Purpose: Produce reproducible descriptive statistics for public NYC lottery
#          data, including unit characteristics, applicant/lease income mixes,
#          and tract-level socioeconomic context.
# ============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readxl,
  lubridate,
  scales
)

clean_count <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "<NA>", "###")] <- NA_character_
  readr::parse_number(x)
}

norm_text <- function(x) {
  stringr::str_to_lower(stringr::str_squish(dplyr::coalesce(as.character(x), "")))
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

extract_ll217_distribution <- function(
    sheet_df,
    section_titles,
    header_col2,
    header_col3,
    primary_labels = c("nyc resident", "nyc"),
    secondary_labels = c("unknown/outside nyc", "not a nyc resident")) {
  c1 <- norm_text(sheet_df[[1]])
  section_idx <- NA_integer_

  for (title in section_titles) {
    idx <- which(c1 == norm_text(title))[1]
    if (!is.na(idx)) {
      section_idx <- idx
      break
    }
  }

  if (is.na(section_idx)) {
    return(tibble())
  }

  window_idx <- section_idx:min(section_idx + 60, nrow(sheet_df))
  sub <- sheet_df[window_idx, , drop = FALSE]

  header_rel <- which(
    norm_text(sub[[2]]) == norm_text(header_col2) &
      norm_text(sub[[3]]) == norm_text(header_col3)
  )[1]

  if (is.na(header_rel)) {
    return(tibble())
  }

  header_idx <- window_idx[header_rel]
  categories <- as.character(unlist(sheet_df[header_idx, 2:ncol(sheet_df)], use.names = FALSE))

  candidate_rows <- seq(header_idx + 1, min(header_idx + 8, nrow(sheet_df)))
  candidate_labels <- norm_text(unlist(sheet_df[candidate_rows, 1], use.names = FALSE))

  primary_rel <- which(candidate_labels %in% norm_text(primary_labels))[1]
  if (is.na(primary_rel)) {
    primary_rel <- 1
  }

  secondary_rel <- which(candidate_labels %in% norm_text(secondary_labels))[1]
  if (is.na(secondary_rel)) {
    secondary_rel <- min(2, length(candidate_rows))
  }

  primary_idx <- candidate_rows[primary_rel]
  secondary_idx <- candidate_rows[secondary_rel]

  primary_vals <- clean_count(unlist(sheet_df[primary_idx, 2:ncol(sheet_df)], use.names = FALSE))
  secondary_vals <- clean_count(unlist(sheet_df[secondary_idx, 2:ncol(sheet_df)], use.names = FALSE))

  tibble(
    category = categories,
    nyc_count = primary_vals,
    non_nyc_count = secondary_vals,
    total_count = dplyr::coalesce(primary_vals, 0) + dplyr::coalesce(secondary_vals, 0)
  ) %>%
    filter(!is.na(category)) %>%
    mutate(category = stringr::str_squish(category)) %>%
    filter(category != "")
}

cat("=== NYC Public Lottery Descriptive Analysis ===\n\n")

# ----------------------------------------------------------------------------
# 1. Load core datasets
# ----------------------------------------------------------------------------

lotteries <- read_csv("../input/advertised_lotteries_by_lottery.csv", show_col_types = FALSE)
lotteries_by_building <- read_csv("../input/advertised_lotteries_by_building.csv", show_col_types = FALSE)
production <- read_csv("../input/affordable_housing_production_by_building.csv", show_col_types = FALSE)
cdbg <- read_csv("../input/cdbg_eligibility_by_census_tract.csv", show_col_types = FALSE)
ll217_income_raw <- read_excel("../input/local_law_217_report_2024.xlsx", sheet = "Income", col_names = FALSE)
ll217_race_raw <- read_excel("../input/local_law_217_report_2024.xlsx", sheet = "Race & Ethnicity", col_names = FALSE)

lotteries <- lotteries %>%
  mutate(
    lottery_start_date = mdy(`Lottery Start Date`),
    lottery_end_date = mdy(`Lottery End Date`)
  )

production <- production %>%
  mutate(
    project_start_date = mdy(`Project Start Date`),
    project_completion_date = mdy(`Project Completion Date`),
    building_completion_date = mdy(`Building Completion Date`)
  )

# ----------------------------------------------------------------------------
# 2. Coverage and year ranges
# ----------------------------------------------------------------------------

coverage_summary <- tibble(
  dataset = c(
    "Advertised Lotteries by Lottery",
    "Advertised Lotteries by Building",
    "Affordable Housing Production by Building",
    "Local Law 217 Report (Income/Race tabs)",
    "CDBG Eligibility by Census Tract"
  ),
  rows = c(
    nrow(lotteries),
    nrow(lotteries_by_building),
    nrow(production),
    nrow(ll217_income_raw),
    nrow(cdbg)
  ),
  columns = c(
    ncol(lotteries),
    ncol(lotteries_by_building),
    ncol(production),
    ncol(ll217_income_raw),
    ncol(cdbg)
  ),
  min_date = c(
    as.character(min(lotteries$lottery_start_date, na.rm = TRUE)),
    NA_character_,
    as.character(min(production$project_start_date, na.rm = TRUE)),
    NA_character_,
    NA_character_
  ),
  max_date = c(
    as.character(max(lotteries$lottery_end_date, na.rm = TRUE)),
    NA_character_,
    as.character(max(production$building_completion_date, na.rm = TRUE)),
    NA_character_,
    NA_character_
  ),
  min_year = c(
    min(year(lotteries$lottery_start_date), na.rm = TRUE),
    NA_real_,
    min(year(production$project_start_date), na.rm = TRUE),
    NA_real_,
    NA_real_
  ),
  max_year = c(
    max(year(lotteries$lottery_end_date), na.rm = TRUE),
    NA_real_,
    max(year(production$building_completion_date), na.rm = TRUE),
    NA_real_,
    NA_real_
  ),
  notes = c(
    "Housing Connect advertised lotteries with start/end dates.",
    "Building-level listing records; no listing date column.",
    "HPD production records beginning in 2014.",
    "Aggregated report tables (no row-level event date column).",
    "NYC Open Data tract-level low/mod income indicators."
  )
)

write_csv(coverage_summary, "../output/coverage_summary.csv")

# ----------------------------------------------------------------------------
# 3. Lottery unit characteristics
# ----------------------------------------------------------------------------

unit_overview <- tibble(
  metric = c(
    "n_lotteries",
    "total_listed_units",
    "median_units_per_lottery",
    "mean_units_per_lottery",
    "total_building_count_records"
  ),
  value = c(
    nrow(lotteries),
    sum(lotteries$`Unit Count`, na.rm = TRUE),
    median(lotteries$`Unit Count`, na.rm = TRUE),
    mean(lotteries$`Unit Count`, na.rm = TRUE),
    sum(lotteries$`Building Count`, na.rm = TRUE)
  )
)

write_csv(unit_overview, "../output/unit_characteristics_overview.csv")

lotteries_by_status <- lotteries %>%
  mutate(`Lottery Status` = dplyr::coalesce(`Lottery Status`, "Unknown")) %>%
  group_by(`Lottery Status`) %>%
  summarise(
    lotteries = n(),
    listed_units = sum(`Unit Count`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(unit_share = listed_units / sum(listed_units, na.rm = TRUE)) %>%
  arrange(desc(listed_units))

write_csv(lotteries_by_status, "../output/lotteries_by_status.csv")

lotteries_by_borough <- lotteries %>%
  mutate(Borough = dplyr::coalesce(Borough, "Unknown")) %>%
  group_by(Borough) %>%
  summarise(
    lotteries = n(),
    listed_units = sum(`Unit Count`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(unit_share = listed_units / sum(listed_units, na.rm = TRUE)) %>%
  arrange(desc(listed_units))

write_csv(lotteries_by_borough, "../output/lotteries_by_borough.csv")

units_by_income_band <- tibble(
  income_band = c(
    "Extremely Low Income",
    "Very Low Income",
    "Low Income",
    "Moderate Income",
    "Middle Income"
  ),
  column_name = c(
    "Applied Income AMI Category - Extremely Low Income",
    "Applied Income AMI Category - Very Low Income",
    "Applied Income AMI Category - Low Income",
    "Applied Income AMI Category - Moderate Income",
    "Applied Income AMI Category - Middle Income"
  )
) %>%
  mutate(units = purrr::map_dbl(column_name, ~ sum(lotteries[[.x]], na.rm = TRUE))) %>%
  mutate(unit_share = units / sum(units, na.rm = TRUE))

write_csv(units_by_income_band, "../output/units_by_income_band.csv")

units_by_bedroom <- tibble(
  bedroom_type = c("Studio", "1 Bedroom", "2 Bedrooms", "3 Bedrooms", "4+ Bedrooms"),
  column_name = c(
    "Unit Distribution - Studio",
    "Unit Distribution - 1 Bedroom",
    "Unit Distribution - 2 Bedrooms",
    "Unit Distribution - 3 Bedrooms",
    "Unit Distribution - 4 Bedroom+"
  )
) %>%
  mutate(units = purrr::map_dbl(column_name, ~ sum(lotteries[[.x]], na.rm = TRUE))) %>%
  mutate(unit_share = units / sum(units, na.rm = TRUE))

write_csv(units_by_bedroom, "../output/units_by_bedroom.csv")

lottery_year_summary <- lotteries %>%
  mutate(start_year = year(lottery_start_date)) %>%
  filter(!is.na(start_year)) %>%
  group_by(start_year) %>%
  summarise(
    lotteries = n(),
    listed_units = sum(`Unit Count`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(start_year)

write_csv(lottery_year_summary, "../output/lottery_year_summary.csv")

# ----------------------------------------------------------------------------
# 4. LL217 applicant vs signed-lease distributions
# ----------------------------------------------------------------------------

income_sections <- tribble(
  ~stage, ~section_title,
  "applications", "Applications Overview - Income",
  "requested_documents", "Invited to Confirm Eligibility Overview - Income",
  "selected_for_tenant_selection", "Selected Applications Overview - Income",
  "signed_leases", "Applicant Signed Lease Overview - Income"
)

income_stage_long <- purrr::pmap_dfr(
  income_sections,
  function(stage, section_title) {
    extract_ll217_distribution(
      sheet_df = ll217_income_raw,
      section_titles = c(section_title),
      header_col2 = "Extremely Low Income",
      header_col3 = "Very Low Income"
    ) %>%
      mutate(stage = stage)
  }
)

if (nrow(income_stage_long) == 0) {
  stop("Could not parse LL217 income sections.")
}

income_stage_long <- income_stage_long %>%
  select(stage, category, nyc_count, non_nyc_count, total_count)

write_csv(income_stage_long, "../output/income_pipeline_counts.csv")

income_stage_totals <- income_stage_long %>%
  group_by(stage) %>%
  summarise(total_count = sum(total_count, na.rm = TRUE), .groups = "drop")

write_csv(income_stage_totals, "../output/income_pipeline_stage_totals.csv")

income_applications <- income_stage_long %>%
  filter(stage == "applications") %>%
  select(category, applications = total_count)

income_signed <- income_stage_long %>%
  filter(stage == "signed_leases") %>%
  select(category, signed_leases = total_count)

income_applicant_vs_lease <- income_applications %>%
  left_join(income_signed, by = "category") %>%
  mutate(
    applications = dplyr::coalesce(applications, 0),
    signed_leases = dplyr::coalesce(signed_leases, 0),
    applications_share = applications / sum(applications, na.rm = TRUE),
    signed_leases_share = signed_leases / sum(signed_leases, na.rm = TRUE),
    leases_per_1000_apps = if_else(applications > 0, 1000 * signed_leases / applications, NA_real_)
  ) %>%
  arrange(desc(applications))

write_csv(income_applicant_vs_lease, "../output/income_applicant_vs_lease.csv")

race_applications <- extract_ll217_distribution(
  sheet_df = ll217_race_raw,
  section_titles = c("Applications Overview - Race/Ethnicity"),
  header_col2 = "American-Indian or Native Alaskan",
  header_col3 = "Asian"
) %>%
  select(category, applications = total_count)

race_signed <- extract_ll217_distribution(
  sheet_df = ll217_race_raw,
  section_titles = c("Signed Leases Overview - Race/Ethnicity"),
  header_col2 = "American-Indian or Native Alaskan",
  header_col3 = "Asian"
) %>%
  select(category, signed_leases = total_count)

race_applicant_vs_lease <- race_applications %>%
  left_join(race_signed, by = "category") %>%
  mutate(
    applications = dplyr::coalesce(applications, 0),
    signed_leases = dplyr::coalesce(signed_leases, 0),
    applications_share = applications / sum(applications, na.rm = TRUE),
    signed_leases_share = signed_leases / sum(signed_leases, na.rm = TRUE)
  ) %>%
  arrange(desc(applications))

write_csv(race_applicant_vs_lease, "../output/race_applicant_vs_lease.csv")

# ----------------------------------------------------------------------------
# 5. Unit location socioeconomic context (CDBG tract data)
# ----------------------------------------------------------------------------

borough_codes <- c(MN = "1", BX = "2", BK = "3", QN = "4", SI = "5")

lottery_building_geo <- lotteries_by_building %>%
  mutate(
    borough_code = dplyr::recode(Borough, !!!borough_codes, .default = NA_character_),
    tract_numeric = suppressWarnings(as.integer(`Census Tract (2020)`)),
    tract_code = if_else(!is.na(tract_numeric), sprintf("%06d", tract_numeric), NA_character_),
    boroct = if_else(!is.na(borough_code) & !is.na(tract_code), paste0(borough_code, tract_code), NA_character_),
    listed_units = as.numeric(`Unit Count`)
  )

cdbg <- cdbg %>%
  rename_with(tolower) %>%
  transmute(
    boroct = stringr::str_pad(as.character(ct_text), width = 7, pad = "0"),
    eligibility = as.character(eligibility),
    lomod_pct = as.numeric(lomod_pct),
    lowmod_population = as.numeric(lowmod_population),
    total_population = as.numeric(totalpop)
  ) %>%
  distinct(boroct, .keep_all = TRUE)

lottery_location_joined <- lottery_building_geo %>%
  left_join(cdbg, by = "boroct")

tract_join_coverage <- tibble(
  metric = c(
    "building_records_total",
    "building_records_with_tract_key",
    "building_records_matched_to_cdbg",
    "share_of_listed_units_with_cdbg_match"
  ),
  value = c(
    nrow(lottery_location_joined),
    sum(!is.na(lottery_location_joined$boroct)),
    sum(!is.na(lottery_location_joined$lomod_pct)),
    sum(lottery_location_joined$listed_units[!is.na(lottery_location_joined$lomod_pct)], na.rm = TRUE) /
      sum(lottery_location_joined$listed_units, na.rm = TRUE)
  )
)

write_csv(tract_join_coverage, "../output/tract_join_coverage.csv")

location_sociodemographics <- lottery_location_joined %>%
  filter(!is.na(eligibility)) %>%
  group_by(eligibility) %>%
  summarise(
    weighted_lomod_pct = weighted_mean_safe(lomod_pct, listed_units),
    buildings = n(),
    listed_units = sum(listed_units, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(unit_share = listed_units / sum(listed_units, na.rm = TRUE)) %>%
  arrange(desc(listed_units))

write_csv(location_sociodemographics, "../output/location_sociodemographics.csv")

borough_lomod_summary <- lottery_location_joined %>%
  filter(!is.na(lomod_pct)) %>%
  group_by(Borough) %>%
  summarise(
    weighted_lomod_pct = weighted_mean_safe(lomod_pct, listed_units),
    buildings = n(),
    listed_units = sum(listed_units, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(listed_units))

write_csv(borough_lomod_summary, "../output/borough_lomod_summary.csv")

# ----------------------------------------------------------------------------
# 6. Check The City data availability (GitHub source)
# ----------------------------------------------------------------------------

thecity_path <- "../../public_lottery_pull/output/thecity_housing_lotteries.csv"
thecity_status <- "missing"
if (file.exists(thecity_path)) {
  first_line <- readLines(thecity_path, n = 1, warn = FALSE)
  if (grepl("git-lfs", first_line, ignore.case = TRUE)) {
    thecity_status <- "git-lfs-pointer"
  } else {
    thecity_status <- "available"
  }
}

thecity_data_status <- tibble(
  dataset = "thecity_housing_lotteries.csv",
  status = thecity_status,
  note = if_else(
    thecity_status == "git-lfs-pointer",
    "GitHub raw download returns LFS pointer text, not records.",
    "Data file appears to contain records."
  )
)

write_csv(thecity_data_status, "../output/thecity_data_status.csv")

# ----------------------------------------------------------------------------
# 7. Write concise markdown summary
# ----------------------------------------------------------------------------

total_listed_units <- sum(lotteries$`Unit Count`, na.rm = TRUE)
apps_total <- income_stage_totals %>%
  filter(stage == "applications") %>%
  pull(total_count)
leases_total <- income_stage_totals %>%
  filter(stage == "signed_leases") %>%
  pull(total_count)

if (length(apps_total) == 0) apps_total <- NA_real_
if (length(leases_total) == 0) leases_total <- NA_real_

overall_lease_per_1000 <- ifelse(is.na(apps_total) || apps_total == 0, NA_real_, 1000 * leases_total / apps_total)

top_app_income <- income_applicant_vs_lease %>%
  slice_max(applications, n = 1, with_ties = FALSE)

top_lease_income <- income_applicant_vs_lease %>%
  slice_max(signed_leases, n = 1, with_ties = FALSE)

cd_eligible_share <- location_sociodemographics %>%
  filter(eligibility == "CD Eligible") %>%
  pull(unit_share)

if (length(cd_eligible_share) == 0) cd_eligible_share <- NA_real_

report_lines <- c(
  "# NYC Housing Lottery Descriptive Snapshot",
  "",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Data Coverage",
  sprintf("- Advertised lottery records: %s (start dates %s to %s).", comma(nrow(lotteries)), min(lotteries$lottery_start_date, na.rm = TRUE), max(lotteries$lottery_end_date, na.rm = TRUE)),
  sprintf("- Affordable housing production records: %s (project starts %s to %s).", comma(nrow(production)), min(production$project_start_date, na.rm = TRUE), max(production$building_completion_date, na.rm = TRUE)),
  sprintf("- Listed lottery units in current advertised-lotteries file: %s.", comma(total_listed_units)),
  sprintf("- The City GitHub file status: %s.", thecity_status),
  "",
  "## Applications vs Signed Leases (LL217 Income Sheet)",
  sprintf("- Total applications (income table total): %s.", ifelse(is.na(apps_total), "NA", comma(apps_total))),
  sprintf("- Total signed leases (income table total): %s.", ifelse(is.na(leases_total), "NA", comma(leases_total))),
  sprintf("- Signed leases per 1,000 applications: %s.", ifelse(is.na(overall_lease_per_1000), "NA", sprintf("%.2f", overall_lease_per_1000))),
  sprintf("- Largest applicant income band: %s (%s applications).", top_app_income$category, comma(top_app_income$applications)),
  sprintf("- Largest signed-lease income band: %s (%s signed leases).", top_lease_income$category, comma(top_lease_income$signed_leases)),
  "",
  "## Location Socioeconomic Context (CDBG Tract Join)",
  sprintf("- Building records matched to CDBG tract table: %s of %s.", comma(tract_join_coverage$value[tract_join_coverage$metric == "building_records_matched_to_cdbg"]), comma(tract_join_coverage$value[tract_join_coverage$metric == "building_records_total"])),
  sprintf("- Share of listed units in matched tracts: %s.", percent(tract_join_coverage$value[tract_join_coverage$metric == "share_of_listed_units_with_cdbg_match"], accuracy = 0.1)),
  sprintf("- Share of listed units in CD-Eligible tracts: %s.", ifelse(is.na(cd_eligible_share), "NA", percent(cd_eligible_share, accuracy = 0.1))),
  "",
  "## Output Files",
  "- coverage_summary.csv",
  "- unit_characteristics_overview.csv",
  "- lotteries_by_status.csv",
  "- lotteries_by_borough.csv",
  "- units_by_income_band.csv",
  "- units_by_bedroom.csv",
  "- lottery_year_summary.csv",
  "- income_pipeline_counts.csv",
  "- income_pipeline_stage_totals.csv",
  "- income_applicant_vs_lease.csv",
  "- race_applicant_vs_lease.csv",
  "- tract_join_coverage.csv",
  "- location_sociodemographics.csv",
  "- borough_lomod_summary.csv",
  "- thecity_data_status.csv"
)

writeLines(report_lines, "../output/eda_summary_report.md")

cat("Saved descriptive outputs to ../output\n")
cat("Done.\n")
