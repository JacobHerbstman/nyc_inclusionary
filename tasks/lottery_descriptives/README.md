# Lottery Descriptives

## Purpose
Produce descriptive statistics and exploratory analysis of NYC Housing Connect lottery data to motivate the research question.

## Inputs (from `public_lottery_pull`)
- `advertised_lotteries_by_lottery.csv` - HPD lottery data (July 2020+)
- `advertised_lotteries_by_building.csv` - Building-level lottery data
- `affordable_housing_production_by_building.csv` - All Housing NY production
- `thecity_housing_lotteries.csv` - 426 lotteries (2014-2019)
- `thecity_lottery_applications.csv` - 20M+ applications
- `local_law_217_report_2024.xlsx` - Applications/leases by demographics
- NYC shapefiles

## Outputs
- `data_summary.csv` - Overview of all datasets
- `data_columns.txt` - Column names for reference
- `production_by_borough.csv` - Building counts by borough
- `production_by_community_district.csv` - Building counts by CD
- `ll217_*.csv` - Parsed Local Law 217 sheets
- `map_affordable_housing.png` - Map of production locations

## Key Descriptive Questions
1. **Income Distribution of Units**: What share of lottery units target each AMI band?
2. **Geographic Concentration**: Where is affordable housing being built?
3. **Competition Ratios**: How many applicants per unit at each income level?
4. **Winner Demographics**: Who actually gets units? (via LL217)
5. **Neighborhood Quality**: What are the tract characteristics of lottery buildings?

## Usage

First, ensure `public_lottery_pull` has been run:
```bash
cd ../public_lottery_pull/code
make
```

Then run this task:
```bash
cd ../lottery_descriptives/code
make
```

## Dependencies
- R packages: tidyverse, sf, readxl, scales, knitr, kableExtra, ggplot2, viridis
