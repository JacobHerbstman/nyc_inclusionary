# Public Lottery Data Pull

## Purpose
Download public NYC Housing Connect lottery data and shapefiles for exploratory analysis.

## Data Sources

### NYC Open Data (HPD)
1. **Advertised Lotteries by Lottery** (`vy5i-a666`)
   - Lottery status, preferences, income levels, unit sizes
   - Coverage: July 2020 onwards
   
2. **Advertised Lotteries by Building** (`nibs-na6y`)
   - Address, BBL, census tract, council district
   - Income levels and unit sizes by building
   
3. **Affordable Housing Production by Building** (`hg8x-zxpr`)
   - All Housing New York units with BBL, census tract, coordinates
   - Coverage: January 2014 onwards
   
4. **Affordable Housing Production by Project** (`hq68-rnsi`)
   - Project-level data including senior units

### HPD Reports
5. **Local Law 217 Reports** (Excel)
   - Applications submitted and leases signed
   - Broken down by income, race/ethnicity, borough
   - Coverage: 2014-present

### The City NY (GitHub)
6. **Housing Lotteries** (2014-2019)
   - 426 lotteries with 1,888 unit groups
   
7. **Lottery Applications**
   - 20+ million individual applications
   - Contains household income data

### NYC Shapefiles
- Community Districts
- Census Tracts (2020)
- Neighborhood Tabulation Areas (2020)
- Borough Boundaries
- City Council Districts
- PUMAs (2020)

## Usage

```bash
cd code
make
```

## Output Files

### CSV/Excel
- `advertised_lotteries_by_lottery.csv`
- `advertised_lotteries_by_building.csv`
- `affordable_housing_production_by_building.csv`
- `affordable_housing_production_by_project.csv`
- `local_law_217_report_2024.xlsx`
- `local_law_217_report_2021.xlsx`
- `thecity_housing_lotteries.csv`
- `thecity_lottery_applications.csv`
- `thecity_methodology.md`

### Shapefiles (GeoPackage)
- `nyc_community_districts.gpkg`
- `nyc_census_tracts_2020.gpkg`
- `nyc_ntas_2020.gpkg`
- `nyc_boroughs.gpkg`
- `nyc_city_council_districts.gpkg`
- `nyc_pumas_2020.gpkg`

## AMI Income Bands (HPD Official Definitions)

| Band | % of AMI | 2025 3-person family |
|------|----------|---------------------|
| Extremely Low | 0-30% | $0 - $43,740 |
| Very Low | 31-50% | $43,741 - $72,900 |
| Low | 51-80% | $72,901 - $116,640 |
| Moderate | 81-120% | $116,641 - $174,960 |
| Middle | 121-165% | $174,961 - $240,570 |

Source: https://www.nyc.gov/site/hpd/services-and-information/area-median-income.page
