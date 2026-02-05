SHELL := bash
include ../../shell_functions.make

# Main target: produce all descriptive statistics and figures
all: ../output/descriptives.done

# Link inputs from upstream task
../input/advertised_lotteries_by_lottery.csv: ../../public_lottery_pull/output/advertised_lotteries_by_lottery.csv | ../input
	ln -sf $(abspath $<) $@

../input/advertised_lotteries_by_building.csv: ../../public_lottery_pull/output/advertised_lotteries_by_building.csv | ../input
	ln -sf $(abspath $<) $@

../input/affordable_housing_production_by_building.csv: ../../public_lottery_pull/output/affordable_housing_production_by_building.csv | ../input
	ln -sf $(abspath $<) $@

../input/thecity_housing_lotteries.csv: ../../public_lottery_pull/output/thecity_housing_lotteries.csv | ../input
	ln -sf $(abspath $<) $@

../input/thecity_lottery_applications.csv: ../../public_lottery_pull/output/thecity_lottery_applications.csv | ../input
	ln -sf $(abspath $<) $@

../input/local_law_217_report_2024.xlsx: ../../public_lottery_pull/output/local_law_217_report_2024.xlsx | ../input
	ln -sf $(abspath $<) $@

../input/nyc_census_tracts_2020.gpkg: ../../public_lottery_pull/output/nyc_census_tracts_2020.gpkg | ../input
	ln -sf $(abspath $<) $@

../input/nyc_community_districts.gpkg: ../../public_lottery_pull/output/nyc_community_districts.gpkg | ../input
	ln -sf $(abspath $<) $@

../input/nyc_boroughs.gpkg: ../../public_lottery_pull/output/nyc_boroughs.gpkg | ../input
	ln -sf $(abspath $<) $@

# Convenience target
link-inputs: ../input/advertised_lotteries_by_lottery.csv \
             ../input/advertised_lotteries_by_building.csv \
             ../input/affordable_housing_production_by_building.csv \
             ../input/thecity_housing_lotteries.csv \
             ../input/thecity_lottery_applications.csv \
             ../input/local_law_217_report_2024.xlsx \
             ../input/nyc_census_tracts_2020.gpkg \
             ../input/nyc_community_districts.gpkg \
             ../input/nyc_boroughs.gpkg

# Run descriptive analysis
../output/descriptives.done: lottery_descriptives.R link-inputs | ../output
	$(R) $<
	touch $@

include ../../generic.make
