SHELL := bash
include ../../shell_functions.make

# Main target: produce all descriptive statistics and summary outputs
all: ../output/descriptives.done

# Link inputs from upstream task
../input/advertised_lotteries_by_lottery.csv: ../../public_lottery_pull/output/advertised_lotteries_by_lottery.csv | ../input
	ln -sf $(abspath $<) $@

../input/advertised_lotteries_by_building.csv: ../../public_lottery_pull/output/advertised_lotteries_by_building.csv | ../input
	ln -sf $(abspath $<) $@

../input/affordable_housing_production_by_building.csv: ../../public_lottery_pull/output/affordable_housing_production_by_building.csv | ../input
	ln -sf $(abspath $<) $@

../input/local_law_217_report_2024.xlsx: ../../public_lottery_pull/output/local_law_217_report_2024.xlsx | ../input
	ln -sf $(abspath $<) $@

../input/cdbg_eligibility_by_census_tract.csv: ../../public_lottery_pull/output/cdbg_eligibility_by_census_tract.csv | ../input
	ln -sf $(abspath $<) $@

# Convenience target
link-inputs: ../input/advertised_lotteries_by_lottery.csv \
             ../input/advertised_lotteries_by_building.csv \
             ../input/affordable_housing_production_by_building.csv \
             ../input/local_law_217_report_2024.xlsx \
             ../input/cdbg_eligibility_by_census_tract.csv

# Run descriptive analysis
../output/descriptives.done: lottery_descriptives.R link-inputs | ../output
	$(R) $<
	touch $@

include ../../generic.make
