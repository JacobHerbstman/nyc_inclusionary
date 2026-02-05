SHELL := bash
include ../../shell_functions.make

# Main target: download all public lottery data and shapefiles
all: ../output/download_lottery.done ../output/download_shapefiles.done ../output/download_github.done

# Download HPD lottery datasets and Local Law 217 from NYC Open Data
../output/download_lottery.done: download_lottery_data.R | ../output
	$(R) $<
	touch $@

# Download NYC shapefiles (community districts, census tracts, NTAs)
../output/download_shapefiles.done: download_shapefiles.R | ../output
	$(R) $<
	touch $@

# Download The City NY lottery analysis data from GitHub
../output/download_github.done: download_github_data.R | ../output
	$(R) $<
	touch $@

include ../../generic.make
