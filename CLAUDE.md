# NYC Inclusionary Housing Project Guidelines

## Data Analysis Workflow
- This project uses a task-based workflow. Every task has a dedicated folder in `tasks/` with its own `code/`, `input/`, and `output/` subfolders.
- Each task should have its own Makefile. Each Makefile should be as clean and simple as possible for readability.
- In Makefiles, limit comments and additional targets such as `clean`. Typical Makefiles should only have a default `all` target and a `link-inputs` target.
- Tasks that use output from "upstream" tasks should use symlinking and Makefiles to connect them together.
- It should be easy to trace the path via Makefiles from the `data_raw/` folder to final outputs.

## Project Structure
- `paper/` - LaTeX paper and sections (future)
- `data_raw/` - Raw data files (not to be modified)
- `tasks/` - Analysis tasks, each with `code/`, `input/`, and `output/` subfolders
- `proposal.tex` - Research proposal

## Running Tasks
- Always execute tasks by running `make` from the `code/` folder within any task
- Make sure all paths are relative to the `code/` folder
- Use `make -n` to preview what commands will be run without executing

## Data Sources
Primary data sources for this project:
- NYC HPD Housing Connect lottery data (data.cityofnewyork.us)
- Local Law 217 reports (applications and leases by income/race/borough)
- The City NY lottery application data (GitHub)
- NYC shapefiles (community districts, census tracts, etc.)
- HUD Area Median Income tables

## Key Links
- HPD Open Data: https://www.nyc.gov/site/hpd/about/open-data.page
- NYC Open Data: https://data.cityofnewyork.us/
- The City NY Data: https://github.com/thecityny/housing-lottery-data
