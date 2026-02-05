# NYC Inclusionary Agent Guide

This file is the operating guide for agents working in this repository. It is based on `CLAUDE.md` and extended with workflow preferences reflected in the existing task structure.

## Project Goal (Orientation)
- Core question: what are the causal effects of NYC inclusionary housing lottery wins on recipient outcomes.
- Identification concept: random log-number assignment within lotteries supports ITT and IV/LATE designs.
- Key heterogeneity: effects by AMI income bands (roughly 30% to 165% AMI).
- Priority outcomes to support in data prep and EDA: lease-up, earnings/employment, safety-net participation, housing stability, shelter entry, and eviction exposure.
- Near-term objective: build reproducible descriptive and data-quality tasks that validate sources, coverage, and join keys before any causal estimation.

## Canonical Project Structure
- `data_raw/`: immutable raw files (never edit in place).
- `tasks/<task_name>/code`: task scripts + task `Makefile`.
- `tasks/<task_name>/input`: symlinked upstream artifacts.
- `tasks/<task_name>/output`: task outputs and `.done` sentinels.
- `tasks/<task_name>/temp`: optional scratch intermediates.
- `tasks/generic.make` and `tasks/shell_functions.make`: shared Makefile includes.

## Required Task Workflow
- Every analysis step is a task under `tasks/` with its own `code/`, `input/`, and `output/` folders.
- Run tasks from the task `code/` directory via `make` (not by running scripts ad hoc).
- Keep all paths relative to `code/`.
- Use `make -n` first when checking command plans.
- Prefer upstream-to-downstream dependency tracing through Makefiles from `data_raw/` to final outputs.

## Makefile Style Preferences
- Keep Makefiles simple and readable.
- Default targets:
  - `all`: main build target.
  - `link-inputs`: present when the task consumes upstream outputs.
- Use explicit symlink rules for upstream artifacts:
  - `ln -sf $(abspath $<) $@`
- Use `.done` markers for completion of script-based build steps.
- Include shared utilities at top/bottom:
  - `include ../../shell_functions.make`
  - `include ../../generic.make`
- Avoid noisy helper targets unless they are genuinely useful.

## Script Conventions (R-First Workflow)
- Prefer one main script per task that is invoked by the task Makefile.
- Scripts should emit progress logs (`cat(...)`) and create deterministic output filenames.
- For large files, include safe guards (size checks/sampling) rather than failing silently.
- Write tabular outputs as CSV and preserve lightweight audit artifacts (for example, column lists and row-count summaries).
- For spatial objects, maintain consistent CRS handling across tasks.

## Data Source Priorities
- NYC HPD / NYC Open Data Housing Connect datasets.
- Local Law 217 reporting files.
- The City NY lottery datasets (lotteries + applications).
- NYC boundary files needed for tract/district-level descriptive analysis.
- HUD/AMI references for band definitions and interpretation.

## EDA Expectations for Early Tasks
- Start with data inventory: row counts, columns, key IDs, temporal coverage.
- Quantify missingness and type inconsistencies before downstream joins.
- Produce clear descriptives tied to the research design:
  - unit distribution by AMI band,
  - applications-per-unit/competition metrics,
  - geography of building locations,
  - applicant/lease patterns from LL217 tabs.
- Save intermediate summary tables used in figures so results are auditable.

## Reproducibility and Safety
- Do not edit upstream task outputs manually.
- Do not modify `data_raw/` files.
- Keep network downloads and transformations in separate tasks when practical.
- Prefer idempotent code: rerunning `make` should not create inconsistent states.

## Git and Collaboration
- Keep commits scoped to a single task or documentation change.
- Document each task with a concise `README.md` (purpose, inputs, outputs, run command).
- When adding a new task, make dependency links explicit so another contributor can trace lineage without reading script internals.
