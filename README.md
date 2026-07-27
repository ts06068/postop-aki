# Postoperative AKI Analysis

## Study Objective

Analyze risk factors and postoperative outcomes associated with acute kidney injury after cardiac and non-cardiac surgery.

## Folder structure

```text
R/                    Analysis functions
data/sample.xlsx      Synthetic sample data
scripts/              Dependency setup
tests/                Smoke test
run_all.R             Analysis entry point
```

## Code structure

`run_all.R` loads the numbered modules in order and runs the pipeline.

- `R/01_data.R`: reads, validates, and derives analysis variables.
- `R/02_summaries.R`: creates cohort, baseline, and length-of-stay summaries.
- `R/03_models.R`: fits AKI risk, mortality, landmark, and competing-risk models.
- `R/04_output.R`: checks sample input and writes results with run metadata.
- `scripts/install_dependencies.R` and `tests/test_pipeline.R`: set up packages and run the smoke test.

## Environment

- R 4.4.3
- Package versions are defined in `DESCRIPTION`.
- Input is file-based; no database connection is required.

## How to run

```powershell
Rscript scripts/install_dependencies.R
Rscript run_all.R --mode=sample
Rscript tests/test_pipeline.R
```

For a full dataset:

```powershell
$env:POSTOP_AKI_DATA_PATH = "D:\data\analysis_dataset.xlsx"
Rscript run_all.R --mode=full
```
