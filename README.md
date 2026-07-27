# Postoperative AKI Analysis

## Study Objective

Estimate risk factors for postoperative acute kidney injury (AKI) and measure its association with mortality, ICU admission, and length of stay after cardiac and non-cardiac surgery.

## Folder structure

```text
R/                    Data preparation, summaries, and models
data/sample.xlsx      Synthetic 96-row test fixture
scripts/              Dependency installation
tests/                Pipeline checks
run_all.R             Command-line entry point
```

## Code structure

`run_all.R` is the entry point. It loads the numbered modules in order.

- `R/01_data.R`: read files, normalize columns, reject invalid input, and derive model variables.
- `R/02_summaries.R`: write cohort counts, baseline tables, and length-of-stay summaries.
- `R/03_models.R`: fit logistic, extended Cox, landmark Cox, and Fine-Gray models.
- `R/04_output.R`: enforce synthetic sample IDs and write tables with run metadata.
- `scripts/install_dependencies.R`: install the exact direct dependencies in `DESCRIPTION`.
- `tests/test_pipeline.R`: check the sample fixture, derived fields, endpoint coding, and R syntax.

## Environment

- Use R 4.4.3. Exact direct dependency versions are in `DESCRIPTION`.
- The installer uses a fixed CRAN snapshot.
- There is no database client. Full mode reads an analysis-ready file.
- `data/sample.xlsx` is synthetic. Keep protected extracts out of Git.

## How to run

```powershell
Rscript scripts/install_dependencies.R
Rscript tests/test_pipeline.R
Rscript run_all.R --mode=sample
```

Sample results go to `outputs/sample/`.

Full mode requires an external extract. Set its path explicitly:

```powershell
$env:POSTOP_AKI_DATA_PATH = "D:\data\analysis_dataset.xlsx"
$env:POSTOP_AKI_OUTPUT_DIR = "outputs\full"
Rscript run_all.R --mode=full
```

Full mode accepts XLSX, XLS, CSV, and pipe-delimited text files.
