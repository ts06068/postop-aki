# Postoperative AKI Analysis

## Study Objective

Estimate risk factors for postoperative acute kidney injury (AKI) and measure its association with mortality, ICU admission, and length of stay after cardiac and non-cardiac surgery.

## Folder structure

```text
postop-aki/
├── .github/
│   └── workflows/
│       └── smoke-test.yml          # Install packages, test, and run the sample in CI
├── R/
│   ├── 01_data.R                   # Read, validate, and derive analysis variables
│   ├── 02_summaries.R              # Build counts, baseline tables, and LOS summaries
│   ├── 03_models.R                 # Fit AKI, mortality, landmark, and ICU models
│   └── 04_output.R                 # Validate sample IDs and write result metadata
├── data/
│   ├── data_dictionary.csv         # Define input columns and accepted aliases
│   └── sample.xlsx                 # Provide the synthetic 96-row test fixture
├── scripts/
│   └── install_dependencies.R      # Install pinned R packages
├── tests/
│   └── test_pipeline.R             # Check fixture values, endpoints, and R syntax
├── .env.example                    # Show full-data input and output settings
├── .gitattributes                  # Enforce text and line-ending rules
├── .gitignore                      # Exclude protected inputs and generated files
├── DESCRIPTION                     # Pin R and direct package versions
├── README.md                       # Explain the project and commands
└── run_all.R                       # Run the analysis from the command line
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
