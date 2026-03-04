# IRCA Exposure and Composition of Admissions (1982–2000)

A data analysis portfolio project that builds a reproducible country–year panel (19 Latin American origin countries, 1982–2000) from INS/OHSS Statistical Yearbook tables and documents how IRCA legalization intensity (1989–1992) correlates with changes in *recorded* occupational composition.

## TL;DR (Headline)
During the 1989–1992 IRCA surge window, the apparent “blue-collar share decline” is **highly sensitive to denominator definition**:

- **Net denominator (known occupations only):** sizable negative association with exposure  
- **Full denominator (known + no-occupation):** association attenuates toward zero  

Interpretation: composition KPIs can move because **denominators/reporting change**, not because category counts collapse. Recommended monitoring during surge windows: report **net share**, **full share**, and **no-occupation share** together.

## Deliverables
- **Report:** `paper.pdf` (and optional HTML via `paper.Rmd`)
- **Pipeline script:** `run.R` (raw inputs → clean panel → outputs)
- **Outputs:** `outputs/tables/`, `outputs/figures/`, `outputs/logs/`
- **Clean dataset:** `data_clean/panel.rds`

## Repository Structure

project/
  data_raw/              # extracted input tables (Excel)
  data_clean/
    panel.rds            # cleaned country-year panel
  outputs/
    tables/              # LaTeX tables
    figures/             # PNG figures
    logs/                # run summary + sessionInfo + diagnostics
  run.R                  # reproducible pipeline
  paper.Rmd              # report source
  paper.pdf              # rendered report

## How to Run (Reproducible)
1. Place the raw Excel inputs in `data_raw/` (see **Data** section).
2. Run the pipeline: Rscript run.R
3. Render the report: Knit paper.Rmd   

## Key Methods (high-level)

- Multi-sheet Excel ingestion + year parsing
- Country–year panel construction (1982–2000)
- KPI engineering: net vs full denominator blue share + no-occupation share
- Descriptive exposure-gradient models (country & year fixed effects; clustered SE)
- Robustness: trends, weights, balanced panel, leave-one-out; small-cluster wild bootstrap (optional)
- Missingness and coverage diagnostics + reproducible outputs

## Data

The inputs in data_raw/ are extracted/compiled tables from publicly available INS Statistical Yearbooks / OHSS publications.
I extracted the specific series needed for this project into Excel format for reproducible analysis. The original yearbooks are publicly accessible online (see citations in the report).

## Reproducibility Notes

The pipeline writes outputs/logs/sessionInfo.txt and outputs/logs/run_summary.txt.
Optional packages are handled gracefully (e.g., patchwork; wild bootstrap tooling).

## Limitations

This is a descriptive exposure-gradient analysis, not a randomized causal design.

Event-study pretrend diagnostics suggest caution in dynamic interpretation.

Occupational coding includes a sizable “no occupation” mass; denominator choice materially affects share-based metrics.

## Contact

Kai A. Sanchez P.
LinkedIn: https://www.linkedin.com/in/kai-sanchez-pajuelo/
Email: kai.sanchez1996@gmail.com
