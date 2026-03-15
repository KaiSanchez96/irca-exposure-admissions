# # When a KPI Lies

A data analyst case study on denominator sensitivity in U.S. immigration admissions data.

## Summary

This project audits a percentage-based KPI built from administrative immigration data: the recorded blue-collar share of immigrant admissions to the United States. The main finding is that the headline decline is large when the denominator includes only known occupations, but shrinks sharply when records with no occupation are included. The project shows how denominator instability and missingness can distort KPI interpretation.

## Main finding

- The blue-collar share appears to decline sharply under the net denominator.
- The same decline becomes much smaller under the full denominator on the same sample.
- Conclusion: the headline KPI is highly sensitive to denominator construction.

## Why this project matters

This is a portfolio project about KPI auditing, denominator sensitivity, missingness, and careful interpretation of administrative data. It is not presented as a causal evaluation of IRCA, but as a measurement and reporting case study.

## Tools and methods

- R
- dplyr / tidyr
- fixest
- ggplot2
- modelsummary
- Panel fixed effects
- Robustness checks
- Reproducible reporting with R Markdown

## Repository contents

- `irca.R`: builds the panel and exports figures/tables
- `IRCA_Exposure_Report.Rmd`: report source
- `IRCA_Exposure_Report.pdf`: final portfolio report
- `outputs/figures/`: exported figures
- `outputs/tables/`: exported regression tables

## How to run

1. Place the raw Excel files in `data_raw/`
2. Run `irca.R`
3. Render `IRCA_Exposure_Report.Rmd` 

## Takeaway

When the denominator is unstable, a percentage KPI can tell the wrong story.
