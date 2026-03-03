# IRCA Exposure and Composition of Admissions (1982–2000)

This project documents how IRCA exposure (measured using 1989–1992 legalizations relative to 1982–1985 admissions)
is associated with changes in the occupational composition of immigrants admitted to the U.S. from 19 Latin American origin countries.

**Report (PDF):**
- `IRCA_Exposure_Report.pdf`

## What’s inside
- Reproducible pipeline (R) that ingests raw Excel tables and builds a country-year panel
- Data QA: missingness and coverage diagnostics
- Results: descriptive exposure-gradient estimates + sensitivity checks
- Key caution: denominator sensitivity due to “no occupation” category

## How to reproduce (optional)
1. Place raw inputs in `data_raw/`
2. Run: `Rscript run.R`
3. Render: `paper.Rmd`
