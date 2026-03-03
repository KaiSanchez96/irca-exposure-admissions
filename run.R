############################################################
# run.R — IRCA project (reproducible pipeline)
# - Auto-detects raw Excel files
# - Builds panel from raw files
# - Runs main FE models + robustness + portfolio add-ons
# - Exports tables (.tex) and figures (.png)
# - Writes logs + panel snapshot (.rds)
############################################################

#### 0) Libraries ####
suppressPackageStartupMessages({
  library(readxl)
  library(janitor)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(fixest)
  library(purrr)
  library(ggplot2)
  library(broom)
  library(modelsummary)
  library(scales)
  library(rlang)
  library(ggrepel)
  library(glue)
  library(forcats)
  library(tibble)
  library(knitr)
})

#### 0.1) Output folders ####
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("data_clean", showWarnings = FALSE, recursive = TRUE)

write_tex <- function(x, path){
  x_chr <- paste0(as.character(x), collapse = "\n")
  writeLines(x_chr, path, useBytes = TRUE)
}

save_plot <- function(p, path, w=7, h=4.5){
  ggplot2::ggsave(path, plot = p, width = w, height = h, dpi = 300)
}

#### 0.2) Single magnitudes writer ####
write_magnitudes_blue <- function(panel_net, fe_blue_net,
                                  out_table = "outputs/tables/tab_magnitudes_blue.tex",
                                  out_plot  = "outputs/figures/fig_std_effects_blue.png") {
  
  expo_vec <- panel_net %>%
    dplyr::distinct(country, irca_intensity) %>%
    dplyr::pull(irca_intensity)
  
  expo_vec <- expo_vec[is.finite(expo_vec)]
  expo_sd  <- sd(expo_vec, na.rm = TRUE)
  expo_iqr <- IQR(expo_vec, na.rm = TRUE)
  
  b_net <- unname(coef(fe_blue_net)["irca_intensity:post"])
  blue_mean_net <- mean(panel_net$blue_share, na.rm = TRUE)
  
  # Effects:
  # - pp: 100 * (effect in share units)
  # - % of mean: (effect in share units / mean share) * 100
  magnitudes <- tibble::tibble(
    Magnitude = c("Per 1 unit exposure",
                  "Per 1 SD exposure",
                  "Per IQR exposure",
                  "As % of mean(Y): 1 SD",
                  "As % of mean(Y): IQR"),
    `Effect (pp / %)` = c(
      100 * b_net,
      100 * b_net * expo_sd,
      100 * b_net * expo_iqr,
      (b_net * expo_sd)  / blue_mean_net * 100,
      (b_net * expo_iqr) / blue_mean_net * 100
    )
  )
  
  tab_mag <- knitr::kable(
    magnitudes,
    format = "latex", booktabs = TRUE, digits = 2,
    caption = "Economic magnitudes for the baseline Net specification (blue-collar share)."
  )
  write_tex(tab_mag, out_table)
  
  plot_mag <- tibble::tibble(
    scale = c("1 SD exposure","IQR exposure"),
    pp = c(100 * b_net * expo_sd, 100 * b_net * expo_iqr)
  )
  
  p_mag <- ggplot(plot_mag, aes(x = scale, y = pp)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype="dashed") +
    labs(x=NULL, y="Effect on blue-collar share (pp)",
         title="Standardized magnitudes (baseline Net)") +
    theme_bw()
  
  save_plot(p_mag, out_plot, w=6.5, h=4)
  
  invisible(list(magnitudes = magnitudes, expo_sd = expo_sd, expo_iqr = expo_iqr))
}

#### 1) File paths  ####

find_one <- function(pattern){
  hits <- list.files(".", pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (length(hits) == 0) stop("No se encontró archivo con patrón: ", pattern)
  hits[1]
}

if (!exists("path_occ"))    path_occ    <- find_one("Occupation latam.*\\.xlsx$")
if (!exists("path_gender")) path_gender <- find_one("Gender-age latam.*\\.xlsx$")
if (!exists("path_class"))  path_class  <- find_one("by class.*\\.xlsx$")
if (!exists("path_adj"))    path_adj    <- find_one("adjust-n\\.arrivals.*\\.xlsx$")

cat("Using files:\n",
    " occ:    ", path_occ, "\n",
    " gender: ", path_gender, "\n",
    " class:  ", path_class, "\n",
    " adj:    ", path_adj, "\n", sep = "")

stopifnot(file.exists(path_occ), file.exists(path_gender), file.exists(path_class), file.exists(path_adj))

#### 2) Helper: read all sheets with year from sheet name ####

read_all_sheets <- function(path) {
  sheets <- excel_sheets(path)
  map_dfr(sheets, function(s) {
    read_excel(path, sheet = s) |>
      clean_names() |>
      mutate(year = suppressWarnings(as.numeric(s)))
  })
}

#### 3) Load raw files ####

occ_raw    <- read_all_sheets(path_occ)
gender_raw <- read_all_sheets(path_gender)
class_raw  <- read_all_sheets(path_class)
adj_raw    <- read_excel(path_adj) |> clean_names()

#### 4) Construct panels ####

## 4A) Occupations

occ <- occ_raw |>
  rename(country = x1) |>
  mutate(
    country = str_trim(country),
    occ_known = white_high + white_support + blue_collar_non_farm + agriculture + services,
    blue_share  = if_else(occ_known > 0, blue_collar_non_farm/occ_known, NA_real_),
    serv_share  = if_else(occ_known > 0, services/occ_known, NA_real_),
    noocc_share = if_else(occ_known + no_occupation > 0,
                          no_occupation/(occ_known + no_occupation), NA_real_)
  )

## 4B) Class of admission + IRCA share

class <- class_raw |>
  rename(country = x1) |>
  mutate(
    country   = str_trim(country),
    total_adm = family_based_admissions + employment_based_admissions +
      humanitarian_admissions + other_miscellaneous +
      coalesce(irca_legalization, 0),
    irca_share = if_else(total_adm > 0, coalesce(irca_legalization, 0)/total_adm, NA_real_)
  )

## 4C) Baseline IRCA intensity: (1989–92 legalizations) / (1982–85 admissions)

irca_intensity <- class |>
  group_by(country) |>
  summarise(
    irca_89_92 = sum(coalesce(irca_legalization,0) * (year %in% 1989:1992), na.rm = TRUE),
    pre_adm    = sum(total_adm * (year %in% 1982:1985), na.rm = TRUE),
    irca_intensity = if_else(pre_adm > 0, irca_89_92/pre_adm, NA_real_),
    .groups = "drop"
  )

## 4D) Adjustments vs New Arrivals

adj_header <- adj_raw[1,]
adj_data   <- adj_raw[-1,] |>
  rename(country = x1) |>
  mutate(country = str_trim(country))

adj_long_vals  <- adj_data |>
  pivot_longer(-country, names_to = "col", values_to = "value")

adj_long_types <- adj_header |>
  pivot_longer(-x1, names_to = "col", values_to = "type") |>
  select(col, type)

adj_panel <- adj_long_vals |>
  left_join(adj_long_types, by = "col") |>
  mutate(
    year  = as.numeric(str_extract(col, "\\d{4}")),
    value = as.numeric(value),
    type  = str_trim(type)
  ) |>
  filter(!is.na(year), !is.na(country)) |>
  mutate(
    type = case_when(
      str_detect(type, "New Arrivals") ~ "new_arrivals",
      str_detect(type, "Adjustment")   ~ "adjustments",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(type)) |>
  select(country, year, type, value) |>
  pivot_wider(names_from = type, values_from = value) |>
  mutate(
    total     = new_arrivals + adjustments,
    adj_share = if_else(total > 0, adjustments/total, NA_real_)
  )

## 4E) Gender panel: male share (country-year)

gender <- gender_raw |>
  rename(country = x1, sex = x2) |>
  mutate(country = str_trim(country), sex = str_trim(sex))

gender_cty <- gender |>
  mutate(
    row_total = under_20_years + x20_29_years + x30_39_years + x40_49_years +
      x50_59_years + x60_69_years + x70_79_years + more_than_80_years
  ) |>
  group_by(country, year) |>
  summarise(
    total_all   = sum(row_total, na.rm = TRUE),
    total_males = sum(row_total[sex == "Males"], na.rm = TRUE),
    male_share  = if_else(total_all > 0, total_males / total_all, NA_real_),
    .groups = "drop"
  )

#### 5) Merge master panel + derived vars ####

panel <- occ |>
  left_join(adj_panel,      by = c("country","year")) |>
  left_join(class,          by = c("country","year")) |>
  left_join(gender_cty,     by = c("country","year")) |>
  left_join(irca_intensity, by = "country") |>
  mutate(
    post         = if_else(year %in% 1989:1992, 1L, 0L),
    white_share  = (white_high + white_support) / occ_known,
    ag_share     = agriculture / occ_known,
    manual_share = (blue_collar_non_farm + agriculture) / occ_known,
    irca_group   = if_else(irca_intensity >= median(irca_intensity, na.rm = TRUE),
                           "High IRCA", "Low IRCA"),
    t = year - min(year, na.rm = TRUE),
    fam_share   = if_else(total_adm > 0, family_based_admissions     / total_adm, NA_real_),
    emp_share   = if_else(total_adm > 0, employment_based_admissions / total_adm, NA_real_),
    hum_share   = if_else(total_adm > 0, humanitarian_admissions     / total_adm, NA_real_),
    other_share = if_else(total_adm > 0, other_miscellaneous         / total_adm, NA_real_)
  )

saveRDS(panel, "data_clean/panel.rds")

panel_total <- panel |> filter(!is.na(irca_intensity), !is.na(post))
panel_net   <- panel_total |> filter(!is.na(adj_share))

############################################################
####  DECOMPOSITION: levels behind the share result
#### Outcomes:
####  - blue_level        = blue_collar_non_farm
####  - occ_known_level   = occ_known
####  - occ_total_level   = occ_known + no_occupation
####  - noocc_level       = no_occupation
#### Plus log(1+x) variants for scale robustness
############################################################

# Use Net sample (consistent with your main spec that includes adj_share)

decomp_df <- panel_net |>
  dplyr::mutate(
    blue_level      = as.numeric(blue_collar_non_farm),
    occ_known_level = as.numeric(occ_known),
    noocc_level     = as.numeric(no_occupation),
    occ_total_level = as.numeric(occ_known + no_occupation),
    
    # log(1+x) versions to reduce scale issues; keep zeros safe
    ln_blue_level      = log1p(pmax(blue_level, 0)),
    ln_occ_known_level = log1p(pmax(occ_known_level, 0)),
    ln_noocc_level     = log1p(pmax(noocc_level, 0)),
    ln_occ_total_level = log1p(pmax(occ_total_level, 0))
  ) |>
  
  # Remove rows with missing key outcomes (just in case)
  
  dplyr::filter(
    is.finite(blue_level),
    is.finite(occ_known_level),
    is.finite(noocc_level),
    is.finite(occ_total_level)
  )

run_decomp <- function(df, y){
  fixest::feols(
    as.formula(paste0(y, " ~ irca_intensity*post + adj_share | country + year")),
    data = df, cluster = ~ country
  )
}

# Level models

m_blue_lvl      <- run_decomp(decomp_df, "blue_level")
m_occ_known_lvl <- run_decomp(decomp_df, "occ_known_level")
m_occ_total_lvl <- run_decomp(decomp_df, "occ_total_level")
m_noocc_lvl     <- run_decomp(decomp_df, "noocc_level")

# Log(1+x) models (often easier to interpret as approx % changes)

m_ln_blue      <- run_decomp(decomp_df, "ln_blue_level")
m_ln_occ_known <- run_decomp(decomp_df, "ln_occ_known_level")
m_ln_occ_total <- run_decomp(decomp_df, "ln_occ_total_level")
m_ln_noocc     <- run_decomp(decomp_df, "ln_noocc_level")

# Export table (levels + logs)

modelsummary::modelsummary(
  list(
    "Blue level"        = m_blue_lvl,
    "Occ known level"   = m_occ_known_lvl,
    "Occ total level"   = m_occ_total_lvl,
    "No-occ level"      = m_noocc_lvl
  ),
  coef_map = c("irca_intensity:post"="IRCA × Post", "adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  stars    = c('*'=.10,'**'=.05,'***'=.01),
  notes    = "Country & Year FE; SE clustered by country. Levels are counts.",
  output   = "outputs/tables/tab_decomp_levels_A.tex"
)

modelsummary::modelsummary(
  list(
    "ln(1+Blue)"        = m_ln_blue,
    "ln(1+Occ known)"   = m_ln_occ_known,
    "ln(1+Occ total)"   = m_ln_occ_total,
    "ln(1+No-occ)"      = m_ln_noocc
  ),
  coef_map = c("irca_intensity:post"="IRCA × Post", "adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  stars    = c('*'=.10,'**'=.05,'***'=.01),
  notes    = "Country & Year FE; SE clustered by country. Log specs use ln(1+x).",
  output   = "outputs/tables/tab_decomp_levels_B.tex"
)

# Coefficient plot for IRCA × Post across the 8 outcomes

extract_irca_post <- function(mod, outcome_label){
  tt <- broom::tidy(mod)
  rr <- tt[tt$term == "irca_intensity:post", ]
  rr$outcome <- outcome_label
  rr
}

coef_df <- dplyr::bind_rows(
  extract_irca_post(m_blue_lvl,      "Blue level"),
  extract_irca_post(m_occ_known_lvl, "Occ known level"),
  extract_irca_post(m_occ_total_lvl, "Occ total level"),
  extract_irca_post(m_noocc_lvl,     "No-occ level"),
  extract_irca_post(m_ln_blue,       "ln(1+Blue)"),
  extract_irca_post(m_ln_occ_known,  "ln(1+Occ known)"),
  extract_irca_post(m_ln_occ_total,  "ln(1+Occ total)"),
  extract_irca_post(m_ln_noocc,      "ln(1+No-occ)")
) |>
  dplyr::mutate(
    est = estimate,
    lo  = estimate - 1.96*std.error,
    hi  = estimate + 1.96*std.error
  )

# --- Split coef plot into LEVELS only vs LOGS only  ---

# Keep a stable ordering

lvl_order <- c("Blue level","Occ known level","Occ total level","No-occ level")
log_order <- c("ln(1+Blue)","ln(1+Occ known)","ln(1+Occ total)","ln(1+No-occ)")

coef_df <- coef_df |>
  dplyr::mutate(
    grp = dplyr::case_when(
      outcome %in% lvl_order ~ "Levels",
      outcome %in% log_order ~ "Logs",
      TRUE ~ "Other"
    )
  )

# 1) Levels plot

coef_lvl <- coef_df |>
  dplyr::filter(grp == "Levels") |>
  dplyr::mutate(outcome = factor(outcome, levels = lvl_order))

p_decomp_levels <- ggplot2::ggplot(coef_lvl, ggplot2::aes(x = outcome, y = est)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = lo, ymax = hi)) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = NULL,
    y = "Estimated effect of IRCA × Post (levels)",
    title = "Decomposition (levels): IRCA × Post effects on numerator/denominators"
  ) +
  ggplot2::theme_bw()

save_plot(p_decomp_levels, "outputs/figures/fig_decomp_levels_only.png", w = 8, h = 4.8)

# 2) Logs plot

coef_log <- coef_df |>
  dplyr::filter(grp == "Logs") |>
  dplyr::mutate(outcome = factor(outcome, levels = log_order))

p_decomp_logs <- ggplot2::ggplot(coef_log, ggplot2::aes(x = outcome, y = est)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = lo, ymax = hi)) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = NULL,
    y = "Estimated effect of IRCA × Post (log(1+x))",
    title = "Decomposition (logs): IRCA × Post effects on numerator/denominators"
  ) +
  ggplot2::theme_bw()

save_plot(p_decomp_logs, "outputs/figures/fig_decomp_logs_only.png", w = 8, h = 4.8)

#### DATA QA — Missingness + coverage ####

vars_for_heat <- c(
  "blue_share","ag_share","manual_share","serv_share","white_share","noocc_share",
  "fam_share","emp_share","hum_share","other_share",
  "male_share","adj_share"
)

# ---- Heatmap (country-year x variable) ----

miss_heat <- panel %>%
  dplyr::select(country, year, dplyr::any_of(vars_for_heat)) %>%
  tidyr::pivot_longer(-c(country, year), names_to="var", values_to="val") %>%
  dplyr::mutate(missing = as.integer(is.na(val)))

p_miss <- ggplot2::ggplot(miss_heat, ggplot2::aes(x=year, y=country, fill=missing)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(limits=c(0,1), breaks=c(0,1),
                               labels=c("Observed","Missing")) +
  ggplot2::facet_wrap(~ var, ncol=3) +
  ggplot2::labs(x=NULL, y=NULL, fill=NULL,
                title="Missingness heatmap by country–year and variable") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 legend.position="bottom")

save_plot(p_miss, "outputs/figures/fig_missing_heatmap.png", w=10, h=8)

# ---- Missingness overall table ----

lab <- c(
  blue_share="Blue share", ag_share="Agriculture share", manual_share="Manual share",
  serv_share="Services share", white_share="White share", noocc_share="No-occupation share",
  fam_share="Family share", emp_share="Employment share", hum_share="Humanitarian share", other_share="Other share",
  male_share="Male share", adj_share="Adjustment share"
)

miss_tab <- panel %>%
  dplyr::summarise(
    dplyr::across(dplyr::any_of(vars_for_heat), ~ sum(is.na(.x)),  .names = "n_miss_{col}"),
    dplyr::across(dplyr::any_of(vars_for_heat), ~ sum(!is.na(.x)), .names = "n_obs_{col}")
  ) %>%
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to      = c(".value","var"),
    names_pattern = "n_(miss|obs)_(.*)"
  ) %>%
  dplyr::rename(n_miss = miss, n_obs = obs) %>%
  dplyr::mutate(
    n_total    = n_obs + n_miss,
    share_miss = n_miss / n_total,
    variable   = dplyr::recode(.data[["var"]], !!!lab)
  ) %>%
  dplyr::select(variable, n_obs, n_miss, n_total, share_miss) %>%
  dplyr::arrange(dplyr::desc(share_miss))

miss_tab_tex <- knitr::kable(
  miss_tab %>%
    dplyr::mutate(`Share missing (%)` = 100*share_miss) %>%
    dplyr::transmute(
      Variable = variable,
      `Obs.` = n_obs, Missing = n_miss, Total = n_total,
      `Share missing (%)` = sprintf("%.1f", `Share missing (%)`)
    ),
  format="latex", booktabs=TRUE,
  caption="Missingness by variable at the country–year level."
)

writeLines(miss_tab_tex, "outputs/tables/tab_missing_overall.tex")

# ---- Missingness by year table ----

miss_by_year <- panel %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(dplyr::across(dplyr::any_of(vars_for_heat), ~ mean(is.na(.x)), .names="{col}"),
                   .groups="drop") %>%
  tidyr::pivot_longer(-year, names_to="var", values_to="share_missing") %>%
  dplyr::mutate(variable = dplyr::recode(.data[["var"]], !!!lab)) %>%
  dplyr::select(year, variable, share_missing) %>%
  tidyr::pivot_wider(names_from = variable, values_from = share_missing) %>%
  dplyr::arrange(year)

miss_by_year_tex <- knitr::kable(
  miss_by_year %>% dplyr::mutate(dplyr::across(-year, ~ sprintf("%.0f\\%%", 100*.x))),
  format="latex", booktabs=TRUE,
  caption="Share missing by variable and year (percent)."
)

writeLines(miss_by_year_tex, "outputs/tables/tab_missing_byyear.tex")

# ---- Coverage by country (key vars) ----

key_vars <- c("blue_share","adj_share","irca_intensity","post")

cov_cty <- panel %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    years_total = dplyr::n_distinct(year),
    years_covered = dplyr::n_distinct(year[ dplyr::if_all(dplyr::any_of(key_vars), ~ !is.na(.x)) ]),
    coverage_ratio = years_covered / years_total,
    .groups="drop"
  ) %>%
  dplyr::arrange(dplyr::desc(coverage_ratio))

cov_cty_tex <- knitr::kable(
  cov_cty %>%
    dplyr::mutate(`Coverage ratio` = sprintf("%.3f", coverage_ratio)) %>%
    dplyr::select(Country = country,
                  `Years total` = years_total,
                  `Years covered (key vars)` = years_covered,
                  `Coverage ratio`),
  format="latex", booktabs=TRUE,
  caption="Country coverage by years and key variables."
)

writeLines(cov_cty_tex, "outputs/tables/tab_coverage_country.tex")

#### EDA — Exposure distribution + exposure vs change in blue ####

# Exposure histogram (country-level)

expo_country <- panel %>%
  dplyr::distinct(country, irca_intensity) %>%
  dplyr::filter(is.finite(irca_intensity))

p_hist <- ggplot2::ggplot(expo_country, ggplot2::aes(x=irca_intensity)) +
  ggplot2::geom_histogram(bins=12) +
  ggplot2::labs(x="IRCA exposure (country-level)", y="Count",
                title="Distribution of IRCA exposure across origin countries") +
  ggplot2::theme_bw()

save_plot(p_hist, "outputs/figures/fig_exposure_hist.png", w=6.5, h=4.2)

# Scatter: exposure vs Δ blue_share (89–92 minus 82–85)

chg_cty <- panel %>%
  dplyr::filter(!is.na(blue_share)) %>%
  dplyr::mutate(period = dplyr::case_when(
    year %in% 1982:1985 ~ "pre",
    year %in% 1989:1992 ~ "irca",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(period)) %>%
  dplyr::group_by(country, period) %>%
  dplyr::summarise(blue = mean(blue_share, na.rm=TRUE), .groups="drop") %>%
  tidyr::pivot_wider(names_from=period, values_from=blue) %>%
  dplyr::left_join(panel %>% dplyr::distinct(country, irca_intensity), by="country") %>%
  dplyr::mutate(d_blue = irca - pre)

p_sc <- ggplot2::ggplot(chg_cty, ggplot2::aes(x=irca_intensity, y=d_blue, label=country)) +
  ggplot2::geom_hline(yintercept=0, linetype="dashed") +
  ggplot2::geom_point() +
  ggrepel::geom_text_repel(size=3, max.overlaps=20) +
  ggplot2::geom_smooth(method="lm", se=FALSE) +
  ggplot2::scale_y_continuous(labels=scales::percent) +
  ggplot2::labs(x="IRCA exposure", y="Δ blue share (89–92 minus 82–85)",
                title="Exposure vs change in blue-collar share (pre vs IRCA surge)") +
  ggplot2::theme_bw()

save_plot(p_sc, "outputs/figures/fig_scatter_exposure_dblue_all.png", w=7, h=4.6)

panel_nomx  <- panel |> filter(country != "Mexico")
panel_nomx_total <- panel_total |> filter(country != "Mexico")
panel_nomx_net   <- panel_net   |> filter(country != "Mexico")

#### 5A) Baseline correlation check (diagnostic) ####

baseline_check <- panel |>
  filter(year %in% 1982:1985) |>
  group_by(country) |>
  summarise(
    pre_blue = mean(blue_share, na.rm = TRUE),
    irca_intensity = first(irca_intensity),
    .groups = "drop"
  ) |>
  filter(is.finite(pre_blue), is.finite(irca_intensity))

baseline_lm <- lm(pre_blue ~ irca_intensity, data = baseline_check)

modelsummary(
  baseline_lm,
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  output = "outputs/tables/tab_baseline_correlation.tex"
)

sink("outputs/logs/baseline_correlation.txt")
cat("Baseline correlation: mean blue_share (1982–1985) ~ IRCA intensity\n\n")
print(summary(baseline_lm))
sink()

#### 6) Helper runners ####

build_samples <- function(df, y) {
  dat_total <- df |> filter(!is.na(.data[[y]]), !is.na(irca_intensity), !is.na(post))
  dat_net   <- dat_total |> filter(!is.na(adj_share))
  list(total = dat_total, net = dat_net)
}

run_main_by_y <- function(df, y) {
  smp <- build_samples(df, y)
  list(
    Total = feols(as.formula(paste(y, "~ irca_intensity*post | country + year")),
                  data = smp$total, cluster = ~ country),
    Net   = feols(as.formula(paste(y, "~ irca_intensity*post + adj_share | country + year")),
                  data = smp$net,   cluster = ~ country)
  )
}

#### 7) MAIN MODELS — Occupation shares ####

m_full_blue   <- run_main_by_y(panel, "blue_share")
m_full_ag     <- run_main_by_y(panel, "ag_share")
m_full_manual <- run_main_by_y(panel, "manual_share")
m_full_serv   <- run_main_by_y(panel, "serv_share")
m_full_white  <- run_main_by_y(panel, "white_share")
m_full_noocc  <- run_main_by_y(panel, "noocc_share")

m_nomx_blue   <- run_main_by_y(panel_nomx, "blue_share")
m_nomx_ag     <- run_main_by_y(panel_nomx, "ag_share")
m_nomx_manual <- run_main_by_y(panel_nomx, "manual_share")
m_nomx_serv   <- run_main_by_y(panel_nomx, "serv_share")
m_nomx_white  <- run_main_by_y(panel_nomx, "white_share")
m_nomx_noocc  <- run_main_by_y(panel_nomx, "noocc_share")

#### 7A) MAIN MODELS — Class shares ####

m_fam   <- run_main_by_y(panel, "fam_share")
m_emp   <- run_main_by_y(panel, "emp_share")
m_hum   <- run_main_by_y(panel, "hum_share")
m_other <- run_main_by_y(panel, "other_share")

m_fam_nx   <- run_main_by_y(panel_nomx, "fam_share")
m_emp_nx   <- run_main_by_y(panel_nomx, "emp_share")
m_hum_nx   <- run_main_by_y(panel_nomx, "hum_share")
m_other_nx <- run_main_by_y(panel_nomx, "other_share")

outcomes_occ <- c("blue_share","ag_share","manual_share","serv_share","white_share","noocc_share")
outcomes_cls <- c("fam_share","emp_share","hum_share","other_share")
outcomes_all <- c(outcomes_occ, outcomes_cls)

meanY_on_sample <- function(df, y, need_adj){
  dat <- df %>% filter(!is.na(.data[[y]]), !is.na(irca_intensity), !is.na(post))
  if (need_adj) dat <- dat %>% filter(!is.na(adj_share))
  mean(dat[[y]], na.rm = TRUE)
}

addrow_means_total <- tibble::tibble(
  term = "Mean of Y",
  !!!setNames(lapply(outcomes_all, \(y) meanY_on_sample(panel, y, need_adj = FALSE)), outcomes_all)
)
addrow_means_net <- tibble::tibble(
  term = "Mean of Y",
  !!!setNames(lapply(outcomes_all, \(y) meanY_on_sample(panel, y, need_adj = TRUE)), outcomes_all)
)

expo_stats <- panel %>%
  distinct(country, irca_intensity) %>%
  summarise(mean_expo = mean(irca_intensity, na.rm=TRUE),
            sd_expo   = sd(irca_intensity,   na.rm=TRUE))
expo_note <- glue("Exposure stats (country-level): mean = {round(expo_stats$mean_expo,3)}, SD = {round(expo_stats$sd_expo,3)}.")

mods_total <- list(
  "Blue"=m_full_blue$Total, "Agri"=m_full_ag$Total, "Manual"=m_full_manual$Total,
  "Services"=m_full_serv$Total, "White"=m_full_white$Total, "No-Occ"=m_full_noocc$Total,
  "Family"=m_fam$Total, "Employment"=m_emp$Total, "Humanitarian"=m_hum$Total, "Other"=m_other$Total
)
mods_net <- list(
  "Blue"=m_full_blue$Net, "Agri"=m_full_ag$Net, "Manual"=m_full_manual$Net,
  "Services"=m_full_serv$Net, "White"=m_full_white$Net, "No-Occ"=m_full_noocc$Net,
  "Family"=m_fam$Net, "Employment"=m_emp$Net, "Humanitarian"=m_hum$Net, "Other"=m_other$Net
)

coef_map_total <- c("irca_intensity:post"="IRCA intensity × Post")
coef_map_net   <- c("irca_intensity:post"="IRCA intensity × Post", "adj_share"="Adjustment share")

panelA_names <- c("Blue","Agri","Manual","Services","White")
panelB_names <- c("No-Occ","Family","Employment","Humanitarian","Other")

panelA_vars <- c("blue_share","ag_share","manual_share","serv_share","white_share")
panelB_vars <- c("noocc_share","fam_share","emp_share","hum_share","other_share")

addrow_means_total_A <- addrow_means_total[, c("term", panelA_vars)]
addrow_means_total_B <- addrow_means_total[, c("term", panelB_vars)]

addrow_means_net_A   <- addrow_means_net[,   c("term", panelA_vars)]
addrow_means_net_B   <- addrow_means_net[,   c("term", panelB_vars)]

modelsummary(
  mods_total[panelA_names],
  coef_map = coef_map_total,
  gof_map  = c("nobs","r.squared.within"),
  add_rows = addrow_means_total_A,
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = paste(
    "Country & Year FE; SE clustered by origin-country. Shares in [0,1]; coefficients are pp changes per one-unit exposure.",
    "Total specs exclude adjustment-of-status share.", expo_note
  ),
  output = "outputs/tables/tab_main_total_A.tex"
)

modelsummary(
  mods_total[panelB_names],
  coef_map = coef_map_total,
  gof_map  = c("nobs","r.squared.within"),
  add_rows = addrow_means_total_B,
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = paste(
    "Country & Year FE; SE clustered by origin-country. Shares in [0,1]; coefficients are pp changes per one-unit exposure.",
    "Total specs exclude adjustment-of-status share.", expo_note
  ),
  output = "outputs/tables/tab_main_total_B.tex"
)

modelsummary(
  mods_net[panelA_names],
  coef_map = coef_map_net,
  gof_map  = c("nobs","r.squared.within"),
  add_rows = addrow_means_net_A,
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = paste(
    "Country & Year FE; SE clustered by origin-country. Shares in [0,1]; coefficients are pp per one-unit exposure.",
    "Net specs include adjustment-of-status share.", expo_note
  ),
  output = "outputs/tables/tab_main_net_A.tex"
)

modelsummary(
  mods_net[panelB_names],
  coef_map = coef_map_net,
  gof_map  = c("nobs","r.squared.within"),
  add_rows = addrow_means_net_B,
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = paste(
    "Country & Year FE; SE clustered by origin-country. Shares in [0,1]; coefficients are pp per one-unit exposure.",
    "Net specs include adjustment-of-status share.", expo_note
  ),
  output = "outputs/tables/tab_main_net_B.tex"
)

mods_net_nomx <- list(
  "Blue"=m_nomx_blue$Net, "Agri"=m_nomx_ag$Net, "Manual"=m_nomx_manual$Net,
  "Services"=m_nomx_serv$Net, "White"=m_nomx_white$Net, "No-Occ"=m_nomx_noocc$Net,
  "Family"=m_fam_nx$Net, "Employment"=m_emp_nx$Net, "Humanitarian"=m_hum_nx$Net, "Other"=m_other_nx$Net
)

modelsummary(
  mods_net_nomx,
  coef_map = coef_map_net,
  gof_map  = c("nobs","r.squared.within"),
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = "Country & Year FE; SE clustered by origin-country. Mexico excluded.",
  output = "outputs/tables/tab_main_net_nomx.tex"
)

#### 7B) Country-specific linear trends robustness (Blue) ####

m_blue_trends_total <- feols(
  blue_share ~ irca_intensity*post | country + year + country[t],
  data = panel_total |> filter(!is.na(blue_share)),
  cluster = ~ country
)
m_blue_trends_net <- feols(
  blue_share ~ irca_intensity*post + adj_share | country + year + country[t],
  data = panel_net |> filter(!is.na(blue_share)),
  cluster = ~ country
)

modelsummary(
  list(
    "Baseline Total" = m_full_blue$Total,
    "Baseline Net"   = m_full_blue$Net,
    "Trends Total"   = m_blue_trends_total,
    "Trends Net"     = m_blue_trends_net
  ),
  coef_map = c("irca_intensity:post"="IRCA intensity × Post",
               "adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = "Country & Year FE; SE clustered by origin-country. Trends specs include origin-specific linear trends (country[t]).",
  output = "outputs/tables/tab_blue_trends_robustness.tex"
)

#### 8) Event study (blue_share, Total) ####

panel_es <- panel |> filter(!is.na(blue_share), !is.na(irca_intensity))
es_blue_total <- feols(
  blue_share ~ i(year, irca_intensity, ref = 1988) | country + year,
  data = panel_es, cluster = ~ country
)

# ---- Wald joint test: pre-period event-study coefficients = 0 ----
# Get the coefficient names and pick the pre-1988 "year::YYYY:irca_intensity" terms
cn <- names(coef(es_blue_total))

pre_terms <- cn[grepl("^year::(198[2-7]|1988).*:irca_intensity$|^year::(198[2-7]).*:irca_intensity$", cn)]
# NOTE: 1988 is the ref year, so it usually won't appear. We include the regex defensively.

# Safer: explicitly parse years from term names
get_year <- function(x) as.numeric(sub("^year::(\\d{4}).*$", "\\1", x))
yr <- suppressWarnings(get_year(pre_terms))
pre_terms <- pre_terms[!is.na(yr) & yr < 1988]

# Run Wald test if we found any terms
if (length(pre_terms) > 0) {
  w_pre <- fixest::wald(es_blue_total, pre_terms)
  
  # Save a simple text log
  sink("outputs/logs/wald_pretrends_es_blue_total.txt")
  cat("Wald joint pre-trends (years < 1988)\n")
  cat("H0: All pre-1988 exposure×year coefficients = 0\n\n")
  cat("stat:", w_pre$stat, "\n")
  cat("df1:",  w_pre$df1, "\n")
  cat("df2:",  w_pre$df2, "\n")
  cat("p:",    w_pre$p, "\n")
  cat("vcov:", w_pre$vcov, "\n")
  cat("Terms tested:\n"); cat(paste(pre_terms, collapse="\n")); cat("\n\n")
  sink()
  
  # Also save a tiny LaTeX table
  w_tab <- tibble::tibble(
    Test      = "Wald joint pre-trends (years < 1988)",
    Statistic = unname(w_pre$stat),
    df1       = unname(w_pre$df1),
    df2       = unname(w_pre$df2),
    p_value   = unname(w_pre$p)
  )
  
  w_tab_tex <- knitr::kable(
    w_tab %>% dplyr::mutate(across(where(is.numeric), ~ signif(.x, 4))),
    format = "latex", booktabs = TRUE,
    caption = "Wald joint test of pre-trend coefficients in the event-study specification."
  )
  write_tex(w_tab_tex, "outputs/tables/tab_wald_pretrends_es_blue_total.tex")
} else {
  warning("No pre-trend terms found for Wald test (check term naming).")
}


es_coefs <- broom::tidy(es_blue_total) |>
  filter(grepl("^year::", term)) |>
  mutate(
    year     = as.numeric(sub("year::(\\d+).*", "\\1", term)),
    rel_year = year - 1988
  )

p_es <- ggplot(es_coefs, aes(x = rel_year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error), width = 0.2) +
  labs(x = "Years relative to 1988",
       y = "Effect of IRCA exposure on blue-collar share (0–1)",
       title = "") +
  theme_bw()

save_plot(p_es, "outputs/figures/fig_es_blue_total.png", w=7, h=4.6)

# Formal pretrend regression (<=1988)
pre_panel <- panel |> filter(year <= 1988, !is.na(blue_share), !is.na(irca_intensity))
m_pretrend <- feols(
  blue_share ~ irca_intensity*t | country,
  data = pre_panel,
  cluster = ~ country
)
modelsummary(
  m_pretrend,
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = "Pre-period only (<=1988). Country FE; SE clustered by country. Tests whether exposure predicts pre-trends.",
  output = "outputs/tables/tab_formal_pretrend_test.tex"
)

#### 9) Early vs Late post (blue_share) ####
panel_period <- panel |>
  mutate(period = case_when(
    year %in% 1982:1988 ~ "pre",
    year %in% 1989:1992 ~ "early",
    year %in% 1993:2000 ~ "late",
    TRUE ~ NA_character_
  ))

panel_2post_total <- panel_period |> filter(!is.na(period), !is.na(blue_share), !is.na(irca_intensity))
panel_2post_net   <- panel_2post_total |> filter(!is.na(adj_share))

fe_blue_2post_total <- feols(
  blue_share ~ i(period, irca_intensity, ref = "pre") | country + year,
  data = panel_2post_total, cluster = ~ country
)
fe_blue_2post_net <- feols(
  blue_share ~ i(period, irca_intensity, ref = "pre") + adj_share | country + year,
  data = panel_2post_net, cluster = ~ country
)

tidy2 <- bind_rows(
  tidy(fe_blue_2post_total) %>% mutate(spec = "Total"),
  tidy(fe_blue_2post_net)   %>% mutate(spec = "Net of adj.")
) %>%
  filter(grepl("period::(early|late).*irca_intensity|irca_intensity.*period::(early|late)", term)) %>%
  mutate(
    label = ifelse(grepl("early", term), "IRCA × Early (89–92)", "IRCA × Late (93–00)"),
    est_pp = estimate*100,
    se_pp  = std.error*100,
    lo = est_pp - 1.96*se_pp, hi = est_pp + 1.96*se_pp
  )

p_early_late <- ggplot(tidy2, aes(x = label, y = est_pp, shape = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.4)) +
  labs(x = NULL, y = "Effect on blue-collar share (pp)",
       title = "IRCA exposure × period on blue-collar share") +
  theme_bw(base_size = 11) +
  theme(legend.title = element_blank())

save_plot(p_early_late, "outputs/figures/fig_early_late_blue.png", w=7, h=4.6)

#### 10) Descriptive: pre-trend group means (1982–1988) ####
blue_pre <- panel |>
  filter(year %in% 1982:1988) |>
  group_by(irca_group, year) |>
  summarise(mean_blue = mean(blue_share, na.rm=TRUE), .groups="drop")

p_pre <- ggplot(blue_pre, aes(x=year, y=mean_blue, color=irca_group)) +
  geom_line() + geom_point() +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  labs(x="Year", y="Mean blue-collar share", color="IRCA exposure",
       title="Pre-IRCA trends: blue-collar share, 1982–1988") +
  theme_bw()

save_plot(p_pre, "outputs/figures/fig_pretrend_groups.png", w=7, h=4.6)

#### 11) Placebos (pre-period only) ####
panel_pre <- panel |> filter(year %in% 1982:1988)

placebos <- list(
  `Fake post >= 1986` = feols(blue_share ~ irca_intensity*I(year>=1986) | country + year,
                              data = panel_pre, cluster = ~ country),
  `Fake post >= 1987` = feols(blue_share ~ irca_intensity*I(year>=1987) | country + year,
                              data = panel_pre, cluster = ~ country)
)

# Map exact term names
nm86 <- names(coef(placebos[[1]]))
nm86_term <- nm86[grepl("irca_intensity.*I\\(year >= 1986\\)|I\\(year >= 1986\\).*irca_intensity", nm86)]
nm87 <- names(coef(placebos[[2]]))
nm87_term <- nm87[grepl("irca_intensity.*I\\(year >= 1987\\)|I\\(year >= 1987\\).*irca_intensity", nm87)]

coef_map_placebo <- c(
  setNames("Exposure × FakePost(86)", nm86_term),
  setNames("Exposure × FakePost(87)", nm87_term)
)

modelsummary(
  placebos,
  coef_map = coef_map_placebo,
  gof_map  = c("nobs"),
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes    = "Country & Year FE; SE clustered by country. Pre-period only.",
  output   = "outputs/tables/tab_placebos.tex"
)

#### 12) Alternative intensity (Blue, Net) + standardized effects ####
class_early <- class |>
  group_by(country) |>
  summarise(
    pre658_avg         = mean(total_adm[year %in% 1980:1985], na.rm = TRUE),
    irca_89_92         = sum(coalesce(irca_legalization, 0) * (year %in% 1989:1992), na.rm = TRUE),
    irca_intensity_alt = if_else(pre658_avg > 0, irca_89_92 / pre658_avg, NA_real_),
    .groups = "drop"
  )

panel_alt <- panel |>
  left_join(class_early |> select(country, irca_intensity_alt), by = "country")

panel_alt_net <- panel_alt |> filter(!is.na(blue_share), !is.na(irca_intensity_alt), !is.na(post), !is.na(adj_share))
panel_alt_nomx_net <- panel_alt_net |> filter(country != "Mexico")

fe_blue_net <- feols(blue_share ~ irca_intensity*post + adj_share | country + year,
                     data = panel_net, cluster = ~ country)


run_blue_net <- function(df){
  fixest::feols(
    blue_share ~ irca_intensity*post + adj_share | country + year,
    data = df, cluster = ~ country
  )
}

run_noocc_net <- function(df){
  # noocc_share exists in panel; but it may be NA in some rows
  df2 <- df |> dplyr::filter(!is.na(noocc_share))
  fixest::feols(
    noocc_share ~ irca_intensity*post + adj_share | country + year,
    data = df2, cluster = ~ country
  )
}

# ---- (1) Drop high-missing years 1995–1997 ----
drop_years <- 1995:1997
panel_net_drop9597 <- panel_net |> dplyr::filter(!(year %in% drop_years))
m_blue_net_drop9597 <- run_blue_net(panel_net_drop9597)

# ---- (2) Drop incomplete-coverage countries (Uruguay, Paraguay) ----
# (Based on your coverage table: these two have incomplete key-var coverage)
drop_cty <- c("Uruguay","Paraguay")
panel_net_drop_incomplete <- panel_net |> dplyr::filter(!(country %in% drop_cty))
m_blue_net_drop_incomplete <- run_blue_net(panel_net_drop_incomplete)

# ---- (3) Shorter window 1982–1995 (often used for data quality) ----
panel_net_8295 <- panel_net |> dplyr::filter(year %in% 1982:1995)
m_blue_net_8295 <- run_blue_net(panel_net_8295)

# ---- (4) Denominator diagnostic: no-occupation share trends + DiD ----

# 4A) Plot noocc_share by exposure group over time
noocc_trends <- panel |> 
  dplyr::filter(!is.na(noocc_share), year %in% 1982:2000) |>
  dplyr::group_by(irca_group, year) |>
  dplyr::summarise(mean_noocc = mean(noocc_share, na.rm = TRUE), .groups = "drop")

p_noocc <- ggplot2::ggplot(noocc_trends, ggplot2::aes(x=year, y=mean_noocc, color=irca_group)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=1.5) +
  ggplot2::scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  ggplot2::labs(
    x="Year", y="Mean no-occupation share",
    color="IRCA exposure",
    title="No-occupation share over time by IRCA exposure group"
  ) +
  ggplot2::theme_bw()

save_plot(p_noocc, "outputs/figures/fig_noocc_trends_by_exposure.png", w=7, h=4.6)

# 4B) DiD on noocc_share in the Net sample (controls adj_share)
m_noocc_net <- run_noocc_net(panel_net)

# ---- Export a single robustness table (baseline vs variants) ----
modelsummary::modelsummary(
  list(
    "Baseline Net"          = fe_blue_net,
    "Drop 1995–1997"        = m_blue_net_drop9597,
    "Drop UY+PY"            = m_blue_net_drop_incomplete,
    "Window 1982–1995"      = m_blue_net_8295
  ),
  coef_map = c("irca_intensity:post"="IRCA × Post", "adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  stars    = c('*'=.10,'**'=.05,'***'=.01),
  notes    = "All models: Country & Year FE; SE clustered by country. Outcome: blue_share.",
  output   = "outputs/tables/tab_robust_extra_blue.tex"
)

# ---- Export no-occupation regression table ----
modelsummary::modelsummary(
  list("No-occ share (Net sample)" = m_noocc_net),
  coef_map = c("irca_intensity:post"="IRCA × Post", "adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  stars    = c('*'=.10,'**'=.05,'***'=.01),
  notes    = "Country & Year FE; SE clustered by country. Outcome: noocc_share.",
  output   = "outputs/tables/tab_noocc_did.tex"
)

#### 12B)  ADD-ONS — magnitudes, bins, mechanism scan, diagnostics ####

# ---- (i) Economic magnitudes table + standardized bar chart ----

write_magnitudes_blue(panel_net = panel_net, fe_blue_net = fe_blue_net)

# ---- (ii) Heterogeneity by exposure quartile (bins) ----
q_tbl <- panel |>
  dplyr::distinct(country, irca_intensity) |>
  dplyr::mutate(q = dplyr::ntile(irca_intensity, 4))

panel_q <- panel_net |>
  dplyr::left_join(q_tbl, by="country") |>
  dplyr::mutate(q = factor(q, levels=1:4, labels=paste0("Q",1:4)))

m_bins <- fixest::feols(
  blue_share ~ i(q, post, ref="Q1") + adj_share | country + year,
  data = panel_q, cluster = ~ country
)

modelsummary::modelsummary(
  list("Blue (Net) bins" = m_bins),
  coef_map = c(
    "q::Q2:post"="Post × Q2",
    "q::Q3:post"="Post × Q3",
    "q::Q4:post"="Post × Q4",
    "adj_share"="Adjustment share"
  ),
  gof_map = c("nobs","r.squared.within"),
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = "Country & Year FE; SE clustered by country. Exposure quartiles at country level; Q1 omitted.",
  output = "outputs/tables/tab_bins_blue.tex"
)

bin_coefs <- broom::tidy(m_bins) |>
  dplyr::filter(grepl("q::Q[2-4]:post", term)) |>
  dplyr::mutate(
    quartile = gsub("q::(Q[2-4]):post","\\1",term),
    pp = 100*estimate,
    lo = 100*(estimate - 1.96*std.error),
    hi = 100*(estimate + 1.96*std.error)
  )

p_bins <- ggplot(bin_coefs, aes(x=quartile, y=pp)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_pointrange(aes(ymin=lo,ymax=hi)) +
  labs(x="Exposure quartile (Q1 omitted)", y="Post effect on blue share (pp)",
       title="Heterogeneity by exposure quartile (Net)") +
  theme_bw()
save_plot(p_bins, "outputs/figures/fig_bins_blue.png", w=6.5, h=4)

# ---- (iii) Mechanism scan: IRCA×Post across multiple outcomes ----
outcomes_mech <- c("blue_share","serv_share","white_share",
                   "fam_share","emp_share",
                   "male_share","adj_share")

run_mech <- function(y){
  dat <- panel |>
    dplyr::filter(!is.na(.data[[y]]), !is.na(irca_intensity), !is.na(post))
  rhs <- if (y == "adj_share") "irca_intensity*post" else "irca_intensity*post + adj_share"
  fixest::feols(as.formula(paste0(y," ~ ", rhs, " | country + year")),
                data = dat, cluster = ~ country)
}

mods_mech <- lapply(outcomes_mech, run_mech)
names(mods_mech) <- outcomes_mech

mods_mech_A <- mods_mech[c("blue_share","serv_share","white_share","fam_share","emp_share")]
mods_mech_B <- mods_mech[c("male_share","adj_share")]

modelsummary::modelsummary(
  mods_mech_A,
  coef_map = c("irca_intensity:post"="IRCA × Post","adj_share"="Adjustment share"),
  gof_map = c("nobs","r.squared.within"),
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = "Country & Year FE; SE clustered by country. Panel A: core outcomes.",
  output="outputs/tables/tab_mechanisms_summary_A.tex"
)

modelsummary::modelsummary(
  mods_mech_B,
  coef_map = c("irca_intensity:post"="IRCA × Post","adj_share"="Adjustment share"),
  gof_map = c("nobs","r.squared.within"),
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = "Country & Year FE; SE clustered by country. Panel B: secondary outcomes (smaller samples for some outcomes).",
  output="outputs/tables/tab_mechanisms_summary_B.tex"
)

coef_df <- dplyr::bind_rows(lapply(names(mods_mech), function(nm){
  tt <- broom::tidy(mods_mech[[nm]])
  out <- tt[tt$term=="irca_intensity:post",]
  out$outcome <- nm
  out
})) |>
  dplyr::mutate(
    pp = 100*estimate,
    lo = 100*(estimate - 1.96*std.error),
    hi = 100*(estimate + 1.96*std.error)
  )

p_mech <- ggplot(coef_df, aes(x=reorder(outcome, pp), y=pp)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_pointrange(aes(ymin=lo,ymax=hi)) +
  coord_flip() +
  labs(x=NULL, y="Effect (pp)",
       title="IRCA × Post effects across outcomes (mechanism scan)") +
  theme_bw()
save_plot(p_mech, "outputs/figures/fig_mechanisms_coefplot.png", w=7, h=4.8)

# ---- (iv) Residual diagnostic (baseline Net) ----
dat_est <- panel_net |>
  dplyr::filter(
    !is.na(blue_share),
    !is.na(irca_intensity),
    !is.na(post),
    !is.na(adj_share)
  )

stopifnot(nrow(dat_est) == length(resid(fe_blue_net)))  

res_df <- data.frame(
  year  = dat_est$year,
  resid = resid(fe_blue_net)
) |>
  dplyr::group_by(year) |>
  dplyr::summarise(mean_resid = mean(resid, na.rm=TRUE), .groups="drop")

p_res <- ggplot(res_df, aes(x=year, y=mean_resid)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line() +
  geom_point() +
  labs(x="Year", y="Mean residual",
       title="Residual sanity check: baseline Net (mean residual by year)") +
  theme_bw()

save_plot(p_res, "outputs/figures/fig_residuals_by_year_blue.png", w=7, h=4)

fe_blue_alt <- feols(blue_share ~ irca_intensity_alt*post + adj_share | country + year,
                     data = panel_alt_net, cluster = ~ country)

fe_blue_net_nx <- feols(blue_share ~ irca_intensity*post + adj_share | country + year,
                        data = panel_nomx_net, cluster = ~ country)
fe_blue_alt_nx <- feols(blue_share ~ irca_intensity_alt*post + adj_share | country + year,
                        data = panel_alt_nomx_net, cluster = ~ country)

modelsummary(
  list(
    "Blue Net (baseline)"  = fe_blue_net,
    "Blue Net (alt denom)" = fe_blue_alt,
    "Blue Net (–MX)"       = fe_blue_net_nx,
    "Blue Net (alt –MX)"   = fe_blue_alt_nx
  ),
  coef_map = c(
    "irca_intensity:post"     = "IRCA intensity × Post",
    "irca_intensity_alt:post" = "IRCA intensity (alt) × Post",
    "adj_share"               = "Adjustment share"
  ),
  gof_map  = c("nobs","r.squared.within"),
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes    = "Country & Year FE; SE clustered by origin-country. Outcome is share in [0,1]; coefficients are pp.",
  output   = "outputs/tables/tab_alt_intensity_blue.tex"
)

#### 13) Weights (Blue, Net) ####
fe_blue_w_adm <- feols(blue_share ~ irca_intensity*post + adj_share | country + year,
                       data = panel_net, weights = ~ total_adm, cluster = ~ country)
fe_blue_w_occ <- feols(blue_share ~ irca_intensity*post + adj_share | country + year,
                       data = panel_net, weights = ~ occ_known, cluster = ~ country)

modelsummary(
  list("Net (unweighted)" = fe_blue_net,
       "Net (w: admissions)" = fe_blue_w_adm,
       "Net (w: known occ.)" = fe_blue_w_occ),
  coef_map = c("irca_intensity:post"="IRCA intensity × Post","adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = "Country & Year FE; SE clustered by country.",
  output = "outputs/tables/tab_weights_blue.tex"
)

#### 14) Full denominator check (Blue) ####
panel_fd <- panel |>
  mutate(blue_fullden = blue_collar_non_farm / (occ_known + no_occupation))

fe_blue_fullden <- feols(blue_fullden ~ irca_intensity*post + adj_share | country + year,
                         data = panel_fd, cluster = ~ country)

modelsummary(
  list("Blue (Net denom)" = fe_blue_net,
       "Blue (Full denom)" = fe_blue_fullden),
  coef_map = c("irca_intensity:post" = "IRCA intensity × Post",
               "adj_share"           = "Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes    = "Country & Year FE; SE clustered by origin-country. Full denom divides by known+unknown occupations.",
  output   = "outputs/tables/tab_full_denom_blue.tex"
)

#### 15) Balanced panel (Blue, Net)  ####
years_full <- sort(unique(panel_net$year))

balanced_countries <- panel_net |>
  group_by(country) |>
  summarise(n_years = n_distinct(year), .groups="drop") |>
  filter(n_years == length(years_full)) |>
  pull(country)

panel_bal <- panel_net |> filter(country %in% balanced_countries)

fe_blue_bal <- feols(
  blue_share ~ irca_intensity*post + adj_share | country + year,
  data = panel_bal, cluster = ~ country
)

modelsummary(
  list("Blue Net (balanced)" = fe_blue_bal),
  coef_map = c("irca_intensity:post"="IRCA × Post","adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  fmt=3, stars=c('*'=.10,'**'=.05,'***'=.01),
  notes="Balanced panel: keeps only countries observed in all years of the Net sample.",
  output="outputs/tables/tab_balanced_blue.tex"
)

#### 16) Leave-one-out (Blue Net) ####
ctrs <- sort(unique(panel_net$country))
b_ref <- unname(coef(fe_blue_net)["irca_intensity:post"])

loo <- lapply(ctrs, function(cntry){
  dat <- filter(panel_net, country != cntry)
  m   <- feols(blue_share ~ irca_intensity*post + adj_share | country + year,
               data = dat, cluster = ~ country)
  data.frame(drop = cntry, beta = unname(coef(m)["irca_intensity:post"]))
})
loo_res <- bind_rows(loo)

p_loo <- ggplot(loo_res, aes(x = reorder(drop, beta), y = beta)) +
  geom_hline(yintercept = b_ref, linetype = "dashed") +
  geom_point() +
  coord_flip() +
  labs(x = "Country left out", y = expression(hat(beta)~"(IRCA intensity × Post)"),
       title = "Leave-one-out DiD coefficient: Blue-collar share (Net)") +
  theme_bw()

save_plot(p_loo, "outputs/figures/fig_loo_blue_net.png", w=7, h=5)

#### 17) Descriptive figures + missingness + admissions + demographics + portfolio add-ons ####


#### 18) SPEC COMPARE  ####
m_base <- fe_blue_net

m_trends <- feols(
  blue_share ~ irca_intensity*post + adj_share | country + year + country[t],
  data = panel_net, cluster = ~ country
)

modelsummary::modelsummary(
  list("Baseline Net"=m_base,
       "Country trends"=m_trends,
       "WLS admissions"=fe_blue_w_adm,
       "Balanced Net"=fe_blue_bal),
  coef_map = c("irca_intensity:post"="IRCA × Post","adj_share"="Adjustment share"),
  gof_map = c("nobs","r.squared.within"),
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = "Comparison of key robustness specifications for blue-collar share.",
  output = "outputs/tables/tab_spec_compare_blue.tex"
)

#### 19) Logs: run summary + sessionInfo ####
run_summary <- c(
  paste0("n_rows = ", nrow(panel)),
  paste0("n_country_year = ", nrow(distinct(panel, country, year))),
  paste0("n_countries = ", n_distinct(panel$country)),
  paste0("min_year = ", min(panel$year, na.rm=TRUE)),
  paste0("max_year = ", max(panel$year, na.rm=TRUE)),
  paste0("irca_intensity range = [", round(min(panel$irca_intensity, na.rm=TRUE),5), ", ", round(max(panel$irca_intensity, na.rm=TRUE),5), "]"),
  paste0("blue_share range = [", round(min(panel$blue_share, na.rm=TRUE),5), ", ", round(max(panel$blue_share, na.rm=TRUE),5), "]"),
  paste0("adj_share range = [", round(min(panel$adj_share, na.rm=TRUE),5), ", ", round(max(panel$adj_share, na.rm=TRUE),5), "]"),
  paste0("balanced countries (net) = ", length(balanced_countries))
)

writeLines(run_summary, "outputs/logs/run_summary.txt")
writeLines(capture.output(sessionInfo()), "outputs/logs/sessionInfo.txt")

cat("DONE.\n- panel saved: data_clean/panel.rds\n- tables: outputs/tables/\n- figures: outputs/figures/\n- logs: outputs/logs/\n")
############################################################