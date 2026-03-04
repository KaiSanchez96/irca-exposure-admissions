############################################################
# run.R — IRCA project (FIXED + Portfolio-forward)
# - Auto-detects raw Excel files
# - Builds panel from raw files
# - Runs main FE models + robustness + portfolio add-ons
# - Exports tables (.tex) and figures (.png)
# - Writes logs + panel snapshot (.rds)
#
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

#### 0.2) General magnitudes writer ####
write_magnitudes <- function(df, model, yvar,
                             out_table,
                             out_plot,
                             caption = "Economic magnitudes") {
  
  stopifnot(yvar %in% names(df))
  expo_vec <- df %>%
    distinct(country, irca_intensity) %>%
    pull(irca_intensity)
  
  expo_vec <- expo_vec[is.finite(expo_vec)]
  expo_sd  <- sd(expo_vec, na.rm = TRUE)
  expo_iqr <- IQR(expo_vec, na.rm = TRUE)
  
  b <- unname(coef(model)["irca_intensity:post"])
  y_mean <- mean(df[[yvar]], na.rm = TRUE)
  
  magnitudes <- tibble::tibble(
    Magnitude = c("Per 1 unit exposure",
                  "Per 1 SD exposure",
                  "Per IQR exposure",
                  "As % of mean(Y): 1 SD",
                  "As % of mean(Y): IQR"),
    `Effect (pp / %)` = c(
      100 * b,
      100 * b * expo_sd,
      100 * b * expo_iqr,
      (b * expo_sd)  / y_mean * 100,
      (b * expo_iqr) / y_mean * 100
    )
  )
  
  tab_mag <- knitr::kable(
    magnitudes,
    format = "latex", booktabs = TRUE, digits = 2,
    caption = caption
  )
  write_tex(tab_mag, out_table)
  
  plot_mag <- tibble::tibble(
    scale = c("1 SD exposure","IQR exposure"),
    pp = c(100 * b * expo_sd, 100 * b * expo_iqr)
  )
  
  p_mag <- ggplot(plot_mag, aes(x = scale, y = pp)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype="dashed") +
    labs(x=NULL, y="Effect (pp)", title=caption) +
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

#### 2) Helper ####
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
    noocc_share = if_else((occ_known + no_occupation) > 0,
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

# exposure median cutoff for groups (log it once)
expo_median <- median(irca_intensity$irca_intensity, na.rm = TRUE)
writeLines(
  paste0("Exposure median cutoff used for irca_group = ", signif(expo_median, 6)),
  "outputs/logs/exposure_cutoff.txt"
)

panel <- occ |>
  left_join(adj_panel,      by = c("country","year")) |>
  left_join(class,          by = c("country","year")) |>
  left_join(gender_cty,     by = c("country","year")) |>
  left_join(irca_intensity, by = "country") |>
  mutate(
    post         = if_else(year %in% 1989:1992, 1L, 0L),
    
    # guarded denominators
    white_share  = if_else(occ_known > 0, (white_high + white_support) / occ_known, NA_real_),
    ag_share     = if_else(occ_known > 0, agriculture / occ_known, NA_real_),
    manual_share = if_else(occ_known > 0, (blue_collar_non_farm + agriculture) / occ_known, NA_real_),
    
    irca_group   = if_else(irca_intensity >= expo_median, "High IRCA", "Low IRCA"),
    t            = year - min(year, na.rm = TRUE),
    
    fam_share   = if_else(total_adm > 0, family_based_admissions     / total_adm, NA_real_),
    emp_share   = if_else(total_adm > 0, employment_based_admissions / total_adm, NA_real_),
    hum_share   = if_else(total_adm > 0, humanitarian_admissions     / total_adm, NA_real_),
    other_share = if_else(total_adm > 0, other_miscellaneous         / total_adm, NA_real_)
  )

saveRDS(panel, "data_clean/panel.rds")

panel_total <- panel |> filter(!is.na(irca_intensity), !is.na(post))
panel_net   <- panel_total |> filter(!is.na(adj_share))

# ---- Full-denominator dataset ----
panel_fd <- panel |>
  mutate(
    occ_total = occ_known + no_occupation,
    blue_fullden = if_else(is.finite(occ_total) & occ_total > 0,
                           blue_collar_non_farm / occ_total,
                           NA_real_)
  )

############################################################
####  DECOMPOSITION: levels behind the share result
############################################################
decomp_df <- panel_net |>
  mutate(
    blue_level      = as.numeric(blue_collar_non_farm),
    occ_known_level = as.numeric(occ_known),
    noocc_level     = as.numeric(no_occupation),
    occ_total_level = as.numeric(occ_known + no_occupation),
    ln_blue_level      = log1p(pmax(blue_level, 0)),
    ln_occ_known_level = log1p(pmax(occ_known_level, 0)),
    ln_noocc_level     = log1p(pmax(noocc_level, 0)),
    ln_occ_total_level = log1p(pmax(occ_total_level, 0))
  ) |>
  filter(
    is.finite(blue_level),
    is.finite(occ_known_level),
    is.finite(noocc_level),
    is.finite(occ_total_level)
  )

run_decomp <- function(df, y){
  feols(
    as.formula(paste0(y, " ~ irca_intensity*post + adj_share | country + year")),
    data = df, cluster = ~ country
  )
}

m_blue_lvl      <- run_decomp(decomp_df, "blue_level")
m_occ_known_lvl <- run_decomp(decomp_df, "occ_known_level")
m_occ_total_lvl <- run_decomp(decomp_df, "occ_total_level")
m_noocc_lvl     <- run_decomp(decomp_df, "noocc_level")

m_ln_blue      <- run_decomp(decomp_df, "ln_blue_level")
m_ln_occ_known <- run_decomp(decomp_df, "ln_occ_known_level")
m_ln_occ_total <- run_decomp(decomp_df, "ln_occ_total_level")
m_ln_noocc     <- run_decomp(decomp_df, "ln_noocc_level")

modelsummary(
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

modelsummary(
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

extract_irca_post <- function(mod, outcome_label){
  tt <- broom::tidy(mod)
  rr <- tt[tt$term == "irca_intensity:post", ]
  rr$outcome <- outcome_label
  rr
}

coef_df <- bind_rows(
  extract_irca_post(m_blue_lvl,      "Blue level"),
  extract_irca_post(m_occ_known_lvl, "Occ known level"),
  extract_irca_post(m_occ_total_lvl, "Occ total level"),
  extract_irca_post(m_noocc_lvl,     "No-occ level"),
  extract_irca_post(m_ln_blue,       "ln(1+Blue)"),
  extract_irca_post(m_ln_occ_known,  "ln(1+Occ known)"),
  extract_irca_post(m_ln_occ_total,  "ln(1+Occ total)"),
  extract_irca_post(m_ln_noocc,      "ln(1+No-occ)")
) |>
  mutate(
    est = estimate,
    lo  = estimate - 1.96*std.error,
    hi  = estimate + 1.96*std.error
  )

lvl_order <- c("Blue level","Occ known level","Occ total level","No-occ level")
log_order <- c("ln(1+Blue)","ln(1+Occ known)","ln(1+Occ total)","ln(1+No-occ)")

coef_df <- coef_df |>
  mutate(
    grp = case_when(
      outcome %in% lvl_order ~ "Levels",
      outcome %in% log_order ~ "Logs",
      TRUE ~ "Other"
    )
  )

coef_lvl <- coef_df |> filter(grp == "Levels") |> mutate(outcome = factor(outcome, levels = lvl_order))
p_decomp_levels <- ggplot(coef_lvl, aes(x = outcome, y = est)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  coord_flip() +
  labs(x = NULL, y = "Estimated effect of IRCA × Post (levels)",
       title = "Decomposition (levels): IRCA × Post effects on numerator/denominators") +
  theme_bw()
save_plot(p_decomp_levels, "outputs/figures/fig_decomp_levels_only.png", w = 8, h = 4.8)

coef_log <- coef_df |> filter(grp == "Logs") |> mutate(outcome = factor(outcome, levels = log_order))
p_decomp_logs <- ggplot(coef_log, aes(x = outcome, y = est)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  coord_flip() +
  labs(x = NULL, y = "Estimated effect of IRCA × Post (log(1+x))",
       title = "Decomposition (logs): IRCA × Post effects on numerator/denominators") +
  theme_bw()
save_plot(p_decomp_logs, "outputs/figures/fig_decomp_logs_only.png", w = 8, h = 4.8)

#### DATA QA — Missingness + coverage ####
vars_for_heat <- c(
  "blue_share","ag_share","manual_share","serv_share","white_share","noocc_share",
  "fam_share","emp_share","hum_share","other_share",
  "male_share","adj_share"
)

miss_heat <- panel %>%
  select(country, year, any_of(vars_for_heat)) %>%
  pivot_longer(-c(country, year), names_to="var", values_to="val") %>%
  mutate(missing = as.integer(is.na(val)))

p_miss <- ggplot(miss_heat, aes(x=year, y=country, fill=missing)) +
  geom_tile() +
  scale_fill_gradient(limits=c(0,1), breaks=c(0,1), labels=c("Observed","Missing")) +
  facet_wrap(~ var, ncol=3) +
  labs(x=NULL, y=NULL, fill=NULL, title="Missingness heatmap by country–year and variable") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position="bottom")
save_plot(p_miss, "outputs/figures/fig_missing_heatmap.png", w=10, h=8)

lab <- c(
  blue_share="Blue share", ag_share="Agriculture share", manual_share="Manual share",
  serv_share="Services share", white_share="White share", noocc_share="No-occupation share",
  fam_share="Family share", emp_share="Employment share", hum_share="Humanitarian share", other_share="Other share",
  male_share="Male share", adj_share="Adjustment share"
)

miss_tab <- panel %>%
  summarise(
    across(any_of(vars_for_heat), ~ sum(is.na(.x)),  .names = "n_miss_{col}"),
    across(any_of(vars_for_heat), ~ sum(!is.na(.x)), .names = "n_obs_{col}")
  ) %>%
  pivot_longer(
    everything(),
    names_to      = c(".value","var"),
    names_pattern = "n_(miss|obs)_(.*)"
  ) %>%
  rename(n_miss = miss, n_obs = obs) %>%
  mutate(
    n_total    = n_obs + n_miss,
    share_miss = n_miss / n_total,
    variable   = recode(.data[["var"]], !!!lab)
  ) %>%
  select(variable, n_obs, n_miss, n_total, share_miss) %>%
  arrange(desc(share_miss))

miss_tab_tex <- knitr::kable(
  miss_tab %>%
    mutate(`Share missing (%)` = 100*share_miss) %>%
    transmute(
      Variable = variable,
      `Obs.` = n_obs, Missing = n_miss, Total = n_total,
      `Share missing (%)` = sprintf("%.1f", `Share missing (%)`)
    ),
  format="latex", booktabs=TRUE,
  caption="Missingness by variable at the country–year level."
)
writeLines(miss_tab_tex, "outputs/tables/tab_missing_overall.tex")

miss_by_year <- panel %>%
  group_by(year) %>%
  summarise(across(any_of(vars_for_heat), ~ mean(is.na(.x)), .names="{col}"),
            .groups="drop") %>%
  pivot_longer(-year, names_to="var", values_to="share_missing") %>%
  mutate(variable = recode(.data[["var"]], !!!lab)) %>%
  select(year, variable, share_missing) %>%
  pivot_wider(names_from = variable, values_from = share_missing) %>%
  arrange(year)

miss_by_year_tex <- knitr::kable(
  miss_by_year %>% mutate(across(-year, ~ sprintf("%.0f\\%%", 100*.x))),
  format="latex", booktabs=TRUE,
  caption="Share missing by variable and year (percent)."
)
writeLines(miss_by_year_tex, "outputs/tables/tab_missing_byyear.tex")

key_vars <- c("blue_share","adj_share","irca_intensity","post")
cov_cty <- panel %>%
  group_by(country) %>%
  summarise(
    years_total = n_distinct(year),
    years_covered = n_distinct(year[ if_all(any_of(key_vars), ~ !is.na(.x)) ]),
    coverage_ratio = years_covered / years_total,
    .groups="drop"
  ) %>%
  arrange(desc(coverage_ratio))

cov_cty_tex <- knitr::kable(
  cov_cty %>%
    mutate(`Coverage ratio` = sprintf("%.3f", coverage_ratio)) %>%
    select(Country = country,
           `Years total` = years_total,
           `Years covered (key vars)` = years_covered,
           `Coverage ratio`),
  format="latex", booktabs=TRUE,
  caption="Country coverage by years and key variables."
)
writeLines(cov_cty_tex, "outputs/tables/tab_coverage_country.tex")

#### EDA — Exposure distribution + exposure vs change in blue ####
expo_country <- panel %>%
  distinct(country, irca_intensity) %>%
  filter(is.finite(irca_intensity))

p_hist <- ggplot(expo_country, aes(x=irca_intensity)) +
  geom_histogram(bins=12) +
  labs(x="IRCA exposure (country-level)", y="Count",
       title="Distribution of IRCA exposure across origin countries") +
  theme_bw()
save_plot(p_hist, "outputs/figures/fig_exposure_hist.png", w=6.5, h=4.2)

chg_cty <- panel %>%
  filter(!is.na(blue_share)) %>%
  mutate(period = case_when(
    year %in% 1982:1985 ~ "pre",
    year %in% 1989:1992 ~ "irca",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(country, period) %>%
  summarise(blue = mean(blue_share, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=period, values_from=blue) %>%
  left_join(panel %>% distinct(country, irca_intensity), by="country") %>%
  mutate(d_blue = irca - pre)

p_sc <- ggplot(chg_cty, aes(x=irca_intensity, y=d_blue, label=country)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point() +
  ggrepel::geom_text_repel(size=3, max.overlaps=20) +
  geom_smooth(method="lm", se=FALSE) +
  scale_y_continuous(labels=scales::percent) +
  labs(x="IRCA exposure", y="Δ blue share (89–92 minus 82–85)",
       title="Exposure vs change in blue-collar share (pre vs IRCA surge)") +
  theme_bw()
save_plot(p_sc, "outputs/figures/fig_scatter_exposure_dblue_all.png", w=7, h=4.6)

panel_nomx       <- panel |> filter(country != "Mexico")
panel_nomx_total <- panel_total |> filter(country != "Mexico")
panel_nomx_net   <- panel_net |> filter(country != "Mexico")

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

#### DASHBOARD FIGURE ####
dash_df <- panel |>
  filter(year %in% 1982:2000) |>
  mutate(
    occ_total = occ_known + no_occupation,
    blue_fullden = if_else(is.finite(occ_total) & occ_total > 0,
                           blue_collar_non_farm / occ_total,
                           NA_real_)
  ) |>
  group_by(irca_group, year) |>
  summarise(
    blue_net   = mean(blue_share, na.rm = TRUE),
    blue_full  = mean(blue_fullden, na.rm = TRUE),
    noocc      = mean(noocc_share, na.rm = TRUE),
    blue_lvl   = mean(as.numeric(blue_collar_non_farm), na.rm = TRUE),
    occ_total  = mean(as.numeric(occ_total), na.rm = TRUE),
    .groups = "drop"
  )

p1 <- ggplot(dash_df, aes(x=year, y=blue_net, color=irca_group)) +
  geom_line() + geom_point(size=1.3) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  labs(x=NULL, y="Blue share (net denom)", color="Exposure group",
       title="A) Blue-collar share (net denominator)") +
  theme_bw() + theme(legend.position="bottom")

p2 <- ggplot(dash_df, aes(x=year, y=noocc, color=irca_group)) +
  geom_line() + geom_point(size=1.3) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  labs(x=NULL, y="No-occupation share", color="Exposure group",
       title="B) No-occupation share (reporting/denominator channel)") +
  theme_bw() + theme(legend.position="bottom")

p3 <- ggplot(dash_df, aes(x=year, y=blue_lvl, color=irca_group)) +
  geom_line() + geom_point(size=1.1) +
  labs(x=NULL, y="Mean count", color="Exposure group",
       title="C) Blue-collar counts (levels)") +
  theme_bw() + theme(legend.position="bottom")

p4 <- ggplot(dash_df, aes(x=year, y=occ_total, color=irca_group)) +
  geom_line() + geom_point(size=1.1) +
  labs(x="Year", y="Mean count", color="Exposure group",
       title="D) Total occupations (known + no-occ)") +
  theme_bw() + theme(legend.position="bottom")

# Combine via patchwork
if (!requireNamespace("patchwork", quietly = TRUE)) {
  warning("Package 'patchwork' not installed. Saving dashboard panels separately.")
  save_plot(p1, "outputs/figures/fig_PORTFOLIO_dash_A.png", w=7, h=4.6)
  save_plot(p2, "outputs/figures/fig_PORTFOLIO_dash_B.png", w=7, h=4.6)
  save_plot(p3, "outputs/figures/fig_PORTFOLIO_dash_C.png", w=7, h=4.6)
  save_plot(p4, "outputs/figures/fig_PORTFOLIO_dash_D.png", w=7, h=4.6)
} else {
  library(patchwork)
  p_dash <- (p1 + p2) / (p3 + p4) + patchwork::plot_layout(guides = "collect")
  save_plot(p_dash, "outputs/figures/fig_PORTFOLIO_dashboard.png", w=10.5, h=8.0)
}

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
  term = "Mean of Y (share units)",
  !!!setNames(lapply(outcomes_all, \(y) meanY_on_sample(panel, y, need_adj = FALSE)), outcomes_all)
)
addrow_means_net <- tibble::tibble(
  term = "Mean of Y (share units)",
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
  coef_map = c("irca_intensity:post"="IRCA intensity × Post", "adj_share"="Adjustment share"),
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

# robust Wald pretrend selection: parse all year::YYYY terms, keep < 1988
cn <- names(coef(es_blue_total))
year_terms <- cn[grepl("^year::\\d{4}.*:irca_intensity", cn)]
get_year <- function(x) as.numeric(sub("^year::(\\d{4}).*$", "\\1", x))
yrs <- suppressWarnings(get_year(year_terms))
pre_terms <- year_terms[is.finite(yrs) & yrs < 1988]

if (length(pre_terms) > 0) {
  w_pre <- fixest::wald(es_blue_total, pre_terms)
  
  sink("outputs/logs/wald_pretrends_es_blue_total.txt")
  cat("Wald joint pre-trends (years < 1988)\n")
  cat("H0: All pre-1988 exposure×year coefficients = 0\n\n")
  cat("stat:", w_pre$stat, "\n")
  cat("df1:",  w_pre$df1, "\n")
  cat("df2:",  w_pre$df2, "\n")
  cat("p:",    w_pre$p, "\n\n")
  cat("Terms tested:\n"); cat(paste(pre_terms, collapse="\n")); cat("\n")
  sink()
  
  w_tab <- tibble::tibble(
    Test      = "Wald joint pre-trends (years < 1988)",
    Statistic = unname(w_pre$stat),
    df1       = unname(w_pre$df1),
    df2       = unname(w_pre$df2),
    p_value   = unname(w_pre$p)
  )
  
  w_tab_tex <- knitr::kable(
    w_tab %>% mutate(across(where(is.numeric), ~ signif(.x, 4))),
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

pre_panel <- panel |> filter(year <= 1988, !is.na(blue_share), !is.na(irca_intensity))
m_pretrend <- feols(
  blue_share ~ irca_intensity*t | country,
  data = pre_panel, cluster = ~ country
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

#### 12) Alternative intensity (Blue, Net) + Portfolio main comparison ####
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

panel_alt_net      <- panel_alt |> filter(!is.na(blue_share), !is.na(irca_intensity_alt), !is.na(post), !is.na(adj_share))
panel_alt_nomx_net <- panel_alt_net |> filter(country != "Mexico")

dat_blue_net <- panel_net |>
  dplyr::filter(!is.na(blue_share), !is.na(irca_intensity), !is.na(post), !is.na(adj_share))

fe_blue_net <- feols(
  blue_share ~ irca_intensity*post + adj_share | country + year,
  data = dat_blue_net,
  cluster = ~ country
)

# ---- PORTFOLIO MAIN RESULT: Net vs Full denominator on SAME sample ----
cmp <- panel_fd |>
  filter(!is.na(irca_intensity), !is.na(post), !is.na(adj_share),
         !is.na(blue_share), !is.na(blue_fullden))

fe_blue_net_cmp <- feols(
  blue_share ~ irca_intensity*post + adj_share | country + year,
  data = cmp, cluster = ~ country
)
fe_blue_fullden_cmp <- feols(
  blue_fullden ~ irca_intensity*post + adj_share | country + year,
  data = cmp, cluster = ~ country
)

modelsummary::modelsummary(
  list(
    "Blue share (Net denom; same sample)"  = fe_blue_net_cmp,
    "Blue share (Full denom; same sample)" = fe_blue_fullden_cmp
  ),
  coef_map = c("irca_intensity:post"="IRCA intensity × Post", "adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  fmt = 3,
  stars = c('*'=.10,'**'=.05,'***'=.01),
  notes = paste(
    "Country & Year FE; SE clustered by origin-country.",
    "Same-sample comparison: rows require both blue_share and blue_fullden observed.",
    "Portfolio takeaway: denominator sensitivity suggests reporting/missingness mechanics are central."
  ),
  output = "outputs/tables/tab_MAIN_net_vs_full_denom.tex"
)

beta_net  <- unname(coef(fe_blue_net_cmp)["irca_intensity:post"])
beta_full <- unname(coef(fe_blue_fullden_cmp)["irca_intensity:post"])

writeLines(
  c(
    "Main contrast (same sample): IRCA×Post coefficient",
    paste0("Net denom  (blue_share):   ", signif(beta_net, 4)),
    paste0("Full denom (blue_fullden): ", signif(beta_full, 4))
  ),
  "outputs/logs/main_contrast_net_vs_full.txt"
)

# Magnitudes (Net and Full denom; consistent yvar)
write_magnitudes(
  df = cmp,
  model = fe_blue_net_cmp,
  yvar = "blue_share",
  out_table = "outputs/tables/tab_magnitudes_blue_NET.tex",
  out_plot  = "outputs/figures/fig_std_effects_blue_NET.png",
  caption = "Magnitudes: Blue share (Net denominator; same sample)"
)

write_magnitudes(
  df = cmp,
  model = fe_blue_fullden_cmp,
  yvar = "blue_fullden",
  out_table = "outputs/tables/tab_magnitudes_blue_FULLDEN.tex",
  out_plot  = "outputs/figures/fig_std_effects_blue_FULLDEN.png",
  caption = "Magnitudes: Blue share (Full denominator; same sample)"
)

#### 12A) Extra robustness helpers ####
run_blue_net <- function(df){
  feols(
    blue_share ~ irca_intensity*post + adj_share | country + year,
    data = df, cluster = ~ country
  )
}

run_noocc_net <- function(df){
  df2 <- df |> filter(!is.na(noocc_share))
  feols(
    noocc_share ~ irca_intensity*post + adj_share | country + year,
    data = df2, cluster = ~ country
  )
}

# Drop high-missing years 1995–1997
drop_years <- 1995:1997
panel_net_drop9597 <- panel_net |> filter(!(year %in% drop_years))
m_blue_net_drop9597 <- run_blue_net(panel_net_drop9597)

# Drop incomplete-coverage countries
drop_cty <- c("Uruguay","Paraguay")
panel_net_drop_incomplete <- panel_net |> filter(!(country %in% drop_cty))
m_blue_net_drop_incomplete <- run_blue_net(panel_net_drop_incomplete)

# Shorter window 1982–1995
panel_net_8295 <- panel_net |> filter(year %in% 1982:1995)
m_blue_net_8295 <- run_blue_net(panel_net_8295)

# Denominator diagnostic: no-occupation trends + DiD
noocc_trends <- panel |>
  filter(!is.na(noocc_share), year %in% 1982:2000) |>
  group_by(irca_group, year) |>
  summarise(mean_noocc = mean(noocc_share, na.rm = TRUE), .groups = "drop")

p_noocc <- ggplot(noocc_trends, aes(x=year, y=mean_noocc, color=irca_group)) +
  geom_line() + geom_point(size=1.5) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  labs(x="Year", y="Mean no-occupation share", color="IRCA exposure",
       title="No-occupation share over time by IRCA exposure group") +
  theme_bw()
save_plot(p_noocc, "outputs/figures/fig_noocc_trends_by_exposure.png", w=7, h=4.6)

m_noocc_net <- run_noocc_net(panel_net)

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

modelsummary::modelsummary(
  list("No-occ share (Net sample)" = m_noocc_net),
  coef_map = c("irca_intensity:post"="IRCA × Post", "adj_share"="Adjustment share"),
  gof_map  = c("nobs","r.squared.within"),
  stars    = c('*'=.10,'**'=.05,'***'=.01),
  notes    = "Country & Year FE; SE clustered by country. Outcome: noocc_share.",
  output   = "outputs/tables/tab_noocc_did.tex"
)

#### 12B) Heterogeneity by exposure quartile (bins) ####
q_tbl <- panel |>
  distinct(country, irca_intensity) |>
  mutate(q = ntile(irca_intensity, 4))

panel_q <- panel_net |>
  left_join(q_tbl, by="country") |>
  mutate(q = factor(q, levels=1:4, labels=paste0("Q",1:4)))

m_bins <- feols(
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
  filter(grepl("q::Q[2-4]:post", term)) |>
  mutate(
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

#### 12C) Mechanism scan ####
outcomes_mech <- c("blue_share","serv_share","white_share",
                   "fam_share","emp_share",
                   "male_share","adj_share")

run_mech <- function(y){
  dat <- panel |> filter(!is.na(.data[[y]]), !is.na(irca_intensity), !is.na(post))
  rhs <- if (y == "adj_share") "irca_intensity*post" else "irca_intensity*post + adj_share"
  feols(as.formula(paste0(y," ~ ", rhs, " | country + year")),
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
  notes = "Country & Year FE; SE clustered by country. Panel B: secondary outcomes.",
  output="outputs/tables/tab_mechanisms_summary_B.tex"
)

coef_df_mech <- bind_rows(lapply(names(mods_mech), function(nm){
  tt <- broom::tidy(mods_mech[[nm]])
  out <- tt[tt$term=="irca_intensity:post",]
  out$outcome <- nm
  out
})) |>
  mutate(
    pp = 100*estimate,
    lo = 100*(estimate - 1.96*std.error),
    hi = 100*(estimate + 1.96*std.error)
  )

p_mech <- ggplot(coef_df_mech, aes(x=reorder(outcome, pp), y=pp)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_pointrange(aes(ymin=lo,ymax=hi)) +
  coord_flip() +
  labs(x=NULL, y="Effect (pp)",
       title="IRCA × Post effects across outcomes (mechanism scan)") +
  theme_bw()
save_plot(p_mech, "outputs/figures/fig_mechanisms_coefplot.png", w=7, h=4.8)

#### Residual diagnostic (baseline Net) ####
dat_est <- panel_net |>
  filter(!is.na(blue_share), !is.na(irca_intensity), !is.na(post), !is.na(adj_share))
stopifnot(nrow(dat_est) == length(resid(fe_blue_net)))

res_df <- data.frame(year = dat_est$year, resid = resid(fe_blue_net)) |>
  group_by(year) |>
  summarise(mean_resid = mean(resid, na.rm=TRUE), .groups="drop")

p_res <- ggplot(res_df, aes(x=year, y=mean_resid)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line() + geom_point() +
  labs(x="Year", y="Mean residual",
       title="Residual sanity check: baseline Net (mean residual by year)") +
  theme_bw()
save_plot(p_res, "outputs/figures/fig_residuals_by_year_blue.png", w=7, h=4)

#### Alt intensity models ####
fe_blue_alt <- feols(blue_share ~ irca_intensity_alt*post + adj_share | country + year,
                     data = panel_alt_net, cluster = ~ country)

fe_blue_net_nx <- feols(blue_share ~ irca_intensity*post + adj_share | country + year,
                        data = panel_nomx_net, cluster = ~ country)
fe_blue_alt_nx <- feols(blue_share ~ irca_intensity_alt*post + adj_share | country + year,
                        data = panel_alt_nomx_net, cluster = ~ country)

modelsummary(
  list(
    "Blue Net (baseline)"   = fe_blue_net,
    "Blue Net (alt denom)"  = fe_blue_alt,
    "Blue Net (–MX)"        = fe_blue_net_nx,
    "Blue Net (alt –MX)"    = fe_blue_alt_nx
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

#### 14) Balanced panel (Blue, Net)  ####
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

#### 15) Leave-one-out (Blue Net) ####
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

#### 16) SPEC COMPARE ####
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

#### 17) Small-cluster inference: wild cluster bootstrap (robust fallback) ####
if (requireNamespace("fwildclusterboot", quietly = TRUE) &&
    requireNamespace("dqrng", quietly = TRUE)) {
  
  set.seed(12345)
  dqrng::dqset.seed(12345)
  

  dat_wb <- dat_blue_net |>
    dplyr::mutate(
      country = factor(country),
      year    = factor(year)
    )
  
  # Baseline FE regression as lm with explicit FE dummies
  lm_fe <- lm(
    blue_share ~ irca_intensity*post + adj_share + country + year,
    data = dat_wb
  )
  
  cat("feols coef:", coef(fe_blue_net)["irca_intensity:post"], "\n")
  cat("lm   coef:", coef(lm_fe)["irca_intensity:post"], "\n")
  
  # Wild cluster bootstrap on the interaction coefficient, clustered by country
  wb <- fwildclusterboot::boottest(
    object  = lm_fe,
    clustid = "country",                 
    param   = "irca_intensity:post",
    B       = 4999,
    type    = "rademacher"
  )
  
  sink("outputs/logs/wild_bootstrap_blue_net.txt")
  cat("Wild cluster bootstrap test (lm FE version of baseline Net)\n")
  print(wb)
  sink()
  
  wb_tab <- tibble::tibble(
    Model = "lm with country+year FE (dummies)",
    Parameter = "IRCA intensity × Post",
    `Bootstrap p-value` = wb$p_val,
    `Bootstrap t-stat`  = wb$t_stat
  )
  
  wb_tex <- knitr::kable(
    wb_tab %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(.x, 4))),
    format="latex", booktabs=TRUE,
    caption="Wild cluster bootstrap inference using an lm FE version of the baseline model (robust to fixest parsing issues)."
  )
  write_tex(wb_tex, "outputs/tables/tab_wildboot_blue_net.tex")
  
} else {
  warning("Packages 'fwildclusterboot' and/or 'dqrng' not installed. Skipping wild bootstrap inference.")
}

#### 18) Logs: run summary + sessionInfo ####
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