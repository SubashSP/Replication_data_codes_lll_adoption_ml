#########################################################################
#  Replication code for:
#  "Farmers' experience and perception shape adoption and use of
#   precision laser land levelling technology in India"
#  Surendran-Padmaja, S. & Parlasca, M. C.
#
#  This script reproduces the quantitative results (Tables 1–2, Figures 2–5,
#  Supplementary Figure 3, Supplementary Tables 3–5) reported in the
#  manuscript. Irrelevant exploratory code from earlier drafts has been
#  removed.
#
#  Required data file: LLL Punjab plot data_revised2_paper1_adoption3.dta
#########################################################################

# ---- 0. Setup ---------------------------------------------------------
# setwd("path/to/replication/folder")   # <-- set working directory

# Install (if needed) and load packages
pkgs <- c("haven", "tidyverse", "ggplot2", "ggpubr", "tidyr", "dplyr",
         "reshape2", "fBasics", "fastDummies", "stargazer", "modelsummary",
         "grf", "ggridges", "scales")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 1. Load data -----------------------------------------------------
data <- read_dta("LLL Punjab plot data_revised2_paper1_adoption3.dta")

# Convert factor variables
factor_variables <- c("levelled_2021", "caste_cat2", "religion_new",
                      "non_farm_binary", "plot_soiltype2", "plot_soilfertility",
                      "plot_erosionprobl", "plot_waterloggprob",
                      "rice_short_duration", "access_govt_ext", "access_kvk",
                      "access_prog_farmer", "access_ngo", "access_farmer_grp",
                      "access_input_dealer", "district")
for (v in factor_variables) data[[v]] <- factor(data[[v]])

data$treat_lll_order <- factor(data$treat_lll_new, ordered = FALSE)
data$district_order  <- factor(data$district,      ordered = FALSE)

# Pusa 44 rice variety dummy
data$pusa_44 <- as.integer(data$plot_kharif_if_rice_variety_name == "Pusa 44")
data$pusa_44[is.na(data$pusa_44)] <- 0

# ---- 2. Table 1: Descriptive statistics --------------------------------
data_desc <- fastDummies::dummy_cols(
  data,
  select_columns = c("levelled_2021", "caste_cat2", "religion_new",
                     "non_farm_binary", "plot_soiltype2", "plot_soilfertility",
                     "plot_erosionprobl", "rice_short_duration",
                     "plot_waterloggprob", "access_govt_ext", "access_kvk",
                     "access_prog_farmer", "access_ngo", "access_farmer_grp",
                     "access_input_dealer", "district")
)

desc_vars <- c("age_hhmember", "edun_hhmember", "total_num_adult_members_HH",
               "women_share", "asset_index", "plot_area_ha", "plot_dist_lll",
               "num_plot", "sp_lll_new", "datairrwater_tbl_21",
               "simpson_index_kh", "simpson_index_rb", "datadstdst_dhq")

summ_stats <- fBasics::basicStats(data_desc[, desc_vars])
summ_stats <- as.data.frame(t(summ_stats))
summ_stats <- summ_stats[, c("Mean", "Stdev", "Minimum",
                             "1. Quartile", "Median", "3. Quartile", "Maximum")]
names(summ_stats)[c(4, 6)] <- c("Lower quartile", "Upper quartile")
print(summ_stats)

# Frequency of adoption (Supplementary Table 3)
table(data$treat_lll_new)
table(data$district, data$treat_lll_new)

# ---- 3. Figure 2: Distribution (violin plots) of yield & irrigation ----
# Outcomes: rice yield, wheat yield, rice irrigation hrs, wheat irrigation hrs
make_violin <- function(df, yvar, ylab) {
  ggplot(df %>% drop_na(!!sym(yvar)),
         aes(x = "", y = !!sym(yvar))) +
    geom_violin(fill = "dodgerblue4", alpha = 0.5) +
    geom_boxplot(width = 0.1, outlier.size = 0.5) +
    labs(x = "", y = ylab) +
    theme_bw(base_size = 14)
}
p_ry  <- make_violin(data, "kharif_rice_yld_ha",  "Rice yield (kg/ha)")
p_wy  <- make_violin(data, "rabi_wheat_yld_ha",   "Wheat yield (kg/ha)")
p_rih <- make_violin(data, "rice_irr_hrs_ha",     "Rice irrigation (h/ha)")
p_wih <- make_violin(data, "wheat_irr_hrs_ha",    "Wheat irrigation (h/ha)")
# ggarrange(p_ry, p_wy, p_rih, p_wih, ncol = 4)  # uncomment to view

# Normality tests
for (v in c("kharif_rice_yld_ha", "rabi_wheat_yld_ha",
            "rice_irr_hrs_ha", "wheat_irr_hrs_ha")) {
  if (v %in% names(data)) {
    x <- na.omit(data[[v]])
    cat(v, " skewness =", fBasics::skewness(x),
        " kurtosis =", fBasics::kurtosis(x), "\n")
  }
}

# ---- 4. Figure 3: Farmers' perception of LLL benefits ------------------
# Perception variables are categorical: "reduces", "increases",
# "no change", "don't know" (variable names: percep_income, percep_yield,
# percep_cost, percep_water — adjust to actual names in the dataset).
perception_vars <- c("percep_income", "percep_yield",
                     "percep_cost", "percep_water")
if (all(perception_vars %in% names(data))) {
  perc_long <- data %>%
    select(all_of(perception_vars)) %>%
    pivot_longer(everything(), names_to = "benefit", values_to = "response") %>%
    drop_na()
  ggplot(perc_long, aes(x = benefit, fill = factor(response))) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "", y = "Share of respondents", fill = "Response") +
    theme_bw(base_size = 14) +
    coord_flip()
}

# ---- 5. Figure 4: Mean yield / irrigation by levelling frequency -------
plot_freq_outcome <- function(df, yvar, ylab) {
  df %>%
    drop_na(treat_lll_new, !!sym(yvar)) %>%
    ggerrorplot(x = "treat_lll_new", y = yvar,
                add = "mean", error.plot = "errorbar",
                color = "black", size = 1,
                stat = "summary", fun.data = "mean_cl_boot") +
    scale_x_discrete(labels = c("Never", "Before 3 yrs",
                                "Last 3 yrs", "2020-21")) +
    labs(x = "Frequency of adoption", y = ylab) +
    theme_bw(base_size = 14) + coord_flip()
}
plot_freq_outcome(data, "kharif_rice_yld_ha", "Rice yield (kg/ha)")
plot_freq_outcome(data, "rabi_wheat_yld_ha",  "Wheat yield (kg/ha)")
plot_freq_outcome(data, "rice_irr_hrs_ha",    "Rice irrigation (h/ha)")
plot_freq_outcome(data, "wheat_irr_hrs_ha",   "Wheat irrigation (h/ha)")

# ---- 6. OLS regressions (Table 2, column "OLS") ------------------------
ols_formula <- function(y) {
  as.formula(paste(y, "~ treat_lll_order + age_hhmember + edun_hhmember +",
    "caste_cat2 + religion_new + total_num_adult_members_HH + women_share +",
    "non_farm_binary + asset_index + plot_area_ha + num_plot +",
    "plot_soiltype2 + plot_soilfertility + plot_erosionprobl +",
    "plot_waterloggprob + rice_short_duration + pusa_44 + sp_lll_new +",
    "access_govt_ext + access_kvk + access_prog_farmer + access_ngo +",
    "access_farmer_grp + access_input_dealer + datairrwater_tbl_21 +",
    "simpson_index_kh + simpson_index_rb + datadstdst_dhq + district_order"))
}

ols_rice_yld  <- lm(ols_formula("kharif_rice_yld_ha"), data = data)
ols_rice_irr  <- lm(ols_formula("rice_irr_hrs_ha"),    data = data)
ols_wheat_yld <- lm(ols_formula("rabi_wheat_yld_ha"),  data = data)
ols_wheat_irr <- lm(ols_formula("wheat_irr_hrs_ha"),   data = data)

stargazer(ols_rice_yld, ols_rice_irr, ols_wheat_yld, ols_wheat_irr,
          type = "text", keep.stat = c("n", "rsq"))

# ---- 7. Multi-arm Causal Forest (Table 2 CATE, Figure 5, Supp. Fig. 3)-
covariates2 <- c("caste_cat2", "religion_new", "non_farm_binary",
                 "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl",
                 "plot_waterloggprob", "rice_short_duration",
                 "access_govt_ext", "access_kvk", "access_prog_farmer",
                 "access_ngo", "access_farmer_grp", "access_input_dealer",
                 "age_hhmember", "edun_hhmember", "total_num_adult_members_HH",
                 "women_share", "asset_index", "plot_area_ha", "num_plot",
                 "datairrwater_tbl_21", "simpson_index_kh",
                 "simpson_index_rb", "datadstdst_dhq", "district")

run_mac_forest <- function(df, yvar) {
  dat <- df %>%
    select(all_of(c(yvar, "treat_lll_new", covariates2))) %>%
    drop_na()
  Y <- as.vector(dat[[yvar]])
  W <- as.factor(dat$treat_lll_new)
  # One-hot encode factors so grf gets a numeric matrix
  X <- model.matrix(~ . - 1, data = dat[, covariates2])

  W.forest <- probability_forest(X, W,
                                 equalize.cluster.weights = FALSE, seed = 2)
  W.hat <- predict(W.forest)$predictions

  Y.forest <- regression_forest(X, Y,
                                equalize.cluster.weights = FALSE, seed = 2)
  Y.hat <- predict(Y.forest)$predictions

  mac <- multi_arm_causal_forest(X = X, Y = Y, W = W,
                                 W.hat = W.hat, Y.hat = Y.hat, seed = 2)
  list(forest = mac, X = X, Y = Y, W = W, covariates = colnames(X))
}

res_rice_yld  <- run_mac_forest(data, "kharif_rice_yld_ha")
res_rice_irr  <- run_mac_forest(data, "rice_irr_hrs_ha")
res_wheat_yld <- run_mac_forest(data, "rabi_wheat_yld_ha")
res_wheat_irr <- run_mac_forest(data, "wheat_irr_hrs_ha")

# Average treatment effects (Table 2, CATE columns)
ate_table <- function(res) average_treatment_effect(res$forest, method = "AIPW")
ate_rice_yld  <- ate_table(res_rice_yld);  print(ate_rice_yld)
ate_rice_irr  <- ate_table(res_rice_irr);  print(ate_rice_irr)
ate_wheat_yld <- ate_table(res_wheat_yld); print(ate_wheat_yld)
ate_wheat_irr <- ate_table(res_wheat_irr); print(ate_wheat_irr)

# ---- 8. Supplementary Figure 3: Variable importance plots --------------
plot_varimp <- function(res, title) {
  vi <- variable_importance(res$forest)
  df_vi <- data.frame(var = res$covariates, imp = as.numeric(vi))
  ggplot(df_vi, aes(x = reorder(var, imp), y = imp)) +
    geom_jitter(color = "steelblue") +
    coord_flip() +
    labs(x = "Variable", y = "Variable importance", title = title) +
    theme_bw(base_size = 12)
}
plot_varimp(res_rice_yld,  "Rice yield")
plot_varimp(res_rice_irr,  "Rice irrigation hours")
plot_varimp(res_wheat_yld, "Wheat yield")
plot_varimp(res_wheat_irr, "Wheat irrigation hours")

# ---- 9. Figure 5: Distribution of CATE by quartiles --------------------
plot_cate_dist <- function(res, xlab) {
  tau <- predict(res$forest, target.sample = "all",
                 estimate.variance = TRUE)
  tau_df <- as.data.frame(tau$predictions[, , 1])
  names(tau_df) <- c("Before 3 yrs", "Last 3 yrs", "2020-21")
  long <- reshape2::melt(tau_df)
  ggplot(long, aes(x = value, y = variable,
                   fill = factor(stat(quantile)))) +
    stat_density_ridges(geom = "density_ridges_gradient",
                        calc_ecdf = TRUE,
                        quantiles = 4, quantile_lines = TRUE) +
    scale_fill_viridis_d(name = "Quartiles") +
    theme_bw(base_size = 14) +
    labs(x = xlab, y = "Frequency of LLL")
}
plot_cate_dist(res_rice_yld,  "Rice yield gain (kg/ha)")
plot_cate_dist(res_rice_irr,  "Rice irrigation change (h/ha)")
plot_cate_dist(res_wheat_yld, "Wheat yield gain (kg/ha)")
plot_cate_dist(res_wheat_irr, "Wheat irrigation change (h/ha)")

# ---- 10. Supplementary Table 4: Best linear fit calibration ------------
# test_calibration is defined for (single-arm) causal_forest. For the
# multi-arm forest we fit binary causal forests contrasting each
# non-"Never" level against "Never" and run test_calibration on each.
calibration_for_level <- function(df, yvar, level_val) {
  dat <- df %>%
    mutate(W_bin = ifelse(treat_lll_new == 0, 0,
                   ifelse(treat_lll_new == level_val, 1, NA))) %>%
    select(all_of(c(yvar, "W_bin", covariates2))) %>%
    drop_na()
  X <- model.matrix(~ . - 1, data = dat[, covariates2])
  Y <- dat[[yvar]]
  W <- dat$W_bin
  W.f <- regression_forest(X, W, seed = 2)
  W.hat <- predict(W.f)$predictions
  Y.f <- regression_forest(X, Y, seed = 2)
  Y.hat <- predict(Y.f)$predictions
  cf <- causal_forest(X = X, Y = Y, W = W,
                      W.hat = W.hat, Y.hat = Y.hat, seed = 2)
  test_calibration(cf)
}
for (yv in c("kharif_rice_yld_ha", "rice_irr_hrs_ha",
             "rabi_wheat_yld_ha",  "wheat_irr_hrs_ha")) {
  cat("\n=====", yv, "=====\n")
  for (lvl in 1:3) {
    cat("Level", lvl, "vs Never:\n")
    print(calibration_for_level(data, yv, lvl))
  }
}

# ---- END OF SCRIPT ----------------------------------------------------
