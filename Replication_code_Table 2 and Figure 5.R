###############################################################################
## Replication Code for:
## "Farmers' experience and perception shape adoption and use of precision 
##  laser land levelling technology in India"
## Authors: Subash Surendran-Padmaja and Martin C. Parlasca
## 
## This code replicates the causal machine learning analysis (Table 2, Figure 5)
## for all four outcomes: Rice Yield, Rice Irrigation, Wheat Yield, Wheat Irrigation
###############################################################################

## Set working directory (adjust path as needed)
# setwd("C:/Users/ZEF/Desktop/PhD/PhD paper/Paper 2 Impact/ML analysis")

## Load required packages
library(haven)
library(grf)
library(policytree)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(fBasics)
library(stargazer)
library(modelsummary)
library(ggalluvial)
library(data.table)
library(scales)
library(ggpubr)
library(DiagrammeR)

###############################################################################
## SECTION 1: RICE YIELD (kharif_rice_yld_ha)
###############################################################################

## 1.1 Load data
data_rice_yld <- read_dta('LLL Punjab plot data_revised2_paper1_adoption3.dta')

## Convert factor variables
factor_vars_rice <- c("levelled_2021", "caste_cat2", "religion_new", "non_farm_binary", 
                      "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", 
                      "plot_waterloggprob", "rice_short_duration", "access_govt_ext", 
                      "access_kvk", "access_prog_farmer", "access_ngo", 
                      "access_farmer_grp", "access_input_dealer", "district")
for (variable in factor_vars_rice) {
  data_rice_yld[[variable]] <- factor(data_rice_yld[[variable]])
}
data_rice_yld$treat_lll_order <- factor(data_rice_yld$treat_lll_new, ordered = FALSE)
data_rice_yld$district_order <- factor(data_rice_yld$district, ordered = FALSE)

## Create Pusa 44 variety dummy
data_rice_yld$pusa_44 <- 0
data_rice_yld$pusa_44[data_rice_yld$plot_kharif_if_rice_variety_name == "Pusa 44"] <- 1

## Subset variables and drop NAs
data_rice_yld <- subset(data_rice_yld, select = c(
  "kharif_rice_yld_ha", "treat_lll_new", "treat_lll_order", "levelled_2021",
  "caste_cat2", "religion_new", "non_farm_binary", "plot_soiltype2", 
  "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob",
  "rice_short_duration", "pusa_44", "access_govt_ext", "access_kvk", 
  "access_prog_farmer", "access_ngo", "access_farmer_grp", 
  "access_input_dealer", "age_hhmember", "edun_hhmember", 
  "total_num_adult_members_HH", "women_share", "asset_index", 
  "plot_area_ha", "plot_dist_lll", "num_plot", "sp_lll_new", 
  "datairrwater_tbl_21", "simpson_index_kh", "simpson_index_rb", 
  "datadstdst_dhq", "district_order"))
data_rice_yld <- data_rice_yld %>% drop_na()

## 1.2 Define outcome, covariates, and treatment
Y_rice_yld <- as.vector(data_rice_yld$kharif_rice_yld_ha)

X_rice_yld <- subset(data_rice_yld, select = c(
  "caste_cat2", "religion_new", "non_farm_binary", "plot_soiltype2", 
  "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob",
  "rice_short_duration", "pusa_44", "access_govt_ext", "access_kvk", 
  "access_prog_farmer", "access_ngo", "access_farmer_grp", 
  "access_input_dealer", "age_hhmember", "edun_hhmember", 
  "total_num_adult_members_HH", "women_share", "asset_index", 
  "plot_area_ha", "plot_dist_lll", "num_plot", "sp_lll_new", 
  "datairrwater_tbl_21", "simpson_index_kh", "simpson_index_rb", 
  "datadstdst_dhq", "district_order"))

W_rice_yld <- as.factor(data_rice_yld$treat_lll_new)

## 1.3 Probability forest for treatment weights (propensity)
W.forest_rice_yld <- probability_forest(X_rice_yld, W_rice_yld,
                                        equalize.cluster.weights = FALSE,
                                        seed = 2)
W.hat_rice_yld <- predict(W.forest_rice_yld, estimate.variance = TRUE)$predictions

## 1.4 Regression forest for expected outcome
Y.forest_rice_yld <- regression_forest(X_rice_yld, Y_rice_yld,
                                       equalize.cluster.weights = FALSE,
                                       seed = 2)
Y.hat_rice_yld <- predict(Y.forest_rice_yld, estimate.variance = TRUE)$predictions

## 1.5 Fit multi-arm causal forest
cf_rice_yld <- multi_arm_causal_forest(X = X_rice_yld, Y = Y_rice_yld, 
                                       W = W_rice_yld,
                                       W.hat = W.hat_rice_yld, 
                                       Y.hat = Y.hat_rice_yld, 
                                       seed = 2)

## 1.6 Average treatment effects (Table 2 - Rice Yield CATE column)
ate_rice_yld <- average_treatment_effect(cf_rice_yld, method = "AIPW")
cat("\n=== Rice Yield: Average Treatment Effects (CATE) ===\n")
print(ate_rice_yld)

## 1.7 Variable importance
varimp_rice_yld <- variable_importance(cf_rice_yld)
vars_rice_yld <- colnames(X_rice_yld)
varimp_df_rice_yld <- data.frame(
  Variable = vars_rice_yld,
  Importance = as.numeric(varimp_rice_yld)
)

## Variable importance plot (Supplementary Figure 3)
ggplot(varimp_df_rice_yld, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_point(color = "steelblue") +
  coord_flip() +
  labs(x = "Variables", y = "Variable importance", title = "Rice Yield - Variable Importance") +
  theme_bw(base_size = 16)

## 1.8 Binary causal calibration check
W_rice_yld_binary <- as.vector(data_rice_yld$levelled_2021)

W.forest_rice_yld_bin <- regression_forest(X_rice_yld, W_rice_yld_binary,
                                           equalize.cluster.weights = FALSE,
                                           seed = 2)
W.hat_rice_yld_bin <- predict(W.forest_rice_yld_bin, estimate.variance = TRUE)$predictions

Y.forest_rice_yld_bin <- regression_forest(X_rice_yld, Y_rice_yld,
                                           equalize.cluster.weights = FALSE,
                                           seed = 2)
Y.hat_rice_yld_bin <- predict(Y.forest_rice_yld_bin, estimate.variance = TRUE)$predictions

cf_rice_yld_bin <- causal_forest(X = X_rice_yld, Y = Y_rice_yld, 
                                 W = W_rice_yld_binary,
                                 Y.hat = Y.hat_rice_yld_bin, 
                                 W.hat = W.hat_rice_yld_bin,
                                 seed = 2, equalize.cluster.weights = FALSE,
                                 tune.parameters = "all")

ate_rice_yld_bin <- average_treatment_effect(cf_rice_yld_bin, target.sample = "overlap")
cat("\n=== Rice Yield: Binary ATE (calibration check) ===\n")
print(ate_rice_yld_bin)

calib_rice_yld <- test_calibration(cf_rice_yld_bin)
cat("\n=== Rice Yield: Calibration Test ===\n")
print(calib_rice_yld)

## 1.9 Heterogeneous treatment effects - Distribution (Figure 5a)
tau_rice_yld <- predict(cf_rice_yld, target.sample = "all", estimate.variance = TRUE)
tau_rice_yld <- as.data.frame(tau_rice_yld)
tau_rice_yld_pred <- tau_rice_yld[, 1:3]
colnames(tau_rice_yld_pred) <- c("Before 3 yrs - Never", "Last 3 yrs - Never", "2020-21 - Never")

tau_rice_yld_long <- reshape2::melt(tau_rice_yld_pred)

ggplot(tau_rice_yld_long, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles") +
  theme_bw(base_size = 16) +
  labs(x = "Rice yield gain (kg/ha)", y = "Frequency of laser land levelling")


###############################################################################
## SECTION 2: WHEAT YIELD (rabi_wheat_yld_ha)
## Based on: Causal_machine_learning_revised_maxwell_codes_wheat_yield2.R
###############################################################################

## 2.1 Load data
data_wheat_yld <- read_dta('LLL Punjab plot data_revised2_paper1_adoption3_wheat_yield.dta')

## Create HD3086 wheat variety dummy
data_wheat_yld$HD3086 <- 0
data_wheat_yld$HD3086[data_wheat_yld$plot_rabi_wheat_variety_name == "HD 3086"] <- 1
data_wheat_yld$HD3086[data_wheat_yld$plot_rabi_wheat_variety_name == "Hd 3086"] <- 1
data_wheat_yld$HD3086[data_wheat_yld$plot_rabi_wheat_variety_name == "Hd3086"] <- 1
data_wheat_yld$HD3086[data_wheat_yld$plot_rabi_wheat_variety_name == "HD3086"] <- 1
data_wheat_yld$HD3086[data_wheat_yld$plot_rabi_wheat_variety_name == "HD3086;"] <- 1

## Create district dummies
data_wheat_yld$district_name_new_num <- 0
data_wheat_yld$district_name_new_num[data_wheat_yld$district_name_new == "Fatehgarh_Sahib_1"] <- 1
data_wheat_yld$district_name_new_num[data_wheat_yld$district_name_new == "Ludhiana_2"] <- 2
data_wheat_yld$district_name_new_num[data_wheat_yld$district_name_new == "Patiala_3"] <- 3
data_wheat_yld$district_name_new_num[data_wheat_yld$district_name_new == "Sangrur_4 "] <- 4

## Convert factor variables
factor_vars_wheat <- c("levelled_2021", "caste_cat2", "religion_new", "non_farm_binary", 
                       "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", 
                       "plot_waterloggprob", "HD3086", "subsidy_lll", "access_govt_ext", 
                       "access_kvk", "access_prog_farmer", "access_ngo", 
                       "access_farmer_grp", "access_input_dealer")
for (variable in factor_vars_wheat) {
  data_wheat_yld[[variable]] <- factor(data_wheat_yld[[variable]])
}
data_wheat_yld$treat_lll_order <- factor(data_wheat_yld$treat_lll_new, ordered = FALSE)
data_wheat_yld$district_order <- factor(data_wheat_yld$district_name_new_num, ordered = FALSE)

## 2.2 OLS regression (Table 2 - Wheat Yield OLS column)
ols_wheat_yld <- lm(rabi_wheat_yld_ha ~ treat_lll_order + age_hhmember + edun_hhmember + 
                      caste_cat2 + religion_new + total_num_adult_members_HH + women_share + 
                      non_farm_binary + asset_index + plot_area_ha + plot_dist_lll + 
                      num_plot + plot_soiltype2 + plot_soilfertility + plot_erosionprobl + 
                      plot_waterloggprob + HD3086 + N_use + sp_lll_new + subsidy_lll + 
                      access_govt_ext + access_kvk + access_prog_farmer + access_ngo + 
                      access_farmer_grp + access_input_dealer + datairrwater_tbl_21 + 
                      simpson_index_kh + simpson_index_rb + lll_village_adopt_share + 
                      datadstdst_dhq + district_order, 
                    data = data_wheat_yld)
cat("\n=== Wheat Yield: OLS Results ===\n")
summary(ols_wheat_yld)
stargazer(ols_wheat_yld, type = "text", keep.stat = c("n", "rsq"))

## 2.3 Prepare data for causal forest
data_wheat_yld_cf <- subset(data_wheat_yld, select = c(
  "rabi_wheat_yld_ha", "treat_lll_new", "caste_cat2", "religion_new", 
  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", 
  "plot_waterloggprob", "access_govt_ext", "access_kvk", "access_prog_farmer", 
  "access_ngo", "access_farmer_grp", "access_input_dealer", "age_hhmember", 
  "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", 
  "plot_area_ha", "subsidy_lll", "num_plot", "sp_lll_new", "datairrwater_tbl_21",
  "simpson_index_kh", "simpson_index_rb", "datadstdst_dhq", "levelled_2021",
  "district_name_new_num", "HD3086", "N_use"))
data_wheat_yld_cf <- data_wheat_yld_cf %>% drop_na()

## Define outcome, covariates, and treatment
Y_wheat_yld <- as.vector(data_wheat_yld_cf$rabi_wheat_yld_ha)

X_wheat_yld <- subset(data_wheat_yld_cf, select = c(
  "caste_cat2", "religion_new", "non_farm_binary", "plot_soiltype2", 
  "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob", 
  "access_govt_ext", "access_kvk", "access_prog_farmer", "access_ngo", 
  "access_farmer_grp", "access_input_dealer", "age_hhmember", "edun_hhmember", 
  "total_num_adult_members_HH", "women_share", "asset_index", "plot_area_ha", 
  "subsidy_lll", "num_plot", "sp_lll_new", "datairrwater_tbl_21",
  "simpson_index_kh", "simpson_index_rb", "datadstdst_dhq", 
  "district_name_new_num", "HD3086", "N_use"))

W_wheat_yld <- as.factor(data_wheat_yld_cf$treat_lll_new)

## 2.4 Probability forest for treatment weights
W.forest_wheat_yld <- probability_forest(X_wheat_yld, W_wheat_yld,
                                         equalize.cluster.weights = FALSE,
                                         seed = 2)
W.hat_wheat_yld <- predict(W.forest_wheat_yld, estimate.variance = TRUE)$predictions

## 2.5 Regression forest for expected outcome
Y.forest_wheat_yld <- regression_forest(X_wheat_yld, Y_wheat_yld,
                                        equalize.cluster.weights = FALSE,
                                        seed = 2)
Y.hat_wheat_yld <- predict(Y.forest_wheat_yld, estimate.variance = TRUE)$predictions

## 2.6 Fit multi-arm causal forest
cf_wheat_yld <- multi_arm_causal_forest(X = X_wheat_yld, Y = Y_wheat_yld, 
                                        W = W_wheat_yld,
                                        W.hat = W.hat_wheat_yld, 
                                        Y.hat = Y.hat_wheat_yld, 
                                        seed = 2)

## 2.7 Average treatment effects (Table 2 - Wheat Yield CATE column)
ate_wheat_yld <- average_treatment_effect(cf_wheat_yld, method = "AIPW")
cat("\n=== Wheat Yield: Average Treatment Effects (CATE) ===\n")
print(ate_wheat_yld)

## 2.8 Variable importance
varimp_wheat_yld <- variable_importance(cf_wheat_yld)
vars_wheat_yld <- colnames(X_wheat_yld)
varimp_df_wheat_yld <- data.frame(
  Variable = vars_wheat_yld,
  Importance = as.numeric(varimp_wheat_yld)
)

## Variable importance plot (Supplementary Figure 3)
ggplot(varimp_df_wheat_yld, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_point(color = "steelblue") +
  coord_flip() +
  labs(x = "Variables", y = "Variable importance", title = "Wheat Yield - Variable Importance") +
  theme_bw(base_size = 16)

## 2.9 Binary causal calibration check (Supplementary Table 4)
W_wheat_yld_binary <- as.vector(data_wheat_yld_cf$levelled_2021)

W.forest_wheat_yld_bin <- regression_forest(X_wheat_yld, W_wheat_yld_binary,
                                            equalize.cluster.weights = FALSE,
                                            seed = 2)
W.hat_wheat_yld_bin <- predict(W.forest_wheat_yld_bin, estimate.variance = TRUE)$predictions

hist(W.hat_wheat_yld_bin, xlab = "Treatment propensity score", 
     main = "Wheat Yield - Propensity Score Distribution")

Y.forest_wheat_yld_bin <- regression_forest(X_wheat_yld, Y_wheat_yld,
                                            equalize.cluster.weights = FALSE,
                                            seed = 2)
Y.hat_wheat_yld_bin <- predict(Y.forest_wheat_yld_bin, estimate.variance = TRUE)$predictions

cf_wheat_yld_bin <- causal_forest(X = X_wheat_yld, Y = Y_wheat_yld, 
                                  W = W_wheat_yld_binary,
                                  Y.hat = Y.hat_wheat_yld_bin, 
                                  W.hat = W.hat_wheat_yld_bin,
                                  seed = 2, equalize.cluster.weights = FALSE,
                                  tune.parameters = "all")

ate_wheat_yld_bin <- average_treatment_effect(cf_wheat_yld_bin, target.sample = "overlap")
cat("\n=== Wheat Yield: Binary ATE (calibration check) ===\n")
print(ate_wheat_yld_bin)

calib_wheat_yld <- test_calibration(cf_wheat_yld_bin)
cat("\n=== Wheat Yield: Calibration Test (Supplementary Table 4) ===\n")
print(calib_wheat_yld)

## 2.10 Heterogeneous treatment effects - Distribution (Figure 5c)
tau_wheat_yld <- predict(cf_wheat_yld, target.sample = "all", estimate.variance = TRUE)
tau_wheat_yld <- as.data.frame(tau_wheat_yld)
tau_wheat_yld_pred <- tau_wheat_yld[, 1:3]
colnames(tau_wheat_yld_pred) <- c("Before 3 yrs - Never", "Last 3 yrs - Never", "2020-21 - Never")

tau_wheat_yld_long <- reshape2::melt(tau_wheat_yld_pred)

ggplot(tau_wheat_yld_long, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles") +
  theme_bw(base_size = 16) +
  labs(x = "Wheat yield gain (kg/ha)", y = "Frequency of laser land levelling")

## 2.11 Policy tree (depth 2 and depth 3)
DR.scores_wheat_yld <- double_robust_scores(cf_wheat_yld)
X_wheat_yld_pt <- X_wheat_yld %>% drop_na()

## Match rows
n_min <- min(nrow(X_wheat_yld_pt), nrow(DR.scores_wheat_yld))
X_wheat_yld_pt <- X_wheat_yld_pt[1:n_min, ]
DR.scores_wheat_yld <- DR.scores_wheat_yld[1:n_min, ]

tr_wheat_yld_d2 <- policy_tree(X_wheat_yld_pt, DR.scores_wheat_yld, depth = 2)
plot(tr_wheat_yld_d2)

tr_wheat_yld_d3 <- hybrid_policy_tree(X_wheat_yld_pt, DR.scores_wheat_yld, depth = 3)
plot(tr_wheat_yld_d3)

## Transition matrix of policy change
tr_assign_wheat_yld <- data_wheat_yld_cf
tr_assign_wheat_yld <- tr_assign_wheat_yld[1:min(nrow(tr_assign_wheat_yld), nrow(X_wheat_yld_pt)), ]
tr_assign_wheat_yld$depth2 <- predict(tr_wheat_yld_d2, X_wheat_yld_pt)

tr_assign_wheat_yld$depth2_cat[tr_assign_wheat_yld$depth2 == 1] <- "Never"
tr_assign_wheat_yld$depth2_cat[tr_assign_wheat_yld$depth2 == 2] <- "Before 3 yrs"
tr_assign_wheat_yld$depth2_cat[tr_assign_wheat_yld$depth2 == 3] <- "Last 3 yrs"
tr_assign_wheat_yld$depth2_cat[tr_assign_wheat_yld$depth2 == 4] <- "2020-21"

tr_assign_wheat_yld$treat_lll_cat[tr_assign_wheat_yld$treat_lll_new == 0] <- "Never"
tr_assign_wheat_yld$treat_lll_cat[tr_assign_wheat_yld$treat_lll_new == 1] <- "Before 3 yrs"
tr_assign_wheat_yld$treat_lll_cat[tr_assign_wheat_yld$treat_lll_new == 2] <- "Last 3 yrs"
tr_assign_wheat_yld$treat_lll_cat[tr_assign_wheat_yld$treat_lll_new == 3] <- "2020-21"

tr_assign_wheat_yld_DT <- data.table(tr_assign_wheat_yld)
TransMatrix_wheat_yld <- tr_assign_wheat_yld_DT[, .(Freq = .N), by = c("treat_lll_cat", "depth2_cat")]

ggplot(data = TransMatrix_wheat_yld,
       aes(axis1 = treat_lll_cat, axis2 = depth2_cat, y = Freq)) +
  geom_alluvium(aes(fill = depth2_cat)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = paste(after_stat(stratum), percent(after_stat(prop))))) +
  scale_x_discrete(limits = c("treat_lll_cat", "depth2_cat"), expand = c(0.15, 0.05)) +
  scale_fill_viridis_d() +
  theme_void(base_size = 20) +
  theme(legend.position = "none")


###############################################################################
## SECTION 3: RICE IRRIGATION (irrigation_hrs_rice_ha)
## Based on: Causal_machine_learning_revised2_punjab_irrigation.R
###############################################################################

## 3.1 Load data
data_rice_irri <- read_dta('LLL Punjab plot data_revised2_paper1_adoption3_irri.dta')

## Convert factor variables
factor_vars_rice_irri <- c("levelled_2021", "caste_cat2", "religion_new", "non_farm_binary", 
                           "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", 
                           "plot_waterloggprob", "rice_short_duration", "access_govt_ext", 
                           "access_kvk", "access_prog_farmer", "access_ngo", 
                           "access_farmer_grp", "access_input_dealer", "district")
for (variable in factor_vars_rice_irri) {
  data_rice_irri[[variable]] <- factor(data_rice_irri[[variable]])
}
data_rice_irri$treat_lll_order <- factor(data_rice_irri$treat_lll_new, ordered = FALSE)
data_rice_irri$district_order <- factor(data_rice_irri$district, ordered = FALSE)

## Create Pusa 44 variety dummy
data_rice_irri$pusa_44 <- 0
data_rice_irri$pusa_44[data_rice_irri$plot_kharif_if_rice_variety_name == "Pusa 44"] <- 1

## Subset and drop NAs for causal forest
data_rice_irri_cf <- subset(data_rice_irri, select = c(
  "irrigation_hrs_rice_ha", "levelled_2021", "treat_lll_new", "caste_cat2", 
  "religion_new", "non_farm_binary", "plot_soiltype2", "plot_soilfertility", 
  "plot_erosionprobl", "plot_waterloggprob", "rice_short_duration", "pusa_44", 
  "pump_hp_new", "access_govt_ext", "access_kvk", "access_prog_farmer", 
  "access_ngo", "access_farmer_grp", "access_input_dealer", "age_hhmember", 
  "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", 
  "plot_area_ha", "num_plot", "sp_lll_new", "datairrwater_tbl_21",
  "simpson_index_kh", "simpson_index_rb", "datadstdst_dhq", "district"))
data_rice_irri_cf <- data_rice_irri_cf %>% drop_na()

## Define outcome, covariates, and treatment
Y_rice_irri <- as.vector(data_rice_irri_cf$irrigation_hrs_rice_ha)

X_rice_irri <- subset(data_rice_irri_cf, select = c(
  "caste_cat2", "religion_new", "non_farm_binary", "plot_soiltype2", 
  "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob",
  "rice_short_duration", "pusa_44", "pump_hp_new", "access_govt_ext", 
  "access_kvk", "access_prog_farmer", "access_ngo", "access_farmer_grp", 
  "access_input_dealer", "age_hhmember", "edun_hhmember", 
  "total_num_adult_members_HH", "women_share", "asset_index", 
  "plot_area_ha", "num_plot", "sp_lll_new", "datairrwater_tbl_21",
  "simpson_index_kh", "simpson_index_rb", "datadstdst_dhq", "district"))

W_rice_irri <- as.factor(data_rice_irri_cf$treat_lll_new)

## 3.2 Probability forest
W.forest_rice_irri <- probability_forest(X_rice_irri, W_rice_irri,
                                         equalize.cluster.weights = FALSE,
                                         seed = 2)
W.hat_rice_irri <- predict(W.forest_rice_irri, estimate.variance = TRUE)$predictions

## 3.3 Regression forest
Y.forest_rice_irri <- regression_forest(X_rice_irri, Y_rice_irri,
                                        equalize.cluster.weights = FALSE,
                                        seed = 2)
Y.hat_rice_irri <- predict(Y.forest_rice_irri, estimate.variance = TRUE)$predictions

## 3.4 Multi-arm causal forest
cf_rice_irri <- multi_arm_causal_forest(X = X_rice_irri, Y = Y_rice_irri, 
                                        W = W_rice_irri,
                                        W.hat = W.hat_rice_irri, 
                                        Y.hat = Y.hat_rice_irri, 
                                        seed = 2)

## 3.5 Average treatment effects (Table 2 - Rice Irrigation CATE column)
ate_rice_irri <- average_treatment_effect(cf_rice_irri, method = "AIPW")
cat("\n=== Rice Irrigation: Average Treatment Effects (CATE) ===\n")
print(ate_rice_irri)

## 3.6 Variable importance
varimp_rice_irri <- variable_importance(cf_rice_irri)
vars_rice_irri <- colnames(X_rice_irri)
varimp_df_rice_irri <- data.frame(
  Variable = vars_rice_irri,
  Importance = as.numeric(varimp_rice_irri)
)

ggplot(varimp_df_rice_irri, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_point(color = "steelblue") +
  coord_flip() +
  labs(x = "Variables", y = "Variable importance", title = "Rice Irrigation - Variable Importance") +
  theme_bw(base_size = 16)

## 3.7 Binary calibration check
W_rice_irri_binary <- as.vector(data_rice_irri_cf$levelled_2021)

W.forest_rice_irri_bin <- regression_forest(X_rice_irri, W_rice_irri_binary,
                                            equalize.cluster.weights = FALSE, seed = 2)
W.hat_rice_irri_bin <- predict(W.forest_rice_irri_bin, estimate.variance = TRUE)$predictions

Y.forest_rice_irri_bin <- regression_forest(X_rice_irri, Y_rice_irri,
                                            equalize.cluster.weights = FALSE, seed = 2)
Y.hat_rice_irri_bin <- predict(Y.forest_rice_irri_bin, estimate.variance = TRUE)$predictions

cf_rice_irri_bin <- causal_forest(X = X_rice_irri, Y = Y_rice_irri, 
                                  W = W_rice_irri_binary,
                                  W.hat = W.hat_rice_irri_bin,
                                  Y.hat = Y.hat_rice_irri_bin,
                                  seed = 2)

ate_rice_irri_bin <- average_treatment_effect(cf_rice_irri_bin, target.sample = "overlap")
cat("\n=== Rice Irrigation: Binary ATE (calibration check) ===\n")
print(ate_rice_irri_bin)

calib_rice_irri <- test_calibration(cf_rice_irri_bin)
cat("\n=== Rice Irrigation: Calibration Test ===\n")
print(calib_rice_irri)

## 3.8 Heterogeneous treatment effects - Distribution (Figure 5b)
tau_rice_irri <- predict(cf_rice_irri, target.sample = "all", estimate.variance = TRUE)
tau_rice_irri <- as.data.frame(tau_rice_irri)
tau_rice_irri_pred <- tau_rice_irri[, 1:3]
colnames(tau_rice_irri_pred) <- c("Before 3 yrs - Never", "Last 3 yrs - Never", "2020-21 - Never")

tau_rice_irri_long <- reshape2::melt(tau_rice_irri_pred)

ggplot(tau_rice_irri_long, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles") +
  theme_bw(base_size = 16) +
  labs(x = "Rice irrigation (h/ha)", y = "Frequency of laser land levelling")


###############################################################################
## SECTION 4: WHEAT IRRIGATION (plot_irri_hrs_wheat_ha)
## Based on: Causal_machine_learning_revised2_punjab_wheat_irrigation.R
###############################################################################

## 4.1 Load data
data_wheat_irri <- read_dta('LLL Punjab plot data_revised2_paper1_adoption3_wheat_irrigation.dta')

## Create HD3086 wheat variety dummy
data_wheat_irri$HD3086 <- 0
data_wheat_irri$HD3086[data_wheat_irri$plot_rabi_wheat_variety_name == "HD 3086"] <- 1
data_wheat_irri$HD3086[data_wheat_irri$plot_rabi_wheat_variety_name == "Hd 3086"] <- 1
data_wheat_irri$HD3086[data_wheat_irri$plot_rabi_wheat_variety_name == "Hd3086"] <- 1
data_wheat_irri$HD3086[data_wheat_irri$plot_rabi_wheat_variety_name == "HD3086"] <- 1
data_wheat_irri$HD3086[data_wheat_irri$plot_rabi_wheat_variety_name == "HD3086;"] <- 1

## Create district dummies
data_wheat_irri$district_name_new_num <- 0
data_wheat_irri$district_name_new_num[data_wheat_irri$district_name_new == "Fatehgarh_Sahib_1"] <- 1
data_wheat_irri$district_name_new_num[data_wheat_irri$district_name_new == "Ludhiana_2"] <- 2
data_wheat_irri$district_name_new_num[data_wheat_irri$district_name_new == "Patiala_3"] <- 3
data_wheat_irri$district_name_new_num[data_wheat_irri$district_name_new == "Sangrur_4 "] <- 4

## Convert factor variables
factor_vars_wheat_irri <- c("levelled_2021", "caste_cat2", "religion_new", "non_farm_binary", 
                            "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", 
                            "plot_waterloggprob", "subsidy_lll", "access_govt_ext", 
                            "access_kvk", "access_prog_farmer", "access_ngo", 
                            "access_farmer_grp", "access_input_dealer")
for (variable in factor_vars_wheat_irri) {
  data_wheat_irri[[variable]] <- factor(data_wheat_irri[[variable]])
}
data_wheat_irri$treat_lll_order <- factor(data_wheat_irri$treat_lll_new, ordered = FALSE)
data_wheat_irri$district_order <- factor(data_wheat_irri$district_name_new_num, ordered = FALSE)

## 4.2 OLS regression (Table 2 - Wheat Irrigation OLS column)
ols_wheat_irri <- lm(plot_irri_hrs_wheat_ha ~ treat_lll_order + age_hhmember + edun_hhmember + 
                       caste_cat2 + religion_new + total_num_adult_members_HH + women_share + 
                       non_farm_binary + asset_index + plot_area_ha + num_plot + 
                       plot_soiltype2 + plot_soilfertility + plot_erosionprobl + 
                       plot_waterloggprob + HD3086 + N_use + pump_hp + sp_lll_new + 
                       subsidy_lll + access_govt_ext + access_kvk + access_prog_farmer + 
                       access_ngo + access_farmer_grp + access_input_dealer + 
                       datairrwater_tbl_21 + simpson_index_kh + simpson_index_rb + 
                       lll_village_adopt_share + datadstdst_dhq + district_order, 
                     data = data_wheat_irri)
cat("\n=== Wheat Irrigation: OLS Results ===\n")
summary(ols_wheat_irri)
stargazer(ols_wheat_irri, type = "text", keep.stat = c("n", "rsq"))

## 4.3 Prepare data for causal forest
data_wheat_irri_cf <- subset(data_wheat_irri, select = c(
  "plot_irri_hrs_wheat_ha", "treat_lll_new", "caste_cat2", "religion_new", 
  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", 
  "plot_waterloggprob", "access_govt_ext", "access_kvk", "access_prog_farmer", 
  "access_ngo", "access_farmer_grp", "access_input_dealer", "age_hhmember", 
  "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", 
  "plot_area_ha", "subsidy_lll", "num_plot", "sp_lll_new", "datairrwater_tbl_21",
  "simpson_index_kh", "simpson_index_rb", "datadstdst_dhq", "levelled_2021",
  "district_name_new_num", "HD3086", "N_use", "pump_hp"))
data_wheat_irri_cf <- data_wheat_irri_cf %>% drop_na()

## Define outcome, covariates, and treatment
Y_wheat_irri <- as.vector(data_wheat_irri_cf$plot_irri_hrs_wheat_ha)

X_wheat_irri <- subset(data_wheat_irri_cf, select = c(
  "caste_cat2", "religion_new", "non_farm_binary", "plot_soiltype2", 
  "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob", 
  "access_govt_ext", "access_kvk", "access_prog_farmer", "access_ngo", 
  "access_farmer_grp", "access_input_dealer", "age_hhmember", "edun_hhmember", 
  "total_num_adult_members_HH", "women_share", "asset_index", "plot_area_ha", 
  "subsidy_lll", "num_plot", "sp_lll_new", "datairrwater_tbl_21",
  "simpson_index_kh", "simpson_index_rb", "datadstdst_dhq", 
  "district_name_new_num", "HD3086", "N_use", "pump_hp"))

W_wheat_irri <- as.factor(data_wheat_irri_cf$treat_lll_new)

## 4.4 Probability forest
W.forest_wheat_irri <- probability_forest(X_wheat_irri, W_wheat_irri,
                                          equalize.cluster.weights = FALSE,
                                          seed = 2)
W.hat_wheat_irri <- predict(W.forest_wheat_irri, estimate.variance = TRUE)$predictions

## 4.5 Regression forest
Y.forest_wheat_irri <- regression_forest(X_wheat_irri, Y_wheat_irri,
                                         equalize.cluster.weights = FALSE,
                                         seed = 2)
Y.hat_wheat_irri <- predict(Y.forest_wheat_irri, estimate.variance = TRUE)$predictions

## 4.6 Multi-arm causal forest
cf_wheat_irri <- multi_arm_causal_forest(X = X_wheat_irri, Y = Y_wheat_irri, 
                                         W = W_wheat_irri,
                                         W.hat = W.hat_wheat_irri, 
                                         Y.hat = Y.hat_wheat_irri, 
                                         seed = 2)

## 4.7 Average treatment effects (Table 2 - Wheat Irrigation CATE column)
ate_wheat_irri <- average_treatment_effect(cf_wheat_irri, method = "AIPW")
cat("\n=== Wheat Irrigation: Average Treatment Effects (CATE) ===\n")
print(ate_wheat_irri)

## 4.8 Variable importance
varimp_wheat_irri <- variable_importance(cf_wheat_irri)
vars_wheat_irri <- colnames(X_wheat_irri)
varimp_df_wheat_irri <- data.frame(
  Variable = vars_wheat_irri,
  Importance = as.numeric(varimp_wheat_irri)
)

ggplot(varimp_df_wheat_irri, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_point(color = "steelblue") +
  coord_flip() +
  labs(x = "Variables", y = "Variable importance", title = "Wheat Irrigation - Variable Importance") +
  theme_bw(base_size = 16)

## 4.9 Binary calibration check
W_wheat_irri_binary <- as.vector(data_wheat_irri_cf$levelled_2021)

W.forest_wheat_irri_bin <- regression_forest(X_wheat_irri, W_wheat_irri_binary,
                                             equalize.cluster.weights = FALSE, seed = 2)
W.hat_wheat_irri_bin <- predict(W.forest_wheat_irri_bin, estimate.variance = TRUE)$predictions

Y.forest_wheat_irri_bin <- regression_forest(X_wheat_irri, Y_wheat_irri,
                                             equalize.cluster.weights = FALSE, seed = 2)
Y.hat_wheat_irri_bin <- predict(Y.forest_wheat_irri_bin, estimate.variance = TRUE)$predictions

cf_wheat_irri_bin <- causal_forest(X = X_wheat_irri, Y = Y_wheat_irri, 
                                   W = W_wheat_irri_binary,
                                   W.hat = W.hat_wheat_irri_bin,
                                   Y.hat = Y.hat_wheat_irri_bin,
                                   seed = 2)

ate_wheat_irri_bin <- average_treatment_effect(cf_wheat_irri_bin, target.sample = "overlap")
cat("\n=== Wheat Irrigation: Binary ATE (calibration check) ===\n")
print(ate_wheat_irri_bin)

calib_wheat_irri <- test_calibration(cf_wheat_irri_bin)
cat("\n=== Wheat Irrigation: Calibration Test ===\n")
print(calib_wheat_irri)

## 4.10 Heterogeneous treatment effects - Distribution (Figure 5d)
tau_wheat_irri <- predict(cf_wheat_irri, target.sample = "all", estimate.variance = TRUE)
tau_wheat_irri <- as.data.frame(tau_wheat_irri)
tau_wheat_irri_pred <- tau_wheat_irri[, 1:3]
colnames(tau_wheat_irri_pred) <- c("Before 3 yrs - Never", "Last 3 yrs - Never", "2020-21 - Never")

tau_wheat_irri_long <- reshape2::melt(tau_wheat_irri_pred)

ggplot(tau_wheat_irri_long, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles") +
  theme_bw(base_size = 16) +
  labs(x = "Wheat irrigation (h/ha)", y = "Frequency of laser land levelling")

## 4.11 Policy tree
DR.scores_wheat_irri <- double_robust_scores(cf_wheat_irri)
X_wheat_irri_pt <- X_wheat_irri %>% drop_na()

n_min <- min(nrow(X_wheat_irri_pt), nrow(DR.scores_wheat_irri))
X_wheat_irri_pt <- X_wheat_irri_pt[1:n_min, ]
DR.scores_wheat_irri <- DR.scores_wheat_irri[1:n_min, ]

tr_wheat_irri_d2 <- policy_tree(X_wheat_irri_pt, DR.scores_wheat_irri, depth = 2)
plot(tr_wheat_irri_d2)

tr_wheat_irri_d3 <- hybrid_policy_tree(X_wheat_irri_pt, DR.scores_wheat_irri, depth = 3)
plot(tr_wheat_irri_d3)


###############################################################################
## END OF REPLICATION CODE
###############################################################################
cat("\n=== Replication complete ===\n")
cat("Results correspond to Table 2, Figure 5, and Supplementary materials\n")
