###Setting the directory for paper 2 Impact of LLL (Final code for yiled 2)
setwd("C:/Users/ZEF/Desktop/PhD/PhD paper/Paper 2 Impact/ML analysis")
##Uploading stata data to r
#install.packages('haven')
library(haven)
data <- read_dta('LLL Punjab plot data_revised2_paper1_adoption3_wheat_yield.dta')
table(data$treat_lll_new) 


###Plot showing the rice yield by frequency of adoption
library(ggpubr) 
library(tidyverse)

frequency_outcome_errorplot=
  data%>% 
  drop_na(treat_lll_new) %>%
  ggerrorplot(x = "treat_lll_new", y = "rabi_wheat_yld_ha",add = "mean", error.plot = "errorbar", color="black",size=1, ggtheme=theme_bw())+
  labs(x="Frequency of adoption",y="Wheat yield (kg/ha)")+
  theme_bw(base_size = 16)+coord_flip()

frequency_outcome_errorplot+aes(x = fct_reorder(treat_lll_new, rabi_wheat_yld_ha))+
  xlab("Frequency of adoption")
frequency_outcome_errorplot3 = frequency_outcome_errorplot+ scale_x_discrete(labels = c("Never","Before last 3 years","Last 3 year", "2020-21" ))
frequency_outcome_errorplot3

###Descriptive statistics
#install.packages('fBasics')
#install.packages('fastDummies')
library(fBasics)
library(fastDummies)

data$HD3086=0
data$HD3086[data$plot_rabi_wheat_variety_name=="HD 3086"]=1
data$HD3086[data$plot_rabi_wheat_variety_name=="Hd 3086"]=1
data$HD3086[data$plot_rabi_wheat_variety_name=="Hd3086"]=1
data$HD3086[data$plot_rabi_wheat_variety_name=="HD3086"]=1
data$HD3086[data$plot_rabi_wheat_variety_name=="HD3086;"]=1
table(data$HD3086)
as.numeric(data$HD3086)

data_desc=fastDummies::dummy_cols(data, select_columns=c("levelled_2021", "caste_cat2",  "religion_new",  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob", "HD3086", "access_govt_ext" , "access_kvk", "access_prog_farmer", "access_ngo", "access_farmer_grp", "access_input_dealer"))

ls(data_desc)
library(fBasics)
summ_stats <- fBasics::basicStats(data_desc[,c("age_hhmember", "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", "plot_area_ha", "plot_dist_lll", "num_plot", "HD3086_1", "sp_lll_new", "datairrwater_tbl_21","simpson_index_kh", "simpson_index_rb", "datadstdst_dhq", "_Iaccess_fa_1", "_Iaccess_go_1","_Iaccess_in_1", "_Iaccess_kv_1" , "_Iaccess_ng_1", "_Iaccess_pr_1", "_Icaste_cat_2", "_Iplot_soil_2", "_Iplot_wate_1", "_Ireligion__1" )])

summ_stats <- as.data.frame(t(summ_stats))

# Rename some of the columns for convenience
summ_stats <- summ_stats[c("Mean", "Stdev", "Minimum", "1. Quartile", "Median",  "3. Quartile", "Maximum")] %>% 
  rename("Lower quartile" = '1. Quartile', "Upper quartile"= "3. Quartile")
summ_stats
###Frequency of adoption by region
table(data$district, data$treat_lll_new)

### OLS and shapely value regression

data$treat_lll_order <- factor( data$treat_lll_new, ordered = FALSE )

#Latitude and longitude varibale creation
data$HH_latitude=as.numeric(data$HH_latitude)
data$HH_longitude=as.numeric(data$HH_longitude)

#install.packages("lubridate")
library(lubridate)
data$plot_rabi_wheat_sowing_date=anydate(data$plot_rabi_wheat_sowing_date)
data$januaryfirst2020=ymd("2020-01-01")

data$plot_rabi_wheat_sowing_date_day=data$plot_rabi_wheat_sowing_date-data$januaryfirst2020
data$plot_rabi_wheat_sowing_date_day=as.numeric(data$plot_rabi_wheat_sowing_date_day)

#District dummies
data$district_name_new_num=0
data$district_name_new_num[data$district_name_new=="Fatehgarh_Sahib_1"]=1
data$district_name_new_num[data$district_name_new=="Ludhiana_2"]=2
data$district_name_new_num[data$district_name_new=="Patiala_3"]=3
data$district_name_new_num[data$district_name_new=="Sangrur_4 "]=4

##converting factor varibales

factor_varibales <-c("levelled_2021", "caste_cat2",  "religion_new",  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob","HD3086", "subsidy_lll", "access_govt_ext" , "access_kvk", "access_prog_farmer", "access_ngo", "access_farmer_grp", "access_input_dealer","HD3086" )
for (variable in factor_varibales) {
  data[[variable]] <- factor(data[[variable]])
}

data$district_order <-factor(data$district_name_new_num, ordered = FALSE)

##ols = lm(rabi_wheat_yld_ha ~treat_lll_order+age_hhmember+ edun_hhmember+caste_cat2+religion_new+total_num_adult_members_HH+women_share+non_farm_binary+asset_index+ plot_area_ha+ plot_dist_lll+ num_plot+plot_soiltype2+plot_soilfertility+plot_erosionprobl+plot_waterloggprob+HD3086+N_use+plot_rabi_wheat_sowing_date_day+sp_lll_new+access_govt_ext+access_kvk+access_prog_farmer+access_ngo+access_farmer_grp+access_input_dealer+ datairrwater_tbl_21+simpson_index_kh+simpson_index_rb+ datadstdst_dhq+district_name_new_num+HH_latitude+HH_longitude, data = data)
#summary(ols)
ols = lm(rabi_wheat_yld_ha ~treat_lll_order+age_hhmember+ edun_hhmember+caste_cat2+religion_new+total_num_adult_members_HH+women_share+non_farm_binary+asset_index+ plot_area_ha+ plot_dist_lll+ num_plot+plot_soiltype2+plot_soilfertility+plot_erosionprobl+plot_waterloggprob+HD3086+N_use+sp_lll_new+subsidy_lll+access_govt_ext+access_kvk+access_prog_farmer+access_ngo+access_farmer_grp+access_input_dealer+ datairrwater_tbl_21+simpson_index_kh+simpson_index_rb+lll_village_adopt_share+ datadstdst_dhq+district_order, data = data)
summary(ols)

##Visualising ols results in table
#install.packages('stargazer')
library(stargazer)
stargazer(ols,
          type="text",
          keep.stat=c("n","rsq"))
##Visualising ols results in graph
#install.packages('modelsummary')
library(modelsummary)
b <- list(geom_vline(xintercept = 0, color = 'orange'))
modelplot(ols,background = b,coef_omit = "Interc")

anova(ols)

###Conventional random forest model
#install.packages('grf')
library(grf)
#install.packages('policytree')
library(policytree)
ls(data)
#install.packages('tidyr')
library(tidyr)

data$HH_latitude=as.numeric(data$HH_latitude)
data$HH_longitude=as.numeric(data$HH_longitude)

data$HD3086=0
data$HD3086[data$plot_rabi_wheat_variety_name=="HD 3086"]=1
data$HD3086[data$plot_rabi_wheat_variety_name=="Hd 3086"]=1
data$HD3086[data$plot_rabi_wheat_variety_name=="Hd3086"]=1
data$HD3086[data$plot_rabi_wheat_variety_name=="HD3086"]=1
data$HD3086[data$plot_rabi_wheat_variety_name=="HD3086;"]=1
table(data$HD3086)

#District dummies
data$district_name_new_num=0
data$district_name_new_num[data$district_name_new=="Fatehgarh_Sahib_1"]=1
data$district_name_new_num[data$district_name_new=="Ludhiana_2"]=2
data$district_name_new_num[data$district_name_new=="Patiala_3"]=3
data$district_name_new_num[data$district_name_new=="Sangrur_4 "]=4

library(anytime)
#install.packages("lubridate")
library(lubridate)
data$plot_rabi_wheat_sowing_date=anydate(data$plot_rabi_wheat_sowing_date)
data$januaryfirst2020=ymd("2020-01-01")

data$plot_rabi_wheat_sowing_date_day=data$plot_rabi_wheat_sowing_date-data$januaryfirst2020
data$plot_rabi_wheat_sowing_date_day=as.numeric(data$plot_rabi_wheat_sowing_date_day)

data=subset(data, select=c("rabi_wheat_yld_ha","treat_lll_new","caste_cat2",  "religion_new",  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob", "access_govt_ext" , "access_kvk", "access_prog_farmer", "access_ngo", "access_farmer_grp", "access_input_dealer", "age_hhmember", "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", "plot_area_ha", "num_plot", "sp_lll_new", "subsidy_lll", "datairrwater_tbl_21","simpson_index_kh", "simpson_index_rb", "datadstdst_dhq","levelled_2021","HD3086","N_use","district_name_new_num"))
library(tidyr)
data=data%>% drop_na()
library(fBasics)
summ_stats <- fBasics::basicStats(data)
summ_stats <- as.data.frame(t(summ_stats))

# Rename some of the columns for convenience
summ_stats <- summ_stats[c("Mean", "Stdev", "Minimum", "1. Quartile", "Median",  "3. Quartile", "Maximum")] %>% 
  rename("Lower quartile" = '1. Quartile', "Upper quartile"= "3. Quartile")
summ_stats


Y_cf_freq=as.vector(data$rabi_wheat_yld_ha )


# Fit regression forest yield gains
X_rf_freq=subset(data, select=c("treat_lll_new","caste_cat2",  "religion_new",  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob", "access_govt_ext" , "access_kvk", "access_prog_farmer", "access_ngo", "access_farmer_grp", "access_input_dealer", "age_hhmember", "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", "plot_area_ha", "plot_dist_lll", "num_plot", "sp_lll_new", "datairrwater_tbl_21","simpson_index_kh", "simpson_index_rb", "datadstdst_dhq","HD3086","N_use", "plot_rabi_wheat_sowing_date_day"))
ls(data)
Y.multi_freq.forest.rf <- regression_forest(X_rf_freq, Y_cf_freq,
                                            equalize.cluster.weights = FALSE,
                                            seed = 2
)

print(Y.multi_freq.forest.rf)

varimp.multi_freq.rf <- variable_importance(Y.multi_freq.forest.rf)

Y.hat.multi.all_freq.rf <- predict(Y.multi_freq.forest.rf, estimate.variance = TRUE)$predictions

summary(Y.hat.multi.all_freq.rf)

X_rf_freq.t1=X_rf_freq
X_rf_freq.t1$treat_lll_new=0

X_rf_freq.t2=X_rf_freq
X_rf_freq.t2$treat_lll_new=1

X_rf_freq.t3=X_rf_freq
X_rf_freq.t3$treat_lll_new=2

X_rf_freq.t4=X_rf_freq
X_rf_freq.t4$treat_lll_new=3

# Calculate yield differences against frequency of laser land levelling
Y.hat.rf.freq.t1 = predict(Y.multi_freq.forest.rf, newdata=X_rf_freq.t1, estimate.variance=TRUE)
Y.hat.rf.freq.t2 = predict(Y.multi_freq.forest.rf, newdata=X_rf_freq.t2, estimate.variance=TRUE)
Y.hat.rf.freq.t3 = predict(Y.multi_freq.forest.rf, newdata=X_rf_freq.t3, estimate.variance=TRUE)
Y.hat.rf.freq.t4 = predict(Y.multi_freq.forest.rf, newdata=X_rf_freq.t4, estimate.variance=TRUE)



Y.hat.rf.freq.t2_t1 = Y.hat.rf.freq.t2$predictions-Y.hat.rf.freq.t1$predictions
Y.hat.rf.freq.t3_t1 = Y.hat.rf.freq.t3$predictions-Y.hat.rf.freq.t1$predictions
Y.hat.rf.freq.t4_t1 = Y.hat.rf.freq.t4$predictions-Y.hat.rf.freq.t1$predictions


library(fBasics)
rf_pred_contrasts=as.data.frame(cbind(Y.hat.rf.freq.t2_t1,Y.hat.rf.freq.t3_t1,Y.hat.rf.freq.t4_t1))

summ_stats_rf <- fBasics::basicStats(rf_pred_contrasts)

summ_stats_rf <- as.data.frame(t(summ_stats_rf))

# Rename some of the columns for convenience
summ_stats_rf <- summ_stats_rf[c("Mean", "Stdev", "Minimum", "1. Quartile", "Median",  "3. Quartile", "Maximum")] %>% 
  rename("Lower quartile" = '1. Quartile', "Upper quartile"= "3. Quartile")

summ_stats_rf

###Visualizing data in kdensity plot

rf_pred_contrasts_long=reshape2::melt(rf_pred_contrasts)

library(ggplot2)
#install.packages("ggridges")
library(ggridges)
ggplot(rf_pred_contrasts_long, aes(x=value, y=variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")+
  theme_bw(base_size = 16)+labs(x="Wheat yield gain(kg/ha)",y="Frequency of laser land levelling")


###Causal random forest model
data=subset(data, select=c("rabi_wheat_yld_ha","treat_lll_new","caste_cat2",  "religion_new",  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob", "access_govt_ext" , "access_kvk", "access_prog_farmer", "access_ngo", "access_farmer_grp", "access_input_dealer", "age_hhmember", "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", "plot_area_ha", "subsidy_lll", "num_plot", "sp_lll_new", "datairrwater_tbl_21","simpson_index_kh", "simpson_index_rb", "datadstdst_dhq","levelled_2021","district_name_new_num","HD3086","N_use"))
library(tidyr)
data=data%>% drop_na()
Y_cf_freq=as.vector(data$rabi_wheat_yld_ha )

X_cf_freq=subset(data, select=c("caste_cat2",  "religion_new",  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob", "access_govt_ext" , "access_kvk", "access_prog_farmer", "access_ngo", "access_farmer_grp", "access_input_dealer", "age_hhmember", "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", "plot_area_ha", "subsidy_lll", "num_plot", "sp_lll_new", "datairrwater_tbl_21","simpson_index_kh", "simpson_index_rb", "datadstdst_dhq","district_name_new_num","HD3086","N_use"))

W_cf_freq <- as.factor(data$treat_lll_new)

##Probability random forest to create weights
W.multi_freq.forest <- probability_forest(X_cf_freq, W_cf_freq,
                                          equalize.cluster.weights = FALSE,
                                          seed = 2
)
W.hat.multi.all_freq <- predict(W.multi_freq.forest, estimate.variance = TRUE)$predictions
# Regression forest to get expected responses 
Y.multi_freq.forest <- regression_forest(X_cf_freq, Y_cf_freq,
                                         equalize.cluster.weights = FALSE,
                                         seed = 2
)

print(Y.multi_freq.forest)

varimp.multi_freq <- variable_importance(Y.multi_freq.forest)
Y.hat.multi.all_freq <- predict(Y.multi_freq.forest, estimate.variance = TRUE)$predictions

# Fit multi-arm causal RF model
multi_freq.forest <- multi_arm_causal_forest(X = X_cf_freq, Y = Y_cf_freq, W = W_cf_freq ,W.hat=W.hat.multi.all_freq,Y.hat=Y.hat.multi.all_freq,seed=2) 

varimp.multi_freq_cf <- variable_importance(multi_freq.forest)

# Average treatment effects
multi_freq_ate=average_treatment_effect(multi_freq.forest, method="AIPW")
multi_freq_ate

varimp.multi_freq_cf <- variable_importance(multi_freq.forest)

vars_freq=c("caste_cat2",  "religion_new",  "non_farm_binary", "plot_soiltype2", "plot_soilfertility", "plot_erosionprobl", "plot_waterloggprob", "access_govt_ext" , "access_kvk", "access_prog_farmer", "access_ngo", "access_farmer_grp", "access_input_dealer", "age_hhmember", "edun_hhmember", "total_num_adult_members_HH", "women_share", "asset_index", "plot_area_ha", "subsidy_lll", "num_plot", "sp_lll_new", "datairrwater_tbl_21","simpson_index_kh", "simpson_index_rb", "datadstdst_dhq","district_name_new_num","HD3086","N_use")
## variable importance plot ----------------------------------------------------
varimpvars_freq=as.data.frame(cbind(varimp.multi_freq_cf,vars_freq))
names(varimpvars_freq)[1]="Variableimportance_freq"
varimpvars_freq$Variableimportance_freq=formatC(varimpvars_freq$Variableimportance_freq, digits = 2, format = "f")
varimpvars_freq$Variableimportance_freq=as.numeric(varimpvars_freq$Variableimportance_freq)
varimpplotRF_freq=ggplot(varimpvars_freq,aes(x=reorder(vars_freq,Variableimportance_freq),y=Variableimportance_freq))+
  geom_jitter(color="steelblue")+
  coord_flip()+
  labs(x="Variables",y="Variable importance")
previous_theme <- theme_set(theme_bw(base_size = 16))
varimpplotRF_freq

###Binary causal calibration check (binary term)

W_cf_freq_binary=as.vector(data$levelled_2021) 

# Probability random forest to create weights
W.multi_freq.forest_binary <- regression_forest(X_cf_freq, W_cf_freq_binary,
                                                equalize.cluster.weights = FALSE,
                                                seed = 2
)
W.hat.multi.all_freq_binary <- predict(W.multi_freq.forest_binary, estimate.variance = TRUE)$predictions

hist(W.hat.multi.all_freq_binary, xlab = "Treatment propensity score")

# Regression forest to get expected responses 
Y.multi_freq.forest_binary <- regression_forest(X_cf_freq, Y_cf_freq,
                                                equalize.cluster.weights = FALSE,
                                                seed = 2
)

print(Y.multi_freq.forest_binary)

varimp.multi_freq_binary <- variable_importance(Y.multi_freq.forest_binary)
Y.hat.multi.all_freq_binary <- predict(Y.multi_freq.forest_binary, estimate.variance = TRUE)$predictions

# Fit binary causal RF model
multi_freq.forest_binary <- causal_forest(X = X_cf_freq, Y = Y_cf_freq, W = W_cf_freq_binary,Y.hat=Y.hat.multi.all_freq_binary,W.hat=W.hat.multi.all_freq_binary ,seed=2,equalize.cluster.weights=FALSE,tune.parameters="all") 
#,

varimp.multi_freq_cf_binary <- variable_importance(multi_freq.forest_binary)

# Average treatment effects
multi_freq_ate_binary=average_treatment_effect(multi_freq.forest_binary,target.sample = "overlap")
multi_freq_ate_binary

multi_freq_binary_calibration=test_calibration(multi_freq.forest_binary)
multi_freq_binary_calibration

##Heterogeneous treatment effect- Distributional analysis

library(ggridges)
library(dplyr)
tau.multi_freq.forest=predict(multi_freq.forest, target.sample = "all",estimate.variance=TRUE)

tau.multi_freq.forest=as.data.frame(tau.multi_freq.forest)


tau.multi_freq.forest_X=data.frame(data,tau.multi_freq.forest)


# Ridges -------------------
tau.multi_freq.forest_pred=tau.multi_freq.forest[,0:3]

library(dplyr)
library(reshape2)
tau.multi_freq.forest_pred=rename(tau.multi_freq.forest_pred,"Before 3 yrs - Never"="predictions.1...0.Y.1")
tau.multi_freq.forest_pred=rename(tau.multi_freq.forest_pred,"Last 3 yrs - Never"="predictions.2...0.Y.1")
tau.multi_freq.forest_pred=rename(tau.multi_freq.forest_pred,"2020-21-Never"="predictions.3...0.Y.1")


tau.multi_freq.forest_pred_long=reshape2::melt(tau.multi_freq.forest_pred[,0:3])

ggplot(tau.multi_freq.forest_pred_long, aes(x=value, y=variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")+
  theme_bw(base_size = 16)+labs(x="Wheat yield gain(kg/ha)",y="Frequency of laser land levelling")

### Policy tree algorithm
## Compute robust scores
DR.scores_freq <- double_robust_scores(multi_freq.forest)
X_cf_freq = X_cf_freq %>% drop_na()
nrow(X_cf_freq)

nrow(DR.scores_freq)

##Matching the row missing
X_cf_freq <- X_cf_freq[1:min(nrow(X_cf_freq), nrow(DR.scores_freq)), ]
DR.scores_freq <- DR.scores_freq[1:min(nrow(X_cf_freq), nrow(DR.scores_freq)), ]

#install.packages("DiagrammeR")
library(DiagrammeR)
tr_freq <- policy_tree(X_cf_freq, DR.scores_freq, depth = 2) 
plot(tr_freq)

tr_freq3 <- hybrid_policy_tree(X_cf_freq, DR.scores_freq, depth = 3) 
plot(tr_freq3)

tr_assignment_freq=data
tr_assignment_freq<- tr_assignment_freq[1:min(nrow(tr_assignment_freq), nrow(X_cf_freq)), ]

tr_assignment_freq$depth2 <- predict(tr_freq, X_cf_freq)
table(tr_assignment_freq$depth2)

##Transition matrix of policy change

tr_assignment_freq$depth2_cat[tr_assignment_freq$depth2 == 1] <- "Never"
tr_assignment_freq$depth2_cat[tr_assignment_freq$depth2 == 2] <- "Before 3 yrs"
tr_assignment_freq$depth2_cat[tr_assignment_freq$depth2 == 3] <- "Last 3 yrs"
tr_assignment_freq$depth2_cat[tr_assignment_freq$depth2 == 4] <- "2020-21"

tr_assignment_freq$treat_lll_new[tr_assignment_freq$treat_lll_new == 0] <- "Never"
tr_assignment_freq$treat_lll_new[tr_assignment_freq$treat_lll_new == 1] <- "Before 3 yrs"
tr_assignment_freq$treat_lll_new[tr_assignment_freq$treat_lll_new == 2] <- "Last 3 yrs"
tr_assignment_freq$treat_lll_new[tr_assignment_freq$treat_lll_new == 3] <- "2020-21"

library(ggalluvial)
library(data.table)
tr_assignment_freqDT = data.table(tr_assignment_freq)
TransitionMatrix_freq <- tr_assignment_freqDT[, (sum <- .N), by = c("treat_lll_new", "depth2_cat")]
library(dplyr)
TransitionMatrix_freq <- rename(TransitionMatrix_freq, Freq = V1)

library(scales)
transitionmatrixplot_freq <- ggplot(
  data = TransitionMatrix_freq,
  aes(axis1 = treat_lll_new, axis2 = depth2_cat, y = Freq)
) +
  geom_alluvium(aes(fill = depth2_cat)) +
  geom_stratum() +
  # geom_text(stat="stratum", aes(label=after_stat(stratum),nudge_y =5))+
  geom_text(stat = "stratum", aes(label = paste(after_stat(stratum), percent(after_stat(prop))))) +
  scale_x_discrete(
    limits = c("treat_lll_new", "depth2_cat"),
    expand = c(0.15, 0.05)
  ) +
  scale_fill_viridis_d() +
  theme_void(base_size = 20) +
  theme(legend.position = "none")

transitionmatrixplot_freq







