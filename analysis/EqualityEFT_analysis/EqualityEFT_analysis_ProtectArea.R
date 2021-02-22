################################
# Analysis of PA develeopment in
# Brazilian Ecological Fiscal Ts
# script authors: nils@droste.io
#                 ...
################################


# this is just to initialize prj

library(tidyverse)
library(here)
library(eventStudy)
library(ggpubr)

# trouble-shooting lfe installation
# install.packages("lfe",repos=unique(c(
#   getOption("repos"),
#   repos="https://cran.microsoft.com/snapshot/2020-12-04/"
# )))
# devtools::install_github("setzler/eventStudy/eventStudy")

# original data set Droste et al. 2017 EPG ----

# load data
PA_df <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/paneldataEFT-BR.csv")) # %>% # dirty hack to allow access /data in directory higher than Rproj root
# mutate(ID = ID %>% as.factor())

EFT <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/EFT.csv")) %>% rename(ID=X1, legislation=`ICMS-E`, enactment=Enactment) 

# minimal constant to logarithmize 0 values
const<-min(PA_df$mun[which(PA_df$mun > 0)])*0.5
const2<-min(PA_df$sta[which(PA_df$sta > 0)])*0.5

# create full dataset
full_df <- left_join(PA_df, EFT) %>% mutate(lnMun=log(mun+const), lnSta=log(sta+const2), lnFed=log(fed), lnTot=log(tot), lnAg=log(agr), lnInd=log(ind), lnPop=log(pop), lnInc=log(inc), year=as.integer(year), legislation=as.integer(legislation), enactment=as.integer(enactment))

# 
# # plot
# full_df %>%  ggpubr::ggboxplot(y="lnMun", x= "icms_e", color="icms_e", add = "jitter") + ggpubr::stat_compare_means(method = "t.test")
# 
# # get summary stats
# full_df %>% group_by(icms_e) %>%
#   summarize(mean = mean(mun),
#             median = median(mun),
#             sd = sd(mun))    


# run the event study
# enactment
event_study_reg_EFTenact <- ES(long_data=full_df %>% data.table::setDT(), outcomevar="lnMun",
                               unit_var="ID", cal_time_var="year",
                               onset_time_var="enactment", cluster_vars="ID",
                               discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
                               cont_covars = c('lnAg','lnInd', 'lnPop', 'lnInc', 'lnSta', 'lnFed'),
                               never_treat_action = "keep",
                               # linearize_pretrends = TRUE,
                               anticipation=1,
                               omitted_event_time = -3,
                               # min_control_gap = 3, max_control_gap = 5,
                               residualize_covariates = TRUE
                               
)

ES_plot_levels(event_study_reg_EFTenact, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")

ES_plot_ATTs(event_study_reg_EFTenact, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")

ES_plot_ATTs(event_study_reg_EFTenact, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")

# run the event study
# legislation
event_study_reg_EFTlegis <- ES(long_data=full_df %>% data.table::setDT(), outcomevar="lnMun",
                               unit_var="ID", cal_time_var="year",
                               onset_time_var="legislation", cluster_vars="ID",
                               #residualize_covariates = TRUE,
                               discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
                               cont_covars = c('lnAg','lnInd', 'lnPop', 'lnInc', 'lnSta', 'lnFed'),
                               never_treat_action = "keep",
                               # linearize_pretrends = TRUE,
                               anticipation=0,
                               omitted_event_time = -3,
                               # min_control_gap = 3, max_control_gap = 5,
                               residualize_covariates = TRUE
                               
)

ES_plot_levels(event_study_reg_EFTlegis, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")

ES_plot_ATTs(event_study_reg_EFTlegis, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")

ES_plot_ATTs(event_study_reg_EFTlegis, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")


# matching method

library(PanelMatch)

DisplayTreatment(unit.id = "ID_int",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "icms_e", data = full_df %>% mutate(ID_int = as.integer(ID %>% as.factor)) %>% as.data.frame()
)

PM.results <- PanelMatch(lag = 2, time.id = "year", unit.id = "ID_int", 
                         treatment = "icms_e", refinement.method = "mahalanobis", 
                         data = full_df %>% mutate(ID_int = as.integer(ID %>% as.factor)) %>% as.data.frame(),
                         match.missing = TRUE, covs.formula = ~ lnAg+lnInd+lnPop+lnInc+lnFed+lnSta+arpa+ama+cer+caa+mat+pan+pam, 
                         size.match = 5, qoi = "att" ,outcome.var = "lnMun",
                         lead = 0:10, forbid.treatment.reversal = FALSE
)

PE.results <- PanelEstimate(sets = PM.results, data = full_df %>% mutate(ID_int = as.integer(ID %>% as.factor)) %>% as.data.frame())

summary(PE.results)
plot(PE.results)

get_covariate_balance(PE.results$matched.sets, covariates = c("lnAg", "lnInd", "lnPop", "lnInc", "lnFed", "lnSta"), data = full_df %>% mutate(ID_int = as.integer(ID %>% as.factor)) %>% as.data.frame(), plot=T) 

PE.results.10percentCI <- PanelEstimate(sets = PM.results, data = full_df %>% mutate(ID_int = as.integer(ID %>% as.factor)) %>% as.data.frame(), confidence.level = 0.90)

summary(PE.results.10percentCI)
plot(PE.results.10percentCI)



# expanded data set ----

# load data
load("C:\\Users\\Nils\\Box\\papers\\work_in_progress\\Cooperman_etal_2021_EqualityEFT\\repo\\data\\completed_sets\\State_Panel_data.RData")

# minimal constant to logarithmize 0 values
const<-min(complete_df$munPA[which(complete_df$munPA > 0)])*0.5
const2<-min(complete_df$staPA[which(complete_df$staPA > 0)])*0.5

# create full datasets

analysis_df <- complete_df %>% mutate(lnMun=log(munPA+const), lnSta=log(staPA+const2), lnFed=log(fedPA), lnTot=log(totPA), lnAg=log1p(share_ag_value_added), lnExtInd=log1p(share_extr_ind_value_added), lnTransInd=log1p(share_trans_ind_value_added), lnPopDens=log(popdens), icms_e_leg=replace_na(icms_e_leg, 0), icms_e_enact=replace_na(icms_e_enact, 0), legislation=as.integer(legislation), enactment=as.integer(enactment), year=as.integer(year)) %>% filter(year>=1989) 

analysis_df_long <- complete_df %>% mutate(lnMun=log(munPA+const), lnSta=log(staPA+const2), lnFed=log(fedPA), lnTot=log(totPA), lnAg=log1p(share_ag_value_added), lnExtInd=log1p(share_extr_ind_value_added), lnTransInd=log1p(share_trans_ind_value_added), lnPopDens=log(popdens), icms_e_leg=replace_na(icms_e_leg, 0), icms_e_enact=replace_na(icms_e_enact, 0), legislation=as.integer(legislation), enactment=as.integer(enactment), year=as.integer(year)) %>% filter(year>=1989) %>% data.table::setDT() 


# plot
analysis_df %>%  ggpubr::ggboxplot(y="lnMun", x= "icms_e_enact", color="icms_e_enact", add = "jitter") + ggpubr::stat_compare_means(method = "t.test")

# get summary stats
analysis_df %>% group_by(icms_e_enact) %>%
  summarize(mean = mean(mun),
            median = median(mun),
            sd = sd(mun))


# run the event study
# enactment
event_study_reg_EFTenact <- ES(long_data=analysis_df_long, outcomevar="lnMun",
                               unit_var="state", cal_time_var="year",
                               onset_time_var="enactment", cluster_vars="state",
                               discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
                               cont_covars = c('lnAg','lnExtInd', 'lnTransInd', 'lnPopDens', 'lnSta', 'lnFed'),
                               never_treat_action = "keep",
                               # linearize_pretrends = TRUE,
                               anticipation=1,
                               omitted_event_time = -3,
                               # min_control_gap = 3, max_control_gap = 5,
                               residualize_covariates = TRUE
)

ES_plot_levels(event_study_reg_EFTenact, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")

ES_plot_ATTs(event_study_reg_EFTenact, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")

ES_plot_ATTs(event_study_reg_EFTenact, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")

# run the event study
# legislation
event_study_reg_EFTlegis <- ES(long_data= analysis_df_long, outcomevar="lnMun",
                               unit_var="state", cal_time_var="year",
                               onset_time_var="legislation", cluster_vars="state",
                               #residualize_covariates = TRUE,
                               discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
                               cont_covars = c('lnAg','lnExtInd', 'lnTransInd', 'lnPopDens', 'lnSta', 'lnFed'),
                               never_treat_action = "keep",
                               # linearize_pretrends = TRUE,
                               anticipation=0,
                               omitted_event_time = -2,
                               # min_control_gap = 3, max_control_gap = 5,
                               residualize_covariates = TRUE
                               
)

ES_plot_levels(event_study_reg_EFTlegis, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")

ES_plot_ATTs(event_study_reg_EFTlegis, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")

ES_plot_ATTs(event_study_reg_EFTlegis, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")


# matching method

library(PanelMatch)

DisplayTreatment(unit.id = "ID",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "icms_e_enact", data = analysis_df %>% mutate(ID = as.integer(state %>% as.factor)) %>% as.data.frame()
)

PM.results <- PanelMatch(lag = 1, time.id = "year", unit.id = "ID", 
                         treatment = "icms_e_enact", refinement.method = "mahalanobis", 
                         data = analysis_df %>% mutate(ID = as.integer(state %>% as.factor)) %>% as.data.frame(),
                         match.missing = TRUE, covs.formula = ~ lnAg+lnExtInd+lnTransInd+lnPopDens+lnFed+lnSta+arpa+ama+cer+caa+mat+pan+pam, 
                         size.match = 5, qoi = "att" ,outcome.var = "lnMun",
                         lead = 0:10, forbid.treatment.reversal = FALSE
)

PE.results <- PanelEstimate(sets = PM.results, data =  analysis_df %>% mutate(ID = as.integer(state %>% as.factor)) %>% as.data.frame(), confidence.level = 0.95)

summary(PE.results)
plot(PE.results)

get_covariate_balance(PE.results$matched.sets, covariates = c("lnAg", "lnExtInd", "lnTransInd", "lnPopDens", "lnFed", "lnSta"), data =analysis_df %>% mutate(ID = as.integer(state %>% as.factor)) %>% as.data.frame(), plot=T) 

PE.results.10percentCI <- PanelEstimate(sets = PM.results, data =analysis_df %>% mutate(ID = as.integer(state %>% as.factor)) %>% as.data.frame(), confidence.level = 0.90)

summary(PE.results.10percentCI)
plot(PE.results.10percentCI)



# DID Sant'Anna Callaway
# devtools::install_github("bcallaway11/did")
library(did)

# set seed so everything is reproducible
set.seed(1814)

analysis_df_did <- analysis_df %>% mutate(ID = as.integer(state %>% as.factor), period = as.integer(as.factor(year)), g_enact=as.integer(enactment-1988), g_enact = g_enact %>% replace_na(0))

mod <- did::att_gt(yname = "lnMun",
       tname = "period",
       idname = "ID",
       gname = "g_enact",
       #xformla = ~lnAg+lnExtInd+lnTransInd+lnPopDens+lnSta+lnFed+arpa+ama+cer+caa+mat+pan+pam,
       anticipation=1,
       control_group="notyettreated",
       data = analysis_df_did,
       
)
summary(mod)
ggdid(mod)

event_std <- did::aggte(mod, type = "dynamic")