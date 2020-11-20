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



# load data
PA_df <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/paneldataEFT-BR.csv")) # %>% # dirty hack to allow access /data in directory higher than Rproj root
 # mutate(ID = ID %>% as.factor())
  
EFT <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/EFT.csv")) %>% rename(ID=X1, legislation=`ICMS-E`, enactment=Enactment) 

# minimal constant to logarithmize 0 values
const<-min(PA_df$mun[which(PA_df$mun > 0)])*0.5
const2<-min(PA_df$sta[which(PA_df$sta > 0)])*0.5

# create full dataset
full_df <- left_join(PA_df, EFT) %>% mutate(lnMun=log(mun+const), lnSta=log(sta+const2), lnFed=log(fed), lnTot=log(tot), lnAg=log(agr), lnInd=log(ind), lnPop=log(pop), lnInc=log(inc), year=as.integer(year), legislation=as.integer(legislation), enactment=as.integer(enactment))


# plot
full_df %>%  ggpubr::ggboxplot(y="lnMun", x= "icms_e", color="icms_e", add = "jitter") + ggpubr::stat_compare_means(method = "t.test")

# get summary stats
full_df %>% group_by(icms_e) %>%
  summarize(mean = mean(mun),
            median = median(mun),
            sd = sd(mun))    


# run the event study
# enactment
event_study_reg_EFTenact <- ES(long_data=full_df %>% data.table::setDT(), outcomevar="lnMun",
               unit_var="ID", cal_time_var="year",
               onset_time_var="enactment", cluster_vars="ID",
               discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
               cont_covars = c('lnAg','lnInd', 'lnPop', 'lnInc', 'lnSta', 'lnFed'),
               never_treat_action = "keep",
               # linearize_pretrends = TRUE,
               anticipation=0,
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


