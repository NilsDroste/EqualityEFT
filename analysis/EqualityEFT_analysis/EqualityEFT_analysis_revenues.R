################################
# Analysis of PA develeopment in
# Brazilian Ecological Fiscal Ts
# script authors: nils@droste.io
#                 ...
################################

library(tidyverse)
library(here)
library(eventStudy)
library(ggpubr)
library(reldist)

# load data
revenue_df <- readxl::read_xls(
  paste0(
    here() %>% str_remove("analysis/EqualityEFT_analysis"),
    "/data/raw/financas_publicas/ipeadata2020-11-18.xls"
  )
) %>% pivot_longer(
  cols = c(-`Sigla`,-`Codigo`,-`Município`),
  names_to = "year",
  values_to = "revenue"
) %>%  rename(state = Sigla,
              code = Codigo,
              municipality = Município) %>% 
  mutate(state = state %>% as.factor(),
         municipality = municipality %>% as.factor(),
         year = year %>% as.numeric())


# compute gini coefficient per year and state
gini_state_df <- aggregate(revenue ~ state + year,
                        data = revenue_df,
                        FUN = "gini") %>% rename(gini=revenue) %>% as_tibble() %>% mutate(gini = replace(gini, gini<0.5, NA)) # throwing out RR 1997, where only one record was found and thus gini 0, also throwing out AP 1998 and 1999, where only two entries where found and gini thus <.5
                                                                                      
# moar data
PA_df <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/paneldataEFT-BR.csv")) %>% rename(state=state) 
EFT <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/EFT.csv")) %>% rename(state=X1, legislation=`ICMS-E`, enactment=Enactment) 

# minimal constant to logarithmize 0 values
const<-min(PA_df$mun[which(PA_df$mun > 0)])*0.5
const2<-min(PA_df$sta[which(PA_df$sta > 0)])*0.5

# create full datasets
full_df <- left_join(PA_df, EFT) %>% mutate(lnMun=log(mun+const), lnSta=log(sta+const2), lnFed=log(fed), lnTot=log(tot), lnAg=log(agr), lnInd=log(ind), lnPop=log(pop), lnInc=log(inc), year=as.integer(year), legislation=as.integer(legislation), enactment=as.integer(enactment)) 

# including gini
full_gini_df <- full_df %>% left_join(gini_state_df) %>% mutate(year = year %>% as.integer(), lnGini = log(gini))

#plot
full_gini_df %>%  ggpubr::ggboxplot(y="gini", x= "icms_e", color="icms_e", add = "jitter") + ggpubr::stat_compare_means(method = "t.test")
full_gini_df %>%  ggpubr::ggboxplot(y="lnGini", x= "icms_e", color="icms_e", add = "jitter") + ggpubr::stat_compare_means(method = "t.test")

  # get summary stats
full_gini_df %>% group_by(icms_e) %>%
  summarize(mean = mean(gini, na.rm = TRUE),
            median = median(gini, na.rm = TRUE),
            sd = sd(gini, na.rm = TRUE))    
# get summary stats
full_gini_df %>% group_by(icms_e) %>%
  summarize(mean = mean(lnGini, na.rm = TRUE),
            median = median(lnGini, na.rm = TRUE),
            sd = sd(lnGini, na.rm = TRUE))    

# run the event study
# enactment
event_study_reg_gini_enact <- ES(long_data=full_gini_df %>% data.table::setDT(), outcomevar="lnGini",
                       unit_var="state", cal_time_var="year",
                       onset_time_var="enactment", cluster_vars="state",
                       discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
                       cont_covars = c('lnAg','lnInd', 'lnPop', 'lnInc', 'lnSta', 'lnFed'),
                       never_treat_action = "keep",
                       #linearize_pretrends = TRUE,
                       anticipation=0,
                       omitted_event_time = -1,
                       # min_control_gap = 3, max_control_gap = 5,
                       resstateualize_covariates = TRUE
                       
)

ES_plot_levels(event_study_reg_gini_enact, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")

ES_plot_ATTs(event_study_reg_gini_enact, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")

ES_plot_ATTs(event_study_reg_gini_enact, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")


event_study_reg_gini_legis <- ES(long_data=full_df %>% data.table::setDT(), outcomevar="lnGini",
                           unit_var="state", cal_time_var="year",
                           onset_time_var="legislation", cluster_vars="state",
                           discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
                           cont_covars = c('lnAg','lnInd', 'lnPop', 'lnInc', 'lnSta', 'lnFed'),
                           # never_treat_action = "keep",
                           linearize_pretrends = TRUE,
                           anticipation=0,
                           # omitted_event_time = -2,
                           # min_control_gap = 3, max_control_gap = 5,
                           residualize_covariates = TRUE
                           
)

ES_plot_levels(event_study_reg_gini_legis, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")

ES_plot_ATTs(event_study_reg_gini_legis, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")

ES_plot_ATTs(event_study_reg_gini_legis, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")



