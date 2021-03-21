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


# load municipal data
load(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "data/municipal/muni_merged.rda"))
data <- data %>% rename("ID"= "Sigla")

# get EFT legislation data
EFT <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/EFT.csv")) %>% rename(ID=X1, legislation=`ICMS-E`, enactment=Enactment) 

# get municipal area data
muns <- geobr::read_municipality(code_muni="all", year=2018) 
muns <- muns %>% mutate(area=sf::st_area(muns)) 
muns_area <- bind_cols(Codigo=muns$code_muni, area= muns$area %>% as.numeric)

# get state level data, i.e. for regional dummies.
load("C:\\Users\\Nils\\Box\\papers\\work_in_progress\\Cooperman_etal_2021_EqualityEFT\\repo\\data\\completed_sets\\State_Panel_data.RData")

# merge
full_df <- left_join(data, EFT) %>% left_join(muns_area) %>% mutate(
  federal_protected_area_share = (Federal_P_Area / area) * 100,
  state_protected_area_share = (State_P_Area / area) * 100,
  municipal_protected_area_share = (Muni_P_Area / area) * 100,
  federal_sustainableuse_area_share = (Federal_SUU_Area / area) * 100,
  state_sustainableuse_area_share = (State_SUU_Area / area) * 100,
  municipal_sustainableuse_area_share = (Muni_SUU_Area / area) * 100,
  log_federal_prot_area_share = federal_protected_area_share %>% log1p(),
  log_state_prot_area_share = state_protected_area_share %>% log1p(),
  log_muni_prot_area_share = municipal_protected_area_share %>% log1p(),
  log_federal_sustain_area_share = federal_sustainableuse_area_share %>% log1p(),
  log_state_sustain_area_share = state_sustainableuse_area_share %>% log1p(),
  log_muni_sustain_area_share = municipal_sustainableuse_area_share %>% log1p(),
  ) %>% left_join(complete_df %>% select(ID=state, arpa, ama, cer, caa, mat, pan, pam) %>% unique()) %>% 
  mutate(Year=Year %>% as.integer(), enactment = enactment %>% as.integer(), legislation = legislation %>% as.integer())


# run the event study
# enactment on mun protected
event_study_muni_prot_EFTenact <- ES(long_data=full_df %>% data.table::setDT(), outcomevar="log_muni_prot_area_share",
                               unit_var="municipality.merge", cal_time_var="Year",
                               onset_time_var="enactment", cluster_vars="ID",
                               discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
                               cont_covars = c('HarvestedArea',
                                               'PopulationEstimates', 
                                               'MunicipalTaxesRevenue', 
                                               'log_federal_sustain_area_share', 
                                               'log_state_sustain_area_share'),
                               never_treat_action = "keep",
                               # linearize_pretrends = TRUE,
                               anticipation=1,
                               omitted_event_time = -3,
                               # min_control_gap = 3, max_control_gap = 5,
                               residualize_covariates = TRUE
                               
)


ES_plot_levels(event_study_muni_prot_EFTenact, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")

ES_plot_ATTs(event_study_muni_prot_EFTenact, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")

ES_plot_ATTs(event_study_muni_prot_EFTenact, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")

# enactment on mun protected
event_study_muni_sust_EFTenact <- ES(long_data=full_df %>% data.table::setDT(), outcomevar="log_muni_sustain_area_share",
                                     unit_var="municipality.merge", cal_time_var="Year",
                                     onset_time_var="enactment", cluster_vars="ID",
                                     discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
                                     cont_covars = c('HarvestedArea',
                                                     'PopulationEstimates', 
                                                     'MunicipalTaxesRevenue', 
                                                     'log_federal_sustain_area_share', 
                                                     'log_state_sustain_area_share'),
                                     never_treat_action = "keep",
                                     # linearize_pretrends = TRUE,
                                     anticipation=1,
                                     omitted_event_time = -3,
                                     # min_control_gap = 3, max_control_gap = 5,
                                     residualize_covariates = TRUE
                                     
)

ES_plot_levels(event_study_muni_sust_EFTenact, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")

ES_plot_ATTs(event_study_muni_sust_EFTenact, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")

ES_plot_ATTs(event_study_muni_sust_EFTenact, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")


# classical

full_df <- full_df %>% mutate(eft_enact=case_when(Year < enactment ~ 0, is.na(enactment) ~ 0, Year >= enactment ~ 1), eft_legis=case_when(Year < legislation ~ 0, legislation %>% is.na() ~ 0, Year >= legislation ~ 1))

full_df %>%  ggpubr::ggboxplot(y="log_muni_sustain_area_share", x= "eft_enact", color="eft_enact", add = "jitter") + ggpubr::stat_compare_means(method = "t.test")


#fixed effects regression (sustainable use area)

# m0.mun.fix <- fixest::feols(log_muni_sustain_area_share ~ eft_enact + HarvestedArea+MunicipalTaxesRevenue+PopulationEstimates+log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share | ID + Year + arpa+ama+cer+caa+mat+pan+pam, data=full_df)
# summary(m0.mun.fix, se="fourway")
# summary(fixest::fixef(m0.mun.fix))
# 
# m1.mun.fix <- fixest::feols(log_muni_sustain_area_share ~ eft_enact  + HarvestedArea+MunicipalTaxesRevenue+PopulationEstimates+log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam, data=full_df)
# summary(m1.mun.fix, se="fourway")
# summary(fixest::fixef(m1.mun.fix))
# 
# # m2.mun.fix <- fixest::feglm(log_muni_sustain_area_share ~ eft_enact + HarvestedArea+MunicipalTaxesRevenue+PopulationEstimates+log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share | as.factor(Codigo) + ID + Year + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=full_df)
# # summary(m2.mun.fix, se="fourway")
# # summary(fixest::fixef(m1.mun.fix))

m3.mun.fix <- fixest::feglm(municipal_sustainableuse_area_share ~ eft_enact + HarvestedArea + MunicipalTaxesRevenue+PopulationEstimates+log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share| as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=full_df)
summary(m3.mun.fix, se="fourway")
summary(fixest::fixef(m3.mun.fix))

m3a.mun.fix_reduc <- fixest::feglm(municipal_sustainableuse_area_share ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=full_df)
summary(m3a.mun.fix_reduc, se="fourway")
summary(fixest::fixef(m3a.mun.fix_reduc))

m3b.mun.fix_red <- fixest::feglm(municipal_sustainableuse_area_share ~ eft_enact + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | as.factor(Codigo) + ID + Year + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=full_df)
summary(m3b.mun.fix_reduc, se="fourway")
summary(fixest::fixef(m3b.mun.fix_reduc))

m4.mun.fix <- fixest::feglm(municipal_sustainableuse_area_share ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share| as.factor(Codigo) + ID^Year + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=full_df) # not working because of collinearity
summary(m4.mun.fix, se="fourway")
summary(fixest::fixef(m4.mun.fix))





#fixed effects regression (strict protection area)

m1.mun.fix_prot <- fixest::feglm(municipal_protected_area_share ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=full_df)
summary(m1.mun.fix_prot, se="fourway")
summary(fixest::fixef(m1.mun.fix_prot))

m2.mun.fix_prot <- fixest::feglm(municipal_protected_area_share ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share | as.factor(Codigo) + ID + Year + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=full_df)
summary(m2.mun.fix_prot, se="fourway")
summary(fixest::fixef(m2.mun.fix_prot))




# # Another trial of panel match
# 
# library(PanelMatch)
# 
# DisplayTreatment(unit.id = "MunID",
#                  time.id = "Year", legend.position = "none",
#                  xlab = "year", ylab = "Country Code",
#                  treatment = "eft_enact", data = full_df %>% mutate(MunID = Codigo %>% as.integer) %>% as.data.frame()
# )
# 
# PM.results <- PanelMatch(lag = 1, time.id = "Year", unit.id = "Codigo", 
#                          treatment = "eft_enact", refinement.method = "mahalanobis", 
#                          data = full_df %>% mutate(MunID = Codigo %>% as.integer) %>% as.data.frame(),
#                          match.missing = TRUE, covs.formula = ~ log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share+arpa+ama+cer+caa+mat+pan+pam, 
#                          size.match = 5, qoi = "att" ,outcome.var = "log_muni_sustain_area_share",
#                          lead = 0:10, forbid.treatment.reversal = FALSE
# )
# 
# PE.results <- PanelEstimate(sets = PM.results, data =  full_df %>% mutate(MunID = Codigo %>% as.integer) %>% as.data.frame(), confidence.level = 0.95)
# 
# summary(PE.results)
# plot(PE.results)



# major alignment

load(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/df_annual_mayors.RData"))

df_PA_major <- left_join(full_df, df_annual_mayors %>% select(1,5:ncol(df_annual_mayors))%>% rename(Codigo=IBGE_code, Year=year) %>% mutate(Codigo = Codigo %>% as.numeric()) %>% as_tibble())

# sustainable use areas
m3a.mun.fix_reduc_party <- fixest::feglm(municipal_sustainableuse_area_share ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=df_PA_major) # changes if Harvested Area is e.g. controlled for such that we have a larger data set.
summary(m3a.mun.fix_reduc_party, se="fourway")

m3a.mun.fix_reduc_party_interact <- fixest::feglm(municipal_sustainableuse_area_share ~  eft_enact*maygov_sameparty + eft_enact*maygov_samecoalition + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam, family = "poisson", data=df_PA_major) # changes if Harvested Area is e.g. controlled for such that we have a larger data set.
summary(m3a.mun.fix_reduc_party_interact, se="fourway")


# prot area
m2.mun.fix_prot_party <- fixest::feglm(municipal_protected_area_share ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=df_PA_major)
summary(m2.mun.fix_prot_party, se="fourway") # changes if Harvested Area is e.g. controlled for such that we have a larger data set.

m2.mun.fix_prot_party_interact <- fixest::feglm(municipal_protected_area_share ~ eft_enact*maygov_sameparty + eft_enact*maygov_samecoalition + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=df_PA_major)
summary(m2.mun.fix_prot_party_interact, se="fourway") # changes if Harvested Area is e.g. controlled for such that we have a larger data set.

m2.mun.fix_prot_party_interact_threeway <- fixest::feglm(municipal_protected_area_share ~ eft_enact*maygov_sameparty*maygov_samecoalition  + HarvestedArea   + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "poisson", data=df_PA_major)
summary(m2.mun.fix_prot_party_interact_threeway, se="fourway") # changes if Harvested Area is e.g. controlled for such that we have a larger data set.



# poverty / equality

m2.mun.fix_reveneue_party_interact <- fixest::feols(MunicipalTaxesRevenue ~ eft_enact*maygov_sameparty + eft_enact*maygov_samecoalition  + HarvestedArea  + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,data=df_PA_major)
summary(m2.mun.fix_reveneue_party_interact, se="fourway") 

# create variable poor
df_PA_major <- df_PA_major %>% group_by(ID,Year) %>% mutate_at(vars(MunicipalTaxesRevenue), list(Poor = function(x) cut(percent_rank(x), c(-Inf,.25,Inf), labels = c(0,1)))) %>% ungroup()

m2.mun.fix_reveneue_party_poor <- fixest::feols(MunicipalTaxesRevenue ~ eft_enact*Poor + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam, data=df_PA_major)
summary(m2.mun.fix_reveneue_party_poor, se="fourway") 

m2.mun.fix_reveneue_party_interact_poor <- fixest::feols(MunicipalTaxesRevenue ~ eft_enact*maygov_sameparty*maygov_samecoalition*Poor + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam, data=df_PA_major)
summary(m2.mun.fix_reveneue_party_interact_poor, se="fourway") 

