################################
# Analysis of PA develeopment in
# Brazilian Ecological Fiscal Ts
# script authors: nils@droste.io
#                 ...
################################


library(tidyverse)
library(lubridate)
library(here)
# library(eventStudy)
library(ggpubr)
library(sf)


# load municipal data
load(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "data/municipal/muni_merged.rda"))
data <- data %>% rename("ID"= "Sigla")

# get EFT legislation data
EFT <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/EFT.csv")) %>% rename(ID=X1, legislation=`ICMS-E`, enactment=Enactment) 

# get municipal area data
muns <- geobr::read_municipality(code_muni="all", simplified = F, year=2018) # currently not working
# muns <- GADMTools::gadm_sf_loadCountries("BRA", level=3, basefile="./")
# muns <-  readRDS(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "data/spatial/BRA/gadm36_BRA_2_sf.rds"))
# muns <- st_read(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "data/spatial/BRA/municipios/municipio5564.shp")) # from https://www.ipea.gov.br/ipeageo/malhas.html
muns <- muns %>% mutate(area=sf::st_area(muns)) 
# save(muns, file=paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "data/spatial/BRA/municipios/municipal_spatial_data.RData"))
# muns_area <- bind_cols(Codigo=muns$CODIGO_MUN, area= muns$area %>% as.numeric)
muns_area <- bind_cols(Codigo=muns$code_muni, area= muns$area %>% as.numeric)
rm(muns)

# get state level data, i.e. for regional dummies.
load("C:\\Users\\Nils\\Box\\papers\\work_in_progress\\Cooperman_etal_2021_EqualityEFT\\repo\\data\\completed_sets\\State_Panel_data.RData")

# merge
full_df <- left_join(data, EFT) %>% mutate(Codigo= Codigo %>% as.numeric()) %>% left_join(muns_area) %>% mutate(
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
  mutate(Year=Year %>% as.integer(), enactment = enactment %>% as.integer(), legislation = legislation %>% as.integer()) %>% ungroup()

# Codigo= substr(Codigo,1,6) %>% as.numeric() when the shp file is used.

# check also geobr::read_biomes() for a more precise calculation of biome overlap with municipalities
# biomes <-  geobr::read_biomes(year = 2019, simplified = T)

# run the event study
# enactment on mun protected
# event_study_muni_prot_EFTenact <- ES(long_data=full_df %>% data.table::setDT(), outcomevar="log_muni_prot_area_share",
#                                unit_var="municipality.merge", cal_time_var="Year",
#                                onset_time_var="enactment", cluster_vars="ID",
#                                discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
#                                cont_covars = c('HarvestedArea',
#                                                'PopulationEstimates', 
#                                                'MunicipalTaxesRevenue', 
#                                                'log_federal_sustain_area_share', 
#                                                'log_state_sustain_area_share'),
#                                never_treat_action = "keep",
#                                # linearize_pretrends = TRUE,
#                                anticipation=1,
#                                omitted_event_time = -3,
#                                # min_control_gap = 3, max_control_gap = 5,
#                                residualize_covariates = TRUE
#                                
# )
# 
# 
# ES_plot_levels(event_study_muni_prot_EFTenact, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")
# 
# ES_plot_ATTs(event_study_muni_prot_EFTenact, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")
# 
# ES_plot_ATTs(event_study_muni_prot_EFTenact, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")
# 
# # enactment on mun sustainable
# event_study_muni_sust_EFTenact <- ES(long_data=full_df %>% data.table::setDT(), outcomevar="log_muni_sustain_area_share",
#                                      unit_var="municipality.merge", cal_time_var="Year",
#                                      onset_time_var="enactment", cluster_vars="ID",
#                                      discrete_covars = c('arpa','ama', 'cer', 'caa', "mat", "pan", "pam"),
#                                      cont_covars = c('HarvestedArea',
#                                                      'PopulationEstimates', 
#                                                      'MunicipalTaxesRevenue', 
#                                                      'log_federal_sustain_area_share', 
#                                                      'log_state_sustain_area_share'),
#                                      never_treat_action = "keep",
#                                      # linearize_pretrends = TRUE,
#                                      anticipation=1,
#                                      omitted_event_time = -3,
#                                      # min_control_gap = 3, max_control_gap = 5,
#                                      residualize_covariates = TRUE
#                                      
# )
# 
# ES_plot_levels(event_study_muni_sust_EFTenact, lower_event = -3, upper_event = 10) + ylab("Mean of the Outcome")
# 
# ES_plot_ATTs(event_study_muni_sust_EFTenact, lower_event = -3, upper_event = 10) + ylab("ATT Estimate (95% CI)")
# 
# ES_plot_ATTs(event_study_muni_sust_EFTenact, lower_event = -3, upper_event = 10, homogeneous_ATT = TRUE) +   ylab("ATT Estimate (95% CI)")


# classical

full_df <- full_df %>% mutate(eft_enact=case_when(Year < enactment ~ 0, is.na(enactment) ~ 0, Year >= enactment ~ 1), eft_legis=case_when(Year < legislation ~ 0, legislation %>% is.na() ~ 0, Year >= legislation ~ 1))

full_df %>%  ggpubr::ggboxplot(y="log_muni_sustain_area_share", x= "eft_enact", color="eft_enact", add = "jitter") + ggpubr::stat_compare_means(method = "t.test")

full_df$municipal_sustainableuse_area_share %>% hist
df_PA_major$log_muni_sustain_area_share %>% hist

# I would argue for either a poisson (log link) or a quasibinomial (logit link) function.


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

m3.mun.fix <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ eft_enact + HarvestedArea + MunicipalTaxesRevenue+PopulationEstimates+log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share| as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam, family = "quasibinomial", data=full_df)
summary(m3.mun.fix, se="fourway")
# summary(fixest::fixef(m3.mun.fix))

m3a.mun.fix_reduc <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "quasibinomial", data=full_df)
summary(m3a.mun.fix_reduc, se="fourway")
# summary(fixest::fixef(m3a.mun.fix_reduc))

m3b.mun.fix_reduc <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ eft_enact  + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | as.factor(Codigo) + ID + Year + arpa+ama+cer+caa+mat+pan+pam,family = "quasibinomial", data=full_df)
summary(m3b.mun.fix_reduc, se="fourway")
# summary(fixest::fixef(m3b.mun.fix_reduc))

m4.mun.fix <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share| as.factor(Codigo) + ID^Year + arpa+ama+cer+caa+mat+pan+pam,family = "quasibinomial", data=full_df) # not working because of collinearity
summary(m4.mun.fix, se="fourway")
summary(fixest::fixef(m4.mun.fix))





#fixed effects regression (strict protection area)

m1.mun.fix_prot <- fixest::feglm(municipal_protected_area_share/100 ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "quasibinomial", data=full_df)
summary(m1.mun.fix_prot, se="fourway")
# summary(fixest::fixef(m1.mun.fix_prot))

m2.mun.fix_prot <- fixest::feglm(municipal_protected_area_share/100 ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share | as.factor(Codigo) + ID + Year + arpa+ama+cer+caa+mat+pan+pam,family = "quasibinomial", data=full_df)
summary(m2.mun.fix_prot, se="fourway")
# summary(fixest::fixef(m2.mun.fix_prot))




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

# Codigo = substr(Codigo,1,6) %>% as.numeric()

# sustainable use areas
m3a.mun.fix_reduc_party <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "quasibinomial", data=df_PA_major) # changes if Harvested Area is e.g. controlled for such that we have a larger data set.
summary(m3a.mun.fix_reduc_party, se="fourway")

m3a.mun.fix_reduc_party_interact <- fixest::feglm(municipal_sustainableuse_area_share/100 ~  eft_enact*maygov_sameparty + eft_enact*maygov_samecoalition + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam, family = "quasibinomial", data=df_PA_major) # changes if Harvested Area is e.g. controlled for such that we have a larger data set.
summary(m3a.mun.fix_reduc_party_interact, se="fourway")


# prot area
m2.mun.fix_prot_party <- fixest::feglm(municipal_protected_area_share/100 ~ eft_enact + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "quasibinomial", data=df_PA_major)
summary(m2.mun.fix_prot_party, se="fourway") # changes if Harvested Area is e.g. controlled for such that we have a larger data set.

m2.mun.fix_prot_party_interact <- fixest::feglm(municipal_protected_area_share/100 ~ eft_enact*maygov_sameparty + eft_enact*maygov_samecoalition + HarvestedArea + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,family = "quasibinomial", data=df_PA_major)
summary(m2.mun.fix_prot_party_interact, se="fourway") # changes if Harvested Area is e.g. controlled for such that we have a larger data set.

m2.mun.fix_prot_party_interact_threeway <- fixest::feglm(municipal_protected_area_share/100 ~ eft_enact*maygov_sameparty*maygov_samecoalition  + HarvestedArea  + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam, family = "quasibinomial", data=df_PA_major)
summary(m2.mun.fix_prot_party_interact_threeway, se="fourway") # changes if Harvested Area is e.g. controlled for such that we have a larger data set.



# poverty / equality
df_PA_major$MunicipalTaxesRevenue %>% hist
df_PA_major$MunicipalTaxesRevenue %>% log1p %>% hist

# This kind of distribution would normally be a case for a tobit or some other left censored regression model, e.g. zero-inflated or hurdle, I think. This could possibly be implemented with a glm model à la https://drizopoulos.github.io/GLMMadaptive/. Such models have confusing terminology when it comes to fixed and random effects, i.e. the latter being varying slopes and the former a common slope but varying intercepts, see e.g. https://stats.stackexchange.com/questions/238214/how-exactly-does-a-random-effects-model-in-econometrics-relate-to-mixed-models?noredirect=1&lq=1 and https://peerj.com/articles/4794/. However, even considering the differences, the mixed effects model procedure (maximum likelihood estimation) can reproduce the classical fixed effects regression by incorporation of a group wise mean as an explanatory variable (and thus providing the demeaning of a fixed effects model).
# see also https://stats.stackexchange.com/questions/474611/zero-inflated-negative-binomial-models-for-panel-data

# library(GLMMadaptive) # only allows one grouping factor.
install.packages("glmmTMB")

# create variable poor
df_PA_major <- df_PA_major %>% group_by(ID,Year) %>% mutate_at(vars(MunicipalTaxesRevenue), list(Poor = function(x) cut(percent_rank(x), c(-Inf,.25,Inf), labels = c(0,1)))) %>% ungroup()

# create average value
df_PA_major <- df_PA_major %>% group_by(Codigo) %>% mutate(MunTaxRevAvg = mean(MunicipalTaxesRevenue, na.rm = T)) %>% ungroup()

# creating log of MunicipalTaxrevenue
df_PA_major <-  df_PA_major %>% mutate(LogMunTaxRev = log1p(MunicipalTaxesRevenue))


# does not work yet
m2.mun.fix_reveneue_party_interact_zigamma <- glmmTMB::glmmTMB(as.integer(MunicipalTaxesRevenue) ~ eft_enact*maygov_sameparty + eft_enact*maygov_samecoalition  + HarvestedArea  + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + MunTaxRevAvg + 1 | as.factor(Codigo) +  1|ID +  1|arpa + 1|ama + 1|cer +  1|caa +  1|mat +  1|pan + 1|pam + 1| ID:Year, family = glmmTMB::truncated_genpois(), ziformula = ~ MunicipalTaxesRevenue, data=df_PA_major )
summary(m2.mun.fix_reveneue_party_interact) 




# the fixed effects approach (not taking the zero-inflation )

m2.mun.fix_reveneue_party_poor <- fixest::feglm(MunicipalTaxesRevenue ~ eft_enact*Poor + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam, family = "poisson", data=df_PA_major)
summary(m2.mun.fix_reveneue_party_poor, se="fourway") 

m2.mun.fix_reveneue_party_interact_poor <- fixest::feglm(MunicipalTaxesRevenue ~ eft_enact*maygov_sameparty*maygov_samecoalition*Poor + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa+ama+cer+caa+mat+pan+pam,  family = "poisson", data=df_PA_major)
summary(m2.mun.fix_reveneue_party_interact_poor, se="fourway") 



# two-way fixed-effects yearly treatment effect
m3a.mun.fix_reduc_party_2wfe <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ eft_enact + i(treated, Year, ref=1991) +log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share| Codigo + ID[Year] ,family = "quasibinomial", data=df_PA_major[df_PA_major$Year>1989,]) 
summary(m3a.mun.fix_reduc_party_2wfe)
coefplot(m3a.mun.fix_reduc_party_2wfe)

# m3a.mun.fix_reduc_party_2wfe_party <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ eft_enact*maygov_samecoalition + i(treated, Year, ref=1997) +log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_prot_area_share| Codigo + ID[Year] ,family = "quasibinomial", data=df_PA_major[df_PA_major$Year>1989,]) 
# summary(m3a.mun.fix_reduc_party_2wfe_party)
# coefplot(m3a.mun.fix_reduc_party_2wfe_party)



# the Sun and Abraham approach (staggered treatmetn)
df_PA_major <- df_PA_major %>% mutate(time_to_treat = replace_na(Year - enactment, -1000), MunYear = paste0(Codigo,":",Year), treated=case_when(!is.na(enactment) ~ 1, is.na(enactment) ~0), post = replace_na(case_when(Year>=enactment ~1, Year<enactment ~ 0), 0), eft_mayor_treatment = post * maygov_sameparty) 
# Note that the time_to_treatment for controls is set to -1000

# create variable of first alignement year of mayor and state gov when EFT are implemented.
Aligned <- df_PA_major %>% filter(eft_mayor_treatment > 0) %>% 
  group_by(Codigo) %>% 
  summarize(aligned_treat_year = min(Year))
df_PA_major <- df_PA_major %>% left_join(Aligned) %>% mutate( time_to_aligned_treat = replace_na( Year - aligned_treat_year, -1000))
# Note that the time_to_treatment for controls is set to -1000


# "Regular" DiD
m3a.mun.fix_reduc_party_regDID <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(treated, time_to_treat, ref = 0, drop = -1000)| Codigo+ ID + Year ,family = "quasibinomial", data=df_PA_major) 
summary(m3a.mun.fix_reduc_party_regDID)

# sustainable use areas
m3a.mun.fix_reduc_party_SunAbr <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(0, -1000))| Codigo + ID + Year ,family = "quasibinomial", data=df_PA_major) 
summary(m3a.mun.fix_reduc_party_SunAbr)

fixest::coefplot(m3a.mun.fix_reduc_party_regDID, xlim=c(-20,25), ylim=c(-10,5))

agg_coef = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x = c(-24:-1, 1:25) + .1
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)


m3a.mun.fix_reduc_party_SunAbr_covariate <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | Codigo + ID + Year ,family = "quasibinomial", data=df_PA_major) 
# summary(m3a.mun.fix_reduc_party_SunAbr_covariate)

agg_coef2 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_covariate, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x2 = c(-24:-1, 1:25) + .2
points(x2, agg_coef2[, 1], pch = 17, col = 6)
ci_low2 = agg_coef2[, 1] - 1.96 * agg_coef2[, 2]
ci_up2 = agg_coef2[, 1] + 1.96 * agg_coef2[, 2]
segments(x0 = x2, y0 = ci_low2, x1 = x2, y1 = ci_up2, col = 6)


# m3a.mun.fix_reduc_party_SunAbr_covariate_less <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | Codigo + ID[Year],family = "quasibinomial", data=df_PA_major) 
# # summary(m3a.mun.fix_reduc_party_SunAbr_covariate)
# 
# agg_coef3 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_covariate_less, "(ti.*at)::(-?[0-9]?[0-9]?)") 
# x3 = c(-29:-2, 0:24) + .3
# points(x3, agg_coef3[, 1], pch = 17, col = 8)
# ci_low3 = agg_coef3[, 1] - 1.96 * agg_coef3[, 2]
# ci_up3 = agg_coef3[, 1] + 1.96 * agg_coef3[, 2]
# segments(x0 = x3, y0 = ci_low3, x1 = x3, y1 = ci_up3, col = 8)

# TODO remove the always treated

m3a.mun.fix_reduc_party_SunAbr_covariate_party <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + maygov_sameparty + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | Codigo + ID + Year,family = "quasibinomial", data=df_PA_major) 
m3a.mun.fix_reduc_party_SunAbr_covariate_party$coeftable

agg_coef4 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_covariate_party, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x4 = c(-17:-1, 1:23) + .3
points(x4, agg_coef4[, 1], pch = 17, col = 2)
ci_low4 = agg_coef4[, 1] - 1.96 * agg_coef4[, 2]
ci_up4 = agg_coef4[, 1] + 1.96 * agg_coef4[, 2]
segments(x0 = x4, y0 = ci_low4, x1 = x4, y1 = ci_up4, col = 2)


m3a.mun.fix_reduc_party_SunAbr_covariate_party_alignedtreat <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(time_to_aligned_treat, f2 = ID, drop = c(-1, -1000)) + eft_enact + maygov_sameparty + maygov_samecoalition + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | Codigo + ID + Year,family = "quasibinomial", data=df_PA_major) 
m3a.mun.fix_reduc_party_SunAbr_covariate_party_alignedtreat %>% fixest::etable()

agg_coef5 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_covariate_party_alignedtreat, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x5 = c(-17:-1, 1:19) + .4
points(x5, agg_coef5[, 1], pch = 17, col = 3)
ci_low5 = agg_coef5[, 1] - 1.96 * agg_coef5[, 2]
ci_up5 = agg_coef5[, 1] + 1.96 * agg_coef5[, 2]
segments(x0 = x5, y0 = ci_low5, x1 = x5, y1 = ci_up5, col = 3)


# lagged variable

# "Regular" DiD
m3a.mun.fix_reduc_party_regDID_lagged <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(treated, time_to_treat, ref = 0, drop = -1000) + l(municipal_sustainableuse_area_share/100, 1:3)| Codigo + ID + Year ,family = "quasibinomial", data=df_PA_major, panel.id = c("Codigo","Year")) 
# summary(m3a.mun.fix_reduc_party_regDID_lagged)

# sustainable use areas
m3a.mun.fix_reduc_party_SunAbr_lagged <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(municipal_sustainableuse_area_share/100, 1:3) | Codigo + ID + Year  ,family = "quasibinomial", data=df_PA_major,panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged %>% fixest::etable()

fixest::coefplot(m3a.mun.fix_reduc_party_regDID_lagged, xlim=c(-26,24), ylim=c(-10,5))

agg_coef = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x = c(-24:-1, 1:25) + .1
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)


m3a.mun.fix_reduc_party_SunAbr_lagged_covariates <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(municipal_sustainableuse_area_share/100, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year  ,family = "quasibinomial", data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged %>% fixest::etable()

agg_coef2 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged_covariates, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x2 = c(-24:-1, 1:25) + .2
points(x2, agg_coef2[, 1], pch = 17, col = 6)
ci_low2 = agg_coef2[, 1] - 1.96 * agg_coef2[, 2]
ci_up2 = agg_coef2[, 1] + 1.96 * agg_coef2[, 2]
segments(x0 = x2, y0 = ci_low2, x1 = x2, y1 = ci_up2, col = 6)

m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party <- fixest::feglm(municipal_sustainableuse_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + maygov_sameparty + l(municipal_sustainableuse_area_share/100, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year,family = "quasibinomial", data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party %>% fixest::etable()

agg_coef4 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x4 = c(-17:-1, 1:23) + .3
points(x4, agg_coef4[, 1], pch = 17, col = 2)
ci_low4 = agg_coef4[, 1] - 1.96 * agg_coef4[, 2]
ci_up4 = agg_coef4[, 1] + 1.96 * agg_coef4[, 2]
segments(x0 = x4, y0 = ci_low4, x1 = x4, y1 = ci_up4, col = 2)


# protected areas

# TODO why is there a protected area share over 100
df_PA_major %>% select(Codigo, Year, municipal_protected_area_share) %>% filter(municipal_protected_area_share>100)
df_PA_major %>% filter(Codigo ==  5007976, Year == 2017)
df_PA_major[df_PA_major$Codigo ==  5007976 & df_PA_major$Year == 2017,]$municipal_protected_area_share <- 100

# "Regular" DiD
m3a.mun.fix_reduc_party_regDID_lagged_prot <- fixest::feglm(municipal_protected_area_share/100 ~ i(treated, time_to_treat, ref = 0, drop = -1000) + l(municipal_protected_area_share/100, 1:3)| Codigo + ID + Year ,family = "quasibinomial", data=df_PA_major, panel.id = c("Codigo","Year")) 
# summary(m3a.mun.fix_reduc_party_regDID_lagged)

m3a.mun.fix_reduc_party_SunAbr_lagged_prot <- fixest::feglm(municipal_protected_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(municipal_protected_area_share/100, 1:3) | Codigo + ID + Year  ,family = "quasibinomial", data=df_PA_major,panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged %>% fixest::etable()

fixest::coefplot(m3a.mun.fix_reduc_party_regDID_lagged_prot)

agg_coef = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x = c(-24:-1, 1:25) + .1
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)


m3a.mun.fix_reduc_party_SunAbr_lagged_covariates_prot <- fixest::feglm(municipal_protected_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(municipal_protected_area_share/100, 1:3) + l(municipal_sustainableuse_area_share/100, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year  ,family = "quasibinomial", data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged_covariates_prot %>% fixest::etable()

agg_coef2 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged_covariates_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x2 = c(-24:-1, 1:25) + .2
points(x2, agg_coef2[, 1], pch = 17, col = 6)
ci_low2 = agg_coef2[, 1] - 1.96 * agg_coef2[, 2]
ci_up2 = agg_coef2[, 1] + 1.96 * agg_coef2[, 2]
segments(x0 = x2, y0 = ci_low2, x1 = x2, y1 = ci_up2, col = 6)

m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party_prot <- fixest::feglm(municipal_protected_area_share/100 ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + maygov_sameparty + l(municipal_protected_area_share/100, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year,family = "quasibinomial", data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party_prot %>% fixest::etable()

agg_coef4 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x4 = c(-17:-1, 1:23) + .3
points(x4, agg_coef4[, 1], pch = 17, col = 2)
ci_low4 = agg_coef4[, 1] - 1.96 * agg_coef4[, 2]
ci_up4 = agg_coef4[, 1] + 1.96 * agg_coef4[, 2]
segments(x0 = x4, y0 = ci_low4, x1 = x4, y1 = ci_up4, col = 2)







# computing first difference as dependent variable

df_PA_major <- df_PA_major %>% mutate(first_diff_prot = Muni_P_Area - lag(Muni_P_Area), first_diff_sust = Muni_SUU_Area - lag(Muni_SUU_Area))

df_PA_major$first_diff_sust %>% hist(breaks=1000)
df_PA_major$first_diff_sust %>% density(na.rm=T) %>% plot()
df_PA_major$first_diff_sust %>% summary

#regular 2wDID
m3a.mun.fix_reduc_party_regDID_fd <- fixest::feols(first_diff_sust ~ i(treated, time_to_treat, ref = 0, drop = -1000)| Codigo+ ID[Year], data=df_PA_major) 
summary(m3a.mun.fix_reduc_party_regDID_fd)
fixest::coefplot(m3a.mun.fix_reduc_party_regDID_fd, xlim=c(-29,25))

m3a.mun.fix_reduc_party_SunAbr_covariate_party_fd <- fixest::feols(first_diff_sust ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + eft_enact*maygov_sameparty + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share | Codigo + ID[Year], data=df_PA_major) 
m3a.mun.fix_reduc_party_SunAbr_covariate_party_fd %>% fixest::etable()

agg_coef6 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_covariate_party_fd, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x6 = c(-17:-1, 1:23) + .4
points(x6, agg_coef6[, 1], pch = 17, col = 3)
ci_low5 = agg_coef6[, 1] - 1.96 * agg_coef6[, 2]
ci_up5 = agg_coef6[, 1] + 1.96 * agg_coef6[, 2]
segments(x0 = x5, y0 = ci_low5, x1 = x5, y1 = ci_up5, col = 3)



# plotting
require(cowplot)

# municipal
df_PA_major %>% group_by(ID, Year) %>% summarise(mean = mean(municipal_sustainableuse_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean municipal sustainable use area share") + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

df_PA_major %>% group_by(ID, Year) %>% summarise(mean = mean(municipal_protected_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean municipal protected use area share")  + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

# state
df_PA_major %>% group_by(ID, Year) %>% summarise(mean = mean(state_sustainableuse_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean state sustainable use area share") + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

df_PA_major %>% group_by(ID, Year) %>% summarise(mean = mean(state_protected_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean state protected use area share")  + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

# federal
df_PA_major %>% group_by(ID, Year) %>% summarise(mean = mean(federal_sustainableuse_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean federal sustainable use area share") + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

df_PA_major %>% group_by(ID, Year) %>% summarise(mean = mean(federal_protected_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean federal protected use area share")  + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)


# municipal

# "Regular" DiD
m3a.mun.fix_reduc_party_regDID_lagged <- fixest::feols(log_muni_sustain_area_share ~ i(treated, time_to_treat, ref = 0, drop = -1000) + l(log_muni_sustain_area_share, 1:3)| Codigo + ID + Year , data=df_PA_major, panel.id = c("Codigo","Year")) 
# summary(m3a.mun.fix_reduc_party_regDID_lagged)

# sustainable use areas
m3a.mun.fix_reduc_party_SunAbr_lagged <- fixest::feols(log_muni_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(log_muni_sustain_area_share, 1:3) | Codigo + ID + Year  , data=df_PA_major,panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged %>% fixest::etable()

fixest::coefplot(m3a.mun.fix_reduc_party_regDID_lagged)

agg_coef = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x = c(-24:-1, 1:25) + .1
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)


m3a.mun.fix_reduc_party_SunAbr_lagged_covariates <- fixest::feols(log_muni_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(log_muni_sustain_area_share, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year  , data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged %>% fixest::etable()

agg_coef2 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged_covariates, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x2 = c(-24:-1, 1:25) + .2
points(x2, agg_coef2[, 1], pch = 17, col = 6)
ci_low2 = agg_coef2[, 1] - 1.96 * agg_coef2[, 2]
ci_up2 = agg_coef2[, 1] + 1.96 * agg_coef2[, 2]
segments(x0 = x2, y0 = ci_low2, x1 = x2, y1 = ci_up2, col = 6)

m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party <- fixest::feols(log_muni_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + maygov_sameparty + l(log_muni_sustain_area_share, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year, data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party %>% fixest::etable()

agg_coef4 = stats::aggregate(m3a.mun.fix_reduc_party_SunAbr_lagged_covariate_party, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x4 = c(-17:-1, 1:23) + .3
points(x4, agg_coef4[, 1], pch = 17, col = 2)
ci_low4 = agg_coef4[, 1] - 1.96 * agg_coef4[, 2]
ci_up4 = agg_coef4[, 1] + 1.96 * agg_coef4[, 2]
segments(x0 = x4, y0 = ci_low4, x1 = x4, y1 = ci_up4, col = 2)



# state level 

df_PA_major %>% select(Codigo, Year, state_sustainableuse_area_share) %>% filter(state_sustainableuse_area_share>100) %>% View()
df_PA_major %>% select(Codigo, Year, state_protected_area_share) %>% filter(state_protected_area_share>100) %>% View()


# "Regular" DiD
m3a.state.fix_reduc_party_regDID_lagged <- fixest::feols(log_state_sustain_area_share ~ i(treated, time_to_treat, ref = 0, drop = -1000) + l(log_state_sustain_area_share, 1:3)| Codigo + ID + Year, data=df_PA_major, panel.id = c("Codigo","Year")) 
# summary(m3a.mun.fix_reduc_party_regDID_lagged)

m3a.state.fix_reduc_party_SunAbr_lagged_prot <- fixest::feols(log_state_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(log_state_sustain_area_share, 1:3) | Codigo + ID + Year, data=df_PA_major,panel.id = c("Codigo","Year")) 
m3a.state.fix_reduc_party_SunAbr_lagged_prot %>% fixest::etable()

fixest::coefplot(m3a.state.fix_reduc_party_regDID_lagged, xlim=c(-26,24))

agg_coef = stats::aggregate(m3a.state.fix_reduc_party_SunAbr_lagged_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x = c(-26:-1, 1:25) + .1
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)

m3a.state.fix_reduc_party_SunAbr_lagged_covariates_prot <- fixest::feols(log_state_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(log_muni_sustain_area_share, 0:3) + l(municipal_sustainableuse_area_share/100, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 1:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year, data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.state.fix_reduc_party_SunAbr_lagged_covariates_prot %>% fixest::etable()

agg_coef2 = stats::aggregate(m3a.state.fix_reduc_party_SunAbr_lagged_covariates_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x2 = c(-26:-1, 1:25) + .2
points(x2, agg_coef2[, 1], pch = 17, col = 6)
ci_low2 = agg_coef2[, 1] - 1.96 * agg_coef2[, 2]
ci_up2 = agg_coef2[, 1] + 1.96 * agg_coef2[, 2]
segments(x0 = x2, y0 = ci_low2, x1 = x2, y1 = ci_up2, col = 6)

m3a.state.fix_reduc_party_SunAbr_lagged_covariate_party_prot <- fixest::feols(log_state_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + maygov_sameparty + l(log_muni_sustain_area_share, 0:3) + l(municipal_sustainableuse_area_share/100, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 1:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year, data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.state.fix_reduc_party_SunAbr_lagged_covariate_party_prot %>% fixest::etable()

agg_coef4 = stats::aggregate(m3a.state.fix_reduc_party_SunAbr_lagged_covariate_party_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x4 = c(-17:-1, 1:24) + .3
points(x4, agg_coef4[, 1], pch = 17, col = 2)
ci_low4 = agg_coef4[, 1] - 1.96 * agg_coef4[, 2]
ci_up4 = agg_coef4[, 1] + 1.96 * agg_coef4[, 2]
segments(x0 = x4, y0 = ci_low4, x1 = x4, y1 = ci_up4, col = 2)


# federal level 

df_PA_major %>% select(Codigo, Year, federal_sustainableuse_area_share) %>% filter(federal_sustainableuse_area_share>100) %>% View()
df_PA_major %>% select(Codigo, Year, federal_protected_area_share) %>% filter(federal_protected_area_share>100) %>% View()


# "Regular" DiD
m3a.federal.fix_reduc_party_regDID_lagged <- fixest::feols(log_federal_sustain_area_share ~ i(treated, time_to_treat, ref = 0, drop = -1000) + l(log_federal_sustain_area_share, 1:3)| Codigo + ID + Year, data=df_PA_major, panel.id = c("Codigo","Year")) 
# summary(m3a.mun.fix_reduc_party_regDID_lagged)

m3a.federal.fix_reduc_party_SunAbr_lagged_prot <- fixest::feols(log_federal_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(log_federal_sustain_area_share, 1:3) | Codigo + ID + Year, data=df_PA_major,panel.id = c("Codigo","Year")) 
m3a.federal.fix_reduc_party_SunAbr_lagged_prot %>% fixest::etable()

fixest::coefplot(m3a.federal.fix_reduc_party_regDID_lagged, xlim=c(-26,24))

agg_coef = stats::aggregate(m3a.federal.fix_reduc_party_SunAbr_lagged_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x = c(-26:-1, 1:25) + .1
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)

m3a.federal.fix_reduc_party_SunAbr_lagged_covariates_prot <- fixest::feols(log_federal_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(log_muni_sustain_area_share, 0:3) + l(municipal_sustainableuse_area_share/100, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_federal_sustain_area_share, 1:3) + l(log_federal_prot_area_share, 0:3) + l(log_federal_prot_area_share,0:3) | Codigo + ID + Year, data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.federal.fix_reduc_party_SunAbr_lagged_covariates_prot %>% fixest::etable()

agg_coef2 = stats::aggregate(m3a.federal.fix_reduc_party_SunAbr_lagged_covariates_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x2 = c(-26:-1, 1:25) + .2
points(x2, agg_coef2[, 1], pch = 17, col = 6)
ci_low2 = agg_coef2[, 1] - 1.96 * agg_coef2[, 2]
ci_up2 = agg_coef2[, 1] + 1.96 * agg_coef2[, 2]
segments(x0 = x2, y0 = ci_low2, x1 = x2, y1 = ci_up2, col = 6)

m3a.federal.fix_reduc_party_SunAbr_lagged_covariate_party_prot <- fixest::feols(log_federal_sustain_area_share ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + maygov_sameparty + l(log_muni_sustain_area_share, 0:3) + l(municipal_sustainableuse_area_share/100, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_federal_sustain_area_share, 1:3) + l(log_federal_prot_area_share, 0:3) + l(log_federal_prot_area_share,0:3) | Codigo + ID + Year, data=df_PA_major, panel.id = c("Codigo","Year")) 
m3a.federal.fix_reduc_party_SunAbr_lagged_covariate_party_prot %>% fixest::etable()

agg_coef4 = stats::aggregate(m3a.federal.fix_reduc_party_SunAbr_lagged_covariate_party_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x4 = c(-17:-1, 1:24) + .3
points(x4, agg_coef4[, 1], pch = 17, col = 2)
ci_low4 = agg_coef4[, 1] - 1.96 * agg_coef4[, 2]
ci_up4 = agg_coef4[, 1] + 1.96 * agg_coef4[, 2]
segments(x0 = x4, y0 = ci_low4, x1 = x4, y1 = ci_up4, col = 2)




