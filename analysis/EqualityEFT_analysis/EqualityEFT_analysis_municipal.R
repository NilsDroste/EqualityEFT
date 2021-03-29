################################
# Analysis of PA develeopment in
# Brazilian Ecological Fiscal Ts
# script authors: nils@droste.io
#                 ...
################################

# env prep ----

library(tidyverse)
library(lubridate)
library(here)
library(sf)

# data prep ----

# load municipal data
load(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "data/municipal/muni_merged.rda"))
data <- data %>% rename("ID"= "Sigla")

# get EFT legislation data
EFT <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/EFT.csv")) %>% rename(ID=X1, legislation=`ICMS-E`, enactment=Enactment) 

# get municipal area data
muns <- geobr::read_municipality(code_muni="all", simplified = T, year=2018) 
muns <- muns %>% mutate(area=sf::st_area(muns)) 
muns_area <- bind_cols(Codigo=muns$code_muni, area= muns$area %>% as.numeric)
rm(muns)

# get state level data, i.e. for regional dummies.
load("C:\\Users\\Nils\\Box\\papers\\work_in_progress\\Cooperman_etal_2021_EqualityEFT\\repo\\data\\completed_sets\\State_Panel_data.RData")

# merge and calculate area shared / logarithmize
full_df <- left_join(data, EFT) %>% mutate(Codigo= Codigo %>% as.numeric()) %>% left_join(muns_area) %>% mutate(
  federal_protected_area_share = (Federal_P_Area / area), # a proportion from 0 to 1
  state_protected_area_share = (State_P_Area / area),
  municipal_protected_area_share = (Muni_P_Area / area),
  federal_sustainableuse_area_share = (Federal_SUU_Area / area),
  state_sustainableuse_area_share = (State_SUU_Area / area),
  municipal_sustainableuse_area_share = (Muni_SUU_Area / area),
  log_federal_prot_area_share = federal_protected_area_share %>% log1p(),
  log_state_prot_area_share = state_protected_area_share %>% log1p(),
  log_muni_prot_area_share = municipal_protected_area_share %>% log1p(),
  log_federal_sustain_area_share = federal_sustainableuse_area_share %>% log1p(),
  log_state_sustain_area_share = state_sustainableuse_area_share %>% log1p(),
  log_muni_sustain_area_share = municipal_sustainableuse_area_share %>% log1p(),
  ) %>% left_join(complete_df %>% select(ID=state, arpa, ama, cer, caa, mat, pan, pam) %>% unique()) %>% 
  mutate(Year=Year %>% as.integer(), enactment = enactment %>% as.integer(), legislation = legislation %>% as.integer()) %>% ungroup()

# TODO
# check also geobr::read_biomes() for a more precise calculation of biome overlap with municipalities
# biomes <-  geobr::read_biomes(year = 2019, simplified = T)

# create treatment dummies
full_df <- full_df %>% mutate(eft_enact=case_when(Year < enactment ~ 0, is.na(enactment) ~ 0, Year >= enactment ~ 1), eft_legis=case_when(Year < legislation ~ 0, legislation %>% is.na() ~ 0, Year >= legislation ~ 1))

#housekeeping
rm(list=c("EFT", "data", "muns_area", "complete_df"))

# load major alignment data
load(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/df_annual_mayors.RData"))

# merge into full df
full_df <- left_join(full_df, df_annual_mayors %>% select(1,5:ncol(df_annual_mayors))%>% rename(Codigo=IBGE_code, Year=year) %>% mutate(Codigo = Codigo %>% as.numeric()) %>% as_tibble()) 

#housekeeping
rm(df_annual_mayors)

# plotting ----

# plotting time series by state with legislation (blue) and adoption (red)
# municipal
full_df %>% group_by(ID, Year) %>% summarise(mean = mean(municipal_sustainableuse_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean municipal sustainable use area share") + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

full_df %>% group_by(ID, Year) %>% summarise(mean = mean(municipal_protected_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean municipal protected use area share")  + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

# state
full_df %>% group_by(ID, Year) %>% summarise(mean = mean(state_sustainableuse_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean state sustainable use area share") + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

full_df %>% group_by(ID, Year) %>% summarise(mean = mean(state_protected_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean state protected use area share")  + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

# federal
full_df %>% group_by(ID, Year) %>% summarise(mean = mean(federal_sustainableuse_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean federal sustainable use area share") + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)

full_df %>% group_by(ID, Year) %>% summarise(mean = mean(federal_protected_area_share), sd = sd(municipal_sustainableuse_area_share), enact = mean(enactment), legis = mean(legislation)) %>% ggplot() + geom_line(aes(y=mean, x=Year)) + ylab("mean federal protected use area share")  + geom_vline(aes(xintercept = enact), col="red") + geom_vline(aes(xintercept = legis), col="blue") + facet_wrap(~ID)


# general comparison without identification strategy
full_df %>%  ggpubr::ggboxplot(y="log_muni_sustain_area_share", x= "eft_enact", color="eft_enact", add = "jitter") + ggpubr::stat_compare_means(method = "t.test")


# check out dep var distribution
# sustainable use
full_df$municipal_sustainableuse_area_share %>% hist(breaks=100)
full_df$log_muni_sustain_area_share %>% hist(breaks=100)

# protected area
full_df$municipal_protected_area_share %>% hist(breaks=100)
full_df$log_muni_prot_area_share %>% hist(breaks=100)

# NOTE: 
# I would argue for either a poisson (log link) or even more fitting a quasibinomial (logit link) function for its lesser distributional assumptions.


# Analysis ----

# the Sun and Abraham approach (staggered treatmetn)
full_df <- full_df %>% mutate(time_to_treat = replace_na(Year - enactment, -1000), MunYear = paste0(Codigo,":",Year), treated=case_when(!is.na(enactment) ~ 1, is.na(enactment) ~0), post = replace_na(case_when(Year>=enactment ~1, Year<enactment ~ 0), 0), eft_mayor_treatment = post * maygov_sameparty) 
# NOTE: that the time_to_treatment for controls is set to -1000

# alternative treatment variable creation.
# create variable of first alignement year of mayor and state gov when EFT are implemented.
Aligned <- full_df %>% filter(eft_mayor_treatment > 0) %>% 
  group_by(Codigo) %>% 
  summarize(aligned_treat_year = min(Year))
full_df <- full_df %>% left_join(Aligned) %>% mutate( time_to_aligned_treat = replace_na( Year - aligned_treat_year, -1000))
# Note that the time_to_treatment for controls is set to -1000
rm(Aligned)

# "Regular" DiD
MunSustUse_regDID <- fixest::feglm(municipal_sustainableuse_area_share ~ i(treated, time_to_treat, ref = 0, drop = -1000)| Codigo+ ID + Year + arpa ,family = "quasibinomial", data=full_df) 
MunSustUse_regDID %>% fixest::etable()

fixest::coefplot(MunSustUse_regDID) # the tails?


# Sun and Abraham
MunSustUse_SunAbr <- fixest::feglm(municipal_sustainableuse_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000))| Codigo + ID + Year + arpa ,family = "quasibinomial", data=full_df) 
MunSustUse_SunAbr %>% fixest::etable()

# aggregate effects to cohort level
agg_coef = stats::aggregate(MunSustUse_SunAbr, "(ti.*at)::(-?[0-9]?[0-9]?)") 

# plot
x = c(-24:-1, 1:25) + .2
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)


# Add in lagged explanatory vars

MunSustUse_SunAbr_lagged_covariates <- fixest::feglm(municipal_sustainableuse_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(municipal_sustainableuse_area_share, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year + arpa ,family = "quasibinomial", data=full_df, panel.id = c("Codigo","Year")) 
MunSustUse_SunAbr_lagged_covariates %>% fixest::etable()

agg_coef2 = stats::aggregate(MunSustUse_SunAbr_lagged_covariates, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x2 = c(-24:-1, 1:25) + .4
points(x2, agg_coef2[, 1], pch = 17, col = 6)
ci_low2 = agg_coef2[, 1] - 1.96 * agg_coef2[, 2]
ci_up2 = agg_coef2[, 1] + 1.96 * agg_coef2[, 2]
segments(x0 = x2, y0 = ci_low2, x1 = x2, y1 = ci_up2, col = 6)


# Add in major alignment var
MunSustUse_SunAbr_lagged_covariate_party <- fixest::feglm(municipal_sustainableuse_area_share ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + maygov_sameparty + maygov_samecoalition + l(municipal_sustainableuse_area_share, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year + arpa ,family = "quasibinomial", data=full_df, panel.id = c("Codigo","Year")) 
MunSustUse_SunAbr_lagged_covariate_party %>% fixest::etable()

agg_coef3 = stats::aggregate(MunSustUse_SunAbr_lagged_covariate_party, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x3 = c(-17:-1, 1:23) + .6
points(x3, agg_coef3[, 1], pch = 17, col = 2)
ci_low3 = agg_coef3[, 1] - 1.96 * agg_coef3[, 2]
ci_up3 = agg_coef3[, 1] + 1.96 * agg_coef3[, 2]
segments(x0 = x3, y0 = ci_low3, x1 = x3, y1 = ci_up3, col = 2)

# TODO remove the always treated (???) not yet the case - but possibly when shortening the time series.



# protected areas

# TODO why is there a protected area share over 1 ????
full_df %>% select(Codigo, Year, municipal_protected_area_share) %>% filter(municipal_protected_area_share>1)
full_df %>% filter(Codigo ==  5007976, Year == 2017)
full_df[full_df$Codigo ==  5007976 & full_df$Year == 2017,]$municipal_protected_area_share <- 1

# "Regular" DiD
MunProtArea_regDID <- fixest::feglm(municipal_protected_area_share ~ i(treated, time_to_treat, ref = 0, drop = -1000) + l(municipal_protected_area_share, 1:3)| Codigo + ID + Year + arpa ,family = "quasibinomial", data=full_df, panel.id = c("Codigo","Year")) 

fixest::coefplot(MunProtArea_regDID, xlim=c(-20,25), ylim=c(-10,5))

# lagged covariates
MunProtArea_regDID_lagged_covariate <- fixest::feglm(municipal_protected_area_share ~ i(time_to_treat, f2 = ID, drop = c(0, -1000)) + l(municipal_protected_area_share, 1:3) + l(municipal_sustainableuse_area_share, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year  ,family = "quasibinomial", data=full_df, panel.id = c("Codigo","Year")) 
MunProtArea_regDID_lagged_covariate %>% fixest::etable()

agg_coef_prot = stats::aggregate(MunProtArea_regDID_lagged_covariate, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x_prot = c(-24:-1, 1:25) + .2
points(x_prot, agg_coef_prot[, 1], pch = 17, col = 4)
ci_low_prot = agg_coef_prot[, 1] - 1.96 * agg_coef_prot[, 2]
ci_up_prot = agg_coef_prot[, 1] + 1.96 * agg_coef_prot[, 2]
segments(x0 = x_prot, y0 = ci_low_prot, x1 = x_prot, y1 = ci_up_prot, col = 4)

# party alignment 
MunProtArea_regDID_lagged_covariate_party_prot <- fixest::feglm(municipal_protected_area_share ~ i(time_to_treat, f2 = ID, drop = c(-1, -1000)) + maygov_sameparty + l(municipal_protected_area_share, 1:3) + l(log_federal_sustain_area_share,0:3) + l(log_state_sustain_area_share, 0:3) + l(log_federal_prot_area_share, 0:3) + l(log_state_prot_area_share,0:3) | Codigo + ID + Year,family = "quasibinomial", data=full_df, panel.id = c("Codigo","Year")) 
MunProtArea_regDID_lagged_covariate_party_prot %>% fixest::etable()

agg_coef_prot_part = stats::aggregate(MunProtArea_regDID_lagged_covariate_party_prot, "(ti.*at)::(-?[0-9]?[0-9]?)") 
x_prot_party = c(-17:-1, 1:23) + .3
points(x_prot_party, agg_coef_prot_part[, 1], pch = 17, col = 2)
ci_low4 = agg_coef_prot_part[, 1] - 1.96 * agg_coef_prot_part[, 2]
ci_up4 = agg_coef_prot_part[, 1] + 1.96 * agg_coef_prot_part[, 2]
segments(x0 = x_prot_party, y0 = ci_low4, x1 = x_prot_party, y1 = ci_up4, col = 2)




# TODO: there is also shares at state and federal level of protected area / sust use of more than 100 !?

# state level 

full_df %>% select(Codigo, Year, state_sustainableuse_area_share) %>% filter(state_sustainableuse_area_share>100) %>% View()
full_df %>% select(Codigo, Year, state_protected_area_share) %>% filter(state_protected_area_share>100) %>% View()


# federal level 

full_df %>% select(Codigo, Year, federal_sustainableuse_area_share) %>% filter(federal_sustainableuse_area_share>100) %>% View()
full_df %>% select(Codigo, Year, federal_protected_area_share) %>% filter(federal_protected_area_share>100) %>% View()



# Equality ----

# poverty / equality
full_df$MunicipalTaxesRevenue %>% hist
full_df$MunicipalTaxesRevenue %>% log1p %>% hist

# This kind of distribution would normally be a case for a tobit or some other left censored regression model, e.g. zero-inflated or hurdle, I think. This could possibly be implemented with a glm model à la https://drizopoulos.github.io/GLMMadaptive/. Such models have confusing terminology when it comes to fixed and random effects, i.e. the latter being varying slopes and the former a common slope but varying intercepts, see e.g. https://stats.stackexchange.com/questions/238214/how-exactly-does-a-random-effects-model-in-econometrics-relate-to-mixed-models?noredirect=1&lq=1 and https://peerj.com/articles/4794/. However, even considering the differences, the mixed effects model procedure (maximum likelihood estimation) can reproduce the classical fixed effects regression by incorporation of a group wise mean as an explanatory variable (and thus providing the demeaning of a fixed effects model).
# see also https://stats.stackexchange.com/questions/474611/zero-inflated-negative-binomial-models-for-panel-data

# create variable poor
full_df <- full_df %>% group_by(ID,Year) %>% mutate_at(vars(MunicipalTaxesRevenue), list(Poor = function(x) cut(percent_rank(x), c(-Inf,.25,Inf), labels = c(0,1)))) %>% ungroup()

# create average value
full_df <- full_df %>% group_by(Codigo) %>% mutate(MunTaxRevAvg = mean(MunicipalTaxesRevenue, na.rm = T)) %>% ungroup()

# creating log of MunicipalTaxrevenue
full_df <-  full_df %>% mutate(LogMunTaxRev = log1p(MunicipalTaxesRevenue))



# library(GLMMadaptive) # only allows one grouping factor.
install.packages("glmmTMB")

# does not work yet
m2.mun.fix_reveneue_party_interact_zigamma <- glmmTMB::glmmTMB(as.integer(MunicipalTaxesRevenue) ~ eft_enact*maygov_sameparty + eft_enact*maygov_samecoalition  + HarvestedArea  + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + MunTaxRevAvg + 1 | as.factor(Codigo) +  1|ID +  1|arpa + 1|ama + 1|cer +  1|caa +  1|mat +  1|pan + 1|pam + 1| ID:Year, family = glmmTMB::truncated_genpois(), ziformula = ~ MunicipalTaxesRevenue, data=full_df )
summary(m2.mun.fix_reveneue_party_interact) 


# the fixed effects approach (not taking the zero-inflation )

m2.mun.fix_reveneue_party_poor <- fixest::feglm(MunicipalTaxesRevenue ~ eft_enact*Poor + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa, family = "poisson", data=full_df)
summary(m2.mun.fix_reveneue_party_poor, se="fourway") 

m2.mun.fix_reveneue_party_interact_poor <- fixest::feglm(MunicipalTaxesRevenue ~ eft_enact*maygov_sameparty*maygov_samecoalition*Poor + log_federal_sustain_area_share + log_state_sustain_area_share + log_federal_prot_area_share + log_state_prot_area_share + log_muni_sustain_area_share + maygov_sameparty + maygov_samecoalition | as.factor(Codigo) + ID[Year] + arpa,  family = "poisson", data=full_df)
summary(m2.mun.fix_reveneue_party_interact_poor, se="fourway") 






