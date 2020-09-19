################################
# Analysis of PA develeopment in
# Brazilian Ecological Fiscal Ts
# script authors: nils@droste.io
#                 ...
################################


# this is just to initialize prj

library(tidyverse)
library(here)


# load data
PA_df <- read_csv(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/raw/paneldataEFT-BR.csv"))  %>% # dirty hack to allow access /data in directory higher than Rproj root
  mutate(ID = ID %>% as.factor())
  
