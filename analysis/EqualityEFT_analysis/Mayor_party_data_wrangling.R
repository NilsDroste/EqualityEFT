library(tidyverse)
library(here)


Mayor_df <- load(paste0(here() %>% str_remove("analysis/EqualityEFT_analysis"), "/data/df_annual_mayors.RData")) # %>% # dirty hack to allow access /data in directory higher than Rproj root
# mutate(ID = ID %>% as.factor())
