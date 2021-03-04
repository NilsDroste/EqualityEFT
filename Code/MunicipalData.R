########## creating municipal data set for equality EFT project
########## Author: fhollenbach
########## Date: 11/20/2020

#### packages
library(tidyverse)
library(here)
library(sf)
library(geobr)
library(rgdal)
library(s2)

CurrentStateTransfers <- read_csv(here("data", "municipal", "CurrentStateTransfers.csv")) %>% 
  rename(Codigo = Código) %>% 
  pivot_longer(!c(Sigla, City, Codigo), names_to = "Year", values_to = "CurrentStateTransfers") %>% 
  mutate(municipality.merge = iconv(City,from="UTF-8",to="ASCII//TRANSLIT"),
         municipality.merge =  gsub(" ", "", municipality.merge, fixed = TRUE),
         municipality.merge = str_replace_all(municipality.merge, "[^[:alnum:]]", ""),
         municipality.merge = tolower(municipality.merge),
         Codigo = as.double(as.character(Codigo)))


ExpensesAgriculture <- read_csv(here("data", "municipal", "ExpensesAgriculture.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ExpensesAgriculture") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))


data <- CurrentStateTransfers %>% 
  left_join(ExpensesAgriculture, by = c("Codigo", "Year"))

ExpensesEducationCulture <- read_csv(here("data", "municipal", "ExpensesEducationCulture.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ExpensesEducationCulture") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))

data <- data %>% 
  left_join(ExpensesEducationCulture, by = c("Codigo", "Year"))



ExpensesLabor <- read_csv(here("data", "municipal", "ExpensesLabor.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ExpensesLabor") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))
data <- data %>% 
  left_join(ExpensesLabor, by = c("Codigo", "Year"))

  
ExpensesSSAssistence <- read_csv(here("data", "municipal", "ExpensesSSAssistence.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ExpensesSSAssistence") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))
data <- data %>% 
  left_join(ExpensesSSAssistence, by = c("Codigo", "Year"))


HarvestedArea <- read_csv(here("data", "municipal", "HarvestedArea.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "HarvestedArea") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))
data <- data %>% 
  left_join(HarvestedArea, by = c("Codigo", "Year"))

ICMSTransfers <- read_csv(here("data", "municipal", "ICMSTransfers.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ICMSTransfers") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))
data <- data %>% 
  left_join(ICMSTransfers, by = c("Codigo", "Year"))


MunicipalTaxesRevenue <- read_csv(here("data", "municipal", "MunicipalTaxesRevenue.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "MunicipalTaxesRevenue") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))
data <- data %>% 
  left_join(MunicipalTaxesRevenue, by = c("Codigo", "Year"))


PopulationEstimates <- read_csv(here("data", "municipal", "PopulationEstimates.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "PopulationEstimates") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))
data <- data %>% 
  left_join(PopulationEstimates, by = c("Codigo", "Year"))


RevenueCurrentTransfers <- read_csv(here("data", "municipal", "RevenueCurrentTransfers.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "RevenueCurrentTransfers") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))
data <- data %>% 
  left_join(RevenueCurrentTransfers, by = c("Codigo", "Year"))

summary(data)
save(data, file = here("data", "municipal", "muni_merged.rda"))

################# EFT shapefiles
load(here("..", "..", "..", "Dropbox", "EFT_Shape", "Todas", "eft.rda"))
load(here("..", "..", "..", "Dropbox", "EFT_Shape", "Todas", "muni.rda"))
load(here("..", "..", "..", "Dropbox", "EFT_Shape", "Todas", "states.rda"))

todas <- readOGR(here("Todas"), layer = "ucstodas")
proj4string(todas) <- CRS("SIRGAS 2000")
eft <- st_as_sf(todas) %>% 
  st_make_valid()


min(eft$ANO_CRIA6)
years <- seq(1990, 2019, 1)



municipios <- unique(muni$code_muni)

sf_use_s2(FALSE)
robust_eft <- function(muni){
  tryCatch({intersection <- st_intersection(muni, eft) %>%
    mutate(land_m2  = geom %>% st_zm() %>% st_area(),
           ANO_CRIA6 = as.double(as.character(ANO_CRIA6)),
           year = case_when(ANO_CRIA6 < 1990 ~ 1990,
                            TRUE ~ ANO_CRIA6)) %>%
    group_by(ESFERA5, year) %>% 
    summarize(land_m2  = sum(land_m2)) %>% 
    as_tibble() %>% 
    dplyr::select(-geom) %>% 
    mutate(code_muni = municipios[i])
  if(dim(intersection)[1]==0){
    intersection <- tibble(ESFERA5 = NA, year = NA, land_m2 = NA, code_muni = municipios[i])
  }
  intersection
  },
  error = function(e) {tibble(ESFERA5 = "Error", year = NA, land_m2 = NA, code_muni = muni$code_muni)}
  )
}

municipality_eft <- tibble()
for(i in 1:length(municipios)){
  muni_sub <- muni %>% 
    filter(code_muni ==municipios[i])
  sub <- robust_eft(muni_sub)
  municipality_eft <- bind_rows(municipality_eft, sub)
}
save(municipality_eft, file = here("..", "..", "..", "Dropbox", "EFT_Shape", "Todas","eft_intersected.rda"))



##### now this has to be reshaped and merged to other municipal data
load(here("data", "municipal", "muni_merged.rda"))
load(here("..", "..", "..", "Dropbox", "EFT_Shape", "Todas","eft_intersected.rda"))

efts <- municipality_eft %>% 
  filter(is.na(ESFERA5)==FALSE) %>% 
  pivot_wider(id_cols = c(code_muni,year), names_from = ESFERA5, values_from = land_m2) %>% 
  mutate(FederalConsFounded = case_when(is.na(federal)==FALSE ~1,
                                     TRUE ~ 0),
         StateConsFounded = case_when(is.na(estadual)==FALSE ~1,
                                   TRUE ~ 0),
         
         MuniConsFounded = case_when(is.na(municipal)==FALSE ~1,
                                     TRUE ~ 0),
         year = as.double(as.character(year))) %>% 
  rename(Codigo = code_muni,
         Year = year)

data <- data %>% 
  mutate(Year = as.double(as.character(Year))) %>% 
  left_join(efts, by = c("Codigo", "Year")) %>% 
  mutate(federal = as.double(as.character(federal)),
         federal = case_when(is.na(federal)==TRUE ~ 0,
                             TRUE ~ federal),
         estadual = as.double(as.character(estadual)),
         estadual = case_when(is.na(estadual)==TRUE ~ 0,
                             TRUE ~ estadual),
         municipal = as.double(as.character(municipal)),
         municipal = case_when(is.na(municipal)==TRUE ~ 0,
                             TRUE ~ municipal),
         FederalConsFounded = case_when(is.na(FederalConsFounded)==TRUE ~ 0,
                               TRUE ~ FederalConsFounded),
         StateConsFounded = case_when(is.na(StateConsFounded)==TRUE ~ 0,
                               TRUE ~ StateConsFounded),
         MuniConsFounded = case_when(is.na(MuniConsFounded)==TRUE ~ 0,
                               TRUE ~ MuniConsFounded)
         ) %>% 
  arrange(Codigo, Year) %>% 
  group_by(Codigo) %>% 
  mutate(FederalConsArea = cumsum(federal),
         StateConsArea = cumsum(estadual),
         MuniConsArea = cumsum(municipal)) %>% 
  select(-c(estadual, federal, municipal))
  

eft <- read_csv(here("data", "raw", "EFT.csv")) %>% 
  rename(ICMS_E = `ICMS-E`,
         Sigla = X1) 
#### create state eft panel

state_panel  <- expand_grid('Sigla' = unique(data$Sigla), 'Year' = unique(data$Year)) %>% 
  mutate(Year = as.double(as.character(Year)),
         ICME_E = 0, 
         EFT_Enactment = 0)

for(i in 1:dim(eft)[2]){
  state_panel$ICME_E[state_panel$Sigla == eft$Sigla[i] & state_panel$Year == eft$ICMS_E[i]] <- 1
  state_panel$EFT_Enactment[state_panel$Sigla == eft$Sigla[i] & state_panel$Year == eft$Enactment[i]] <- 1
}

save(data, file = here("data", "municipal", "muni_merged.rda"))



