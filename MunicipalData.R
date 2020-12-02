########## creating municipal data set for equality EFT project
########## Author: fhollenbach
########## Date: 11/20/2020

#### packages
library(here)

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
  mutate(ibge7dig = as.double(as.character(Codigo)))


ExpensesEducationCulture <- read_csv(here("data", "municipal", "ExpensesEducationCulture.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ExpensesEducationCulture") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))

ExpensesLabor <- read_csv(here("data", "municipal", "ExpensesLabor.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ExpensesLabor") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))

  
ExpensesSSAssistence <- read_csv(here("data", "municipal", "ExpensesSSAssistence.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ExpensesSSAssistence") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))


HarvestedArea <- read_csv(here("data", "municipal", "HarvestedArea.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "HarvestedArea") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))

ICMSTransfers <- read_csv(here("data", "municipal", "ICMSTransfers.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "ICMSTransfers") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))


MunicipalTaxesRevenue <- read_csv(here("data", "municipal", "MunicipalTaxesRevenue.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "MunicipalTaxesRevenue") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))


PopulationEstimates <- read_csv(here("data", "municipal", "PopulationEstimates.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "PopulationEstimates") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))


RevenueCurrentTransfers <- read_csv(here("data", "municipal", "RevenueCurrentTransfers.csv")) %>% 
  rename(Codigo = Código) %>% 
  select(-c(Sigla,City)) %>% 
  pivot_longer(!Codigo, names_to = "Year", values_to = "RevenueCurrentTransfers") %>% 
  mutate(Codigo = as.double(as.character(Codigo)))




select(-c(municipality)) %>%
  mutate(municipality.merge = iconv(municipio,from="UTF-8",to="ASCII//TRANSLIT"),
         municipality.merge =  gsub(" ", "", municipality.merge, fixed = TRUE),
         municipality.merge = str_replace_all(municipality.merge, "[^[:alnum:]]", ""),
         municipality.merge = tolower(municipality.merge),
         ibge7dig = as.double(as.character(ibge7dig))) %>%
  rename(State = state)



data_population <- tibble()
for(i in 1991){
  dat <- populacao_municipios(i)  
  
  
}

ipea_serie_carrega

all_series <- available_series(language = c("en")) #%>% 
  filter(status == "Active" & freq == "Yearly")

territories <- available_territories(language = c("en"))

states <- territories %>% 
  filter(uname == "States")

munis <- territories %>% 
  filter(uname != "States" & uname != "Brazil" & uname != "State/Metropolitan region") %>%
  filter(tcode )


transfers <- ipeadata(code = "RTRCOESTM")
