########## creating municipal data set for equality EFT project
########## Author: fhollenbach
########## Date: 11/20/2020

#### packages
library(tidyverse)
library(here)
library(sf)
library(geobr)

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
todas <- readOGR(here("..", "..", "..", "Dropbox", "EFT_Shape", "Todas"), layer = "ucstodas")
proj4string(todas) <- CRS("EPSG:4618")
eft <- st_as_sf(todas)
eft <- st_transform(eft, "SIRGAS 2000")

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

p <- ggplot() 
p <- p + geom_sf(data=eft, fill="#2D3E50", color="#FEBF57", size= 0.3, show.legend = FALSE)
#p <- p +  labs(subtitle="Municipalities", size=8) 
p <- p +  theme_minimal() + no_axis
p <- p + geom_sf(data = b, color = "red", alpha =0.4)
#p <- p + geom_sf(data = eft, color = "green")
p


munis <- st_read(here("..", "..", "..", "Dropbox", "EFT_Shape", "br_municipios", "BR_Municipios_2019.shp"), package="sf", stringsAsFactors = FALSE)






eft <- st_read(here("..", "..", "..", "Dropbox", "EFT_Shape", "Todas", "ucstodas.shp"), package="sf")

eft <- st_read(here("..", "..", "..", "Dropbox", "EFT_Shape", "Municipal","Integral", "ucsmi.shp"), package="sf")

uni <- st_read(here("..", "..", "..", "Dropbox", "EFT_Shape", "Municipal","Integral", "ucsmi.shp"), package="sf")


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
