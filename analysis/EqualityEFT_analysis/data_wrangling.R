# data wrangling

PA_data <- bind_rows(
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Rondônia!A156:N179", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Acre!A96:N119", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Amazonas!A185:N208", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Roraima!A88:N111", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Pará!A201:N224", col_names = F), 
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Tocantins!A147:N170", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Maranhão!A116:N139", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Piauí!A137:N160", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Ceará!A218:N241", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Rio Grande do Norte!A155:N178", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Paraíba!A97:N120", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Pernambuco!A111:N134", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Alagoas!A91:N114", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Sergipe!A72:N95", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Bahia!A305:N328", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Minas Gerais!A404:N427", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Espirito Santo!A215:N238", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Rio de Janeiro!A445:N468", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "São Paulo!A400:N423", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Paraná!A273:N296", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Santa Catarina!A213:N236", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Rio Grande do Sul!A235:N258", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Mato Grosso do Sul!A168:N191", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Mato Grosso!A188:N211", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Goiás!A239:N262", col_names = F),
  readxl::read_excel("C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i.xlsx",
                     range = "Distrito Federal!A162:N185", col_names = F)
) %>% rename(state     = "...1",
             year      = "...2",
             fedPA     = "...3",
             staPA     = "...4",
             munPA     = "...5",
             totPA     = "...6",
             fedPI     = "...7",
             staPI     = "...8",
             munPI     = "...9",
             totPI     = "...10",
             fedUS     = "...11",
             staUS     = "...12",
             munUS     = "...13",
             totUS     = "...14")
             
EFT <-
  read_csv(paste0(
    here() %>% str_remove("analysis/EqualityEFT_analysis"),
    "/data/raw/EFT.csv"
  )) %>% rename(state = X1,
                legislation = `ICMS-E`,
                enactment = Enactment)

EFT_df <-
  left_join(cbind(
    state = rep(EFT$state, times = 1, each = length(c(1991:2014))),
    year = c(1991:2014)
  ) %>% as_tibble() %>% mutate(year = year %>% as.numeric),
  EFT) %>% mutate(
    icms_e_leg = case_when(year < legislation ~ 0, year >= legislation ~ 1),
    icms_e_enact = case_when(year < enactment ~ 0, year >= enactment ~ 1)
  )

arpa <- c("RO", "AC", "AM", "RR", "PA", "AP", "MT")

pop_dens_data <-
  readxl::read_excel(
    "C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//POP.xlsx",
    sheet = 1,
    range = "A31:AB55",
    col_names = T
  ) %>% pivot_longer(-Year, names_to = "state", values_to = "popdens") %>% select(state, year =
                                                                                    Year, popdens) %>% arrange (state, year) %>% mutate(year = year %>% as.numeric())

biomas  <-
  readxl::read_excel(
    "C://Users//Nils Droste//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//biomas.xlsx",
    sheet = 1,
    range = "A1:M28",
    col_names = T
  ) %>% select(1, 8:13) %>% rename(
    state = `UF's`,
    ama = ama...8,
    cer = cer...9,
    caa = caa...10,
    mat = mat...11,
    pan = pan...12,
    pam = pam...13
  )



# joining everything
PA_data %>% left_join(EFT_df) %>% mutate(arpa = case_when(state %in% arpa ~ 1, !state %in% arpa ~0)) %>% left_join(biomas) %>% left_join(pop_dens_data)
