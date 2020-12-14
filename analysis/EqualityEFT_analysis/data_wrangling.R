# data wrangling

PA_data_path <- "C://Users//Nils//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//CU_i_upd.xlsx"

PA_data <- bind_rows(
  readxl::read_excel(PA_data_path,
                     range = "Rondônia!A150:N179", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Acre!A90:N119", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Amazonas!A179:N208", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Roraima!A82:N111", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Pará!A195:N224", col_names = F), 
  readxl::read_excel(PA_data_path,
                     range = "Tocantins!A141:N170", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Maranhão!A110:N139", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Piauí!A131:N160", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Ceará!A212:N241", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Rio Grande do Norte!A149:N178", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Paraíba!A91:N120", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Pernambuco!A105:N134", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Alagoas!A85:N114", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Sergipe!A68:N95", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Bahia!A299:N328", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Minas Gerais!A398:N427", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Espirito Santo!A209:N238", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Rio de Janeiro!A439:N468", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "São Paulo!A394:N423", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Paraná!A267:N296", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Santa Catarina!A207:N236", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Rio Grande do Sul!A229:N258", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Mato Grosso do Sul!A162:N191", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Mato Grosso!A182:N211", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Goiás!A233:N262", col_names = F),
  readxl::read_excel(PA_data_path,
                     range = "Distrito Federal!A156:N185", col_names = F)
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
    "C://Users//Nils//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//POP.xlsx",
    sheet = 1,
    range = "A31:AB55",
    col_names = T
  ) %>% pivot_longer(-Year, names_to = "state", values_to = "popdens") %>% select(state, year =
                                                                                    Year, popdens) %>% arrange (state, year) %>% mutate(year = year %>% as.numeric())

biomas  <-
  readxl::read_excel(
    "C://Users//Nils//Box//papers//published//Droste_etal_2017_EFT-Brazil//data//biomas.xlsx",
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



# economy 

econ_data_path <-  "C:/Users/Nils/Box/papers/work_in_progress/Cooperman_etal_2021_EqualityEFT/repo/data/state/regional accounts/"

files_till_2004 <- list.files(here(econ_data_path, "1985-2004"))

tot_value_added_2004 <- bind_rows(
  here(econ_data_path, "1985-2004", files_till_2004[1]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[1] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[2]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[2] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[3]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[3] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[4]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[4] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[5]) %>% unzip() %>% readxl::read_excel(range = "Total!A55:G75") %>% mutate(state= files_till_2004[5] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[6]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[6] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[7]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[7] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[8]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[8] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[9]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[9] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[10]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[10] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[11]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[11] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[12]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[12] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[13]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[13] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[14]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[14] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[15]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[15] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[16]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[16] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[17]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[17] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[18]) %>% unzip() %>% readxl::read_excel(range = "Total!A55:G75") %>% mutate(state= files_till_2004[18] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[19]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[19] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[20]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[20] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[21]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[21] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[22]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[22] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[23]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[23] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[24]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[24] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[25]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[25] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[26]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[26] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[27]) %>% unzip() %>% readxl::read_excel(range = "Total!A56:G76") %>% mutate(state= files_till_2004[27] %>% str_remove(".zip"))
)

ag_value_added_2004 <- bind_rows(
  here(econ_data_path, "1985-2004", files_till_2004[1]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[1] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[2]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[2] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[3]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[3] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[4]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[4] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[5]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[5] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[6]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[6] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[7]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[7] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[8]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[8] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[9]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[9] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[10]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[10] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[11]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[11] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[12]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[12] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[13]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[13] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[14]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[14] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[15]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[15] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[16]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[16] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[17]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[17] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[18]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A55:G75") %>% mutate(state= files_till_2004[18] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[19]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[19] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[20]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[20] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[21]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[21] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[22]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[22] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[23]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[23] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[24]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[24] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[25]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[25] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[26]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[26] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[27]) %>% unzip() %>% readxl::read_excel(range = "Agropecuária!A56:G76") %>% mutate(state= files_till_2004[27] %>% str_remove(".zip"))
)

extr_value_added_2004 <- bind_rows(
  here(econ_data_path, "1985-2004", files_till_2004[1]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[1] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[2]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[2] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[3]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[3] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[4]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[4] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[5]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[5] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[6]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[6] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[7]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[7] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[8]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[8] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[9]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[9] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[10]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[10] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[11]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[11] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[12]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[12] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[13]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[13] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[14]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[14] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[15]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[15] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[16]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[16] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[17]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[17] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[18]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A55:G75") %>% mutate(state= files_till_2004[18] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[19]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[19] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[20]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[20] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[21]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[21] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[22]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[22] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[23]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[23] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[24]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[24] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[25]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[25] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[26]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[26] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[27]) %>% unzip() %>% readxl::read_excel(range = "Ind Extr Mineral!A56:G76") %>% mutate(state= files_till_2004[27] %>% str_remove(".zip"))
)

transInd_value_added_2004 <- bind_rows(
  here(econ_data_path, "1985-2004", files_till_2004[1]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[1] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[2]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[2] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[3]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[3] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[4]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[4] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[5]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[5] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[6]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[6] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[7]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[7] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[8]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[8] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[9]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[9] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[10]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[10] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[11]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[11] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[12]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[12] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[13]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[13] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[14]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[14] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[15]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[15] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[16]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[16] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[17]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[17] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[18]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A55:G75") %>% mutate(state= files_till_2004[18] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[19]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[19] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[20]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[20] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[21]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[21] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[22]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[22] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[23]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[23] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[24]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[24] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[25]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[25] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[26]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[26] %>% str_remove(".zip")),
  here(econ_data_path, "1985-2004", files_till_2004[27]) %>% unzip() %>% readxl::read_excel(range = "Ind Transformação!A56:G76") %>% mutate(state= files_till_2004[27] %>% str_remove(".zip"))
)

value_added_2004 <- tot_value_added_2004 %>% select(year=ANO, Total_value_added = `VALOR ADICIONADO PREÇO CORRENTE`, state) %>% 
  left_join(ag_value_added_2004 %>% select(year=ANO, Agricultural_value_added = `VALOR ADICIONADO PREÇO CORRENTE`, state)) %>% 
  left_join(extr_value_added_2004 %>% select(year=ANO, Extr_Ind_value_added = `VALOR ADICIONADO PREÇO CORRENTE`, state)) %>% 
  left_join(transInd_value_added_2004 %>% select(year=ANO, Trans_Ind_value_added = `VALOR ADICIONADO PREÇO CORRENTE`, state)) %>% 
  mutate(share_ag_value_added = (Agricultural_value_added/Total_value_added)*100, 
         share_extr_ind_value_added = (Extr_Ind_value_added/Total_value_added)*100,
         share_trans_ind_value_added = (Trans_Ind_value_added/Total_value_added)*100) %>% 
  left_join(as.data.frame(
    cbind(FullName = c("Acre","Alagoas","Amazonas","Amapa","Bahia","Ceara","Distrito_Federal","Espirito_Santo","Goias","Maranhao","Mata_Grosso","Mata_Grosso_do_Sul","Minas_Gerais","Para","Paraiba","Parana","Pernambuco","Piaui","Rio_de_Janeiro","Rio_Grande_do_Norte","Rio_Grande_do_Sul","Rondonia","Roraima","Santa_Catarina","Sao_Paulo","Sergipe","Tocantins"),
          Abbreviation = c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO"))),
    by = c("state" = "FullName")
  )

files_till_2018 <- here(econ_data_path, "2002-2018", "ods") %>% list.files()

tot_value_added_2018 <- bind_rows(
  here(econ_data_path, "2002-2018", "ods", "Tabela2.ods") %>% readODS::read_ods(sheet = "Tabela2_1", skip=49) %>% slice(1:17) %>% mutate(state = "Rondonia"),
  here(econ_data_path, "2002-2018", "ods", "Tabela3.ods") %>% readODS::read_ods(sheet = "Tabela3_1", skip=49) %>% slice(1:17) %>% mutate(state = "Acre"),
  here(econ_data_path, "2002-2018", "ods", "Tabela4.ods") %>% readODS::read_ods(sheet = "Tabela4_1", skip=49) %>% slice(1:17) %>% mutate(state = "Amazonas"),
  here(econ_data_path, "2002-2018", "ods", "Tabela5.ods") %>% readODS::read_ods(sheet = "Tabela5_1", skip=49) %>% slice(1:17) %>% mutate(state = "Roraima"),
  here(econ_data_path, "2002-2018", "ods", "Tabela6.ods") %>% readODS::read_ods(sheet = "Tabela6_1", skip=49) %>% slice(1:17) %>% mutate(state = "Para"),
  here(econ_data_path, "2002-2018", "ods", "Tabela7.ods") %>% readODS::read_ods(sheet = "Tabela7_1", skip=49) %>% slice(1:17) %>% mutate(state = "Amapa"),
  here(econ_data_path, "2002-2018", "ods", "Tabela8.ods") %>% readODS::read_ods(sheet = "Tabela8_1", skip=49) %>% slice(1:17) %>% mutate(state = "Tocantins"),
  here(econ_data_path, "2002-2018", "ods", "Tabela10.ods") %>% readODS::read_ods(sheet = "Tabela10_1", skip=49) %>% slice(1:17) %>% mutate(state = "Maranhao"),
  here(econ_data_path, "2002-2018", "ods", "Tabela11.ods") %>% readODS::read_ods(sheet = "Tabela11_1", skip=49) %>% slice(1:17) %>% mutate(state = "Piaui"),
  here(econ_data_path, "2002-2018", "ods", "Tabela12.ods") %>% readODS::read_ods(sheet = "Tabela12_1", skip=49) %>% slice(1:17) %>% mutate(state = "Ceara"),
  here(econ_data_path, "2002-2018", "ods", "Tabela13.ods") %>% readODS::read_ods(sheet = "Tabela13_1", skip=49) %>% slice(1:17) %>% mutate(state = "Rio Grande do Norte"),
  here(econ_data_path, "2002-2018", "ods", "Tabela14.ods") %>% readODS::read_ods(sheet = "Tabela14_1", skip=49) %>% slice(1:17) %>% mutate(state = "Paraiba"),
  here(econ_data_path, "2002-2018", "ods", "Tabela15.ods") %>% readODS::read_ods(sheet = "Tabela15_1", skip=49) %>% slice(1:17) %>% mutate(state = "Pernambuco"),
  here(econ_data_path, "2002-2018", "ods", "Tabela16.ods") %>% readODS::read_ods(sheet = "Tabela16_1", skip=49) %>% slice(1:17) %>% mutate(state = "Alagoas"),
  here(econ_data_path, "2002-2018", "ods", "Tabela17.ods") %>% readODS::read_ods(sheet = "Tabela17_1", skip=49) %>% slice(1:17) %>% mutate(state = "Sergipe"),
  here(econ_data_path, "2002-2018", "ods", "Tabela18.ods") %>% readODS::read_ods(sheet = "Tabela18_1", skip=49) %>% slice(1:17) %>% mutate(state = "Bahia"),
  here(econ_data_path, "2002-2018", "ods", "Tabela20.ods") %>% readODS::read_ods(sheet = "Tabela20_1", skip=49) %>% slice(1:17) %>% mutate(state = "Minas Gerais"),
  here(econ_data_path, "2002-2018", "ods", "Tabela21.ods") %>% readODS::read_ods(sheet = "Tabela21_1", skip=49) %>% slice(1:17) %>% mutate(state = "Espirito Santo"),
  here(econ_data_path, "2002-2018", "ods", "Tabela22.ods") %>% readODS::read_ods(sheet = "Tabela22_1", skip=49) %>% slice(1:17) %>% mutate(state = "Rio de Janeiro"),
  here(econ_data_path, "2002-2018", "ods", "Tabela23.ods") %>% readODS::read_ods(sheet = "Tabela23_1", skip=49) %>% slice(1:17) %>% mutate(state = "Sao Paolo"),
  here(econ_data_path, "2002-2018", "ods", "Tabela25.ods") %>% readODS::read_ods(sheet = "Tabela25_1", skip=49) %>% slice(1:17) %>% mutate(state = "Parana"),
  here(econ_data_path, "2002-2018", "ods", "Tabela26.ods") %>% readODS::read_ods(sheet = "Tabela26_1", skip=49) %>% slice(1:17) %>% mutate(state = "Santa Catarina"),
  here(econ_data_path, "2002-2018", "ods", "Tabela27.ods") %>% readODS::read_ods(sheet = "Tabela27_1", skip=49) %>% slice(1:17) %>% mutate(state = "Rio Grande do Sul"),
  here(econ_data_path, "2002-2018", "ods", "Tabela29.ods") %>% readODS::read_ods(sheet = "Tabela29_1", skip=49) %>% slice(1:17) %>% mutate(state = "Mato Grosso do Sul"),
  here(econ_data_path, "2002-2018", "ods", "Tabela30.ods") %>% readODS::read_ods(sheet = "Tabela30_1", skip=49) %>% slice(1:17) %>% mutate(state = "Mato Grosso"),
  here(econ_data_path, "2002-2018", "ods", "Tabela31.ods") %>% readODS::read_ods(sheet = "Tabela31_1", skip=49) %>% slice(1:17) %>% mutate(state = "Goias"),
  here(econ_data_path, "2002-2018", "ods", "Tabela32.ods") %>% readODS::read_ods(sheet = "Tabela32_1", skip=49) %>% slice(1:17) %>% mutate(state = "Distrito Federal"),
) %>% filter(ANO>2004)


ag_value_added_2018 <- bind_rows(
  here(econ_data_path, "2002-2018", "ods", "Tabela2.ods") %>% readODS::read_ods(sheet = "Tabela2_2", skip=49) %>% slice(1:17) %>% mutate(state = "Rondonia"),
  here(econ_data_path, "2002-2018", "ods", "Tabela3.ods") %>% readODS::read_ods(sheet = "Tabela3_2", skip=49) %>% slice(1:17) %>% mutate(state = "Acre"),
  here(econ_data_path, "2002-2018", "ods", "Tabela4.ods") %>% readODS::read_ods(sheet = "Tabela4_2", skip=49) %>% slice(1:17) %>% mutate(state = "Amazonas"),
  here(econ_data_path, "2002-2018", "ods", "Tabela5.ods") %>% readODS::read_ods(sheet = "Tabela5_2", skip=49) %>% slice(1:17) %>% mutate(state = "Roraima"),
  here(econ_data_path, "2002-2018", "ods", "Tabela6.ods") %>% readODS::read_ods(sheet = "Tabela6_2", skip=49) %>% slice(1:17) %>% mutate(state = "Para"),
  here(econ_data_path, "2002-2018", "ods", "Tabela7.ods") %>% readODS::read_ods(sheet = "Tabela7_2", skip=49) %>% slice(1:17) %>% mutate(state = "Amapa"),
  here(econ_data_path, "2002-2018", "ods", "Tabela8.ods") %>% readODS::read_ods(sheet = "Tabela8_2", skip=49) %>% slice(1:17) %>% mutate(state = "Tocantins"),
  here(econ_data_path, "2002-2018", "ods", "Tabela10.ods") %>% readODS::read_ods(sheet = "Tabela10_2", skip=49) %>% slice(1:17) %>% mutate(state = "Maranhao"),
  here(econ_data_path, "2002-2018", "ods", "Tabela11.ods") %>% readODS::read_ods(sheet = "Tabela11_2", skip=49) %>% slice(1:17) %>% mutate(state = "Piaui"),
  here(econ_data_path, "2002-2018", "ods", "Tabela12.ods") %>% readODS::read_ods(sheet = "Tabela12_2", skip=49) %>% slice(1:17) %>% mutate(state = "Ceara"),
  here(econ_data_path, "2002-2018", "ods", "Tabela13.ods") %>% readODS::read_ods(sheet = "Tabela13_2", skip=49) %>% slice(1:17) %>% mutate(state = "Rio Grande do Norte"),
  here(econ_data_path, "2002-2018", "ods", "Tabela14.ods") %>% readODS::read_ods(sheet = "Tabela14_2", skip=49) %>% slice(1:17) %>% mutate(state = "Paraiba"),
  here(econ_data_path, "2002-2018", "ods", "Tabela15.ods") %>% readODS::read_ods(sheet = "Tabela15_2", skip=49) %>% slice(1:17) %>% mutate(state = "Pernambuco"),
  here(econ_data_path, "2002-2018", "ods", "Tabela16.ods") %>% readODS::read_ods(sheet = "Tabela16_2", skip=49) %>% slice(1:17) %>% mutate(state = "Alagoas"),
  here(econ_data_path, "2002-2018", "ods", "Tabela17.ods") %>% readODS::read_ods(sheet = "Tabela17_2", skip=49) %>% slice(1:17) %>% mutate(state = "Sergipe"),
  here(econ_data_path, "2002-2018", "ods", "Tabela18.ods") %>% readODS::read_ods(sheet = "Tabela18_2", skip=49) %>% slice(1:17) %>% mutate(state = "Bahia"),
  here(econ_data_path, "2002-2018", "ods", "Tabela20.ods") %>% readODS::read_ods(sheet = "Tabela20_2", skip=49) %>% slice(1:17) %>% mutate(state = "Minas Gerais"),
  here(econ_data_path, "2002-2018", "ods", "Tabela21.ods") %>% readODS::read_ods(sheet = "Tabela21_2", skip=49) %>% slice(1:17) %>% mutate(state = "Espirito Santo"),
  here(econ_data_path, "2002-2018", "ods", "Tabela22.ods") %>% readODS::read_ods(sheet = "Tabela22_2", skip=49) %>% slice(1:17) %>% mutate(state = "Rio de Janeiro"),
  here(econ_data_path, "2002-2018", "ods", "Tabela23.ods") %>% readODS::read_ods(sheet = "Tabela23_2", skip=49) %>% slice(1:17) %>% mutate(state = "Sao Paolo"),
  here(econ_data_path, "2002-2018", "ods", "Tabela25.ods") %>% readODS::read_ods(sheet = "Tabela25_2", skip=49) %>% slice(1:17) %>% mutate(state = "Parana"),
  here(econ_data_path, "2002-2018", "ods", "Tabela26.ods") %>% readODS::read_ods(sheet = "Tabela26_2", skip=49) %>% slice(1:17) %>% mutate(state = "Santa Catarina"),
  here(econ_data_path, "2002-2018", "ods", "Tabela27.ods") %>% readODS::read_ods(sheet = "Tabela27_2", skip=49) %>% slice(1:17) %>% mutate(state = "Rio Grande do Sul"),
  here(econ_data_path, "2002-2018", "ods", "Tabela29.ods") %>% readODS::read_ods(sheet = "Tabela29_2", skip=49) %>% slice(1:17) %>% mutate(state = "Mato Grosso do Sul"),
  here(econ_data_path, "2002-2018", "ods", "Tabela30.ods") %>% readODS::read_ods(sheet = "Tabela30_2", skip=49) %>% slice(1:17) %>% mutate(state = "Mato Grosso"),
  here(econ_data_path, "2002-2018", "ods", "Tabela31.ods") %>% readODS::read_ods(sheet = "Tabela31_2", skip=49) %>% slice(1:17) %>% mutate(state = "Goias"),
  here(econ_data_path, "2002-2018", "ods", "Tabela32.ods") %>% readODS::read_ods(sheet = "Tabela32_2", skip=49) %>% slice(1:17) %>% mutate(state = "Distrito Federal"),
) %>% filter(ANO>2004)

extr_ind_value_added_2018 <- bind_rows(
  here(econ_data_path, "2002-2018", "ods", "Tabela2.ods") %>% readODS::read_ods(sheet = "Tabela2_3", skip=49) %>% slice(1:17) %>% mutate(state = "Rondonia"),
  here(econ_data_path, "2002-2018", "ods", "Tabela3.ods") %>% readODS::read_ods(sheet = "Tabela3_3", skip=49) %>% slice(1:17) %>% mutate(state = "Acre"),
  here(econ_data_path, "2002-2018", "ods", "Tabela4.ods") %>% readODS::read_ods(sheet = "Tabela4_3", skip=49) %>% slice(1:17) %>% mutate(state = "Amazonas"),
  here(econ_data_path, "2002-2018", "ods", "Tabela5.ods") %>% readODS::read_ods(sheet = "Tabela5_3", skip=49) %>% slice(1:17) %>% mutate(state = "Roraima"),
  here(econ_data_path, "2002-2018", "ods", "Tabela6.ods") %>% readODS::read_ods(sheet = "Tabela6_3", skip=49) %>% slice(1:17) %>% mutate(state = "Para"),
  here(econ_data_path, "2002-2018", "ods", "Tabela7.ods") %>% readODS::read_ods(sheet = "Tabela7_3", skip=49) %>% slice(1:17) %>% mutate(state = "Amapa"),
  here(econ_data_path, "2002-2018", "ods", "Tabela8.ods") %>% readODS::read_ods(sheet = "Tabela8_3", skip=49) %>% slice(1:17) %>% mutate(state = "Tocantins"),
  here(econ_data_path, "2002-2018", "ods", "Tabela10.ods") %>% readODS::read_ods(sheet = "Tabela10_3", skip=49) %>% slice(1:17) %>% mutate(state = "Maranhao"),
  here(econ_data_path, "2002-2018", "ods", "Tabela11.ods") %>% readODS::read_ods(sheet = "Tabela11_3", skip=49) %>% slice(1:17) %>% mutate(state = "Piaui"),
  here(econ_data_path, "2002-2018", "ods", "Tabela12.ods") %>% readODS::read_ods(sheet = "Tabela12_3", skip=49) %>% slice(1:17) %>% mutate(state = "Ceara"),
  here(econ_data_path, "2002-2018", "ods", "Tabela13.ods") %>% readODS::read_ods(sheet = "Tabela13_3", skip=49) %>% slice(1:17) %>% mutate(state = "Rio Grande do Norte"),
  here(econ_data_path, "2002-2018", "ods", "Tabela14.ods") %>% readODS::read_ods(sheet = "Tabela14_3", skip=49) %>% slice(1:17) %>% mutate(state = "Paraiba"),
  here(econ_data_path, "2002-2018", "ods", "Tabela15.ods") %>% readODS::read_ods(sheet = "Tabela15_3", skip=49) %>% slice(1:17) %>% mutate(state = "Pernambuco"),
  here(econ_data_path, "2002-2018", "ods", "Tabela16.ods") %>% readODS::read_ods(sheet = "Tabela16_3", skip=49) %>% slice(1:17) %>% mutate(state = "Alagoas"),
  here(econ_data_path, "2002-2018", "ods", "Tabela17.ods") %>% readODS::read_ods(sheet = "Tabela17_3", skip=49) %>% slice(1:17) %>% mutate(state = "Sergipe"),
  here(econ_data_path, "2002-2018", "ods", "Tabela18.ods") %>% readODS::read_ods(sheet = "Tabela18_3", skip=49) %>% slice(1:17) %>% mutate(state = "Bahia"),
  here(econ_data_path, "2002-2018", "ods", "Tabela20.ods") %>% readODS::read_ods(sheet = "Tabela20_3", skip=49) %>% slice(1:17) %>% mutate(state = "Minas Gerais"),
  here(econ_data_path, "2002-2018", "ods", "Tabela21.ods") %>% readODS::read_ods(sheet = "Tabela21_3", skip=49) %>% slice(1:17) %>% mutate(state = "Espirito Santo"),
  here(econ_data_path, "2002-2018", "ods", "Tabela22.ods") %>% readODS::read_ods(sheet = "Tabela22_3", skip=49) %>% slice(1:17) %>% mutate(state = "Rio de Janeiro"),
  here(econ_data_path, "2002-2018", "ods", "Tabela23.ods") %>% readODS::read_ods(sheet = "Tabela23_3", skip=49) %>% slice(1:17) %>% mutate(state = "Sao Paolo"),
  here(econ_data_path, "2002-2018", "ods", "Tabela25.ods") %>% readODS::read_ods(sheet = "Tabela25_3", skip=49) %>% slice(1:17) %>% mutate(state = "Parana"),
  here(econ_data_path, "2002-2018", "ods", "Tabela26.ods") %>% readODS::read_ods(sheet = "Tabela26_3", skip=49) %>% slice(1:17) %>% mutate(state = "Santa Catarina"),
  here(econ_data_path, "2002-2018", "ods", "Tabela27.ods") %>% readODS::read_ods(sheet = "Tabela27_3", skip=49) %>% slice(1:17) %>% mutate(state = "Rio Grande do Sul"),
  here(econ_data_path, "2002-2018", "ods", "Tabela29.ods") %>% readODS::read_ods(sheet = "Tabela29_3", skip=49) %>% slice(1:17) %>% mutate(state = "Mato Grosso do Sul"),
  here(econ_data_path, "2002-2018", "ods", "Tabela30.ods") %>% readODS::read_ods(sheet = "Tabela30_3", skip=49) %>% slice(1:17) %>% mutate(state = "Mato Grosso"),
  here(econ_data_path, "2002-2018", "ods", "Tabela31.ods") %>% readODS::read_ods(sheet = "Tabela31_3", skip=49) %>% slice(1:17) %>% mutate(state = "Goias"),
  here(econ_data_path, "2002-2018", "ods", "Tabela32.ods") %>% readODS::read_ods(sheet = "Tabela32_3", skip=49) %>% slice(1:17) %>% mutate(state = "Distrito Federal"),
) %>% filter(ANO>2004)

transInd_value_added_2018 <- bind_rows(
  here(econ_data_path, "2002-2018", "ods", "Tabela2.ods") %>% readODS::read_ods(sheet = "Tabela2_4", skip=49) %>% slice(1:17) %>% mutate(state = "Rondonia"),
  here(econ_data_path, "2002-2018", "ods", "Tabela3.ods") %>% readODS::read_ods(sheet = "Tabela3_4", skip=49) %>% slice(1:17) %>% mutate(state = "Acre"),
  here(econ_data_path, "2002-2018", "ods", "Tabela4.ods") %>% readODS::read_ods(sheet = "Tabela4_4", skip=49) %>% slice(1:17) %>% mutate(state = "Amazonas"),
  here(econ_data_path, "2002-2018", "ods", "Tabela5.ods") %>% readODS::read_ods(sheet = "Tabela5_4", skip=49) %>% slice(1:17) %>% mutate(state = "Roraima"),
  here(econ_data_path, "2002-2018", "ods", "Tabela6.ods") %>% readODS::read_ods(sheet = "Tabela6_4", skip=49) %>% slice(1:17) %>% mutate(state = "Para"),
  here(econ_data_path, "2002-2018", "ods", "Tabela7.ods") %>% readODS::read_ods(sheet = "Tabela7_4", skip=49) %>% slice(1:17) %>% mutate(state = "Amapa"),
  here(econ_data_path, "2002-2018", "ods", "Tabela8.ods") %>% readODS::read_ods(sheet = "Tabela8_4", skip=49) %>% slice(1:17) %>% mutate(state = "Tocantins"),
  here(econ_data_path, "2002-2018", "ods", "Tabela10.ods") %>% readODS::read_ods(sheet = "Tabela10_4", skip=49) %>% slice(1:17) %>% mutate(state = "Maranhao"),
  here(econ_data_path, "2002-2018", "ods", "Tabela11.ods") %>% readODS::read_ods(sheet = "Tabela11_4", skip=49) %>% slice(1:17) %>% mutate(state = "Piaui"),
  here(econ_data_path, "2002-2018", "ods", "Tabela12.ods") %>% readODS::read_ods(sheet = "Tabela12_4", skip=49) %>% slice(1:17) %>% mutate(state = "Ceara"),
  here(econ_data_path, "2002-2018", "ods", "Tabela13.ods") %>% readODS::read_ods(sheet = "Tabela13_4", skip=49) %>% slice(1:17) %>% mutate(state = "Rio Grande do Norte"),
  here(econ_data_path, "2002-2018", "ods", "Tabela14.ods") %>% readODS::read_ods(sheet = "Tabela14_4", skip=49) %>% slice(1:17) %>% mutate(state = "Paraiba"),
  here(econ_data_path, "2002-2018", "ods", "Tabela15.ods") %>% readODS::read_ods(sheet = "Tabela15_4", skip=49) %>% slice(1:17) %>% mutate(state = "Pernambuco"),
  here(econ_data_path, "2002-2018", "ods", "Tabela16.ods") %>% readODS::read_ods(sheet = "Tabela16_4", skip=49) %>% slice(1:17) %>% mutate(state = "Alagoas"),
  here(econ_data_path, "2002-2018", "ods", "Tabela17.ods") %>% readODS::read_ods(sheet = "Tabela17_4", skip=49) %>% slice(1:17) %>% mutate(state = "Sergipe"),
  here(econ_data_path, "2002-2018", "ods", "Tabela18.ods") %>% readODS::read_ods(sheet = "Tabela18_4", skip=49) %>% slice(1:17) %>% mutate(state = "Bahia"),
  here(econ_data_path, "2002-2018", "ods", "Tabela20.ods") %>% readODS::read_ods(sheet = "Tabela20_4", skip=49) %>% slice(1:17) %>% mutate(state = "Minas Gerais"),
  here(econ_data_path, "2002-2018", "ods", "Tabela21.ods") %>% readODS::read_ods(sheet = "Tabela21_4", skip=49) %>% slice(1:17) %>% mutate(state = "Espirito Santo"),
  here(econ_data_path, "2002-2018", "ods", "Tabela22.ods") %>% readODS::read_ods(sheet = "Tabela22_4", skip=49) %>% slice(1:17) %>% mutate(state = "Rio de Janeiro"),
  here(econ_data_path, "2002-2018", "ods", "Tabela23.ods") %>% readODS::read_ods(sheet = "Tabela23_4", skip=49) %>% slice(1:17) %>% mutate(state = "Sao Paolo"),
  here(econ_data_path, "2002-2018", "ods", "Tabela25.ods") %>% readODS::read_ods(sheet = "Tabela25_4", skip=49) %>% slice(1:17) %>% mutate(state = "Parana"),
  here(econ_data_path, "2002-2018", "ods", "Tabela26.ods") %>% readODS::read_ods(sheet = "Tabela26_4", skip=49) %>% slice(1:17) %>% mutate(state = "Santa Catarina"),
  here(econ_data_path, "2002-2018", "ods", "Tabela27.ods") %>% readODS::read_ods(sheet = "Tabela27_4", skip=49) %>% slice(1:17) %>% mutate(state = "Rio Grande do Sul"),
  here(econ_data_path, "2002-2018", "ods", "Tabela29.ods") %>% readODS::read_ods(sheet = "Tabela29_4", skip=49) %>% slice(1:17) %>% mutate(state = "Mato Grosso do Sul"),
  here(econ_data_path, "2002-2018", "ods", "Tabela30.ods") %>% readODS::read_ods(sheet = "Tabela30_4", skip=49) %>% slice(1:17) %>% mutate(state = "Mato Grosso"),
  here(econ_data_path, "2002-2018", "ods", "Tabela31.ods") %>% readODS::read_ods(sheet = "Tabela31_4", skip=49) %>% slice(1:17) %>% mutate(state = "Goias"),
  here(econ_data_path, "2002-2018", "ods", "Tabela32.ods") %>% readODS::read_ods(sheet = "Tabela32_4", skip=49) %>% slice(1:17) %>% mutate(state = "Distrito Federal"),
) %>% filter(ANO>2004)

value_added_2018 <- tot_value_added_2018 %>% select(year=ANO, Total_value_added = `VALOR A PREÇO CORRENTE`, state) %>% 
  left_join(ag_value_added_2018 %>% select(year=ANO, Agricultural_value_added = `VALOR A PREÇO CORRENTE`, state)) %>% 
  left_join(extr_ind_value_added_2018 %>% select(year=ANO, Extr_Ind_value_added = `VALOR A PREÇO CORRENTE`, state)) %>% 
  left_join(transInd_value_added_2018 %>% select(year=ANO, Trans_Ind_value_added = `VALOR A PREÇO CORRENTE`, state)) %>% 
  mutate(share_ag_value_added = (Agricultural_value_added/Total_value_added)*100, 
         share_extr_ind_value_added = (Extr_Ind_value_added/Total_value_added)*100,
         share_trans_ind_value_added = (Trans_Ind_value_added/Total_value_added)*100) %>% 
  left_join(as.data.frame(
      cbind(FullName = c("Acre","Alagoas","Amazonas","Amapa","Bahia","Ceara","Distrito Federal","Espirito Santo","Goias","Maranhao","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Para","Paraiba","Parana","Pernambuco","Piaui","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondonia","Roraima","Santa Catarina","Sao Paulo","Sergipe","Tocantins"),
            Abbreviation = c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO"))),
    by = c("state" = "FullName")
  ) %>% as_tibble() %>% mutate(year=year %>% as.double())

value_added <- bind_rows(value_added_2004, value_added_2018) %>% mutate(FullName=state,state=Abbreviation) %>% select(-Abbreviation)


# joining everything
complete_df <- PA_data %>% left_join(EFT_df) %>% mutate(arpa = case_when(state %in% arpa ~ 1, !state %in% arpa ~0)) %>% left_join(biomas) %>% left_join(pop_dens_data) %>% left_join(value_added %>% select(state,year,starts_with("share")),by = c("year", "state"))

# write_out
save(complete_df, file="C://Users//Nils//Box//papers//work_in_progress//Cooperman_etal_2021_EqualityEFT//repo//data//completed_sets//State_panel_data.RData")

# minimal constant to logarithmize 0 values
const<-min(complete_df$mun[which(complete_df$mun > 0)])*0.5
const2<-min(complete_df$sta[which(complete_df$sta > 0)])*0.5

# create full dataset
full_df <- left_join(PA_df, EFT) %>% mutate(lnMun=log(mun+const), lnSta=log(sta+const2), lnFed=log(fed), lnTot=log(tot), lnAg=log(agr), lnInd=log(ind), lnPop=log(pop), lnInc=log(inc), year=as.integer(year), legislation=as.integer(legislation), enactment=as.integer(enactment))
