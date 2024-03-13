library(sidrar)
library(tidyverse)
library(geobr)
library(colorspace)
library(patchwork)

pib_municipios_sp<- get_sidra(x = 5938, 
                           geo= "City",
                           geo.filter = list("State" = 35),
                           variable = 37,
                           period = as.character(2012:2021))



pib_municipios_sp <- janitor::clean_names(pib_municipios_sp)






pib_total_sp<- 
  pib_municipios_sp |>
  summarise(total_ano_sp = sum(valor),
            .by = ano)

pib_total_sp %>%
  saveRDS("pib_total_sp.rds")


populacao_municipios_1991_2022 <- read_csv("populacao_municipios_1991_2022.csv")



mun_pop_entre_50k_100k<-
  (populacao_municipios_1991_2022 %>%
  filter(between(populacao,50001,100000),
         sigla_uf == "SP") %>%
  distinct(id_municipio))$id_municipio

contas_pib_sp<- pib_municipios_sp<- get_sidra(x = 5938, 
                                              geo= "City",
                                              geo.filter = list("City"= mun_pop_entre_50k_100k) ,
                                              period = as.character(2012:2021))


pib_trabalho<-
  pib_municipios_sp %>%
  inner_join(pib_total_sp) %>%
  mutate(perc= (valor/total_ano_sp)*100)

pib_trabalho %>%
  saveRDS("pib_trabalho.rds")

mapa_sp<- 
  geobr::read_municipality(simplified = FALSE) %>%
  filter(abbrev_state == "SP")

mapa_sp %>%
  saveRDS("mapa_sp.rds")


