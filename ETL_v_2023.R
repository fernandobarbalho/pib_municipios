library(sidrar)
library(tidyverse)
library(geobr)
library(colorspace)
library(patchwork)

pib_municipios_sp<- get_sidra(x = 5938, 
                           geo= "City",
                           geo.filter = list("State" = 35),
                           variable = 37,
                           period = as.character(2012:2023))



pib_municipios_sp <- janitor::clean_names(pib_municipios_sp)






pib_total_sp<- 
  pib_municipios_sp |>
  summarise(total_ano_sp = sum(valor),
            .by = ano)

pib_total_sp %>%
  saveRDS("pib_total_sp_2023.rds")


populacao_municipios_1991_2022 <- read_csv("populacao_municipios_1991_2022.csv")



mun_pop_entre_50k_100k<-
  (populacao_municipios_1991_2022 %>%
  filter(between(populacao,50001,100000),
         sigla_uf == "SP") %>%
  distinct(id_municipio))$id_municipio


pib_trabalho<-
  pib_municipios_sp %>%
  inner_join(pib_total_sp) %>%
  mutate(perc= (valor/total_ano_sp)*100)


top_20_50k<-
  (pib_trabalho %>%
     filter(ano%in% c(2023),
            municipio_codigo %in% mun_pop_entre_50k_100k) %>%
     slice_max(order_by = perc, n=20))$municipio_codigo

contas_pib_sp<- pib_municipios_sp<- get_sidra(x = 5938, 
                                              geo= "City",
                                              geo.filter = list("City"= top_20_50k) ,
                                              period = c("2012","2023"))

contas_pib_sp %>%
  saveRDS("contas_pib_sp.rds")


pib_trabalho %>%
  saveRDS("pib_trabalho_2023.rds")

mapa_sp<- 
  geobr::read_municipality(simplified = FALSE) %>%
  filter(abbrev_state == "SP")

mapa_sp %>%
  saveRDS("mapa_sp.rds")


mapa_sp_estado<- 
  geobr::read_state (code_state = 35, simplified = FALSE) 

mapa_sp_estado %>%
  saveRDS("mapa_sp_estado.rds")


mapa_sedes_sp<- 
  geobr::read_municipal_seat()%>%
  filter(abbrev_state == "SP")


mapa_sedes_sp %>%
  saveRDS("mapa_sedes_sp.rds")

