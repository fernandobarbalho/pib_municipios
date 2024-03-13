graf1<-
  mapa_sp %>%
  mutate(municipio_codigo = as.character(code_muni)) %>%
  inner_join(
    pib_trabalho %>%
      filter(ano==2021)
  ) %>%
  ggplot() +
  geom_sf(aes(fill=perc)) +
  scale_fill_continuous_sequential(palette = "Heat 2") +
  theme_void() +
  labs(
    title = "Distribuição do PIB de São Paulo",
    subtitle = "Ano referência: 2021",
    fill = "(%) PIB de SP",
    caption = "Fonte: IBGE"
  )


graf1<-
  mapa_sp %>%
  mutate(municipio_codigo = as.character(code_muni)) %>%
  inner_join(
    pib_trabalho %>%
      filter(ano==2021)
  ) %>%
  ggplot() +
  geom_sf(aes(fill=perc)) +
  scale_fill_continuous_sequential(palette = "Heat 2") +
  theme_void() +
  labs(
    title = "Distribuição do PIB de São Paulo",
    subtitle = "Todos municípios do Estado",
    fill = "(%) PIB de SP",
    caption = "Ano referência: 2021. Fonte: IBGE"
  )+
  theme(
    panel.background = element_rect(fill = "black")
  )



graf2<-
  mapa_sp %>%
  mutate(municipio_codigo = as.character(code_muni)) %>%
  #filter(municipio_codigo != "3550308") %>%
  inner_join(
    pib_trabalho %>%
      filter(ano==2021) %>%
      mutate(perc = ifelse(municipio_codigo == "3550308",NA,perc))
  ) %>%
  ggplot() +
  geom_sf(aes(fill=perc)) +
  scale_fill_continuous_sequential(palette = "Heat 2", na.value = "lightgray") +
  theme_void() +
  labs(
    title = "Distribuição do PIB de São Paulo",
    subtitle = "Todos municípios do Estado menos a capital",
    fill = "(%) PIB de SP",
    caption = "Ano referência: 2021. Fonte: IBGE"
  ) +
  theme(
    panel.background = element_rect(fill = "black")
  )


graf1

graf2


# Definindo os intervalos
intervalos <- c(0, 2000, 5000, 10000, 20000, 50000, 100000, 500000, Inf)
nomes_faixas <- c("até 2.000", "2.001 a 5.000", "5.001 a 10.000", "10.001 a 20.000", "20.001 a 50.000", "50.001 a 100.000", "100.001 a 500.000", "> 500.000")

# Adicionando a coluna de faixa populacional
populacao_municipios_1991_2022$faixa_populacional <- cut(populacao_municipios_1991_2022$populacao, breaks = intervalos, labels = nomes_faixas, right = FALSE)

# Visualizando os dados
print(dados)


populacao_municipios_1991_2022 %>%
  mutate(municipio_codigo = as.character(id_municipio),
         ano = as.character(ano)) %>%
  inner_join(pib_trabalho) %>%
  summarise(soma_perc = sum(perc),
            .by= c(ano,faixa_populacional )) %>%
  ggplot()+
  geom_col(aes(x=ano, y= soma_perc)) +
  facet_wrap(faixa_populacional~., scales = "free_y") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank()
  ) +
  labs(
    title = "Evolução da soma dos percentuais de PIB",
    subtitle = "Municípios de São Paulo por faixas de população",
    y= "soma do percentual do PIB"
  )

