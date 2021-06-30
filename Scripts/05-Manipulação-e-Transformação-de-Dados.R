# --------------------------------------------------- 
# 05 - Manipulação e Transformação de Dados 
# 22 jun 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar pacotes  -------------------------------------------
library(tidyverse)

# Solução da tarefa da semana passada  -------------------------------------------
# Carregar os dados atribuindo os tipos adequados às variáveis
tadpoles_raw <- read_csv(file = "tadpoles_raw.csv", 
                         col_names = TRUE, 
                         na = c("NA"),
                         col_types = cols(Date = col_date(format = "%m/%d/%y"),
                                          Stream = col_factor(),
                                          Species = col_factor(),
                                          `Stage of development` = col_number(),
                                          `Depth (cm)` = col_number()))

# Filtrar selecionar, renomear e abreviar o nome das espécies e gerar categorias para os estágios
tadpoles_raw <- tadpoles_raw %>% 
  filter(Method == "Dipnet") %>% 
  select(date = Date, 
         stream = Stream, 
         species = Species,
         stage = `Stage of development`,
         total_len = `Total Length (mm)`, 
         body_len = `Body Length (mm)`) %>% 
  mutate(species = as.factor(str_replace_all(species, 
                                             c("Aplastodiscus eugenioi" = "Ae", 
                                               "Bokermannohyla circumdata" = "Bc",
                                               "Crossodactylus aeneus" = "Ca",
                                               "Hylodes asper" = "Ha",
                                               "Hylodes charadranaetes" = "Hc",
                                               "Hylodes lateristrigatus" = "Hl",
                                               "Hylodes pipilans" = "Hp",
                                               "Physalaemus signifer" = "Ps",
                                               "Proceratophrys appendiculata" = "Pa",              "Proceratophrys boiei" = "Pb",
                                               "Rhinella ornata" = "Ro", 
                                               "Scinax albicans" = "Sa",
                                               "Scinax flavoguttatus" = "Sf"))),
         stage = cut(stage, 
                     breaks = c(25, 30, 35, 40, 42, 46),
                     labels = c("25-30", "31-35", "36-40", "41-42", ">42"),
                     include.lowest = TRUE))

# Corrigir os valores fora da amplitude esperada de variação
tadpoles_raw$total_len[which(
  tadpoles_raw$total_len > 100
)] <- tadpoles_raw$total_len[which(
  tadpoles_raw$total_len > 100)] /100

tadpoles_clean <- tadpoles_raw

# Transformar o código na função read_tadpoles_raw()  -------------------------------------------
read_tadpoles_raw <- function(data) {
  # carregar pacote
  library(tidyverse)
  # carregar dados atribuindo os tipos corretos
  tadpoles_raw <- read_csv(file = data, 
                           col_names = TRUE, 
                           na = c("NA"),
                           col_types = cols(Date = col_date(format = "%m/%d/%y"),
                                            Stream = col_factor(),
                                            Species = col_factor(),
                                            `Stage of development` = col_number(),
                                            `Depth (cm)` = col_number()))
  # manipular e limpar dados
  tadpoles_raw <- tadpoles_raw %>% 
    filter(Method == "Dipnet") %>% # filtrar subconjunto de obs de interesse
    select(date = Date, 
           stream = Stream, 
           species = Species,
           stage = `Stage of development`,
           total_len = `Total Length (mm)`, 
           body_len = `Body Length (mm)`) %>% # selecionar ariáveis de interesse
    mutate(species = as.factor(str_replace_all(species, # Renomear espécies e converter novamente em fator
                                               coll(c("Aplastodiscus eugenioi" = "Ae", 
                                                      "Bokermannohyla circumdata" = "Bc",
                                                      "Crossodactylus aeneus" = "Ca",
                                                      "Hylodes asper" = "Ha",
                                                      "Hylodes charadranaetes" = "Hc",
                                                      "Hylodes lateristrigatus" = "Hl",
                                                      "Hylodes pipilans" = "Hp",
                                                      "Physalaemus signifer" = "Ps",
                                                      "Proceratophrys appendiculata" = "Pa",              "Proceratophrys boiei" = "Pb",
                                                      "Rhinella ornata" = "Ro", 
                                                      "Scinax albicans" = "Sa",
                                                      "Scinax flavoguttatus" = "Sf")))),
           stage = cut(stage, 
                       breaks = c(25, 30, 35, 40, 42, 46),
                       labels = c("25-30", "31-35", "36-40", "41-42", ">42"),
                       include.lowest = TRUE))
  # corrigindo valores fora da amplitude de variação esperada
  tadpoles_raw$total_len[which(
    tadpoles_raw$total_len > 100
  )] <- tadpoles_raw$total_len[which(
    tadpoles_raw$total_len > 100)] /100
  # retornar os dados limpos
  return(tadpoles_raw)
}

# Carregar e inspecionar os dados -------------------------------------------
# carregar dados
tadpoles_clean <- read_tadpoles_raw("tadpoles_raw.csv")

# inspecionar dados
glimpse(tadpoles_clean)

# Frequência absoluta e relativa de girinos  -------------------------------------------
tadpoles_freq <- tadpoles_clean %>%  
  count(species) %>% 
  mutate(freq_rel = round(n/sum(n), 3)) %>% 
  drop_na()

tadpoles_freq

# Frequência absoluta e relativa de girinos por riachos  -------------------------------------------
tadpoles_freq_stream <- tadpoles_clean %>% 
  group_by(stream, species) %>% 
  summarise(n = n()) %>% 
  mutate(freq_rel = round(n/sum(n), 3)) %>% 
  drop_na()

tadpoles_freq_stream

# Frequência absoluta e relativa de girinos por riacho e por mês  -------------------------------------------
tadpoles_freq_date_stream <- tadpoles_clean %>% 
  group_by(date = zoo::as.yearmon(date), stream, species) %>% 
  summarise(n = n()) %>% 
  mutate(freq_rel = round(n/sum(n), 3)) %>% 
  drop_na()

tadpoles_freq_date_stream

# Número de espécies por riacho  -------------------------------------------
tadpoles_rich_stream <- tadpoles_freq_stream %>% 
  count(stream, name = "n_sp")

tadpoles_rich_stream

# solução alternativa
tadpoles_rich_stream <- tadpoles_freq_stream %>% 
  mutate(dummy = rep(1, length(species))) %>% 
  group_by(stream) %>% 
  summarise(n_sp = sum(dummy))

tadpoles_rich_stream

# Número de espécies por mês por riacho
tadpoles_rich_date_stream <- tadpoles_freq_date_stream %>% 
  count(date, stream, name = "n_sp")

tadpoles_rich_date_stream

# solução alternativa
tadpoles_freq_date_stream %>%
  group_by(date) %>% 
  count(stream, name = "n_sp")

# solução alternativa
tadpoles_freq_date_stream %>% 
  mutate(dummy = rep(1, length(species))) %>% 
  group_by(date, stream) %>% 
  summarise(n_sp = sum(dummy))

# Número de girinos por estágio e por espécie  -------------------------------------------
tadpoles_stage_sp <- tadpoles_clean %>% 
  count(species, stage) %>% 
  drop_na()

tadpoles_stage_sp

# solução alternativa
tadpoles_clean %>% 
  group_by(species, stage) %>% 
  count(species) %>% 
  drop_na()

# Tamanho do corpo, total, razão corpo:total e estágio por espécie  -------------------------------------------
tadpoles_len_sp <- tadpoles_clean %>% 
  select(date, species, stage, body_len, total_len) %>% 
  mutate(date = zoo::as.yearmon(date),
         ratio = body_len/total_len)

tadpoles_len_sp

# Estatística descritiva do tamanho do corpo dos girinos  -------------------------------------------
body_len_decription <- tadpoles_clean %>% 
  group_by(species) %>% 
  summarise(media = mean(body_len, na.rm = TRUE),
            desvpad = sd(body_len, na.rm = TRUE),
            min = quantile(body_len, na.rm = TRUE)[1],
            qrt2 = quantile(body_len, na.rm = TRUE)[2],
            mediana = quantile(body_len, na.rm = TRUE)[3],
            qrt4 = quantile(body_len, na.rm = TRUE)[4],
            maximo = quantile(body_len, na.rm = TRUE)[5], 
            n = n()) %>% 
  drop_na()

body_len_decription

# Estatística descritiva do tamanho total dos girinos  -------------------------------------------
total_len_decription <- tadpoles_clean %>% 
  group_by(species) %>% 
  summarise(media = mean(total_len, na.rm = TRUE),
            desvpad = sd(total_len, na.rm = TRUE),
            min = quantile(total_len, na.rm = TRUE)[1],
            qrt2 = quantile(total_len, na.rm = TRUE)[2],
            mediana = quantile(total_len, na.rm = TRUE)[3],
            qrt4 = quantile(total_len, na.rm = TRUE)[4],
            maximo = quantile(total_len, na.rm = TRUE)[5], 
            n = n()) %>% 
  drop_na()

total_len_decription

# Estatística descritiva da frequência de girinos por espécie  -------------------------------------------
freq_abs_decription <- tadpoles_freq_stream %>% 
  group_by(species) %>% 
  summarise(media = mean(n),
            desvpad = sd(n),
            min = quantile(n)[1],
            qrt2 = quantile(n)[2],
            mediana = quantile(n)[3],
            qrt4 = quantile(n)[4],
            maximo = quantile(n)[5], 
            n = n())

freq_abs_decription

# Estatística descritiva do número total de espécies  -------------------------------------------
tadpoles_rich_decription <- tadpoles_rich_stream %>% 
  summarise(media = mean(n_sp),
            desvpad = sd(n_sp),
            min = quantile(n_sp)[1],
            qrt2 = quantile(n_sp)[2],
            mediana = quantile(n_sp)[3],
            qrt4 = quantile(n_sp)[4],
            maximo = quantile(n_sp)[5], 
            n = n())

tadpoles_rich_decription

# Estatística descritiva do número de espécies por riacho  -------------------------------------------
tadpoles_rich_stream_decription <- tadpoles_rich_date_stream %>%   
  group_by(stream) %>% 
  summarise(media = mean(n_sp),
            desvpad = sd(n_sp),
            min = quantile(n_sp)[1],
            qrt2 = quantile(n_sp)[2],
            mediana = quantile(n_sp)[3],
            qrt4 = quantile(n_sp)[4],
            maximo = quantile(n_sp)[5], 
            n = n())

tadpoles_rich_stream_decription

