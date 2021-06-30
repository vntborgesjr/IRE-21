# --------------------------------------------------- 
# 06 - Transformação e Visualização de Dados 
# 06 jul 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar pacotes  -------------------------------------------
library(tidyverse)

# OBS: ANTES DE COMEÇAR A EXECUTAR ESSE SCRIPT, ABRA E EXECUTE O SCRIPT DA
# AULA 05 PARA GERAR OS OBJETOS NECESSÁRIOS PARA A EXECUÇÃO DESSE SCRIPT

# Untidy data - carregar dados  -------------------------------------------
streams_raw <- readxl::read_xlsx("streams.xlsx")

# Formato largo para longo - gather()  -------------------------------------------
gather(data = streams_raw, 
       key = "original_width", 
       value = "width", 
       Width.1:Width.5)

gather(data = streams_raw, 
       key = "original_depth", 
       value = "depth", 
       Depth.1:Depth.25)

# tidy sintaxe
streams_raw %>% 
  select(date = Data, stream = Stream, Width.1:Width.5) %>% 
  gather(key = "original_width", value = "width", Width.1:Width.5)

streams_raw %>% 
  select(date = Data, stream = Stream, Depth.1:Depth.25) %>% 
  gather(key = "original_depth", value = "depth", Depth.1:Depth.25)

# com tidy sintaxe fica mais fácil transformar o data frame mais de uma variável de uma vez só
streams_raw %>% 
  select(date = Data, stream = Stream, 
         Width.1:Width.5,
         Depth.1:Depth.25) %>% 
  gather(key = "original_width", 
         value = "width",
         Width.1:Width.5) %>% 
  gather(key = "original_depth", 
         value = "depth", 
         Depth.1:Depth.25)

# Gerar novo data frame longo  -------------------------------------------
streams_raw2 <- streams_raw %>% 
  select(date = Data, stream = Stream, 
         Width.1:Width.5,
         Depth.1:Depth.25) %>% 
  gather(key = "original_width", 
         value = "width",
         Width.1:Width.5) %>% 
  gather(key = "original_depth", 
         value = "depth", 
         Depth.1:Depth.25) %>% 
  mutate(date = zoo::as.yearmon(date))

streams_raw2

# Separar uma coluna em duas  -------------------------------------------
# separar original_width
streams_raw2 %>% 
  separate(col = original_width, 
           into = c("original_width", "width_obs"))

# separar original_detph
streams_raw2 %>% 
  separate(col = "original_depth", 
           into = c("original_depth", "depth_obs"))

# separar as duas colunas ao mesmo tempo
streams_raw2 %>% 
  separate(col = "original_width", 
           into = c("original_width", "width_obs")) %>% 
  separate(col = "original_depth", 
           into = c("original_depth", "depth_obs"))

# Formato longo para largo  -------------------------------------------
# Frequencia das espécies por riacho
tadpoles_community <- tadpoles_freq_stream %>%
  select(stream, species, n) %>% 
  group_by(stream) %>% 
  spread(key = species, 
         value =  n,
         fill = 0)

tadpoles_community

# Frequência das espécies por mês por riacho
tadpoles_community_date <- tadpoles_freq_date_stream %>%
  select(date, stream, species, n) %>% 
  group_by(date, stream) %>% 
  spread(key = species, 
         value =  n,
         fill = 0)

tadpoles_community_date

# Unindo dois data frames com inner_join()  -------------------------------------------
# carregar dados
streams_variables <- read_csv("streams_variables.csv",
                              col_types = cols(
                                stream = col_factor()
                              ))

streams_date_variables <- read_csv("streams_date_variables.csv",
                                   col_types = cols(
                                     stream = col_factor()
                                   ))

# transformar tipo da data
streams_date_variables <- streams_date_variables %>% 
  mutate(date = zoo::as.yearmon(date)) 

# Unindo riqueza de espécies por riacho com variáveis ambientais por raicho
tadpoles_rich_stream %>% 
  inner_join(streams_variables, 
             by = c("stream" = "stream"))

# Unindo riqueza de espécies por data riacho com variáveis ambientais por raicho
tadpoles_rich_date_stream %>% 
  inner_join(streams_date_variables,
             by = c("date" = "date", "stream" = "stream"))

# ggplot2 - grmática gráfica em R  -------------------------------------------
tadpoles_len_sp %>%  
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len)) +
  geom_point()

# Localizando e corrigindo os erros  -------------------------------------------
# Lcalizando erros em Ca e Hc
tadpoles_len_sp %>% 
  filter(species == "Ca" | species == "Hc", body_len > 30)

# Corrigindo erros
# Resolvendo valores trocados em HC
tadpoles_len_sp$total_len[which(
  tadpoles_len_sp$species == "Hc" &
    tadpoles_len_sp$total_len == 9.31
)] <- 32.64
tadpoles_len_sp$body_len[which(
  tadpoles_len_sp$species == "Hc" &
    tadpoles_len_sp$body_len == 32.64
)] <- 9.31

# Localizando erros em Sf
tadpoles_len_sp %>% 
  filter(species == "Sf", body_len > 15)

# Corrigindo erro em Sf
# Resolvendo valores trocados em Sf
tadpoles_len_sp$total_len[which(
  tadpoles_len_sp$species == "Sf" &
    tadpoles_len_sp$total_len == 5.52 |
    tadpoles_len_sp$total_len == 6.30
)] <- c(17.73, 15.97)
tadpoles_len_sp$body_len[which(
  tadpoles_len_sp$species == "Sf" &
    tadpoles_len_sp$body_len == 17.73 |
    tadpoles_len_sp$body_len == 15.97
)] <- c(5.52, 6.30)

# Nova inspeção visual  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len)) +
  geom_point() # tudo ok

tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = total_len)) +
  geom_point() # tudo ok

# Adicionando estético color  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage)) + 
  geom_point()

# Adicionando atributo geométrico position  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage)) +
  geom_point(position = "jitter")

# Adicionando estético alpha  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7)) + 
  geom_point(position = "jitter")

# Adicionando estético shape  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7,
             shape = stage)) + 
  geom_point(position = "jitter")

# Adicionando estético size  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7,
             shape = stage,
             size = stage)) + 
  geom_point(position = "jitter")

# Gráfico razoável  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, y = body_len, 
             color = stage, alpha = 0.7)) +
  geom_point(position = position_jitter(0.25,
                                        seed = 136))

# Modificando estético scale_*_*()  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("Species") + 
  scale_y_continuous("Body Length (mm)") + 
  scale_color_discrete("Stage")

# limits
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("Species") + 
  scale_y_continuous("Body Length (mm)",
                     limits = c(0, 30)) +
  scale_color_discrete("Stage")

# breaks
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("Species") + 
  scale_y_continuous("Body Length (mm)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 5)) +
  scale_color_discrete("Stage")

# expand
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("Species") + 
  scale_y_continuous("Body Length (mm)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     expand = c(0, 0)) +
  scale_color_discrete("Stage")

# scale_color_discrete()
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("Species") + 
  scale_y_continuous("Body Length (mm)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     expand = c(0, 0)) +
  scale_color_discrete("Development \nClass",
                       labels = c("I", "II", "III", "IV", "V"))

# usando labs() para renomear estéticos
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len, 
             color = stage, 
             alpha = 0.7)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  labs(x = "Species",
       y = "Body Length (mm)",
       color = "Stage")