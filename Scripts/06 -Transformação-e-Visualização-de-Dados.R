# --------------------------------------------------- 
# 06 - Transformação e Visualização de Dados 
# 06 jul 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar pacotes  -------------------------------------------
library(tidyverse)
source("IRE_functions.R")
tadpoles_clean <- read_tadpoles_raw("/home/cla/Documentos/Vitor/Laboratórios/UNIRIO/Disciplinas/Introdução-ao-R-para-Ecólogos/IRE-21/Dados/tadpoles_raw.csv")

# OBS: ANTES DE COMEÇAR A EXECUTAR ESSE SCRIPT, ABRA E EXECUTE O SCRIPT DA
# AULA 05 PARA GERAR OS OBJETOS NECESSÁRIOS PARA A EXECUÇÃO DESSE SCRIPT

# Untidy data - carregar dados  -------------------------------------------
streams_raw <- readxl::read_xlsx("streams.xlsx")
streams_raw <- readxl::read_xlsx("/home/cla/Documentos/Vitor/Laboratórios/UNIRIO/Disciplinas/Introdução-ao-R-para-Ecólogos/IRE-21/Dados/streams.xlsx")
streams_variables <- read_csv("/home/cla/Documentos/Vitor/Laboratórios/UNIRIO/Disciplinas/Introdução-ao-R-para-Ecólogos/IRE-21/Dados/streams_variables.csv",
                              col_types = cols(
                                stream = col_factor()
                              ))
streams_date_variables <- read_csv("/home/cla/Documentos/Vitor/Laboratórios/UNIRIO/Disciplinas/Introdução-ao-R-para-Ecólogos/IRE-21/Dados/streams_date_variables.csv",
                                   col_types = cols(
                                     stream = col_factor()
                                   ))

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

# Gráfico de barras - geom_bar()  -------------------------------------------
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species) %>% 
  drop_na() %>% 
  ggplot(aes(x = species)) +
  geom_bar()
# Gráfico de barras - geom_col()  -------------------------------------------
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  count(species) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, y = n)) +
  geom_col()

# continuaremos apenas com geom_bar()
# Atributo geométrico fill  -------------------------------------------
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species) %>% 
  drop_na() %>% 
  ggplot(aes(x = species)) +
  geom_bar(fill = "red") # altera a cor das barras

# Atributo geométrico color  -------------------------------------------
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species) %>% 
  drop_na() %>% 
  ggplot(aes(x = species)) +
  geom_bar(fill = "red", 
           color = "black") # altera o contorno das barras

# Mapeamento estético fill  -------------------------------------------
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) + # gera baras empilhadas 
  geom_bar(color = "black") # position = "stack" (default, não precisa especificar)

# Atributo geométrico position  -------------------------------------------
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) +
  geom_bar(color = "black", 
           position = "dodge") # gera barras lado a lado

tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) +
  geom_bar(color = "black",
           position = position_dodge()) # gera o mesmo resultado, mas permite maio controle

tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) +
  geom_bar(color = "black",
           position = position_dodge(width = 0.2),
           alpha = 0.5)

tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) +
  geom_bar(color = "black", 
           position = "fill") # gera barras empilhadas representando proporções

# Atributo geométrico width  -------------------------------------------
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) +
  geom_bar(color = "black",
           width = 0.7, # controla a largura de barras 
           position = "fill")

# Modificando estéticos scale_*_*()  -------------------------------------------
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) +
  geom_bar(color = "black", 
           position = "dodge",
           width = 0.7) +
  scale_x_discrete("Species") + # altera caracterísicas do eixo x
  scale_y_continuous("Number of tadpoles") # altera características do eixo y

tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) +
  geom_bar(color = "black", 
           position = "dodge",
           width = 0.7) +
  scale_x_discrete("Species") + 
  scale_y_continuous("Number of tadpoles",
                     breaks = seq(0, 175, 25)) # controla o intervalo entre valores do eixo

minhas_cores <- c("black", "white", "blue", "green", "yellow", "red", "purple")
tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, fill = stream)) +
  geom_bar(color = "black", 
           position = "dodge",
           width = 0.7) +
  scale_x_discrete("Species") +
  scale_y_continuous("Number of tadpoles", 
                     breaks = seq(0, 175, 25)) +
  scale_fill_discrete("Stream", type = minhas_cores) # controla as cores das barras

tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, fill = stream)) +
  geom_bar(color = "black", 
           position = "dodge",
           width = 0.7) +
  scale_x_discrete("Species") +
  scale_y_continuous("Number of tadpoles", 
                     breaks = seq(0, 175, 25)) +
  scale_fill_brewer("Stream") # gera conres sequênciais

# Histograma geom_hist()  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>%
  drop_na() %>% 
  ggplot(aes(x = body_len)) +  
  geom_histogram()

# Atributo geométrico binwidth  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>%
  drop_na() %>% 
  ggplot(aes(x = body_len)) +  
  geom_histogram(binwidth = 5) # controla a largura da barra

# Atributo geométrico center  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>%
  drop_na() %>% 
  ggplot(aes(x = body_len)) +  
  geom_histogram(binwidth = 1,
                 center = 0.5) # controla local do rótulo no eixo x

# Mapeamento estético fill  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>%
  drop_na() %>% 
  ggplot(aes(x = body_len,
             fill = species)) + # gera histogramas com diferentes cores a partir de uma variável categórica
  geom_histogram(binwidth = 1,
                 center = 0.5)

# Histograma de proporções  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>%
  drop_na() %>% 
  ggplot(aes(x = body_len, 
             y = ..density.., # plota proporções ao em vez de contagens
             fill = species)) +   
  geom_histogram(binwidth = 1,
                 center = 0.5)

# Mapeamento estético scal_*_*()  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>%
  drop_na() %>% 
  ggplot(aes(x = body_len, 
             y = ..density.., 
             fill = species)) +   
  geom_histogram(binwidth = 1,
                 center = 0.5, 
                 alpha = 0.5) +
  scale_x_continuous("Species") + # controla características do eixo x
  scale_y_continuous("Relative frequency") + # controla características do eixo y
  scale_fill_brewer("Species") # controla as cores do estético fill
 
# Gráfico de pontos com uma variável - geom_point()  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len)) +
  geom_point()

# Mapeamento estético color  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species)) + # atribui diferentes cores a partir de uma variável categórica
  geom_point()

# Atributo geométrico position  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species)) +
  geom_point(position = "jitter") # adiciona variação aleatória ao pontos sobrepostos

# Mapeamento estético alpha  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species, 
             alpha = 0.3)) + # atribui transparência aos pontos
  geom_point(position = "jitter")

# Mapeamento estético shape  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species, 
             alpha = 0.3, 
             shape = species)) + # atribui diferentes formas aos pontos de acordo com uma variável categórica
  geom_point(position = "jitter")

# Mapeamento estético size  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species, 
             alpha = 0.3,
             shape = species,
             size = stage)) + # atribui diferentes tamanhos de acordo com uma variável preferencialmente contínua
  geom_point(position = "jitter")

# Modificando esteticos com scale_*_*()  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species, 
             alpha = 0.3,
             shape = species)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("") +
  scale_y_continuous("Body Length (mm)") +
  scale_color_discrete("Species")

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species, 
             alpha = 0.3,
             shape = species)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("") + 
  scale_y_continuous("Body Length (mm)",
                     limits = c(0, 30)) + # atribui limites ao eixo y
  scale_color_discrete("Species")

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species, 
             alpha = 0.3,
             shape = species)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("") + 
  scale_y_continuous("Body Length (mm)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 5)) + # determina intervalo entre valores no eixo y
  scale_color_discrete("Species")

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species, 
             alpha = 0.3,
             shape = species)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("") + 
  scale_y_continuous("Body Length (mm)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     expand = c(0, 0)) +
  scale_color_discrete("Species",
                       labels = c("Hylodes \npipilans", "Proceratophrys \nappendiculata", "Scinax \nflavoguttatus"))
# controla características relacionadas ao estético color

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species, 
             alpha = 0.3,
             shape = species)) +
  geom_point(position = 
               position_jitter(0.25,
                               seed = 136), 
             show.legend = c(alpha = FALSE, shape = FALSE)) +
  labs(x = "", # também pode ser utilizado para controlar características dos eixos
       y = "Body Length (mm)",
       color = "Species") +
  scale_color_discrete(labels = c("Hylodes \npipilans", "Proceratophrys \nappendiculata", "Scinax \nflavoguttatus"))

# Relação entre variáveis geom_point() -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len,
             y = body_len)) +
  geom_point()

# Modificando estéticos  -------------------------------------------
tadpoles_len_sp %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species)) + # atribui diferentes cores de acordo com os níveis de uma variável categórica
  geom_point()

# Sobrepondo geométricos  -------------------------------------------
# gera tabela com as médias de tamanho total e corporal para as três espécies
tadpoles_media <- tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  group_by(species) %>% 
  summarise_all(mean, na.rm = TRUE)

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pb", "Sa")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species)) + 
  geom_point() + 
  geom_point(data = tadpoles_media, # sobrepondo mais uma camada de pontos
             shape = 15,
             size = 8,
             stroke = 2)

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pb", "Sa")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species)) + 
  geom_point() + 
  geom_point(data = tadpoles_media,
             shape = 21,
             size = 8,
             fill = "black", # aterando a aparência dos pontos para gerar destaque
             stroke = 2)

# Atributo geométrico alpha  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pb", "Sa")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species)) + 
  geom_point(alpha = 0.5,
             show.legend = c(alpha = FALSE)) # atribuindo transparência aos pontos

# Atributo geométrico shape  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pb", "Sa")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species)) + 
  geom_point(shape = 1) # altera a forma dos pontos

# Mapeamento estético scale_*_*()  -------------------------------------------

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pb", "Sa")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species)) + 
  geom_point(shape = 1,
             alpha = 0.5) +
  scale_x_continuous("Total length (mm)",
                     breaks = seq(0, round(max(tadpoles_len_sp$total_len,   na.rm = TRUE)), 10)) +
  scale_y_continuous("Body length (mm)",
                     breaks = seq(0, round(max(tadpoles_len_sp$body_len, na.rm = TRUE)), 5)) +
  scale_color_discrete("Species",
                       labels = c("Hylodes \npipilans", "Proceratophrys\nappendiculata", "Scinax \nflavoguttatus"))

# Variação temporal geom_line()  -------------------------------------------
tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  count(time = date, species) %>% 
  group_by(time) %>% 
  summarize(n = sum(n)) %>% 
  ggplot(aes(x = time,
             y = n)) +
  geom_line()

# Mapeamento estético color  -------------------------------------------
tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  count(time = date, species) %>% 
  ggplot(aes(x = time, 
             y = n,
             color = species)) + # gerar diferentes linhas atribuindo diferentes cores de acordo com os níveis de uma variável categórica
  geom_line()

# Mapeamento estético linetype  -------------------------------------------
tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  count(time = date, species) %>% 
  ggplot(aes(x = time, 
             y = n,
             linetype = species)) + # gerar diferentes tipos de linhas de acordo com os níveis de uma variável categórica
  geom_line()

# Mapeamento estético color e linetype  -------------------------------------------
tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  count(time = date, species) %>% 
  ggplot(aes(x = time, 
             y = n,
             color = species, # diferentes cores
             linetype = species)) + # diferentes tipos de linhas
  geom_line()

# Sobrepondo camadas geom_point() e geom_line()  -------------------------------------------
tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  count(time = date, species) %>% 
  ggplot(aes(x = time, 
             y = n,
             color = species,
             linetype = species)) +
  geom_line() +
  geom_point(aes(shape = species)) # adiciona pontos com diferentes formas de acordo com os níveis de uma variável categórica

# Mapeamento estético scale_*_*()  -------------------------------------------
tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  count(time = date, species) %>% 
  ggplot(aes(x = time, 
             y = n,
             color = species,
             linetype = species)) +  
  geom_line() +
  geom_point(aes(shape = species)) + 
  scale_x_continuous("Time") + # controla características do eixo x
  scale_y_continuous("Number of tadpoles",
                     breaks = seq(0, 40, 5)) # controla caracteristicas do eixo y

# Camada themes()  -------------------------------------------
# Gráfico base para alterações
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species",
                       labels = c(Hp = "Hylodes \npipilans",
                                  Pa = "Proceratophrys \nappendiculata",
                                  Sf = "Scinax \nflavoguttatus"))

# Alterando características do texto  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)",
       y = "Body length (mm)") +
  scale_color_discrete("Species") +
  theme(text = element_text(family = "Serif"), #<< altera a fonte de todo o texto presente no gráfico
        axis.title = element_text(size = 24, #<< altera o tamaanho da fonte do título dos eixos
                                  color = "blue")) #<< altera a cor da fonte

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species",
                       labels = c(Hp = "Hylodes \npipilans",
                                  Pa = "Proceratophrys \nappendiculata",
                                  Sf = "Scinax \nflavoguttatus")) +
  theme(text = element_text(family = "Serif"), 
        axis.title = element_text(size = 24,
                                  color = "blue"),
        axis.text = element_text(size = 20)) #<< altera o tamanho da fonte dos rótulos dos eixos

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species",
                       labels = c(Hp = "Hylodes \npipilans",
                                  Pa = "Proceratophrys \nappendiculata",
                                  Sf = "Scinax \nflavoguttatus")) +
  theme(text = element_text(family = "Serif"), 
        axis.title = element_text(size = 24,
                                  color = "blue"),
        axis.text = element_text(size = 20), 
        legend.title = element_text(size = 24, #<< altara o tamanho da fonte do título da legenda
                                    color = "blue"),
        legend.text = element_text(size = 20, #<< altera o tamanho da fonte do texto da legenda
                                   face = "italic")) #<< transforma o texto da legenda em itálico

# Elementos de linha  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species",
                       labels = c(Hp = "Hylodes \npipilans",
                                  Pa = "Proceratophrys \nappendiculata",
                                  Sf = "Scinax \nflavoguttatus")) +
  theme(text = element_text(family = "Serif"), 
        axis.title = element_text(size = 24,
                                  color = "blue"),
        axis.text = element_text(size = 20), 
        legend.title = element_text("Species", #<<
                                    size = 24, #<<
                                    color = "blue"),
        legend.text = element_text(size = 20,
                                   face = "italic"),
        axis.line = element_line()) #<< gera as linhas dos eixos x e y

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species",
                       labels = c(Hp = "Hylodes \npipilans",
                                  Pa = "Proceratophrys \nappendiculata",
                                  Sf = "Scinax \nflavoguttatus")) +
  theme(text = element_text(family = "Serif"), 
        axis.title = element_text(size = 24,
                                  color = "blue"),
        axis.text = element_text(size = 20), 
        legend.title = element_text("Species", #<<
                                    size = 24, #<<
                                    color = "blue"),
        legend.text = element_text(size = 20,
                                   face = "italic"),
        axis.line = element_line(),
        panel.grid = element_blank()) #<< oculta as linhas de grid verticais e horizontais na área de plotagem

# Elementos de retângulo  -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species",
                       labels = c(Hp = "Hylodes \npipilans",
                                  Pa = "Proceratophrys \nappendiculata",
                                  Sf = "Scinax \nflavoguttatus")) +
  theme(text = element_text(family = "Serif"), 
        axis.title = element_text(size = 24,
                                  color = "blue"),
        axis.text = element_text(size = 20), 
        legend.title = element_text("Species", #<<
                                    size = 24, #<<
                                    color = "blue"),
        legend.text = element_text(size = 20,
                                   face = "italic"),
        axis.line = element_line(),
        panel.grid = element_blank(),
        panel.background = element_blank()) #<< remove o fundo cinza da área de plotagem

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species",
                       labels = c(Hp = "Hylodes \npipilans",
                                  Pa = "Proceratophrys \nappendiculata",
                                  Sf = "Scinax \nflavoguttatus")) +
  theme(text = element_text(family = "Serif"), 
        axis.title = element_text(size = 24,
                                  color = "blue"),
        axis.text = element_text(size = 20), 
        legend.title = element_text("Species", #<<
                                    size = 24, #<<
                                    color = "blue"),
        legend.text = element_text(size = 20,
                                   face = "italic"),
        axis.line = element_line(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.key = element_blank()) #<< remove o fundo cinza das chaves da legenda

# Desativando elementos   -------------------------------------------
tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species") +
  theme(line = element_blank(), #<< desativa todos os elementos de linha
        rect = element_blank(), #<< desativa todos os elementos de retangulo
        text = element_blank()) #<< desativa todos os elementos de texto

# Salvando e reutilizando camada de tema  -------------------------------------------
bd_tl <- tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, y = body_len, color = species)) +
  geom_point(alpha = 0.5) +
  labs(x = "Total length (mm)", y = "Body length (mm)") +
  scale_color_discrete("Species",
                       labels = c(Hp = "Hylodes \npipilans",
                                  Pa = "Proceratophrys \nappendiculata",
                                  Sf = "Scinax \nflavoguttatus"))

meu_tema <- theme(text = element_text(family = "Serif"), #<<
                  axis.title = element_text(size = 24,
                                            color = "blue"),
                  axis.text = element_text(size = 20), 
                  legend.title = element_text("Species", #<<
                                              size = 24, #<<
                                              color = "blue"),
                  legend.text = element_text(size = 20,
                                             face = "italic"),
                  axis.line = element_line(),
                  panel.grid = element_blank(),
                  panel.background = element_blank(),
                  legend.key = element_blank())

bd_tl +
  meu_tema

tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             fill = stream)) +
  geom_bar(color = "black", 
           position = "dodge",
           width = 0.7) +
  scale_x_discrete("Species",
                   labels = c("Hylodes \npipilans",
                              "Proceratophrys\nappendiculata",
                              "Scinax \nflavoguttatus")) + 
  scale_y_continuous("Number of tadpoles",
                     breaks = seq(0, 175, 25)) +
  scale_color_discrete("Species") +
  meu_tema 

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>%
  drop_na() %>% 
  ggplot(aes(x = body_len, 
             y = ..density.., 
             fill = species)) +   
  geom_histogram(binwidth = 1,
                 center = 0.5, 
                 alpha = 0.5) +
  scale_x_continuous("Species",
                     labels = c("Hylodes \npipilans",
                                "Proceratophrys\nappendiculata",
                                "Scinax \nflavoguttatus")) + 
  scale_y_continuous("Relative frequency") + 
  scale_fill_discrete("Species") +
  meu_tema #<<

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = 0, 
             y = body_len, 
             color = species)) +
  geom_point(shape = 0,
             position = 
               position_jitter(0.25,
                               seed = 136)) +
  scale_x_discrete("") + 
  scale_y_continuous("Body Length (mm)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 5)) +
  scale_color_discrete("Species",
                       labels = c("Hylodes \npipilans", 
                                  "Proceratophrys \nappendiculata", 
                                  "Scinax \nflavoguttatus")) +
  meu_tema

tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pb", "Sa")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species)) + 
  geom_point(shape = 1,
             alpha = 0.5) +
  scale_x_continuous("Total length (mm)",
                     breaks = seq(0, round(max(tadpoles_len_sp$total_len,   na.rm = TRUE)), 10)) +
  scale_y_continuous("Body length (mm)",
                     breaks = seq(0, round(max(tadpoles_len_sp$body_len, na.rm = TRUE)), 5)) +
  scale_color_discrete("Species",
                       labels = c("Hylodes \npipilans",
                                  "Proceratophrys\nappendiculata",
                                  "Scinax \nflavoguttatus")) +
  meu_tema #<<

tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  count(time = date, species) %>% 
  ggplot(aes(x = time, 
             y = n,
             color = species,
             linetype = species)) +  
  geom_line(show.legend = c(linetype = FALSE)) +
  geom_point(aes(shape = species),
             show.legend = c(shape = FALSE)) + 
  scale_x_continuous("Time") + 
  scale_y_continuous("Number of tadpoles",
                     breaks = seq(0, 40, 5)) +
  scale_color_discrete("Species",
                       labels = c("Hylodes \npipilans",
                                  "Proceratophrys\nappendiculata",
                                  "Scinax \nflavoguttatus")) +
  meu_tema

# Camadas de tema pré-construídas  -------------------------------------------
bd_tl +
  theme_classic() #<< 

bd_tl +
  theme_classic(base_size = 22, #<< controla o tamanho da fonte
                base_family = "serif") #<<  controla o tipo da fonte

bd_tl +
  theme_minimal() #<< 

bd_tl +
  theme_dark() #<<  

library(ggthemes)
bd_tl +
  theme_tufte()

bd_tl +
  theme_excel()

bd_tl +
  theme_gdocs()

original <- theme_update(text = element_text(family = "Serif"), #<< torna este tema default para todos os gráficos
                         axis.title = element_text(size = 24,
                                                   color = "blue"),
                         axis.text = element_text(size = 20), 
                         legend.title = element_text("Species", 
                                                     size = 24, 
                                                     color = "blue"),
                         legend.text = element_text(size = 20,
                                                    face = "italic"),
                         axis.line = element_line(),
                         panel.grid = element_blank(),
                         panel.background = element_blank(),
                         legend.key = element_blank())

bd_tl 

theme_set(theme_grey()) # retorna ao tema original como default...

bd_tl 

theme_set(theme_classic()) # ou qualquer outro tema 

bd_tl 

