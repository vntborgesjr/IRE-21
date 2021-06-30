# --------------------------------------------------- 
# 04 - Manipulação e Limpeza de Dados 
# 10 jun 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar tidyverse  -------------------------------------------
library(tidyverse)

# Carrega dados  -------------------------------------------
tadpoles_raw <- read_csv(file = "tadpoles_raw.csv", 
                         col_names = TRUE, 
                         na = c("NA"))

# Explorar dados  -------------------------------------------
head(tadpoles_raw, n = 8)
glimpse(tadpoles_raw)
library(skimr)
skim(tadpoles_raw)

# Selecionar e alterar a ordem das colunas  -------------------------------------------
select(tadpoles_raw, Species, `Total Length (mm)`, 
       `Body Length (mm)`, `Stage of development`) # sintaxe clássica

tadpoles_raw %>% 
  select(Species, `Total Length (mm)`, `Body Length (mm)`,
         `Stage of development`) # sintaxe tidy

# Renomear as variáveis -------------------------------------------
tadpoles_raw %>% 
  select(date = Date, stream = Stream, 
         x_coord = `Longitude (UTM)`,
         y_coord = `Latitude (UTM)`, 
         method = Method, species = Species, 
         stage = `Stage of development`, 
         body_len = `Body Length (mm)`, 
         total_len = `Total Length (mm)`,
         depth = `Depth (cm)`) 

# Atribuir tipos  -------------------------------------------
mean(tadpoles_raw$Depth) # não funciona!!
mean(tadpoles_raw$Depth, na.rm = TRUE) # também não funciona!!

head(tadpoles_raw$`Depth (cm)`) # vetor do tipo caracter, por isso não funciona
head(as.numeric(tadpoles_raw$`Depth (cm)`)) # convertido para numérico

# Atribuir tipo - data  -------------------------------------------
library(lubridate)
head(tadpoles_raw$Date)
head(mdy(tadpoles_raw$Date)) # formato mês dia ano
str(mdy(tadpoles_raw$Date))

# Atribuir tipo - fator  -------------------------------------------
head(as.factor(tadpoles_raw$Stream))
head(as.factor(tadpoles_raw$Species))
head(as.factor(tadpoles_raw$`Stage of development`))

# Renomear caracteres  -------------------------------------------
library(stringr)
str_replace_all(tadpoles_raw$Species,
                c("Aplastodiscus eugenioi" = "Ae", 
                  "Bokermannohyla circumdata" = "Bc",
                  "Crossodactylus aeneus" = "Ca",
                  "Hylodes asper" = "Ha",
                  "Hylodes charadranaetes" = "Hc",
                  "Hylodes lateristrigatus" = "Hl",
                  "Hylodes pipilans" = "Hp",
                  "Physalaemus signifer" = "Ps",
                  "Proceratophrys appendiculata" = "Pa",
                  "Proceratophrys boiei" = "Pb",
                  "Rhinella ornata" = "Ro", 
                  "Scinax albicans" = "Sa",
                  "Scinax flavoguttatus" = "Sf"))

# Atribuir tipo com mutate()  -------------------------------------------
mutate(tadpoles_raw, `Depth (cm)` = as.numeric(`Depth (cm)`)) # sintaxe clássica
tadpoles_raw %>% 
  mutate(`Depth (cm)` = as.numeric(`Depth (cm)`)) %>% 
  head() # sintaxe tidy

tadpoles_raw %>% 
  mutate(Date = mdy(Date), Stream = as.factor(Stream),
         Species = as.factor(Species), 
         `Stage of development` = as.factor(`Stage of development`),
         `Depth (cm)` = as.numeric(`Depth (cm)`)) # atribuir a multiplas variáveis

# Abreviar o nome das espécies  -------------------------------------------
tadpoles_raw %>% 
  mutate(Species = str_replace_all(Species,
                                   c("Aplastodiscus eugenioi" = "Ae", 
                                     "Bokermannohyla circumdata" = "Bc",
                                     "Crossodactylus aeneus" = "Ca",
                                     "Hylodes asper" = "Ha",
                                     "Hylodes charadranaetes" = "Hc",
                                     "Hylodes lateristrigatus" = "Hl",
                                     "Hylodes pipilans" = "Hp",
                                     "Physalaemus signifer" = "Ps",
                                     "Proceratophrys appendiculata" = "Pa",
                                     "Proceratophrys boiei" = "Pb",
                                     "Rhinella ornata" = "Ro", 
                                     "Scinax albicans" = "Sa",
                                     "Scinax flavoguttatus" = "Sf")))

# Selecionar e atribuir o tipo a múltiplas variáveis  -------------------------------------------
mutate(select(tadpoles_raw, date = Date, stream = Stream,
              x_coord = `Longitude (UTM)`,
              y_coord = `Latitude (UTM)`, method = Method, 
              species = Species, stage = `Stage of development`, 
              body_len = `Body Length (mm)`, 
              total_len = `Total Length (mm)`,
              depth = `Depth (cm)`), 
       date = mdy(date), stream = as.factor(stream),
       species = as.factor(species), 
       stage = as.factor(stage),
       depth = as.numeric(depth)) # sintaxe clássica

tadpoles_raw %>% 
  select(date = Date, stream = Stream,
         x_coord = `Longitude (UTM)`,
         y_coord = `Latitude (UTM)`, method = Method, 
         species = Species, stage = `Stage of development`, 
         body_len = `Body Length (mm)`, 
         total_len = `Total Length (mm)`,
         depth = `Depth (cm)`) %>% 
  mutate(data = mdy(date), stream = as.factor(stream),
         species = as.factor(species), 
         stage = as.factor(stage),
         depth = as.numeric(depth)) # sintaxe tidy

# Atribuir tipo às variáveis ao carregar os dados  -------------------------------------------
tadpoles_raw <- read_csv(file = "tadpoles_raw.csv", 
                         col_names = TRUE, 
                         na = c("NA"),
                         col_types = cols(Date = col_date(format = "%m/%d/%y"),
                                          Stream = col_factor(),
                                          Species = col_factor(),
                                          `Stage of development` = col_factor(),
                                          `Depth (cm)` = col_number()))

glimpse(tadpoles_raw) # inspecionar os dados

# Filtrar os dados  -------------------------------------------
filter(tadpoles_raw, Method == "Dipnet") # sintaxe clássica

tadpoles_raw %>% 
  filter(Method == "Dipnet") # sintaxe tidy

# Filtrando múltipĺas variáveis  -------------------------------------------
filter(tadpoles_raw, Method == "Dipnet", 
       Species == "Aplastodiscus eugenioi",
       `Stage of development` == 25) # sintaxe clássica

tadpoles_raw %>% 
  filter(Method == "Dipnet",
         Species == "Aplastodiscus eugenioi",
         `Stage of development` == 25) # sintaxe tidy

# Ordenar observações  -------------------------------------------
arrange(tadpoles_raw, `Total Length (mm)`) # crescente, sintaxe clássica

tadpoles_raw %>% 
  arrange(`Total Length (mm)`) # crescente sintaxe clássica

arrange(tadpoles_raw, desc(`Total Length (mm)`)) # decrescente, sintaxe clássica

tadpoles_raw %>% 
  arrange(desc(`Total Length (mm)`)) # decrescente, sintaxe tidy

# Filtrar e ordenar os dados  -------------------------------------------
filter(arrange(tadpoles_raw, desc(`Total Length (mm)`)), 
      Species == "Aplastodiscus eugenioi") # sintaxe clássica

tadpoles_raw %>% 
  filter(Species == "Aplastodiscus eugenioi") %>% 
  arrange(desc(`Total Length (mm)`)) # sintaxe tidy

# Filtrar, ordenar e transformar  -------------------------------------------
tadpoles_raw %>%
  select(Species, `Stage of development`, `Body Length (mm)`, `Total Length (mm)`) %>% 
  filter(Species == "Hylodes asper") %>% 
  arrange(desc(`Stage of development`)) %>% 
  mutate(ratio_bl_tl = `Body Length (mm)`/`Total Length (mm)`)

# Summarizar  -------------------------------------------
summarise(tadpoles_raw, media = mean(`Body Length (mm)`, na.rm = TRUE)) # sintaxe clássica

tadpoles_raw %>% 
  summarise(media = mean(`Body Length (mm)`, na.rm = TRUE)) # sintaxe tidy

# Identificar valores fora da amplitude de variação esperada  -------------------------------------------
library(assertive)
assert_all_are_in_closed_range(tadpoles_raw$`Body Length (mm)`,
                               lower = 2, upper = 50)

# Eliminar valores ausentes - NA's  -------------------------------------------
tadpoles_raw %>% 
  select(`Stage of development`, `Body Length (mm)`, `Total Length (mm)`) %>% 
  drop_na()

# Identificar valores fora da amplitude de variação  -------------------------------------------
tadpoles_raw %>% 
  select(`Body Length (mm)`) %>% 
  drop_na() %>% 
  summarise(assert_all_are_in_closed_range(`Body Length (mm)`,
                                           lower = 2, upper = 45))

tadpoles_raw %>%
  select(Species, `Total Length (mm)`) %>% 
  drop_na() %>% 
  ggplot(aes(x = Species, y = `Total Length (mm)`)) +
  geom_point()

tadpoles_raw  %>%  
  select(`Total Length (mm)`) %>% 
  drop_na() %>% 
  summarise(assert_all_are_in_closed_range(`Total Length (mm)`,
                                           lower = 5, upper = 100))

tadpoles_raw  %>%  
  select(`Total Length (mm)`) %>% 
  drop_na() %>% 
  filter(`Total Length (mm)` < 2500) %>% 
  summarise(assert_all_are_in_closed_range(`Total Length (mm)`,
                                           lower = 5, upper = 100))

tadpoles_raw %>%  
  select(Species, `Total Length (mm)`) %>% 
  drop_na()  %>% 
  mutate(`Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 2909, 29.09),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 4005, 40.05),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 5374, 53.74),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 4827, 48.27),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 3961, 39.61),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 2589, 25.89)) %>% 
  summarise(assert_all_are_in_closed_range(`Total Length (mm)`,
                                           lower = 5, upper = 100))

# Gráfico de pontos  -------------------------------------------
tadpoles_raw %>%  
  select(Species, `Total Length (mm)`) %>% 
  drop_na()  %>% 
  mutate(`Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 2909, 29.09),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 4005, 40.05),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 5374, 53.74),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 4827, 48.27),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 3961, 39.61),
         `Total Length (mm)` = replace(`Total Length (mm)`, 
                                       `Total Length (mm)` == 2589, 25.89)) %>% 
  ggplot(aes(x = Species, y = `Total Length (mm)`)) +
  geom_point()

# Gerar sumários múltiplos de uma só vez  -------------------------------------------
tadpoles_raw %>% 
  summarise(media = mean(`Body Length (mm)`, na.rm = TRUE),
            desvpad = sd(`Body Length (mm)`, na.rm = TRUE),
            min = quantile(`Body Length (mm)`, na.rm = TRUE)[1],
            qrt2 = quantile(`Body Length (mm)`, na.rm = TRUE)[2],
            mediana = quantile(`Body Length (mm)`, na.rm = TRUE)[3],
            qrt4 = quantile(`Body Length (mm)`, na.rm = TRUE)[4],
            maximo = quantile(`Body Length (mm)`, na.rm = TRUE)[5],
            n = n())


tadpoles_raw %>% 
  filter(Species == "Crossodactylus aeneus") %>% 
  summarise(media = mean(`Body Length (mm)`, na.rm = TRUE),
            desvpad = sd(`Body Length (mm)`, na.rm = TRUE),
            min = quantile(`Body Length (mm)`, na.rm = TRUE)[1],
            qrt2 = quantile(`Body Length (mm)`, na.rm = TRUE)[2],
            mediana = quantile(`Body Length (mm)`, na.rm = TRUE)[3],
            qrt4 = quantile(`Body Length (mm)`, na.rm = TRUE)[4],
            maximo = quantile(`Body Length (mm)`, na.rm = TRUE)[5],
            n = n()) # para uma espécie

tadpoles_raw %>% 
  filter(Method = "Dipnet", 
         Species == "Crossodactylus aeneus") %>% 
  summarise(media = mean(`Body Length (mm)`, na.rm = TRUE),
            desvpad = sd(`Body Length (mm)`, na.rm = TRUE),
            min = quantile(`Body Length (mm)`, na.rm = TRUE)[1],
            qrt2 = quantile(`Body Length (mm)`, na.rm = TRUE)[2],
            mediana = quantile(`Body Length (mm)`, na.rm = TRUE)[3],
            qrt4 = quantile(`Body Length (mm)`, na.rm = TRUE)[4],
            maximo = quantile(`Body Length (mm)`, na.rm = TRUE)[5],
            n = n()) # para uma espécie e um método

# Sumarizar variável para grupos de observações  -------------------------------------------
summarise(group_by(tadpoles_raw, Species), 
          media = mean(`Body Length (mm)`, na.rm = TRUE),
          desvpad = sd(`Body Length (mm)`, na.rm = TRUE),
          min = quantile(`Body Length (mm)`, na.rm = TRUE)[1],
          qrt2 = quantile(`Body Length (mm)`, na.rm = TRUE)[2],
          mediana = quantile(`Body Length (mm)`, na.rm = TRUE)[3],
          qrt4 = quantile(`Body Length (mm)`, na.rm = TRUE)[4],
          maximo = quantile(`Body Length (mm)`, na.rm = TRUE)[5],
          n = n()) # sintaxe clássica

tadpoles_raw %>% 
  group_by(Species) %>% 
  summarise(media = mean(`Body Length (mm)`, na.rm = TRUE),
            desvpad = sd(`Body Length (mm)`, na.rm = TRUE),
            min = quantile(`Body Length (mm)`, na.rm = TRUE)[1],
            qrt2 = quantile(`Body Length (mm)`, na.rm = TRUE)[2],
            mediana = quantile(`Body Length (mm)`, na.rm = TRUE)[3],
            qrt4 = quantile(`Body Length (mm)`, na.rm = TRUE)[4],
            maximo = quantile(`Body Length (mm)`, na.rm = TRUE)[5],
            n = n()) # sintax tidy

# Contar elementos  -------------------------------------------
tadpoles_raw %>% 
  count(Species)

tadpoles_raw %>% 
  group_by(Species) %>% 
  summarise(n = n()) # gera o mesmo reusltado que count()

tadpoles_raw %>% 
  count(Species, 
        `Stage of development`) %>% 
  mutate(`n (%)` = n/sum(n))

tadpoles_raw %>% 
  group_by(Species, 
           `Stage of development`) %>% 
  summarize(n = n()) %>% 
  mutate(`N (%)` = n/sum(n))

tadpoles_raw %>% 
  count(Species, 
        `Stage of development`) %>% 
  mutate(`n (%)` = n/sum(n))

tadpoles_raw %>% 
  group_by(Species, 
           `Stage of development`) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(`N (%)` = n/sum(n))

tadpoles_raw %>% 
  group_by(Species) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(`N (%)` = n/sum(n))

# rm(list = ls())