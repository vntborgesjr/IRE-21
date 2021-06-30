###################################################################### 
# read_tadpoles_raw(data)  
# Description: Carrega a planilha de dados tadpoles_raw, atribui os tipos
# adequados e seleciona e renomeia as variáveis de interesse, abrevia
# o nome das espécies e corrige valores fora da amplitude de variação 
# esperada
# data: deve ser alimentado com o arquivo tadpoles_raw.csv
######################################################################
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
                                            `Stage of development` = col_factor(),
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
                                                      "Scinax flavoguttatus" = "Sf")))))
  
  # corrigindo valores fora da amplitude de variação esperada
  tadpoles_raw$total_len[which(
    tadpoles_raw$total_len > 100
  )] <- tadpoles_raw$total_len[which(
    tadpoles_raw$total_len > 100)] /100
  
  # retornar os dados limpos
  return(tadpoles_raw)
}
######################################################################


