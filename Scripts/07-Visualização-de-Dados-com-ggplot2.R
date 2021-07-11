# --------------------------------------------------- 
# 07 - Visualização de Dados com ggplot2
# 13 jul 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar pacotes  -------------------------------------------
library(dplyr)
library(ggplot2)
library(RColorBrewer)
source("IRE_functions.R")
tadpoles_clean <- read_tadpoles_raw("tadpoles_raw.csv")

ids <- factor(rep(c("Dados", "Estéticos", "Geométricos", "Estatísticas",
                    "Facetas", "Coordenadas","Temas"), 
                  each = 4))

x <- rep(c(2, 1, 3 , 4), 7)

y <- c(0, 1, 1, 0)

for (i in 5:length(ids)) {
  y[i] <- y[i - 4] + 0.75
}

y1 <- y[2]

for (i in 2:7) {
  y1[i] <- y1[i - 1] + 0.75
}

positions <- data.frame(camada = ids,
                        x = x,
                        y = y)

positions %>% 
  ggplot(aes(x = x,
             y = y, 
             fill = camada)) +
  geom_polygon(alpha  = 0.5,
               show.legend = FALSE) +
  scale_fill_manual(values = c(Dados = "#1B9E77",
                               Estéticos = "#D95F02",
                               Geométricos = "#7570B3",
                               Estatísticas = "#E7298A",
                               Facetas = "#66A61E",
                               Coordenadas = "#E6AB02",
                               Temas = "#A6761D")) +
  annotate(geom = "text", 
           x = c(0.5, 0.3, 0.125, 0.2, 0.45, 0.1, 0.5), 
           y = y1, 
           label = unique(positions$camada),
           family = "serif",
           fontface = "italic",
           size = 6,
           color = c(Dados = "#1B9E77",
                     Estéticos = "#D95F02",
                     Geométricos = "#7570B3",
                     Estatísticas = "#E7298A",
                     Facetas = "#66A61E",
                     Coordenadas = "#E6AB02",
                     Temas = "#A6761D")) + 
  xlim(-0.6, 4) +
  theme(text = element_blank(),
        line = element_blank(),
        panel.background = element_rect(fill = "black"),
        panel.border = element_rect(color = "black"))


