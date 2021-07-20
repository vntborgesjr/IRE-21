# --------------------------------------------------- 
# 07 - Visualização de Dados com ggplot2
# 13 jul 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar pacotes  -------------------------------------------
library(tidyverse)
library(RColorBrewer)
library(ggbeeswarm)
source("IRE-21/Slides/IRE_functions.R")
getwd()
# Carregar dados -------------------------------------------------
tadpoles_clean <- read_tadpoles_raw("IRE-21/Dados/tadpoles_raw.csv")

# ligação entre geom_() e stat_() -------------------------------------------------
p <- tadpoles_len_sp %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>%
  drop_na() %>% 
  ggplot(aes(x = body_len))

p + geom_histogram()
p + stat_bin()  # produz o mesmo resultado

p +  geom_bar(stat = "bin")

p <- tadpoles_clean %>% 
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  select(species, stream) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, fill = stream))

p + geom_bar()
p + stat_count()

# geom_smooth() e stat_smooth() -------------------------------------------------
p <- tadpoles_clean %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
         p +  geom_point() +
           geom_smooth() 

p <- tadpoles_clean %>% 
     drop_na() %>% 
     ggplot(aes(x = total_len, 
     y = body_len))

p +  geom_point() +
     geom_smooth(se = FALSE)

p <- tadpoles_clean %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))

p +  geom_point() +
  geom_smooth(method = "loess", 
              se = FALSE) 

p <- tadpoles_clean %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))

p +  geom_point() +
  geom_smooth(method = "loess", 
              se = FALSE, span = 0.1)

p <- iris %>%
  ggplot(aes(x = Sepal.Length, 
             y = Sepal.Width,
             color = Species))

p +  geom_point(show.legend = FALSE) +
  geom_smooth(method = "loess", 
              se = FALSE, span = 0.4)

p <- iris %>%
  ggplot(aes(x = Sepal.Length, 
             y = Sepal.Width,
             color = Species))

p +  geom_point(show.legend = FALSE) +
  geom_smooth(method = "loess", 
              se = FALSE, span = 1)

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))

p +  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE)

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))

p +  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE)

# Gr?ficos com estat?sticas -------------------------------------------------
p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len))
p + geom_jitter(width = 0.25,
                seed = 136, 
                alpha = 0.3)

tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  group_by(species) %>% 
  summarise(media = mean(body_len),
            `+sd` = media + sd(body_len),
            `-sd` = media - sd(body_len))

tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  group_by(species) %>% 
  summarise(media = mean(body_len),
            `+sd` = media + sd(body_len),
            `-sd` = media - sd(body_len)) %>% 
  ggplot(aes(y = media, x = species)) +
  geom_pointrange(aes(ymin = `-sd`,
                      ymax = `+sd`))

library(Hmisc)
smean.sdl(tadpoles_clean$body_len, 
          mult = 1, na.rm = TRUE)

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len))
p + stat_summary(fun.data = mean_sdl,
                 fun.args = list(mult = 1))

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len))
p + stat_summary(fun = mean,
                 geom = "point") +
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.1)

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = species, 
             y = body_len))
p + stat_summary(fun = mean,
                 geom = "point") +
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.1)

# Verificando distribui??es -------------------------------------------------
p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf"), 
         total_len < 70,
         body_len < 25) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))

p + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red")

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf"), 
         total_len < 70,
         body_len < 25) %>% 
  drop_na() %>% 
  ggplot(aes(x = body_len))
p + geom_histogram(aes(y = ..density..)) +
  geom_rug() +
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(tadpoles_clean$body_len[tadpoles_clean$body_len < 25], na.rm = TRUE),
                            sd = sd(tadpoles_clean$body_len[tadpoles_clean$body_len < 25], na.rm = TRUE)))

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf"), 
         total_len < 70,
         body_len < 25) %>% 
  drop_na() %>% 
  ggplot(aes(sample = body_len))
p + stat_qq() +
  geom_qq_line(color = "red")

# Camada de facetas -------------------------------------------------
p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len,
             color = species))
p + geom_point()

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(species))

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))

p + geom_point() +
  facet_grid(. ~ species)

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(rows = vars(species))

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(species ~ .)

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(rows = vars(stage),
             cols= vars(species))

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(stage ~ species)

# Renomeando facetas -------------------------------------------------
p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(species), 
             labeller = label_value)

p <- tadpoles_clean %>%
  filter(species %in% c("Hp", "Pa", "Sf")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(species), 
             labeller = label_both)

tadpoles_clean$species <- fct_recode(tadpoles_clean$species,
                                     `Hylodes pipilans` = "Hp")
p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes pipilans")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(Species, 
                         Stage))

# Reordenando facetas -------------------------------------------------
tadpoles_clean$species <- fct_recode(tadpoles_clean$species,
                                     `Hylodes \npipilans` = "Hp",
                                     `Proceratophrys \nappendiculata` = "Pa",
                                     `Scinax \nflavoguttatus` = "Sf")
p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(Species))

tadpoles_clean$species <- fct_relevel(tadpoles_clean$species,
                                      "Scinax \nflavoguttatus",
                                      "Proceratophrys \nappendiculata",
                                      "Hylodes \npipilans")
p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(Species))

# Ajustando o espa?o de plotagem -------------------------------------------------
p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(Species),
             rows = vars(Stage))

p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(Species),
             rows = vars(Stage),
             scales = "free_x")

p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(Species),
             rows = vars(Stage),
             scales = "free_y")

p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_grid(cols = vars(Species),
             rows = vars(Stage),
             scales = "free")

p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = body_len, 
             y = Species,
             color = Species))
p + geom_jitter(height = 0.1, 
                show.legend = FALSE) +
  facet_grid(rows = vars(Stage))

p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = body_len, 
             y = Species,
             color = Species))
p + geom_jitter(height = 0.1, 
                show.legend = FALSE) +
  facet_grid(rows = vars(Stage),
             scales = "free_y",
             space = "free_y")

# Facet_wrap() -------------------------------------------------

p <- tadpoles_clean %>%
  rename(Species = species, Stage = stage) %>% 
  filter(Species %in% c("Hylodes \npipilans", 
                        "Proceratophrys \nappendiculata", 
                        "Scinax \nflavoguttatus")) %>% 
  drop_na() %>% 
  ggplot(aes(x = total_len, 
             y = body_len))
p + geom_point() +
  facet_wrap(vars(Species, Stage),
             scales = "free")

p + geom_point() +
  facet_wrap(vars(Species, Stage),
             scales = "free", 
             nrow = 3,
             ncol = 5)

# Facet_wrap() e plotes marginais  -------------------------------------------
p + geom_point() +
  facet_grid(rows = vars(Species),
             cols = vars(Stage),
             scales = "free",
             margins = TRUE)

# Camada de coordenadas  -------------------------------------------
# Razão entre eixo x e y 
p + 
  geom_point() +
  geom_smooth(se = FALSE)

p + 
  geom_point() +
  geom_smooth(se = FALSE) +
  coord_cartesian(xlim = c(40, 50)) # alterando razão xy 1

p + 
  geom_point() +
  geom_smooth(se = FALSE) +
  xlim(40, 50) # alterando razão xy 2

p +
  geom_point() +
  scale_x_continuous(limits = c(40, 50)) # alterando razão xy 3

p + geom_point() +
  coord_fixed(ratio = 1,
              ylim = c(0, 25),
              xlim = c(0, 80)) # alterando razão xy 4

p + geom_point() +
  coord_cartesian(expand = 0)

p + geom_point() +
  coord_cartesian(expand = 1)

p + geom_point() +
  coord_cartesian(expand = 0,
                  clip = "off")

# Coordenadas x escalas  -------------------------------------------
# Log10 dos eixos x e y
ggplot(msleep, aes(y = 1, 
             x = bodywt)) +
  geom_jitter() +
  scale_x_continuous(limits = c(0, 7000),
                     breaks = seq(0, 7000, 1000)) +
  coord_fixed(ratio = 2500)
  
ggplot(msleep, aes(y = 1, 
               x = log10(bodywt))) + # log10 direto nos dados
  geom_jitter() +
  scale_x_continuous(limits = c(-3, 4),
                     breaks = -3:4) +
  coord_fixed(ratio = 3)

ggplot(msleep, aes(y = 1, 
             x = log10(bodywt))) + # log10 direto nos dados
  geom_jitter() +
  scale_x_continuous() +
  annotation_logticks(sides = "b") +
  coord_fixed(ratio = 3)

ggplot(msleep, aes(y = 1, 
              x = bodywt)) +
  geom_jitter() +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 7000)) + # aqui os valores do eixo correspondem aos valores reais dos dados
  coord_fixed(ratio = 3)

ggplot(msleep, aes(y = 1, 
              x = bodywt)) +
  geom_jitter() +
  coord_trans(x = "log10")  # aqui os valores do eixo correspondem aos valores reais dos dados

# exemplos em gráficos de relações - y vs. x
ggplot(msleep, aes(x = bodywt,
                   y = brainwt)) +
  geom_point()

ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Scale_ functions")

ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  coord_trans(x = "log10",
              y = "log10")

ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Scale_ functions")

ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE) +
  coord_trans(x = "log10",
              y = "log10")

# Invertando os eixos  -------------------------------------------
p +
  geom_point()

p +
  geom_point() +
  coord_flip()

# Dobrando eixos  -------------------------------------------
airquality$Year <- rep(1973, nrow(airquality))

airquality <- airquality %>% 
  unite("Date", Month:Year,
        sep = "-") %>% 
  mutate(Date = lubridate::as_date(Date,
                                   format = "%m-%d-%Y"))
  
ggplot(airquality, aes(x = Date,
                       y = Temp)) +
  geom_line() +
  labs(x = "Date (1973)",
       y = "Fahrenheit")

# eixo em Fahrenheit
y_breaks <- c(59, 68, 77, 86, 95, 104)

# criar segundo eixo em Celsius
y_labels <- ((y_breaks -32) * 5)/9

# Create a secondary x-axis
secondary_y_axis <- sec_axis(
  # Use identity transformation
  trans = identity,
  name = "Celsius",
  # Define breaks and labels as above
  breaks = y_breaks,
  labels = y_labels
)

# adicionar segundo eixo ao plote
ggplot(airquality, aes(x = Date,
                       y = Temp)) +
  geom_line() +
  labs(x = "Date (1973)",
       y = "Fahrenheit") +
  scale_y_continuous(sec.axis = secondary_y_axis)

# Coordenada polor  -------------------------------------------
# transformando gráficos de barra...
tadpoles_clean %>% 
  filter(species %in% c("Ae", "Pb", "Sa")) %>% 
  ggplot(aes(x = 1, 
             fill = species)) +
  geom_bar()

# ...gráfico de pizza
tadpoles_clean %>% 
  filter(species %in% c("Ae", "Pb", "Sa")) %>% 
  ggplot(aes(x = 1, 
             fill = species)) +
  geom_bar() +
  coord_polar(theta = "y")

# ...gráfico de pizza
tadpoles_clean %>% 
  filter(species %in% c("Ae", "Pb", "Sa")) %>% 
  ggplot(aes(x = 1, 
             fill = species)) +
  geom_bar(width = 0.1) +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0.5, 1.5))

# Bonus - Gráfico do início da aula -------------------------------------------------
ids <- factor(rep(c("Dados", "Estéticos", "Geométricos","Temas"), 
                  each = 4))

x <- rep(c(2, 1, 3 , 4), 4)

y <- c(0, 0.75, 0.75, 0)

for (i in 5:length(ids)) {
  y[i] <- y[i - 4] + 0.75
}

y1 <- y[2]

for (i in 2:4) {
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
                               Est?ticos = "#D95F02",
                               Geom?tricos = "#7570B3",
                               Temas = "#A6761D")) +
  annotate(geom = "text", 
           x = c(0.5, 0.3, 0.125, 0.5), 
           y = y1, 
           label = unique(positions$camada),
           family = "serif",
           fontface = "italic",
           size = 12,
           color = c(Dados = "#1B9E77",
                     Est?ticos = "#D95F02",
                     Geom?tricos = "#7570B3",
                     Temas = "#A6761D")) + 
  xlim(-0.6, 4) +
  theme(text = element_blank(),
        line = element_blank(),
        panel.background = element_rect(fill = NULL, 
                                        color = NULL))

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
                               Est?ticos = "#D95F02",
                               Geom?tricos = "#7570B3",
                               Estat?sticas = "#E7298A",
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
                     Est?ticos = "#D95F02",
                     Geom?tricos = "#7570B3",
                     Estat?sticas = "#E7298A",
                     Facetas = "#66A61E",
                     Coordenadas = "#E6AB02",
                     Temas = "#A6761D")) + 
  xlim(-0.6, 4) +
  theme(text = element_blank(),
        line = element_blank(),
        panel.background = element_rect(fill = "darkgrey"))

# Bonus 2 -plots beeswarm e violino   -------------------------------------------
library(ggbeeswarm)
# Úteis para comparar distribuições
# Beeswarm
tadpoles_clean %>% 
  filter(species %in% c("Ca", "Pb", "Sa")) %>% 
  ggplot(aes(x = species, 
             y = body_len)) +
  geom_beeswarm(color = "steelblue", size = 0.5)

# Violino
tadpoles_clean %>% 
  filter(species %in% c("Ca", "Pb", "Sa")) %>% 
  ggplot(aes(x = species, 
             y = body_len)) +
  geom_violin(fill = "steelblue")

# Beeswarm + violino
tadpoles_clean %>% 
  filter(species %in% c("Ca", "Pb", "Sa")) %>% 
  ggplot(aes(x = species, 
             y = body_len)) +
  geom_beeswarm(color = "steelblue", 
                size = 0.2) +
  geom_violin(fill = "steelblue",
              alpha = 0.3)

# Violino + boxplot
tadpoles_clean %>% 
  filter(species %in% c("Ca", "Pb", "Sa")) %>% 
  ggplot(aes(x = species, 
             y = body_len)) +
  geom_violin(fill = "steelblue") +
  geom_boxplot(alpha = 0.3)

# Violino + boxplot + beeswarm
tadpoles_clean %>% 
  filter(species %in% c("Ca", "Pb", "Sa")) %>% 
  ggplot(aes(x = species, 
             y = body_len)) +
  geom_violin(fill = "steelblue") +
  geom_boxplot(alpha = 0.3) +
  geom_beeswarm(color = "steelblue", size = 0.5)

# Ridgline plot
library(ggridges)
tadpoles_clean %>% 
  filter(species %in% c("Ca", "Pb", "Sa")) %>% 
  ggplot(aes(y = species, 
             x = body_len)) +
  geom_density_ridges(bandwidth = 2)

library(ggridges)
tadpoles_clean %>% 
  filter(species == "Ca") %>% 
  ggplot(aes(y = factor(date), 
             x = body_len)) +
  geom_density_ridges(bandwidth = 2)
