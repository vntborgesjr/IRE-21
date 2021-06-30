# --------------------------------------------------- 
# 02 - Introdução ao R parte 2 - Indexação e funções loop
# 01 jun 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Exercício- indexação de vetor  -------------------------------------------

numeros <- 1:100
# alterniva para criar sequencias
?seq
numeros <- seq(from = 1, to = 100, by = 1)

impar_par <- c("impar", "par")
impar_par

impar_par1 <- rep(impar_par, times = 50)
impar_par1

numeros[impar_par1 == "impar"]
numeros[impar_par1 == "par"]

numeros[impar_par == "impar"]
numeros[impar_par == "par"]

# Indexando data frame  -------------------------------------------

df$n_sp[df$n_sp > 38]

df$n_sp[df$cat1 == "A"]

df[df$cat1 == "A", 'n_sp']

df[df$cat1 == "A", c('n_sp', 'area', 'cat2')]

df[df$cat1 == "A", ]

df[df$cat1 == "A", 1:2]

df[df$cat1 == "A", 4:5]

# Obtenha o número de espécies (n_sp) pertencentes a 
# categoria A OU C
df[df$cat1 == "A" | df$cat1 == "C", "n_sp"]

# Obtenha o número de espécies petencentes a áreas maiores 
# ou iguais a 60 E menores ou iguais a 70
df[df$area >= 60 & df$area <= 70, 1]

# Obtenha o número de espécies pertencentes a áreas menores 
# que 30 OU maior que 80
df[df$area < 30 | df$area > 80, "n_sp"]

# Obtenha os valores de área diferentes das categorias 
# (cat1) "A" OU "B"
df[df$cat1 == "A" | df$cat1 == "B", "area"]

# Alterando o nome das colunas do data frame  -------------------------------------------
names(df)
names(df)[4]
names(df)[4] <- "predacao"
names(df)[3] <- "isolamento"
names(df)

# Excluindo valores  -------------------------------------------
df$n_sp[-1]
df[-92, -5]
df[92, "area"] <- 55.7042236

# Funções loop  -------------------------------------------
# apply()
# para usar a função apply, precisaremos criar uma matriz
matriz <- matrix(data = 1:20, nrow = 5, ncol = 4)
apply(X = matriz, MARGIN = 1, FUN = mean)
apply(X = matriz, MARGIN = 2, FUN = mean)

# similar as funções rowMeans e colMeans
rowMeans(matriz)
colMeans(matriz)

# aggregate()
resu_aggregate <- aggregate(x = df$area, 
                            by = list(df$predacao, df$isolamento), 
                            FUN = mean)
resu_aggregate[1, ]
resu_aggregate[2, ]
resu_aggregate
class(resu_aggregate)

# corrigindo os nomes das colunas
names(resu_aggregate) <- c("predacao", "isolamento", "area")
names(resu_aggregate)
resu_aggregate

resu_aggregate2 <- aggregate(x = df[, 1:2], 
                             by = list(df$predacao, df$isolamento), 
                             FUN = mean)
names(resu_aggregate2)[1:2] <- c("predacao", "isolamento")

# Aplique a função aggregate() para determinar o comprimento
# médio da pétala de todas as espécies, no conjunto de dados iris
?datasets
# consultado a base de dados
library(help = "datasets")
str(iris)
mean(iris$Petal.Length[iris$Species == "virginica"])
mean(iris$Petal.Length[iris$Species == "setosa"])
mean(iris$Petal.Length[iris$Species == "versicolor"])
aggregate(iris$Petal.Length, list(iris$Species), mean)
aggregate(iris[, 1:4], list(iris$Species), mean)

# tapply()  -------------------------------------------
# média de Var2 por nível de fator
resu_tapply <- tapply(X = df$area, INDEX = list(df$isolamento), 
                      FUN = mean)
class(resu_tapply)
resu_tapply

# Para mais de um fator
resu_tapply2 <- tapply(X = df$area, INDEX = list(df$isolamento,
                                                 df$predacao),
                       FUN = mean)
class(resu_tapply2)

# Aplique a função tapply() para determinar a taxa de absorção de CO2
# (uptake) para os diferentes tratamentos # (Treatment) e para as
# diferentes origens das plantas (Type), no conjunto de dados CO2.
str(CO2)
tapply(CO2$uptake, list(CO2$Treatment, CO2$Type), mean)

# by()  -------------------------------------------
# média de Var1 por nível de Fator2
resu_by <- by(data = df$n_sp, INDICES = list(df$isolamento),
              FUN = mean)
class(resu_by)
resu_by[1][1] # obter um valor dentro da lista

# média de Var2 por nível de Fator1 e Fator2
by(data = df$n_sp, 
   INDICES = list(df$isolamento,
                  df$predacao), FUN = mean)

# Aplique a função by() para determinar a taxa de absorção média 
# de CO2 (uptake) para os diferentes tratamentos (Treatment) e
# diferentes origens das plantas (Type), no conjunto de dados CO2.
by(CO2$uptake, list(CO2$Treatment, CO2$Type), mean)

# lapply()  -------------------------------------------
# gerar a lista
lista_df <- split(x = df$n_sp, f = df$isolamento)
# média de n_sp por nível de isolamento
lapply(X = lista_df, FUN = mean)

# gerar diferentes data frames
lista_df2 <- split(x = df, f = df$isolamento)
lista_df2

# sapply()  -------------------------------------------
sapply(X = lista_df, FUN = mean, simplify = TRUE)
sapply(X = lista_df, FUN = mean, simplify = FALSE)

lapply(lista_df2, mean) # não funciona porque os elementos da lista
# são data frames. Para lapply() e sapply funcionarem cada elemento
# da lista tem que ser unidimensional (vetor)

# Aplique as funções lapply() e sapply() para determinar a média da 
# massa seca de plantas (weight) de acordo com o tratamento (group) 
# usando o conjunto de dados PlantGrowth.
class(PlantGrowth)
nova_lista <- split(PlantGrowth$weight, PlantGrowth$group)
lapply(nova_lista, mean)
sapply(nova_lista, mean)

# mapply()  -------------------------------------------
seq_len(2)
mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  
       c(A = 10, B = 0, C = -10))
mapply(lm,
       list(df$n_sp ~ df$area),
       list(df$n_sp ~ df$isolamento))
plot(df$n_sp ~ df$area)
abline(lm(df$n_sp ~ df$area))
