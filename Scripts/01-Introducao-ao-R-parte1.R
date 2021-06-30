# --------------------------------------------------- 
# 01 - Introdução ao R 
# 25 mai 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Instalar pacotes  -------------------------------------------
#install.packages("readxl")

# Carregar pacotes  -------------------------------------------
library(readxl)

# Carregar dados  -------------------------------------------
numero_de_especies <- read.table(file = "numero_de_especies.txt")
area <- read.csv(file = "area.csv") # essa linha de comando carrega um conjunto de dados
variavel_catagorica1 <- read_xlsx("variavel_categorica1.xlsx")
variavel_catagorica2 <- read.table("variavel_categorica2.txt")
variavel_catagorica3 <- read.csv("variavel_categorica3.csv")

# Identificar os tipos de objeto  -------------------------------------------
class(numero_de_especies)
str(numero_de_especies)

class(area)
str(area)

class(variavel_catagorica1)
str(variavel_catagorica1)


class(variavel_catagorica2)
str(variavel_catagorica2)

class(variavel_catagorica3)
str(variavel_catagorica3)

# Gerar vetores a partir dos data frame  -------------------------------------------
numero_de_especies <- numero_de_especies$x
area <- area$x
variavel_catagorica1 <- variavel_catagorica1$B
variavel_catagorica2 <- variavel_catagorica2$x
variavel_catagorica3 <- variavel_catagorica3$x

# Identificar os tipos de vetores  -------------------------------------------
class(numero_de_especies)
typeof(numero_de_especies)
str(numero_de_especies)

class(area)
typeof(area)
str(area)

class(variavel_catagorica1)
typeof(variavel_catagorica1)
str(variavel_catagorica1)

class(variavel_catagorica2)
typeof(variavel_catagorica2)
str(variavel_catagorica2)

class(variavel_catagorica3)
typeof(variavel_catagorica3)
str(variavel_catagorica3)

# Identificando vetores com funções lógicas tipo is...()  -------------------------------------------
is.character(numero_de_especies)
is.integer(numero_de_especies)

# Construir vetor usando c()  -------------------------------------------
caracter <- c("Dark", "side", "of", "the", "moon")
typeof(caracter)

inteiros <- c(1, 2, 3, 4, 6, 7, 8, 9)
typeof(inteiros)

inteiros <- as.integer(c(1, 2, 3, 4, 6, 7, 8, 9))
typeof(inteiros)

caracter <- as.character(inteiros)
typeof(caracter)

# Regras de coerção  -------------------------------------------
c(variavel_catagorica2, variavel_catagorica3)
sum(variavel_catagorica3)

c(numero_de_especies, variavel_catagorica1)
c(numero_de_especies, area)

# Tarnsformar vetores em listas  -------------------------------------------
lista1 <- list(numero_de_especies, area)
lista1

lista2 <- list(n_sp = numero_de_especies, arae = area)
lista2

str(lista1)
str(lista2)

# Obter um objeto da lista  -------------------------------------------
lista1[[1]]
lista2$n_sp

# Gravar e carregar uma lista  -------------------------------------------
saveRDS(lista1, "lista.rds")
lista <- readRDS("lista.rds")

# Criar uma matriz  -------------------------------------------
m1 <- matrix(1:10, nrow = 2, ncol = 5)
m1

m2 <- matrix(1:10, nrow = 2, ncol = 5, byrow = TRUE)
m2

m3 <- matrix(1:10, nrow = 2, ncol = 5, byrow = FALSE)
m3

# Criar um Data Frame  -------------------------------------------
df <- data.frame(n_sp = numero_de_especies,
                 area = area,
                 cat1 = variavel_catagorica1,
                 cat2 = variavel_catagorica2,
                 cat3 = variavel_catagorica3)

# Homogeinizar o comprimento dos vetores  -------------------------------------------
length(numero_de_especies)
length(area)
length(variavel_catagorica1)
length(variavel_catagorica2)
length(variavel_catagorica3)

# Bonus - outra forma de calcular o comprimento dos vetores
lapply(list(numero_de_especies, 
            area, 
            variavel_catagorica1, 
            variavel_catagorica2,
            variavel_catagorica3),
       length)

# Indexação por posição  -------------------------------------------
# Solução 1 - reduzir o tamanho dos vetores
numero_de_especies1 <- numero_de_especies[1:92]
area1 <- area[1:92]
variavel_catagorica1_1 <- variavel_catagorica1[1:92]
variavel_catagorica2_1 <- variavel_catagorica2[1:92]

# Tentar criar novamente o data frame
df <- data.frame(n_sp = numero_de_especies1,
                 area = area1,
                 cat1 = variavel_catagorica1_1,
                 cat2 = variavel_catagorica2_1,
                 cat3 = variavel_catagorica3)

str(df)

# Solução 2 - aumentar o comprimento dos vetores
variavel_catagorica1[94] <- NA
variavel_catagorica3[93:94] <- NA

# Tentar criar novamente o data frame
df2 <- data.frame(n_sp = numero_de_especies,
                 area = area,
                 cat1 = variavel_catagorica1,
                 cat2 = variavel_catagorica2,
                 cat3 = variavel_catagorica3)

str(df2)

numero_de_especies[33] <- 370

boxplot(numero_de_especies ~ variavel_catagorica1)

numero_de_especies[33] <- 37

boxplot(numero_de_especies ~ variavel_catagorica1)

# Gravar um data frame no diretório de trabalho  -------------------------------------------
write.table(df, "df.txt")
write.table(df2, "df2.txt")

# Indexação de data frames  -------------------------------------------
df[5, 3] 
df[60:70, 1:2]
df[c(15, 33, 41), c(1, 2, 4)]

# Indexação por lógica  -------------------------------------------
numero_de_especies > 40
numero_de_especies[numero_de_especies > 40]

variavel_catagorica1 == "B"
numero_de_especies[variavel_catagorica1 == "B"]

# valores de area > 50 OU valores de area < 20
area[area > 50 | area < 20]

# número de espécies <= 40 E número de espécies > 30
numero_de_especies[numero_de_especies <= 40 & numero_de_especies > 30]
numero_de_especies[numero_de_especies %in% 31:40]

# número de espécies pertencentes a categoria "C" (variavel_categorica1)
numero_de_especies[variavel_catagorica1 == "C"]

