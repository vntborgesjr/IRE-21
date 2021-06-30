# --------------------------------------------------- 
# 03 - Introdução ao R - Funções 
# 05 jun 2021 
# VNTBJR 
# --------------------------------------------------- 
#
# Funções como "caixa preta"  -------------------------------------------
valores <- 1:10
mean(valores) # média
sd(valores) # desvio padrão
max(valores) # maximo
min(valores) # mínimo
range(valores) # amplitude
median(valores) # mediana
quantile(valores) # inter-quartil

# Argumentos  -------------------------------------------
args(mean)
args(sd)
args(max)
args(min)
args(range)
args(median)
args(quantile)
args(lm)
args(glm)

# Exemplo simples - função soma()  -------------------------------------------
# função que soma dois valores
soma <- function(x, y) {
  print(x + y)
}

# usando a função soma
soma(2, 5)
v1 <- 1:10
v2 <- 1:10
soma(v1, v2)

# Outro exemplo simples - função razão()  -------------------------------------------
# função que calcula a razão entre dois valores
razao <- function(valor1, valor2){
  print(valor1/valor2)
}

# uso da função
razao(valor1 = 10, valor2 = 5)

# Uso de aragumentos  -------------------------------------------

# forma mais 'correta'
razao(valor1 = 10, valor2 = 5)

# mesmo resultado
razao(valor2 = 5, valor1 = 10)

# não é necessário forcencer os nomes dos argumentos
razao(10, 5)
razao(5, 10)
razao(valor1 = 5, valor2 = 10)

# Adicionando argumento default - numérico  -------------------------------------------
# vamos dar um valor padrão para um dos tratamentos
razao <- function(valor1, valor2 = 100){
  print(valor1/valor2)
}

# se não especificarmos o argumento valor2
razao(valor1 = 50)

# e se especificarmos o argumento valor2?
razao(valor1 = 10, valor2 = 5)

# Usando objetos dentro da função  -------------------------------------------
# salvando resultado do cálculo em um objeto dentro da função
razao <- function(valor1 = 10, valor2 = 100){
  resposta <- valor1/valor2
}

# usando a função - cade o resultado?
razao(valor1 = 10)

# obrigando o R a mostrar o resultado
(razao(valor1 = 10, valor2 = 5))

# outra forma de obter o resultado
cade <- razao(valor1 = 10, valor2 = 5)
cade

# a forma adequada - usando a função print()
razao <- function(valor1 = 10, valor2 = 100){
  resposta <- valor1/valor2
  print(resposta)
}

# usando função
razao(valor1 = 10, valor2 = 5)
razao()

# outra forma adequada - adicionando o controlador de fluxo return()
razao <- function(valor1 = 10, valor2 = 100){
  resposta <- valor1/valor2
  return(resposta)
}

# usando função
razao(valor1 = 10, valor2 = 5)
razao()

# Função mais complexa - jogar dados  -------------------------------------------
# criar o dado
dado <- 1:6

# simular um arremeso 
# como é uma aleatorização, precisamos usar o set.seed
set.seed(0)

# queremos apenas um arremeso da moeda
numero_de_arremessos <- 1
sample(x = dado, size = numero_de_arremessos)

# testando com 10 arremessos
# para que sample funcione precisamos "setar" o argumento replace = TRUE
set.seed(0)
numero_de_arremessos <- 10
sample(x = dado, size = numero_de_arremessos)
set.seed(0)
sample(x = dado, size = numero_de_arremessos, replace = TRUE)

# criando a função
# criando função
arremecar_dado <- function(numero_de_arremessos) {
  dado <- 1:6
  resultado <- sample(x = dado, size = numero_de_arremessos,
                      replace = TRUE)
  return(resultado)
}
# lancando moeda 33 vezes
set.seed(0)
arremecar_dado(numero_de_arremessos = 33)

# usando a função table para facilitar a visualização dos reusltados
set.seed(0)
table(arremecar_dado(numero_de_arremessos = 33))

# Arremesso de dado viciado  -------------------------------------------

dado <- 1:6
n_de_arremessos <- 10
# vetor de probabilidades
probabilidades <- c(0.29, 0.29, 0.22, 0.11, 0.05, 0.04)
sum(probabilidades) # a soma deve ser = 1
set.seed(100)
table(sample(x = dado, size = n_de_arremessos, replace = TRUE, prob = probabilidades))
# note o aumento na frequência de valores baixos

# Arremossos de um dado viciado
# arremesso de dado viciado
arremecar_dado <- function(numero_de_arremessos, probabilidades) {
  dado <- 1:6
  resultado1 <- sample(x = dado, size = numero_de_arremessos,
                       replace = TRUE, probabilidades)
  resultado2 <- table(sample(x = dado, size = numero_de_arremessos,
                             replace = TRUE, probabilidades))
  return(list(Bruto = resultado1, Frequencias = resultado2))
}

# testar a função
set.seed(100)
arremecar_dado(numero_de_arremessos = 33, probabilidades = probabilidades)

# Argumento default numérico  -------------------------------------------
# arremesso de dado honesto
arremecar_dado <- function(numero_de_arremessos, probabilidades = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)) {
  dado <- 1:6
  resultado1 <- sample(x = dado, size = numero_de_arremessos,
                       replace = TRUE, probabilidades)
  resultado2 <- table(sample(x = dado, size = numero_de_arremessos,
                             replace = TRUE, probabilidades))
  return(list(Bruto = resultado1, Frequencias = resultado2))
}

# testar a função
set.seed(100)
arremecar_dado(numero_de_arremessos = 33)


# Analisando diferentes planilhas  -------------------------------------------
df_bruto1 <- read.table("df1.txt", header = TRUE)
df_limpo1 <- df_bruto1[, c('n_sp', 'area', 'isolamento')]
df_limpo1$isolamento <- as.factor(isolamento)
df_limpo1 <- df_bruto1[!is.na(df_limpo1$n_sp), ]

df_bruto2 <- read.table("df2.txt", header = TRUE)
df_limpo2 <- df_bruto2[, c('n_sp', 'area', 'isolamento')]
df_limpo2$isolamento <- as.factor(df_limpo3$isolamento)
df_limpo2 <- df_bruto2[!is.na(df_limpo2$n_sp), ]

df_bruto3 <- read.table("df3.txt", header = TRUE)
df_limpo3 <- df_bruto3[, c('n_sp', 'area', 'isolamento')]
df_limpo3$isolamento <- as.factor(df_limpo3$isolamento) 
df_limpo3 <- df_bruto3[!is.na(df_limpo3$n_sp), ]

df_bruto4 <- read.table("df4.txt", header = TRUE)
df_limpo4 <- df_bruto4[, c('n_sp', 'area', 'isolamento')]
df_limpo4$isolamento <- as.factor(df_limpo4$isolamento)
df_limpo4 <- df_bruto4[!is.na(df_limpo4$n_sp), ]

# Converter script em função  -------------------------------------------
importar_dados <- function(nome_do_arquivo) { 
  df_bruto <- read.table(nome_do_arquivo, header = TRUE) # nome generalista
  df_limpo <- df_bruto[, c('n_sp', 'area', 'isolamento')] # nome generalista
  df_limpo$isolamento <- as.factor(df_limpo$isolamento)
  df_limpo <- df_bruto[!is.na(df_limpo$n_sp), ]
}

# Teste da função
df1 <- importar_dados("df1.txt")
df2 <- importar_dados("df2.txt")
df3 <- importar_dados("df3.txt")
df4 <- importar_dados("df4.txt")

# rm(list = ls())

# Converter scripts em funções (alternativa)  -------------------------------------------
# Primeira função
importar_dados <- function(nome_do_arquivo) { 
  df_limpo <- df_bruto[!is.na(df_bruto$n_sp), 
                         c('n_sp', 'area', 'isolamento')]
}  

# Teste da função
df1 <- importar_dados("df1.txt")
df2 <- importar_dados("df2.txt")
df3 <- importar_dados("df3.txt")
df4 <- importar_dados("df4.txt")

# Controladores de fluxo - if() {} else {}  -------------------------------------------
# função ifelse()
v1 <- c(1:100)
ifelse(test = v1 > 50, yes = 1, no = 0)

# vamos criar um objeto com o número 5
x <- 5
# e vamos usar a estrutura de controle if, else
teste_condicional <- if(x < 10){
  "número menor que 10"
} else {
  "número é maior que 10"
}

teste_condicional

# vamos criar um objeto com o número 20
x <- 20
# e vamos usar a estrutura de controle if, else
teste_condicional <- if(x < 10){
  "número menor que 10"
} else {
  "número é maior que 10"
}

teste_condicional

# vamos criar um objeto com o número 10
x <- 10
# e vamos usar a estrutura de controle if, else
teste_condicional <- if(x < 10){
  "número menor que 10"
} else if(x == 10) {
  "número é igual a 10"
} else {
  "número é maior que 10"
}

teste_condicional

# Argumento default lógico  -------------------------------------------
# arremesso de dado honesto com controle parcial de resultados
arremecar_dado <- function(numero_de_arremessos, 
                           probabilidades = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6), 
                           resu = TRUE) {
  dado <- 1:6
  resultado1 <- sample(x = dado, size = numero_de_arremessos,
                       replace = TRUE, probabilidades)
  resultado2 <- table(sample(x = dado, size = numero_de_arremessos,
                             replace = TRUE, probabilidades))
  resultados <- list(Bruto = resultado1, Frequencias = resultado2)
  if(resu == TRUE) {
    return(resultados$Frequencias)
  } else {
    return(resultados$Bruto)
  }
}

# testar a função
set.seed(100)
arremecar_dado(numero_de_arremessos = 33, resu = TRUE)
set.seed(100)
arremecar_dado(numero_de_arremessos = 33, resu = FALSE)

# Argumento default categórico  -------------------------------------------
# arremesso de dado honesto com controle total de resultados
arremecar_dado <- function(numero_de_arremessos, 
                           probabilidades = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6), 
                           resu = c("Ambos", "Bruto", "Frequencias")) {
  dado <- 1:6
  resultado1 <- sample(x = dado, size = numero_de_arremessos,
                       replace = TRUE, probabilidades)
  resultado2 <- table(sample(x = dado, size = numero_de_arremessos,
                             replace = TRUE, probabilidades))
  resultados <- list(Bruto = resultado1, Frequencias = resultado2)
  resu <- match.arg(resu)
  if(resu == "Frequencias") {
    return(resultados$Frequencias)
  } else if(resu == "Bruto") {
    return(resultados$Bruto)
  } else {
    return(resultados)
  }
}

# testar a função
set.seed(100)
arremecar_dado(numero_de_arremessos = 33)
arremecar_dado(numero_de_arremessos = 33, resu = "Frequencia")
arremecar_dado(numero_de_arremessos = 33, resu = "Bruto")
arremecar_dado(numero_de_arremessos = 33, resu = "Ambos")

# Controladores de fluxo - for() {}  -------------------------------------------
for (i in 1:10) { # para cada i de 1 a 10
  print(i) # imprima o valor de i
  Sys.sleep(time = 1) # aguarde 1 segundo antes de
  # prosseguir
  # repita a operaçao
}

for (i in 1:10) { # para cada i de 1 a 10
  plot(x = i, y = i) # imprima o valor de i
  Sys.sleep(time = 1) # aguarde 1 segundo antes de
  # prosseguir
  # repita a operaçao
}

v1 <- 50:60
for (i in 1:length(v1)) { # para cada i de 1 a 10
  print(v1[i]) # imprima o valor de i
  Sys.sleep(time = 1) # aguarde 1 segundo antes de
  # prosseguir
  # repita a operaçao
}

df
for (i in 1:nrow(df)) { # para cada i de 1 a 10
  print(df[i, ]) # imprima o valor de i
  Sys.sleep(time = 1) # aguarde 1 segundo antes de
  # prosseguir
  # repita a operaçao
}

for (i in 1:nrow(df)) { # para cada i de 1 a 10
  for (j in 1:ncol(df)) {
    print(df[i, j]) # imprima o valor de i
    Sys.sleep(time = 1) # aguarde 1 segundo antes de
    # prosseguir
    # repita a operaçao 
  }
}

# Exemplo: sequência de Fibonacci  -------------------------------------------
# primeiro criamos um objeto com os dois primeiros números da sequência
vetor1 <- c(1, 1)

# o que acontece se indexarmos a segunda posição deste
# vetor?
vetor1[3]

# Some a primeira e a segunda posição do `vetor1` através da indexação
vetor1[1] + vetor1[2]
vetor1[3] <- vetor1[1] + vetor1[2]
i <- 3
vetor1[i] <- vetor1[i-2] + vetor1[i-1]

# Use o `for() {}` para criar a sequência Fibonacci com 10 elementos
for(i in 3:10) {
  vetor1[i] <- vetor1[i-2] + vetor1[i-1]
}
vetor1

# Controlador de fluxo - while() {}  -------------------------------------------
contagem  <- 0
while (contagem < 10) {
  print(contagem)
  Sys.sleep(0.5)
  contagem <- contagem + 1
}

# Combinando o while com outros controladores de fluxo
# quanto dinheiro tinha na minha conta?
minha_conta <- 0
# como ela vai flutuar?
while(minha_conta < 100 & minha_conta > -100){ 
  # enquanto o que eu tiver na conta estiver em 100 e -100
  print(minha_conta) # imprime o saldo
  destino <- sample(x = c("triste", "feliz"), size = 1) 
  # ai entra o destino, como sempre
  if(destino == "feliz") { # se o destino for feliz
    # deposita 20 na minha conta  
    minha_conta <- minha_conta + 20 
  } else { # se o destino for triste
    # tira 20 da minha conta
    minha_conta <- minha_conta - 20 
  }
  Sys.sleep(0.5)
}

# Controladores de fluxo - repeeat() + break()  -------------------------------------------
x <- 1
repeat { # aqui está um controlador de fluxo
  print(x)
  Sys.sleep(time = 1)
  x = x+1
  if (x == 6){
    break # aqui está o outro controlador de fluxo
  }
}

# Controladores de fluxo - next()  -------------------------------------------
x <- 1:10
for (val in x) {
  if (val > 3 & val < 6){
    next # aqui está o controlador de fluxo
  }
  print(val)
  Sys.sleep(time = 1)
}

