if(!require(data.table))
  install.packages("data.table")
library(data.table)

if(!require(readr))
  install.packages("readr")
library(readr)

if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if(!require(qqplotr))
  install.packages("qqplotr")
library(qqplotr)


if(!require(ggpubr))
  install.packages("ggpubr")
library(ggpubr)

if(!require(rstatix))
  install.packages("rstatix") 
library(rstatix)   

if(!require(sqldf))
  install.packages("sqldf")
library(sqldf)

if(!require(pacman)) 
  install.packages("pacman")
library(pacman)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)


#Analisando dados do patrim�nio
# Dispon�vel em https://www.kaggle.com/datasets/ashishjangra27/ted-talks


# Escopo


# banco ou conjunto de dados

# an�lise estatistica contendo:
  
# 1. an�lise descritiva
#  a. medias resumo justificadas (o seu racional para escolher estas medidas)
#  b. gr�fico justificado

# 2. an�lise inferencial (justificando a escolha de cada teste ou an�lise)
#  2.1 pelo menos um teste com multiplas vari�veis
#  2.2 pelo menos um teste com duas v�riaveis
#  2.3 pelo menos uma regress�o

# 3. interpretar os resultados

# enviar para amdeana@uni9.pro.br at� segunda ate 23:59

dados <- read.csv(file.choose())

# Ideias
# Gr�fico com valor pelo estado
# Media do valores de patrimonio
# grafico de compara��o de estado

# Apenas selecionando colunas que vamos utilizar
dados_filtrados <- filter(dados, views > 30000, likes > 3000)

# Quais s�o candidatos a terem uma rela��o linear?

# Aparentemente sim, os dados tem uma rela��o linear
plot(views ~ likes, data = dados_filtrados)

#Fazendo modelo linear

modelo <- lm(views ~ likes, dados_filtrados)
summary(rstandard(modelo))
plot(modelo)


#normalidade dos res�duos

shapiro_test(modelo$residuals)


#outliers
summary(rstandard(modelo))


summary(modelo)


top_in_view_and_like = filter(dados_filtrados, views > 9000000, likes > 1400000)


top_in_view_and_like %>%
  ggplot(aes(x =  author, y = likes))+
  geom_point()+
  geom_smooth(method = 'lm', col = 'red')+
  theme_classic()

leveneTest(views~likes, dados)


