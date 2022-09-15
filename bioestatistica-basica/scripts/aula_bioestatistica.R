#Script para criar gráficos da altura da turma
# Tue Sep  6 15:36:20 2022 ------------------------------

#packages ----
library(dplyr)
library(ggplot2)
library(readxl)

#import data frame ----
dados <- read_excel("data/raw/altura_turma.xlsx")
dados2 <- read_excel("data/raw/temp_dossel.xlsx")

#boxplot ----
boxplot(dados$altura~dados$sexo, xlab = "Sexo", ylab = "Altura (cm)")

#scatterplot ----
plot(dados$altura, dados$pe, xlab = "Altura (cm)", ylab = "Tamanho do tênis")

ggplot(dados, aes(x = altura, y = pe, color = sexo))+
  geom_point(size = 3)+
  labs(x = "Altura (cm)", y = "Tamanho do tênis")+
  theme_bw(base_size = 20)

#hist ----
hist(dados$altura)

#Distribuicao normal ----
plot(density(dados$altura))

shapiro.test(dados$altura)

#plotando dados temperatura ----
ggplot(dados2, aes(x = dossel, y = temp))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", col = "brown4", size = 2.3, pch = 8)+
  labs(x = "Cobertura dossel", y = "Temperatura (°C)")+
  theme_bw(base_size = 20)
