#Script para criar gráficos da altura da turma
# Tue Sep  6 15:36:20 2022 ------------------------------

#packages ----
library(dplyr)
library(ggplot2)

#import data frame ----
dados <- read_excel("data/raw/altura_turma.xlsx")

#boxplot ----
boxplot(dados$altura~dados$sexo)

#scatterplot ----
plot(dados$altura, dados$pe)

ggplot(dados, aes(x = altura, y = pe, color = sexo))+
  geom_point(size = 3)+
  theme_bw(base_size = 20)

#hist ----
hist(dados$altura)
