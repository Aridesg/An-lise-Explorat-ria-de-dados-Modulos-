#Primeira aula exploratoria de dados
2+3
#Install.packages("tidyverse")
library(ggplot2) # Pacote grafico 
library(dplyr) # Manipulação de dados
library(tidyr) # Ajuste de formato  
mpg 

?mpg  #atalho Ctrl+L limpa a tela


names(mpg)  #execultar a ação Ctrl+Enter

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class, size=cyl)) + 
  geom_smooth(method='lm') +
  facet_wrap(~manufacturer)

lm(mpg, formula = hwy ~ displ)

mpg %>% filter(manufacturer == 'toyota') ->mpg2
mpg2

lista = unique(mpg$manufacturer)
lista

for (i in lista) {print(filter(mpg, manufacturer==i))}


unique(mpg$drv)

lm(mpg, formula = hwy ~ displ + drv)

mpg %>% mutate(drv2 = drv) %>%
  mutate(drv2 =ifelse(drv2=="f", "0.f", drv2)) %>%
  lm(formula = hwy ~ displ + drv2)
  
mpg

getwd()

write.csv2(mpg, "mpg.csv", row.names=FALSE)

read.csv2("mpg2.csv") -> mpg2
mpg2
# primeiro comando para instalar o pacote rio - install.packages(rio)
install.packages("rio", dependencies=TRUE) 

import("mpg2.xlsx") %>% as_tibble() -> mpg3
mpg3

import("mpg2.csv", dec=",") %>% as_tibble() -> mpg4
mpg4

dados = tibble(cor = c("azul, preto, preto, azul, azul"),
               valor = 1:5); dados

summarise(dados, total = sum(valor))

group_by(dados, cor) -> dados2; dados2

summarise(dados, total = sum(valor))
