# 3a Aula de Análise Exploratória de dados (Modulos)

site <- "https://raw.githubsercontent.com/"
diretorio <- "gustavomirapalheta/classes_datasets/master/"
arquivo <- "formandos.csv"
fonte <- paste0(site, diretorio, arquivo, sep="")
fonte

fonte <- "https://raw.githubusercontent.com/gustavomirapalheta/classes_datasets/master/formandos.csv"

library(rio)
library(dplyr)
dados <- as_tibble(import(fonte, encoding='Latin-1'))
dados

#Média de variável salário
dados$salario %>% mean()
dados$salario %>% median()

filter(dados, salario > 0) -> dados2

hist(dados2$salario)
hist(dados2$salario)

mean(dados2$salario)
median(dados2$salario)


# Erro do modelo "benchmark" ou "bobo"

media = mean(dados2$salario); media
n = length(dados2$salario); n
y = dados2$salario; y

SSE = sum((y - media)^2); SSE
erro1 = sqrt(SSE / (n-1)); erro1


# 2o Modelo: Salario = F(sexo)
#salario^= b0 + b1 = Sexo

dados2$sexo
unique(dados2$sexo)
lm(dados2, formula = salario ~ sexo) -> modelo2; modelo2
predict(modelo2, dados2) -> y2; y2

erro2 = sqrt(sum((y2-y)^2)/(n-1)); erro2

library(ggplot2)
ggplot(dados2, aes(x=sexo, y=salario)) +
  geom_boxplot()

#3o Modelo: salário = f(sexo) (sem o outlier masculino)
filter(dados2, salario < max(salario)) -> dados3
ggplot(dados3, aes(x=sexo, y=salario)) + geom_boxplot()

lm(dados3, formula = salario ~ sexo) -> modelo3; modelo3
summary(modelo3)

summary(modelo3)

# 4o Modelo: Salário = f(setor)
ggplot(dados3, aes(x=setor, y=salario)) + geom_boxplot()

lm(dados3, formula = salario ~ setor) -> modelo4; modelo4
summary(modelo4)

#5o Modelo: salario = f(desempenho)
unique(dados3$desempenho)

lm(dados3, formula = salario ~ desempenho) ->modelo5; modelo5

mutate(dados3,
       desempenho = ifelse(desempenho == "fraco", "0.fraco", desempenho)) -> dados4; dados4

lm(dados4, formula = salario ~ desempenho) -> modelo5b; modelo5b

summary(modelo5b)

mutate(dados4, desempenho = ifelse(desempenho == "regular",
                      "1.regular", desempenho)) -> dados5
dados5

ggplot(dados5,aes(x=desempenho, y=salario)) + geom_boxplot()

#6o Modelo: salário = f(idade)
lm(dados5, formula = salario ~ idade) -> modelo6; modelo6
summary(modelo6)

#7o Modelo: Salário = f(experiencia)

lm(dados5, formula = salario ~ experiencia) -> modelo7; modelo7
summary(modelo7)

#8o Modelo: Salário = f(idade, experiencia)

lm(dados5, formula = salario ~ idade + experiencia) -> modelo8; modelo8
summary(modelo8)

#9o Modelo: Salário = f(lingua)
unique(dados5$lingua)

lm(dados5, formula = salario ~ lingua) -> modelo9; modelo9
summary(modelo9)


sum(dados5$lingua == "Outra")

#10o Modulo: salário = f(avaliação geral)
dados5$avaliacao_geral
unique(dados5$avaliacao_geral)

library(stringr)
str_replace(c("4,3","5,1","9,7"),",",".")

mutate(dados5,
       avaliacao_geral = str_replace(avaliacao_geral, ",","."),
       avaliacao_geral = as.numeric(avaliacao_geral)) -> dados6; dados6

lm(dados5, formula = salario ~ avaliacao_geral) -> modulo10; modulo10
summary(modulo10)

#11o Modelo: salário = f(admissão)
names(dados6)

lm(dados6, formula = salario ~ admissao_bruto + admissao_quant + admissao_verbal + admissao_geral) -> modulo11
summary(modulo11)

#12o Modelo: Salário = f(Todas as variaveis)
select(dados6, ~ registro) %>%

lm(dados6,
   formula = salario ~ idade + experiencia + setor + desempenho) -> modelo12
summary(modelo12)

#Final da 2a Aula de Análise Exploratória de Dados

x <- tibble(nome = c("John", "Paul", "Ringo", "Rarrison", "Peter"),
            instrumento = c("guitarra", "baixo", "bateria", "guitarra", "teclado"))


y <- tibble(nome = c("John", "Paul", "Ringo", "Harrison", "Sruart", "Davies"),
            banda = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))


library(dplyr)
inner_join(x,y,by = c("nome"="nome"))

full_join(x,y)


#Reformatação de tabelas
library(tidyr)

table2
spread(table2, key=type, value = count)

table4a
gather(table4a, c('1999', '2000'), key="year", value = "cases") -> table4a2; table4a2

table4b
gather(table4b, c('1999', '2000'), key="year", value="population") -> table4b2; table4b2

inner_join(table4a2, table4b2, by = c("nome"="nome", "country"="country"))

table3
separate(table3, col="rate", into=c("cases","population"), sep="/")

table5
unite(table5, col="year", c("century", "year"), sep="")

