# Define pasta de trabalho onde encontrar arquivos de dados
setwd("C:/RJ/Ensino/Amostragem2012/Rdata")
# Carrega pacotes requeridos
library(stratification)
library(sampling)
library(survey)
# Lê o arquivo de dados com a população
fazendas = read.csv(file="Fazendas.csv", header=TRUE, sep=";")
str(fazendas) 
# Transforma em fatores as variáveis categóricas do arquivo de dados
fazendas = transform(fazendas, CLASSE = as.factor(CLASSE))
fazendas = transform(fazendas, REGIAO = as.factor(REGIAO))
(estratos1 = strata.LH(fazendas$AREA, initbh=c(36, 50, 72), n=50, Ls=4, takeall=1))
str(estratos1)
fazendas = transform(fazendas, estrato=1)
aux1 = (fazendas$AREA > 49.5 & fazendas$AREA <= 78) 
fazendas$estrato[aux1] = 2
aux1 = (fazendas$AREA > 78 & fazendas$AREA <= 131) 
fazendas$estrato[aux1] = 3
aux1 = (fazendas$AREA > 131) 
fazendas$estrato[aux1] = 4
table(fazendas$estrato)
estratos1$nh
# Seleciona as unidades da amostra AES
(fazendas.amo = strata(data=fazendas, stratanames=c("estrato"), estratos1$nh,
                       method=c("srswor")))