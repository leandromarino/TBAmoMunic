################################################################################
#####======================================================================#####
#####                                                                      #####
#####   IDENTIFICACAO                                                      #####
#####                                                                      #####
#####======================================================================#####
################################################################################

###   DEFINICAO DO TRABALHO E DE VARIAVEIS DE AMBIENTE
options(width=180,scipen=50,repos = 'http://cran-r.c3sl.ufpr.br/')


#instalar pacote a partir de zip local
#dirpacote<-'C:\\Users\\leandromarino\\Documents\\Projetos\\TBAmoMunic\\trunk\\'
#install.packages(paste(dirpacote,'RCurl_1.95-3.zip'))

### CARREGANDO OS PACOTES NECESSARIOS
library(xlsReadWrite)
library(SOAR)
library(survey)
library(stratification)
library(sampling)
require(RCurl)
require(foreign)


### LEITURA DOS DADOS
## Inicialmente o arquivo estava em formato .xlsx e foi convertido no Excel 2010
## para o .xls
url <- 'https://raw.github.com/leandromarino/TBAmoMunic/master/Munic.dat'
url <- getURL(url,ssl.verifypeer = FALSE)
munic <- read.table(textConnection(url), colClasses='character',header=T,quote='',
         sep='\t')
str(munic)


p1a.cv <- p1b.cv <- p1c.cv <- p1d.cv <- 
p2a.cv <- p2b.cv <- p2c.cv <- p2d.cv <- 
p3a.cv <- p3b.cv <- p3c.cv <- p3d.cv <- 
p4a.cv <- p4b.cv <- p4c.cv <- p4d.cv <- 
p5a.cv <- p5b.cv <- p5c.cv <- p5d.cv <- NA


#Y1 = FuncADMD    - Total de funcionario ativos da administracao direta;
#Y2 = Maternidade - Existe Maternidade no municipio?
#Y3 = Emergencia  - Existe Unidade de emergencia no municipio?
munic <- transform(munic,
            Populacao=as.integer(Populacao),
            FuncADMD=as.integer(FuncADMD),
            Regiao=substr(CodMunic,1,1))
munic <- transform(munic,
            mater = ifelse(Maternidade=="Sim",1,ifelse(Maternidade=='Não',0,NA)),
            emerg    = ifelse(Emergencia=="Sim",1,ifelse(Emergencia=='Não',0,NA)))
munic <- transform(munic,
            materemerg = ifelse(rowSums(cbind(mater,emerg)) == 2,1,0))
munic <- munic[rowSums(is.na(munic))==0,]
dim(munic)
munic[1:10,]





#####----------------------------------------------------------------------#####
#####----------------------------------------------------------------------#####
### ITEM 1 - Use os valores populacionais das variaveis de interesse para
### determinar o CV esperado para os estimadores dos parametros de interesse.

### PLANO 1 - AAS
### PLANO 2 - AES de Munic por Reg com Aloc igual
### PLANO 3 - AES de Munic por Pop talque sqrt(Estrado_i) == sqrt(Estrato_j) 
###           \forall i,j \in 1:5
### PLANO 4 - AES por 'corte' pi = 1 se pop >= 500.000, aas nos demais
### PLANO 5 - AAS com uso do estimador de razao usando var aux o tot da pop


#   Considere de interesse estimar os seguintes parâmetros populacionais:
#   a) Total de funcionários ativos da administração direta;
#   b) Razão da população por funcionário ativo da administração direta;
#   c) Proporção de municípios com maternidade;
#   d) Proporção de municípios com maternidade e emergência.
#   Considere a idéia de selecionar uma amostra de n=200 municípios para uma
# pesquisa por amostragem junto à população de municípios, com um dos planos 
# amostrais abaixo indicados.

N <- nrow(munic)
n <- 200    
(f <- n/N)

#######################
## Plano 1
#######################
#   a) Total de funcionários ativos da administração direta
(p1a.var <- N^2 * ( 1/n - 1/N ) * var(munic$FuncADMD))
(p1a.tot <- sum(munic$FuncADMD))
(p1a.cv <- sqrt(p1a.var)/p1a.tot)

#   b) Razão da população por funcionário ativo da administração direta;
med = mean(munic$FuncADMD)
(p1b.raz <- sum(munic$Populacao)/sum(munic$FuncADMD))
(p1b.var <- (1-f)/(n*med^2)*1/(N-1)*sum((munic$Populacao-p1b.raz*munic$FuncADMD)^2))
(p1b.cv <- sqrt(p1b.var)/p1b.raz)

#   c) Proporção de municípios com maternidade;
(p1c.prop <- mean(munic$mater))
(p1c.var <- ((N-n)/(N-1)) * p1c.prop * (1-p1c.prop)*(1/n))
(p1c.cv <- sqrt(p1c.var)/p1c.prop)

#   d) Proporção de municípios com maternidade e emergência.
(p1d.prop <- mean(munic$materemerg))
(p1d.var <- ((N-n)/(N-1)) * p1d.prop * (1-p1d.prop)*(1/n))
(p1d.cv <- sqrt(p1d.var)/p1d.prop)


#######################
## Plano 2
#######################
estrato <- munic$Regiao
(Nest <- matrix(table(estrato),ncol=5))
(nest <- rep(200/5,5))
(Wh <- Nest/N)
est.munic <- split(munic,factor(estrato))

#   a) Total de funcionários ativos da administração direta
func <- split(munic$FuncADMD,estrato)
(tot.est <- do.call(c,lapply(func,sum)))
(p2a.tot <- sum(tot.est))
(var.intra <- do.call(c,lapply(func,var)))
(p2a.var <- N^2  * sum(Wh^2 * (1/nest - 1/Nest)*var.intra))
(p2a.cv <- sqrt(p2a.var)/p2a.tot)


#   b) Razão da população por funcionário ativo da administração direta;
#### ver no cochran
#med = mean(munic$FuncADMD)
#(p2b.raz <- sum(munic$Populacao)/sum(munic$FuncADMD))
#(p2b.var <- (1-f)/(n*med^2)*1/(N-1)*sum(munic$Populacao-p2b.raz*munic$FuncADMD)^2)
#(p2b.cv <- sqrt(p2b.var)/p2b.raz)


#   c) Proporção de municípios com maternidade;
mater <- split(munic$mater,estrato)
(prop.est <- do.call(c,lapply(mater,mean)))
(p2c.prop <- sum(Wh*prop.est))
var.intra <- ((Nest-nest)/(Nest-1)) * prop.est * (1-prop.est)*(1/nest)
(p2c.var <-  sum(Wh^2 * (1/nest - 1/Nest)*var.intra))
(p2c.cv <- sqrt(p2c.var)/p2c.prop)   

#   d) Proporção de municípios com maternidade e emergência.
materemerg <- split(munic$materemerg,estrato)
(prop.est <- do.call(c,lapply(materemerg,mean)))
(p2d.prop <- sum(Wh*prop.est))
var.intra <- ((Nest-nest)/(Nest-1)) * prop.est * (1-prop.est)*(1/nest)
(p2d.var <-  sum(Wh^2 * (1/nest - 1/Nest)* var.intra))
(p2d.cv <- sqrt(p2d.var)/p2d.prop)   





resumo <- data.frame(plano = paste('Plano',1:5),
                     parametro_a=round(c(p1a.cv,p2a.cv,p3a.cv,p4a.cv,p5a.cv),4),
                     parametro_b=round(c(p1b.cv,p2b.cv,p3b.cv,p4b.cv,p5b.cv),4),
                     parametro_c=round(c(p1c.cv,p2c.cv,p3c.cv,p4c.cv,p5c.cv),4),
                     parametro_d=round(c(p1d.cv,p2d.cv,p3d.cv,p4d.cv,p5d.cv),4))
resumo