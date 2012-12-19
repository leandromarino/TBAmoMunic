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
munic.exc <- munic[rowSums(is.na(munic))!=0,]
munic <- munic[rowSums(is.na(munic))==0,]
dim(munic)
munic[1:10,]

for( i in 1:5){
print( sum(munic.exc[munic.exc$Regiao==i,'Populacao']) /
      (sum(munic.exc[munic.exc$Regiao==i,'Populacao']) +
       sum(munic[munic$Regiao==i,'Populacao'])) *100)
}



#####----------------------------------------------------------------------#####
#####----------------------------------------------------------------------#####
### ITEM 1 - Use os valores populacionais das variaveis de interesse para
### determinar o CV esperado para os estimadores dos parametros de interesse.

### PLANO 1 - AAS
### PLANO 2 - AES de Munic por Reg com Aloc igual
### PLANO 3 -   AES de Munic por Pop talque sqrt(Estrado_i) == sqrt(Estrato_j) 
###           \forall i,j \in 1:5
### PLANO 4 - AES por 'corte' pi = 1 se pop >= 500.000 e cap, aas nos demais
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
(fest <- nest/Nest)
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
func <- split(munic$FuncADMD,estrato)
pop <-  split(munic$Populacao,estrato)
(p2b.raz <- sum(munic$Populacao)/sum(munic$FuncADMD))
(raz.est <- p2b.raz)
(medest <- do.call(c,lapply(func,mean)))
(X <- sum(munic$FuncADMD))
(var.intraX <- do.call(c,lapply(func,var)))
(var.intraY <- do.call(c,lapply(pop,var))) 
(sd.intraX <- sqrt(var.intraX))
(sd.intraY <- sqrt(var.intraY))
cor.XY <- list()
for(i in 1:5) cor.XY[[i]] <- cor(func[[i]],pop[[i]])
(cor.XY <- do.call(c,cor.XY)) 
(p2b.var <- (1/X^2) * sum(Nest^2 * (1- fest)/nest * (var.intraY + 
         raz.est^2*var.intraX - 2 * raz.est * sd.intraY * sd.intraX * cor.XY)))
(p2b.cv <- sqrt(p2b.var)/p2b.raz)


#   c) Proporção de municípios com maternidade;
mater <- split(munic$mater,estrato)
(prop.est <- do.call(c,lapply(mater,mean)))
(p2c.prop <- sum(Wh*prop.est))
(var.intra <- do.call(c,lapply(mater,var)))
(p2c.var <-  sum(Wh^2 * (1/nest - 1/Nest)*var.intra))
(p2c.cv <- sqrt(p2c.var)/p2c.prop)   

#   d) Proporção de municípios com maternidade e emergência.
materemerg <- split(munic$materemerg,estrato)
(prop.est <- do.call(c,lapply(materemerg,mean)))
(p2d.prop <- sum(Wh*prop.est))
(var.intra <- do.call(c,lapply(materemerg,var)))
(p2d.var <-  sum(Wh^2 * (1/nest - 1/Nest)* var.intra))
(p2d.cv <- sqrt(p2d.var)/p2d.prop)   




#######################
## Plano 3
#######################
sum(munic$Populacao)/5
munic <- transform(munic,sqrtPop = sqrt(Populacao))
munic <- munic[order(munic$sqrtPop),]

sum(munic$sqrtPop)/5

aux <- matrix(NA,nrow=nrow(munic),ncol=5)
for(i in 1:5){
aux[,i] <- cumsum(munic$sqrtPop) <= (sum(munic$sqrtPop)/5)*i
}
estrato <- 6-rowSums(aux)
table(estrato)
for(i in 1:5) print(sum(munic$sqrtPop[estrato==i]))


(Nest <- matrix(table(estrato),ncol=5))
(nest <- rep(200/5,5))
(Wh <- Nest/N)
est.munic <- split(munic,factor(estrato))

#   a) Total de funcionários ativos da administração direta
func <- split(munic$FuncADMD,estrato)
(tot.est <- do.call(c,lapply(func,sum)))
(p3a.tot <- sum(tot.est))
(var.intra <- do.call(c,lapply(func,var)))
(p3a.var <- N^2  * sum(Wh^2 * (1/nest - 1/Nest)*var.intra))
(p3a.cv <- sqrt(p3a.var)/p3a.tot)


#   b) Razão da população por funcionário ativo da administração direta;
func <- split(munic$FuncADMD,estrato)
pop <-  split(munic$Populacao,estrato)
(p3b.raz <- sum(munic$Populacao)/sum(munic$FuncADMD))
(raz.est <- p3b.raz)
(medest <- do.call(c,lapply(func,mean)))
(X <- sum(munic$FuncADMD))
(var.intraX <- do.call(c,lapply(func,var)))
(var.intraY <- do.call(c,lapply(pop,var))) 
(sd.intraX <- sqrt(var.intraX))
(sd.intraY <- sqrt(var.intraY))
cor.XY <- list()
for(i in 1:5) cor.XY[[i]] <- cor(func[[i]],pop[[i]])
(cor.XY <- do.call(c,cor.XY)) 
(p3b.var <- (1/X^2) * sum(Nest^2 * (1- fest)/nest * (var.intraY + 
         raz.est^2*var.intraX - 2 * raz.est * sd.intraY * sd.intraX * cor.XY)))
(p3b.cv <- sqrt(p3b.var)/p3b.raz)


#   c) Proporção de municípios com maternidade;
mater <- split(munic$mater,estrato)
(prop.est <- do.call(c,lapply(mater,mean)))
(p3c.prop <- sum(Wh*prop.est))
(var.intra <- do.call(c,lapply(mater,var)))
(p3c.var <-  sum(Wh^2 * (1/nest - 1/Nest)*var.intra))
(p3c.cv <- sqrt(p3c.var)/p3c.prop)

#   d) Proporção de municípios com maternidade e emergência.
materemerg <- split(munic$materemerg,estrato)
(prop.est <- do.call(c,lapply(materemerg,mean)))
(p3d.prop <- sum(Wh*prop.est))
(var.intra <- do.call(c,lapply(materemerg,var)))
(p3d.var <-  sum(Wh^2 * (1/nest - 1/Nest)* var.intra))
(p3d.cv <- sqrt(p3d.var)/p3d.prop)





#######################
## Plano 4
#######################
munic <- munic[order(munic$CodMunic),]
capitais <- c('1100205','1200401','1302603','1400100','1501402','1600303',
              '1721000','2111300','2211001','2304400','2408102','2507507',
              '2611606','2704302','2800308','2927408','3106200','3205309',
              '3304557','3550308','4106902','4205407','4314902','5002704',
              '5103403','5208707','5300108')
estratocerto <- rep(FALSE,nrow(munic))
estratocerto[is.element(munic$CodMunic,substr(capitais,1,6))] <- TRUE
estratocerto[munic$Populacao >= 500000] <- TRUE
table(estratocerto)

estrato <- rep(0,nrow(munic))
estrato[ estratocerto] <- 1
estrato[!estratocerto] <- 2

aux <- rep(FALSE,nrow(munic))
aux[estrato==2] <- TRUE

(Nest <- table(estrato)[2])
(nest <- 200-44)
est.munic <- split(munic,factor(estrato))



N <- Nest
n <- nest
(f <- n/N)

#   a) Total de funcionários ativos da administração direta
(p4a.var <- N^2 * ( 1/n - 1/N ) * var(munic$FuncADMD[aux]))
(p4a.tot <- sum(munic$FuncADMD))
(p4a.cv <- sqrt(p4a.var)/p4a.tot)

(p4a.tot <- sum(munic$FuncADMD[aux]))
(p4a.cv <- sqrt(p4a.var)/p4a.tot)

#   b) Razão da população por funcionário ativo da administração direta;
med = mean(munic$FuncADMD)
(p4b.raz <- sum(munic$Populacao)/sum(munic$FuncADMD))
(razao.est <- sum(munic$Populacao[aux])/sum(munic$FuncADMD[aux]))
(p4b.var <- (1-f)/(n*med^2)*1/(N-1)*
         sum((munic$Populacao[aux]-razao.est*munic$FuncADMD[aux])^2))
(p4b.cv <- sqrt(p4b.var)/p4b.raz)

#   c) Proporção de municípios com maternidade;
(p4c.prop <- mean(munic$mater))
(prop.est <- mean(munic$mater[aux]))
(p4c.var <- ((N-n)/(N-1)) * prop.est * (1-prop.est)*(1/n))
(p4c.cv <- sqrt(p4c.var)/p4c.prop)

#   d) Proporção de municípios com maternidade e emergência.
(p4d.prop <- mean(munic$materemerg))
(prop.est <- mean(munic$materemerg[aux]))
(p4d.var <- ((N-n)/(N-1)) * prop.est * (1-prop.est)*(1/n))
(p4d.cv <- sqrt(p4d.var)/p4d.prop)










#######################
## Plano 5
#######################

N <- nrow(munic)
n <- 200    
(f <- n/N)

(medX <- mean(munic$Populacao))
X <- munic$Populacao
Y <- munic$FuncADMD
(R <- sum(Y)/sum(X))

#   a) Total de funcionários ativos da administração direta
(p5a.var <- N^2 * ((1-f)/n) * sum((Y - R*X)^2)*1/(N-1))
(p5a.tot <- sum(munic$FuncADMD))
(p5a.cv <- sqrt(p5a.var)/p5a.tot)


#   b) Razão da população por funcionário ativo da administração direta;
#Este cálculo não faz sentido.

#   c) Proporção de municípios com maternidade;
Y <- munic$mater
(R <- sum(Y)/sum(X))
(p5c.var <- ((1-f)/n) * sum((Y - R*X)^2)*1/(N-1))
(p5c.tot <- mean(munic$mater))
(p5c.cv <- sqrt(p5c.var)/p5c.tot)

#   d) Proporção de municípios com maternidade e emergência.
Y <- munic$materemerg
(R <- sum(Y)/sum(X))
(p5d.var <- ((1-f)/n) * sum((Y - R*X)^2)*1/(N-1))
(p5d.tot <- mean(munic$materemerg))
(p5d.cv <- sqrt(p5d.var)/p5d.tot)



resumo <- data.frame(plano = paste('Plano',1:5),
                     parametro_a=c(p1a.cv,p2a.cv,p3a.cv,p4a.cv,p5a.cv)*100,
                     parametro_b=c(p1b.cv,p2b.cv,p3b.cv,p4b.cv,p5b.cv)*100,
                     parametro_c=c(p1c.cv,p2c.cv,p3c.cv,p4c.cv,p5c.cv)*100,
                     parametro_d=c(p1d.cv,p2d.cv,p3d.cv,p4d.cv,p5d.cv)*100)
resumo

epas <- resumo[-1,]
for(i in 1:4){
epas[i,2:5] <- resumo[i+1,2:5] / resumo[1,2:5]
}
epas


#write.xls(resumo,'c:/Projetos/TbAmoMunic/trunk/cvs.xls')
#write.xls(epas,'c:/Projetos/TbAmoMunic/trunk/epas.xls')




#####----------------------------------------------------------------------#####
#####----------------------------------------------------------------------#####
### ITEM 3 - Selecione uma amostra de municípios segundo o esquema amostral que 
###você escolheu em 2.

sum(munic$Populacao)/5
munic <- transform(munic,sqrtPop = sqrt(Populacao))
munic <- munic[order(munic$sqrtPop),]
sum(munic$sqrtPop)/5
aux <- matrix(NA,nrow=nrow(munic),ncol=5)
for(i in 1:5){
aux[,i] <- cumsum(munic$sqrtPop) <= (sum(munic$sqrtPop)/5)*i
}
estrato <- 6-rowSums(aux)
table(estrato)

munic$estrato <- estrato
munic.est <- split(munic,estrato)

munic.amo <- list()
for( i in 1:5){
aux <- sample(1:nrow(munic.est[[i]]),40)
munic.amo[[i]] <- munic.est[[i]][aux,]
}

amo.munic <- do.call(rbind,munic.amo)
dim(amo.munic)
str(amo.munic)

#write.table(amo.munic,'c:/Projetos/TbAmoMunic/trunk/amo_Munic.dat',sep='\t',
#            quote=F,row.names=F)
url <- 'https://raw.github.com/leandromarino/TBAmoMunic/master/amo_Munic.dat'
url <- getURL(url,ssl.verifypeer = FALSE)
munic <- read.table(textConnection(url), colClasses='character',header=T,quote='',
         sep='\t')
str(munic)



#################

(Nest <- matrix(table(estrato),ncol=5))
(N = sum(Nest))
(nest <- rep(200/5,5))
(Wh <- Nest/N)
est.munic <- munic.amo
estrato <- amo.munic$estrato

#   a) Total de funcionários ativos da administração direta
func <- split(amo.munic$FuncADMD,estrato)
(tot.est <- do.call(c,lapply(func,sum)))
(amop3a.tot <- sum(tot.est/nest*Nest))
(var.intra <- do.call(c,lapply(func,var)))
(amop3a.var <- N^2  * sum(Wh^2 * (1/nest - 1/Nest)*var.intra))
(amop3a.cv <- sqrt(amop3a.var)/amop3a.tot)

#   b) Razão da população por funcionário ativo da administração direta;
func <- split(amo.munic$FuncADMD,estrato)
pop <-  split(amo.munic$Populacao,estrato)
#(amop3b.raz <- sum(amo.munic$Populacao)/sum(amo.munic$FuncADMD))
(amop3b.raz <- sum(do.call(c,lapply(pop,sum))/40) / 
               sum(do.call(c,lapply(func,sum))/40))
(raz.est <- amop3b.raz)
(medest <- do.call(c,lapply(func,mean)))
(X <- sum(medest*Nest))
(var.intraX <- do.call(c,lapply(func,var)))
(var.intraY <- do.call(c,lapply(pop,var))) 
(sd.intraX <- sqrt(var.intraX))
(sd.intraY <- sqrt(var.intraY))
cor.XY <- list()
for(i in 1:5) cor.XY[[i]] <- cor(func[[i]],pop[[i]])
(cor.XY <- do.call(c,cor.XY)) 
(amop3b.var <- (1/X^2) * sum(Nest^2 * (1- fest)/nest * (var.intraY + 
         raz.est^2*var.intraX - 2 * raz.est * sd.intraY * sd.intraX * cor.XY)))
(amop3b.cv <- sqrt(amop3b.var)/amop3b.raz)


#   c) Proporção de amo.municípios com maternidade;
mater <- split(amo.munic$mater,estrato)
(prop.est <- do.call(c,lapply(mater,mean)))
(amop3c.prop <- sum(Wh*prop.est))
(var.intra <- do.call(c,lapply(mater,var)))
(amop3c.var <-  sum(Wh^2 * (1/nest - 1/Nest)*var.intra))
(amop3c.cv <- sqrt(amop3c.var)/amop3c.prop)

#   d) Proporção de amo.municípios com maternidade e emergência.
materemerg <- split(amo.munic$materemerg,estrato)
(prop.est <- do.call(c,lapply(materemerg,mean)))
(amop3d.prop <- sum(Wh*prop.est))
(var.intra <- do.call(c,lapply(materemerg,var)))
(amop3d.var <-  sum(Wh^2 * (1/nest - 1/Nest)* var.intra))
(amop3d.cv <- sqrt(amop3d.var)/amop3d.prop)

