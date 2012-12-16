################################################################################
#####======================================================================#####
#####                                                                      #####
#####   IDENTIFICACAO                                                      #####
#####                                                                      #####
#####======================================================================#####
################################################################################

###   DEFINICAO DO TRABALHO E DE VARIAVEIS DE AMBIENTE
options(width=180)


#instalar pacote a partir de zip local
#install.packages('C:\\Users\\leandromarino\\Documents\\Projetos\\TBAmoMunic\\trunk\\RCurl_1.95-3.zip')

### CARREGANDO OS PACOTES NECESSARIOS
library(xlsReadWrite)
library(SOAR)
library(survey)
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



#####----------------------------------------------------------------------#####
#####----------------------------------------------------------------------#####
### ITEM 1 - Use os valores populacionais das variaveis de interesse para
### determinar o CV esperado para os estimadores dos parametros de interesse.

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




sum(munic$Populacao)/sum(munic$FuncADMD)
munic$Populacao/munic$FuncADMD


varint <- c('e1_func','e2_raz','e3_mater','e4_materemerg')
(mdPop <- apply(munic[,varint],2,mean,na.rm=T))
(sdPop <- apply(munic[,varint],2,sd,na.rm=T))
(cvPop <- sdPop/mdPop)



#####----------------------------------------------------------------------#####
#####----------------------------------------------------------------------#####
### ITEM 2 - Compare os resultados para os diversos planos amostrais e indique
### qual deles vocÃª escolheria, justificando sua escolha.

## PLANO 1
                    