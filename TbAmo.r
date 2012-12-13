################################################################################
#####======================================================================#####
#####                                                                      #####
#####   IDENTIFICAÇÃO                                                      #####
#####                                                                      #####
#####======================================================================#####
################################################################################

### DEFINIÇÃO DO LOCAL DE TRABALHO E VARIÁVEIS DE AMBIENTE
setwd('I:\\Mestrado ENCE 2012\\03 - Amostragem\\TrabalhoFinal\\')
options(width=180)

### CARREGANDO OS PACOTES NECESSÁRIOS
library(xlsReadWrite)
library(SOAR)
library(survey)




### LEITURA DOS DADOS
## Inicialmente o arquivo estava em formato .xlsx e foi convertido no Excel 2010
## para o .xls
classcol = c(rep('character',5),rep('numeric',2),rep('character',2))
url <- 'https://raw.github.com/leandromarino/TBAmoMunic/master/Munic.dat'
munic <- read.table(url,colClasses=classcol,sep='\t',header=T)
rm(classcol)
str(munic)


#####----------------------------------------------------------------------#####
#####----------------------------------------------------------------------#####
### ITEM 1 - Use os valores populacionais das variáveis de interesse para
### determinar o CV esperado para os estimadores dos parâmetros de interesse.

#Y1 = FuncADMD    - Total de funcionários ativos da administração direta;
#Y2 = Maternidade - Existe Maternidade no município?
#Y3 = Emergencia  - Existe Unidade de emergência no município?
munic <- transform(munic,
            func =FuncADMD,
            mater=ifelse(Maternidade=="Sim",1,ifelse(Maternidade=='Não',0,NA)),
            emerg=ifelse(Emergencia=="Sim",1,ifelse(Emergencia=='Não',0,NA)))
munic <- transform(munic,
            materemerg = ifelse(rowSums(cbind(mater,emerg)) == 2,1,0),
            )

munic[1:10,]
varint <- c('func','mater','emerg')
(mdPop <- apply(munic[,varint],2,mean,na.rm=T))
(sdPop <- apply(munic[,varint],2,sd,na.rm=T))
(cvPop <- sdPop/mdPop)



#####----------------------------------------------------------------------#####
#####----------------------------------------------------------------------#####
### ITEM 2 - Compare os resultados para os diversos planos amostrais e indique
### qual deles você escolheria, justificando sua escolha.

## PLANO 1
