# Define pasta de trabalho onde encontrar arquivos de dados
#setwd("E:/Amostragem2012/Rdata")
# Carrega pacotes requeridos
library(stratification)
library(sampling)
library(survey)
# Lê o arquivo de dados com a população
fazendas = read.csv(file="Fazendas.csv", header=TRUE, sep=";")
str(fazendas) 
# Cria estratos para exercício 1
fazendas = transform(fazendas, Estrato1 = (AREA >= 100)+1)
fazendas = transform(fazendas, Estrato1 = as.factor(Estrato1))
table(fazendas$Estrato1)
str(fazendas)
# Calcula e armazena resumos das variáveis por estrato
Estrato1.Yh = aggregate( cbind(QUANT, RECEITA, DESPESA) ~ Estrato1, data=fazendas,FUN=sum)
Estrato1.Yh
str(Estrato1.Yh)
write.csv2(Estrato1.Yh, file="Estrato1.Yh.csv")
Estrato1.Yh = Estrato1.Yh[,-1]
Estrato1.Sh2 = aggregate( cbind(QUANT, RECEITA, DESPESA) ~ Estrato1, data=fazendas,FUN=var)
write.csv2(Estrato1.Sh2, file="Estrato1.Sh2.csv")
Estrato1.Sh2 = Estrato1.Sh2[,-1]
Estrato1.Nh = as.vector(table(fazendas$Estrato1))
write.csv2(Estrato1.Nh, file="Estrato1.Nh.csv")
# Calcula variância e CV do estimador HT de total para variáveis de pesquisa - Plano 1
Plano1.nh = c(25, 25)
Plano1.Vartot = colSums(Estrato1.Nh^2 * ((1 / Plano1.nh) - (1 / Estrato1.Nh)) * Estrato1.Sh2)
Plano1.CVtot = 100 * sqrt(Plano1.Vartot) / colSums(Estrato1.Yh)
Plano1.CVtot
# Calcula variância e CV do estimador HT de total para variáveis de pesquisa - Plano 2
Plano2.nh = c(14, 36)
Plano2.Vartot = colSums(Estrato1.Nh^2 * ((1 / Plano2.nh) - (1 / Estrato1.Nh)) * Estrato1.Sh2)
Plano2.CVtot = 100 * sqrt(Plano2.Vartot) / colSums(Estrato1.Yh)
Plano2.CVtot
# Calcula variâncias da variável AREA por estrato
Estrato1.Sh2x = aggregate( AREA ~ Estrato1, data=fazendas, FUN=var)
Estrato1.Sh2x = Estrato1.Sh2x[,-1] 
# Calcula alocação ótima da amostra
Plano4.nh = 50 * Estrato1.Nh * sqrt(Estrato1.Sh2x) / sum( Estrato1.Nh * sqrt(Estrato1.Sh2x) )
Plano4.nh = round(Plano4.nh)
Plano4.nh 
# Calcula variância e CV do estimador HT de total para variáveis de pesquisa - Plano 2
Plano4.Vartot = colSums(Estrato1.Nh^2 * ((1 / Plano4.nh) - (1 / Estrato1.Nh)) * Estrato1.Sh2)
Plano4.CVtot = 100 * sqrt(Plano4.Vartot) / colSums(Estrato1.Yh)
Plano4.CVtot
# Calcula alocação ótima da amostra
Plano5.nh = 50 * Estrato1.Nh / sum(Estrato1.Nh)
Plano5.nh = round(Plano5.nh)
Plano5.nh 
# Calcula variância e CV do estimador HT de total para variáveis de pesquisa - Plano 2
Plano5.Vartot = colSums(Estrato1.Nh^2 * ((1 / Plano5.nh) - (1 / Estrato1.Nh)) * Estrato1.Sh2)
Plano5.CVtot = 100 * sqrt(Plano5.Vartot) / colSums(Estrato1.Yh)
Plano5.CVtot
(grava.cv = rbind(Plano1.CVtot, Plano2.CVtot, Plano4.CVtot, Plano5.CVtot))
# Determina limites de estratos e alocação - Plano 6
(Estrato6 = strata.LH(fazendas$AREA, initbh=c(80), n=50, Ls=2, takeall=0))
str(Estrato6)
# Cria variável de estratificação usando limites definidos
(Estrato6.limites = c(0, Estrato6$bh, 282))
(fazendas$Estrato6 = cut(fazendas$AREA, Estrato6.limites))
table(fazendas$Estrato6)
# Calcula e armazena resumos das variáveis por estrato
Estrato6.Yh = aggregate( cbind(QUANT, RECEITA, DESPESA) ~ Estrato6, data=fazendas,FUN=sum)
Estrato6.Yh = Estrato6.Yh[,-1]
Estrato6.Sh2 = aggregate( cbind(QUANT, RECEITA, DESPESA) ~ Estrato6, data=fazendas,FUN=var)
Estrato6.Sh2 = Estrato6.Sh2[,-1]
Estrato6.Nh = as.vector(table(fazendas$Estrato6))
# Calcula variância e CV do estimador HT de total para variáveis de pesquisa - Plano 3
Plano6.nh = Estrato6$nh
Plano6.Vartot = colSums(Estrato6.Nh^2 * ((1 / Plano6.nh) - (1 / Estrato6.Nh)) * Estrato6.Sh2)
Plano6.CVtot = 100 * sqrt(Plano6.Vartot) / colSums(Estrato6.Yh)
Plano6.CVtot
(grava.cv = rbind(Plano2.CVtot, Plano1.CVtot, Plano5.CVtot, Plano4.CVtot, Plano6.CVtot))
# Determina limites de estratos e alocação - Plano 3
(Estrato3 = strata.LH(fazendas$AREA, n=50, Ls=5))
str(Estrato3)
# Cria variável de estratificação usando limites definidos
Estrato3.limites = c(0, Estrato3$bh, 282)
fazendas$Estrato3 = cut(fazendas$AREA, Estrato3.limites)
table(fazendas$Estrato3)
# Calcula e armazena resumos das variáveis por estrato
Estrato3.Yh = aggregate( cbind(QUANT, RECEITA, DESPESA) ~ Estrato3, data=fazendas,FUN=sum)
write.csv2(Estrato3.Yh, file="Estrato3.Yh.csv")
Estrato3.Yh = Estrato3.Yh[,-1]
Estrato3.Sh2 = aggregate( cbind(QUANT, RECEITA, DESPESA) ~ Estrato3, data=fazendas,FUN=var)
write.csv2(Estrato3.Sh2, file="Estrato3.Sh2.csv")
Estrato3.Sh2 = Estrato3.Sh2[,-1]
Estrato3.Nh = as.vector(table(fazendas$Estrato3))
write.csv2(Estrato3.Nh, file="Estrato3.Nh.csv")
# Calcula variância e CV do estimador HT de total para variáveis de pesquisa - Plano 3
Plano3.nh = Estrato3$nh
Plano3.Vartot = colSums(Estrato3.Nh^2 * ((1 / Plano3.nh) - (1 / Estrato3.Nh)) * Estrato3.Sh2)
Plano3.CVtot = 100 * sqrt(Plano3.Vartot) / colSums(Estrato3.Yh)
Plano3.CVtot
# Seleciona as unidades da amostra AES com plano 3
(fazendas.amo3 = strata(data=fazendas, stratanames=c("Estrato3"), Plano3.nh,
                       method=c("srswor")))
# Obtém dados das variáveis de pesquisa para amostra selecionada
fazendas.aux = fazendas[fazendas.amo3$ID_unit, c("AREA", "QUANT", "RECEITA", "DESPESA")]
fazendas.amo3 = cbind(fazendas.amo3, fazendas.aux)
# Cria objeto de desenho correspondente à amostra selecionada
fazendas.plano3 = svydesign(data=fazendas.amo3, ids=~1, strata=~Estrato3, fpc=~Prob)
summary(fazendas.plano3)
# Calcula estimativas de totais com amostra selecionada
(fazendas.est = svytotal(~QUANT+RECEITA+DESPESA, design=fazendas.plano3))
str(fazendas.est)
(fazendas.cv = 100* sqrt(diag(vcov(fazendas.est))) / coef(fazendas.est))
# Calcula CVs para plano AAS com n = 50
N = nrow(fazendas)
n = 50
fazendas.yvar = fazendas[,c("QUANT", "RECEITA", "DESPESA")]
Plano0.Var = N^2 * (1/n - 1/N) * diag(var(fazendas.yvar))
(Plano0.cv = 100 * sqrt(Plano0.Var) / colSums(fazendas.yvar) )
# Cria objeto para armazenar resultados
(grava.cv = rbind(Plano0.cv, Plano1.CVtot, Plano2.CVtot, Plano3.CVtot, fazendas.cv))
write.csv2(grava.cv, file="ComparaCV.csv")