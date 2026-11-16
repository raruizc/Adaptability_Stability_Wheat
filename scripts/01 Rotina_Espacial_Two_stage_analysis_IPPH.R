#Start####
rm(list=ls(all=T)) 

##Mudar o diretorio

#setwd("C:/Users/Ricardo Ruiz/Desktop/Mestrado GMP/Dissertação/Scripts_Mestrado_Ricardo")

##Ler o arquivo de dados
dir()
library(readxl)
dados <- read_excel("data/Dados.xlsx", sheet = "Dados")
dados$ipph <- dados$PROD*(dados$PH+22)/100

## Transformar dados a factores
dados <- transform(dados, REP = factor(REP), IBLOCK = factor(IBLOCK),
                   COL = factor(COL), ROW = factor(ROW), 
                   TREATMENT = factor(TREATMENT), ANO = factor(ANO),  
                   LOCAL = factor(LOCAL), AMBIENTE =factor(AMBIENTE))
dados1 <- dados
nomamb <- levels(droplevels(dados)$AMBIENTE)
ambientej = c(1:2,5,7,9) #ambientes com layout regular
#ambientej = c(3:4,6,8) #ambientes com layout irregular
dados <- dados[dados$AMBIENTE %in% nomamb[ambientej], ]

#### Analise por ambiente ####
#### Analise Espacial - ASREML ####
library(asreml)
library(ggplot2)
library(asremlPlus)
source("R/icREML.R")

# variavel = 13
# #i=14
# for (i in variavel)
# {

for (i in 10:ncol(dados))
{
  cat("------------------------------------------------------------------", "\n")
  cat("ANAVA POR AMBIENTE", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  dados.i <- dados[,c(1:9,i)]
  #dados.i <- na.omit(dados.i)
  nomamb <- levels(droplevels(dados.i)$AMBIENTE)
  
  for (j in 5)
    
  #for (j in 1:nlevels(droplevels(dados.i)$AMBIENTE))
  {
    cat("------------------------------------------------------------------", "\n")
    cat("Analise Espacial - Variavel:", nomamb[j], colnames(dados)[i], "\n")
    cat("------------------------------------------------------------------", "\n")
    dados.j <- droplevels(dados.i[dados.i$AMBIENTE==nomamb[j],])
    colnames(dados.j)[ncol(dados.j)] <- "y"
    dados.j <- dados.j[with(dados.j,order(ROW,COL)),]
    
    ## Modelo base 0: Latice
    m0 = asreml(y ~ REP + TREATMENT,
                random = ~ REP:IBLOCK,
                residual = ~ units,trace=FALSE,
                data = dados.j)
    m0 <- update.asreml(m0)

    ## Modelo base 1: Linha x Coluna
    m1 = asreml(y ~ TREATMENT,
                random = ~ ROW + COL,
                residual = ~ ROW:COL, trace = FALSE,
                data = dados.j)
    m1 <- update.asreml(m1)

    ## Modelo 2: AR1 (linhas) X AR1 (colunas)
    m2 = asreml(y ~ TREATMENT,
                random = ~ ROW + COL,
                residual = ~ ar1(ROW):ar1(COL),trace=FALSE,
                data = dados.j)
    m2 <- update.asreml(m2)

    ## SELECAO DO MODELO - Verbylla - correcao LogLik
    fm <- list(m0=m0, m1=m1, m2=m2)
    selection <- icREML(fm)
    best.model <- as.character(selection$model[which(selection$BIC==min(selection$BIC))])

    aa <- list(m0,m1,m2)
    names(aa) <- selection$model
    m_b <- aa[[best.model]]

    ## EFICIENCIA RELATIVA
    ## M0 LATICE VS M1 LINHA-COLUNA
    pred_m0<- predict(m0, classify = "TREATMENT", average = c("REP"))
    pred_m1<- predict(m1, classify = "TREATMENT")
    ER0.1 <- (pred_m0$avsed^2/pred_m1$avsed^2)*100

    ## M0 LATICE vs MELHOR MODELO
    pred_m0<- predict(m0, classify = "TREATMENT", average = c("REP"))
    pred_b<- predict(m_b, classify = "TREATMENT")
    ER0.b <- (pred_m0$avsed^2/pred_b$avsed^2)*100

    ## M1 LINHA-COLUNA VS MELHOR MODELO
    ER1.b <- (pred_m1$avsed^2/pred_b$avsed^2)*100

    ER <- c(ER0.1,ER0.b,ER1.b)
    names(ER) <- c("ER0.1","ER0.b","ER1.b")

    if (best.model=="m0") print(paste("Melhor model ---> ", best.model,"-->","Modelo base --Latice",sep = ''))
    if (best.model=="m1") print(paste("Melhor model ---> ", best.model,"-->","Modelo base --Linha-Coluna",sep = ''))
    if (best.model=="m2") print(paste("Melhor model ---> ", best.model,"-->","Modelo Linha-Coluna AR1xAR1",sep = ''))

    print("Eficiencia Relativa")
    print(ER)

    ## ANOVA Melhor modelo baseado no BIC
    summ_b<-summary(m_b)
    print(summ_b$varcomp)
    print(wald.asreml(m_b))
    print(wald.asreml(m_b, denDF = "default", ssType = "conditional"))
    
    #Para os ambientes irregulares
    # summ_b<-summary(m0)
    # print(summ_b$varcomp)
    # print(wald.asreml(m0))
    # print(wald.asreml(m0, denDF = "default", ssType = "conditional"))
    # pred_b<- predict(m0, classify = "TREATMENT", average = c("REP"))
    
    #Medias fenotipicas ajustadas e SE
    write.csv(pred_b$pvals,file = paste("output/tables/output/tables/M_",colnames(dados)[i],"_",nomamb[j],"_",best.model,".csv",sep = ""))
    #write.csv(pred_b$pvals,file = paste("output/tables/output/tables/M_",colnames(dados)[i],"_",nomamb[j],"_m0.csv",sep = ""))
    
  }
  
}

####Second stage####
trait = "PMS"

#Computing the weights  
env1 <- read.csv(file = paste("output/tables/M_",trait,"_ITIRAPUA-2022_","m0.csv",sep = ""),h = T)
env2 <- read.csv(file = paste("output/tables/M_",trait,"_ITIRAPUA-2023_","m2.csv",sep = ""),h = T)
env3 <- read.csv(file = paste("output/tables/M_",trait,"_ITUTINGA-2022_","m0.csv",sep = ""),h = T)
env4 <- read.csv(file = paste("output/tables/M_",trait,"_ITUTINGA-2023_","m0.csv",sep = ""),h = T)
env5 <- read.csv(file = paste("output/tables/M_",trait,"_LAMBARI-2022_","m2.csv",sep = ""),h = T)
env6 <- read.csv(file = paste("output/tables/M_",trait,"_LAMBARI-2023_","m0.csv",sep = ""),h = T)
env7 <- read.csv(file = paste("output/tables/M_",trait,"_LAVRAS-2022_","m1.csv",sep = ""),h = T)
env8 <- read.csv(file = paste("output/tables/M_",trait,"_LAVRAS-2023_","m0.csv",sep = ""),h = T)
env9 <- read.csv(file = paste("output/tables/M_",trait,"_SAO GONCALO SAPUCAI-2022_","m1.csv",sep = ""),h = T)

# BLUEs_SE <- rbind(env1,env2,env3,env4,env5,env6,env7,env8,env9)
# BLUEs_SE$envf <- rep(1:9,each=16)
# BLUEs <- BLUEs_SE[,c(6,2:3)]
# SE <- BLUEs_SE[,c(6,2,4)]
# SE$std.error <- SE$std.error^2
# colnames(SE) <- c("envf","trt_names","VarBLUE")
# 
# w1 <- diag(SE$VarBLUE[SE$envf=="1"])
# w2 <- diag(SE$VarBLUE[SE$envf=="2"])
# w3 <- diag(SE$VarBLUE[SE$envf=="3"])
# w4 <- diag(SE$VarBLUE[SE$envf=="4"])
# w5 <- diag(SE$VarBLUE[SE$envf=="5"])
# w6 <- diag(SE$VarBLUE[SE$envf=="6"])
# w7 <- diag(SE$VarBLUE[SE$envf=="7"])
# w8 <- diag(SE$VarBLUE[SE$envf=="8"])
# w9 <- diag(SE$VarBLUE[SE$envf=="9"])
# 
# MatW <- list(w1,w2,w3,w4,w5,w6,w7,w8,w9)
# # library(cIRT)
# # W = solve(direct_sum(MatW))
# W = solve(as.matrix(bdiag(w1,w2,w3,w4,w5,w6,w7,w8,w9)))

BLUEs_SE <- rbind(env1,env2,env3,env4,env5,env6,env7,env8,env9)

nomamb <- levels(droplevels(dados1)$AMBIENTE)

BLUEs_SE$envf <- rep(nomamb,each=16)
BLUEs_SE <- BLUEs_SE[,c(-1,-5)]
BLUEs_SE$std.error <- 1/(BLUEs_SE$std.error^2)
BLUEs_SE$Wsi <- sqrt(BLUEs_SE$std.error)

#Split string into columns
library(stringr)
aa <- data.frame(str_split_fixed(BLUEs_SE$envf, "-", n=Inf))
BLUEs_SE$Local <- aa$X1
BLUEs_SE$Ano <- aa$X2
#head(BLUEs_SE)

#Second stage
library(asreml)
data1 <- BLUEs_SE[with(BLUEs_SE, order(envf)), ]
data1$envf <- as.factor(data1$envf)
data1$TREATMENT <- as.factor(data1$TREATMENT)
data1$Local <- as.factor(data1$Local)
data1$Ano <- as.factor(data1$Ano)
#str(data1)

#envf - combina??o de local e ano
# mod3 <- asreml(fixed= predicted.value ~ envf + TREATMENT + TREATMENT:envf,
#                weights = std.error,
#                family = asr_gaussian(dispersion=1),trace = FALSE,
#                data=data1)
# 
# # mod3 <- asreml(fixed= predicted.value ~ envf,
# #                random= ~ fa(envf,2):TREATMENT,
# #                weights = std.error,
# #                family = asr_gaussian(dispersion=1),trace = FALSE,
# #                data=data1)
# # mod3 <- update.asreml(mod3)
# 
# 
# bb <- summary(mod3)
# wald.asreml(mod3)
# 
# pred_g<- predict(mod3, classify = "TREATMENT", average ="envf" )
# write.csv(pred_g$pvals,file = paste("output/tables/Medias_G","_IPPH.csv",sep = ""))
# 
# pred_a<- predict(mod3, classify = "envf", average = "TREATMENT")
# write.csv(pred_a$pvals,file = paste("output/tables/Medias_A","_IPPH.csv",sep = ""))
# 
# pred_ga<- predict.asreml(mod3, classify = "TREATMENT:envf", average = c("TREATMENT","envf"))
# write.csv(pred_ga$pvals,file = paste("output/tables/Medias_GxA","_IPPH.csv",sep = ""))


#desdobrando envf - local e ano

mod <- asreml(fixed= predicted.value ~ Local + Ano + Local:Ano +
                 TREATMENT + TREATMENT:Local + TREATMENT:Ano + TREATMENT:Local:Ano,
               weights = std.error,
               family = asr_gaussian(dispersion=1),trace = FALSE,
               data=data1)

sum_mod <- summary(mod)
wald.asreml(mod)

pred_l <- predict.asreml(mod, classify = "Local", average =c("TREATMENT","Ano" ))
write.csv(pred_l$pvals,file = paste("output/tables/Medias_Local_",trait,".csv",sep = ""))

pred_ano <- predict.asreml(mod, classify = "Ano", average =c("TREATMENT","Local"))
write.csv(pred_ano$pvals,file = paste("output/tables/Medias_Ano_",trait,".csv",sep = ""))

pred_lano <- predict.asreml(mod, classify = "Local:Ano", average =c("TREATMENT","Ano" ))
write.csv(pred_lano$pvals,file = paste("output/tables/Medias_LxAno_",trait,".csv",sep = ""))

pred_gl<- predict.asreml(mod, classify = "TREATMENT:Local", average =c("TREATMENT","Ano" ))
write.csv(pred_gl$pvals,file = paste("output/tables/Medias_GxL_",trait,".csv",sep = ""))

pred_gano<- predict.asreml(mod, classify = "TREATMENT:Ano", average = c("TREATMENT","Local"))
write.csv(pred_gano$pvals,file = paste("output/tables/Medias_GxAno_",trait,".csv",sep = ""))

library(tidyr)

#from long to wide format

# #tabela de m?dias gen?tipos x ambientes
# tabGxA <- pred_ga$pvals[,-c(4,5)]
# #head(tabGxA)
# tabGxA_wi <- spread(data = tabGxA, envf, predicted.value)
# write.csv(tabGxA_wi,file = paste("output/tables/Tabela_GxA_",trait,".csv",sep = ""))

#tabela de m?dias gen?tipos x locais
tabGxL <- pred_gl$pvals[,-c(4,5)]
#head(tabGxL)
tabGxL_wi <- spread(data = tabGxL, Local, predicted.value)
write.csv(tabGxL_wi,file = paste("output/tables/Tabela_GxL_",trait,".csv",sep = ""))

#tabela de m?dias gen?tipos x anos
tabGxAano <- pred_gano$pvals[,-c(4,5)]
#head(tabGxAano)
tabGxAno_wi <- spread(data = tabGxAano, Ano, predicted.value)
write.csv(tabGxAno_wi,file = paste("output/tables/Tabela_GxAno_",trait,".csv",sep = ""))

