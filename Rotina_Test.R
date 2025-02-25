#Start####
rm(list=ls(all=T)) 

##Mudar o diretorio

setwd("C:/Users/Ricardo Ruiz/Desktop/Mestrado GMP/Dissertação/Scripts_Mestrado_Ricardo")

##Ler o arquivo de dados
dir()
library(readxl)
dados <- read_excel("Dados.xlsx", sheet = "Dados")


## Transformar dados a factores
str(dados)
dados <- transform(dados, REP = factor(REP), IBLOCK = factor(IBLOCK),
                   COL = factor(COL), ROW = factor(ROW), 
                   TREATMENT = factor(TREATMENT), ANO = factor(ANO),  
                   LOCAL = factor(LOCAL), AMBIENTE =factor(AMBIENTE))
str(dados)

library(lme4)
library(ggplot2)
library(lmerTest)
library(nlme)
library(emmeans)
library(multcomp)
library(tidyr)
library(tidyverse)
library(sommer)
source("sk.r")

#### Análise por ambiente y variable ####
i=10

for (i in 10:ncol(dados))
{
  cat("------------------------------------------------------------------", "\n")
  cat("ANAVA POR AMBIENTE", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  dados.i <- dados[,c(1:9,i)]
  dados.i <- na.omit(dados.i)
  nomamb <- levels(droplevels(dados.i)$AMBIENTE)
  
  statsamb <- matrix(0, nlevels(droplevels(dados.i)$AMBIENTE),6)
  rownames(statsamb) <- levels(droplevels(dados.i)$AMBIENTE)
  colnames(statsamb) <- c("QME", "CVe", "Acuracia", "Fc Genot", "Pr > Fc", "Shapiro_P-value")
  
  j=1
  for (j in 1:nlevels(droplevels(dados.i)$AMBIENTE))
  {
    cat("------------------------------------------------------------------", "\n")
    cat("Anova - Variavel:", nomamb[j], colnames(dados)[i], "\n")
    cat("------------------------------------------------------------------", "\n")
    lmer.i <- lmer(dados.i[dados.i$AMBIENTE==nomamb[j],ncol(dados.i)] ~ REP + (1|IBLOCK) + TREATMENT, data=dados.i[dados.i$AMBIENTE==nomamb[j],])
    print(lrt.i <- ranova(lmer.i))
    print(anova.i <- anova(lmer.i))
    GLe.i <- anova.i$DenDF[2]
    
    statsamb[nomamb[j],1] <- sigma(lmer.i)^2
    statsamb[nomamb[j],2] <- sqrt(statsamb[nomamb[j],1])/mean(dados.i[dados.i$AMBIENTE==nomamb[j],ncol(dados.i)])*100
    statsamb[nomamb[j],3] <- sqrt(1-1/anova.i$'F value'[2])
    statsamb[nomamb[j],4] <- anova.i$'F value'[2]
    statsamb[nomamb[j],5] <- anova.i$'Pr(>F)'[2]
    
    resid <- residuals(lmer.i)
    statsamb[nomamb[j],6] <- shapiro.test(resid)$ p.value
    
  }
  cat("------------------------------------------------------------------", "\n")
  cat("Anovas Individuais (Ambiente) - Variavel:", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  print(statsamb)
  cat("Teste de Hartley - Variavel:", colnames(dados)[i], "\n")
  print("Fmax"); print(Fmax <- max(statsamb[,1])/min(statsamb[,1]))
  print("P > Fmax"); print(Pvalor <- SuppDists::pmaxFratio(q = Fmax, df = GLe.i, k=nlevels(droplevels(dados.i)$LOCAL), lower.tail=F, log.p=FALSE))
  if (Pvalor < 0.05) print("Variancias residuais heterogeneas - Modelo heterocedastico indicado") else print("Variancias residuais homogeneas")
  
}

#### Estimação por Covariancia ####

i=13

for (i in 10:ncol(dados))
{
  cat("------------------------------------------------------------------", "\n")
  cat("ANAVA POR LOCAL", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  dados.i <- dados[,c(1:8,14,i)]
  dados.i <- na.omit(dados.i)
  nomamb <- levels(droplevels(dados.i)$LOCAL)
  nomano <- levels(droplevels(dados.i)$ANO)
  
  statsamb <- matrix(0, nlevels(droplevels(dados.i)$LOCAL),6)
  rownames(statsamb) <- levels(droplevels(dados.i)$LOCAL)
  colnames(statsamb) <- c("QME", "CVe", "Acuracia", "Fc Genot", "Pr > Fc", "Shapiro_P-value")
  
  # subindex j for year
  j=1
  # subindex k for enviroment
  k=1
  
  for (j in 1:nlevels(droplevels(dados.i)$ANO)) {
    ## Detach by year
    dados.j <- dados.i[dados.i$ANO==nomano[j],]
    dados.j <- na.omit(dados.j)
    for (k in 1:nlevels(droplevels(dados.i)$LOCAL)) {
      cat("------------------------------------------------------------------", "\n")
      cat("Anova - Variavel:", nomano[j], nomamb[k], colnames(dados)[i], "\n")
      cat("------------------------------------------------------------------", "\n")
      lmer.i <- lmer(dados.j[dados.j$LOCAL==nomamb[k],ncol(dados.j)] ~ REP + (1|IBLOCK) + TREATMENT + Dummy, data=dados.j[dados.j$LOCAL==nomamb[k],])
      summary(lmer.i)
      print(lrt.i <- ranova(lmer.i))
      print(anova.i <- anova(lmer.i))
      GLe.i <- anova.i$DenDF[2]
      
      statsamb[nomamb[k],1] <- sigma(lmer.i)^2
      statsamb[nomamb[k],2] <- sqrt(statsamb[nomamb[k],1])/mean(dados.j[dados.j$LOCAL==nomamb[k],ncol(dados.j)])*100
      statsamb[nomamb[k],3] <- sqrt(1-1/anova.i$'F value'[2])
      statsamb[nomamb[k],4] <- anova.i$'F value'[2]
      statsamb[nomamb[k],5] <- anova.i$'Pr(>F)'[2]
      
      resid <- residuals(lmer.i)
      order(resid)
      statsamb[nomamb[k],6] <- shapiro.test(resid)$ p.value
      
      qqnorm(resid, main = paste("Normal Q-Q Plot of" ,nomano[j], nomamb[k], colnames(dados)[i]))
      identify(qqnorm(resid, main = paste("Normal Q-Q Plot of" , nomano[j], nomamb[k], colnames(dados)[i])))
      dados.jk <- dados.j[dados.j$LOCAL==nomamb[k],]
      dados.jk[c(1:48),]
    }
    cat("------------------------------------------------------------------", "\n")
    cat("Anovas Individuais (Local) - Variavel:", nomano[j], colnames(dados)[i], "\n")
    cat("------------------------------------------------------------------", "\n")
    print(statsamb)
    cat("Teste de Hartley - Variavel:", colnames(dados)[i], "\n")
    print("Fmax"); print(Fmax <- max(statsamb[,1])/min(statsamb[,1]))
    print("P > Fmax"); print(Pvalor <- SuppDists::pmaxFratio(q = Fmax, df = GLe.i, k=nlevels(droplevels(dados.i)$LOCAL), lower.tail=F, log.p=FALSE))
    if (Pvalor < 0.05) print("Variancias residuais heterogeneas - Modelo heterocedastico indicado") else print("Variancias residuais homogeneas")
    
  }
}

#### Analise por Local e Ano####
i=12

for (i in 9:ncol(dados))
{
  cat("------------------------------------------------------------------", "\n")
  cat("ANAVA POR LOCAL", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  dados.i <- dados[,c(1:8,i)]
  dados.i <- na.omit(dados.i)
  nomamb <- levels(droplevels(dados.i)$LOCAL)
  nomano <- levels(droplevels(dados.i)$ANO)
  
  
  statsamb <- matrix(0, nlevels(droplevels(dados.i)$LOCAL),6)
  rownames(statsamb) <- levels(droplevels(dados.i)$LOCAL)
  colnames(statsamb) <- c("QME", "CVe", "Acuracia", "Fc Genot", "Pr > Fc", "Shapiro_P-value")
  
  # subindex j for year
  j=2
  # subindex k for enviroment
  k=4
  for (j in 1:nlevels(droplevels(dados.i)$ANO)) {
    ## Detach by year
    dados.j <- dados.i[dados.i$ANO==nomano[j],]
    dados.j <- na.omit(dados.j)
    for (k in 1:nlevels(droplevels(dados.i)$LOCAL)) {
      cat("------------------------------------------------------------------", "\n")
      cat("Anova - Variavel:", nomano[j], nomamb[k], colnames(dados)[i], "\n")
      cat("------------------------------------------------------------------", "\n")
      lmer.i <- lmer(dados.j[dados.j$LOCAL==nomamb[k],ncol(dados.j)] ~ REP + (1|IBLOCK) + TREATMENT, data=dados.j[dados.j$LOCAL==nomamb[k],])
      print(lrt.i <- ranova(lmer.i))
      print(anova.i <- anova(lmer.i))
      GLe.i <- anova.i$DenDF[2]
      
      statsamb[nomamb[k],1] <- sigma(lmer.i)^2
      statsamb[nomamb[k],2] <- sqrt(statsamb[nomamb[k],1])/mean(dados.j[dados.j$LOCAL==nomamb[k],ncol(dados.j)])*100
      statsamb[nomamb[k],3] <- sqrt(1-1/anova.i$'F value'[2])
      statsamb[nomamb[k],4] <- anova.i$'F value'[2]
      statsamb[nomamb[k],5] <- anova.i$'Pr(>F)'[2]
      
      resid <- residuals(lmer.i)
      residuo <- order(resid)
      resid[residuo]
      statsamb[nomamb[k],6] <- shapiro.test(resid)$ p.value
      
      qqnorm(resid, main = paste("Normal Q-Q Plot of" ,nomano[j], nomamb[k], colnames(dados)[i]))
      identify(qqnorm(resid, main = paste("Normal Q-Q Plot of" , nomano[j], nomamb[k], colnames(dados)[i])))
      dados.jk <- dados.j[dados.j$LOCAL==nomamb[k],]
      dados.jk[c(1:48),]
    }
    cat("------------------------------------------------------------------", "\n")
    cat("Anovas Individuais (Local) - Variavel:", nomano[j], colnames(dados)[i], "\n")
    cat("------------------------------------------------------------------", "\n")
    print(statsamb)
    
    # cat("Teste de Hartley - Variavel:", colnames(dados)[i], "\n")
    # print("Fmax"); print(Fmax <- max(statsamb[,1])/min(statsamb[,1]))
    # print("P > Fmax"); print(Pvalor <- SuppDists::pmaxFratio(q = Fmax, df = GLe.i, k=nlevels(droplevels(dados.i)$LOCAL), lower.tail=F, log.p=FALSE))
    # if (Pvalor < 0.05) print("Variancias residuais heterogeneas - Modelo heterocedastico indicado") else print("Variancias residuais homogeneas")
     
  }
 
}

#### Análise Espacial - ASREML ####
library(asreml)
library(ggplot2)
library(asremlPlus)
source("icREML.R")

#head(dados)

variavel = 13
#i=10
for (i in variavel)
{
  
# for (i in 10:ncol(dados))
# {
  cat("------------------------------------------------------------------", "\n")
  cat("ANAVA POR AMBIENTE", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  dados.i <- dados[,c(1:9,i)]
  #dados.i <- na.omit(dados.i)
  nomamb <- levels(droplevels(dados.i)$AMBIENTE)
  
  # statsamb <- matrix(0, nlevels(droplevels(dados.i)$AMBIENTE),6)
  # rownames(statsamb) <- levels(droplevels(dados.i)$AMBIENTE)
  # colnames(statsamb) <- c("QME", "CVe", "Acuracia", "Fc Genot", "Pr > Fc", "Shapiro_P-value")
  
  ambientej = c(3:4,6,8)
  # ambientej = 4
  # j=4
  
  for (j in ambientej)
  {
    
  # for (j in 1:nlevels(droplevels(dados.i)$AMBIENTE))
  # {
    cat("------------------------------------------------------------------", "\n")
    cat("Analise Espacial - Variavel:", nomamb[j], colnames(dados)[i], "\n")
    cat("------------------------------------------------------------------", "\n")
    dados.j <- droplevels(dados.i[dados.i$AMBIENTE==nomamb[j],])
    #str(dados.j)
    colnames(dados.j)[ncol(dados.j)] <- "y"
    #dados.j$ROWn <- as.numeric(dados.j$ROW)
    #dados.j$COLn <- as.numeric(dados.j$COL)
    dados.j <- dados.j[with(dados.j,order(ROW,COL)),]
    
    ## Modelo base 0: Latice
    m0 = asreml(y ~ REP + TREATMENT,
                random = ~ REP:IBLOCK,
                residual = ~ units,
                data = dados.j, verbose = TRUE)
    m0 <- update.asreml(m0)
    #summary(m0)$varcomp
    
    # ## Modelo base 1: Linha x Coluna
    # m1 = asreml(y ~ TREATMENT,
    #             random = ~ ROW + COL,
    #             residual = ~ ROW:COL,
    #             data = dados.j)
    # m1 <- update.asreml(m1)
    # #summary(m1)$varcomp
    # 
    # ## Modelo 2: AR1 (linhas) X AR1 (colunas)
    # m2 = asreml(y ~ TREATMENT,
    #             random = ~ ROW + COL,
    #             residual = ~ ar1(ROW):ar1(COL),
    #             data = dados.j)
    # m2 <- update.asreml(m2)
    # #summary(m2)$varcomp
    # 
    # ## SELECAO DO MODELO - Verbylla - correcao LogLik
    # fm <- list(m0=m0, m1=m1, m2=m2)
    # selection <- icREML(fm)
    # best.model <- as.character(selection$model[which(selection$BIC==min(selection$BIC))])
    # 
    # aa <- list(m0,m1,m2)
    # names(aa) <- selection$model
    # m_b <- aa[[best.model]]
    # 
    # ## EFICIENCIA RELATIVA
    # ## M1 LINHA-COLUNA VS M0 LATICE
    # pred_m0<- predict(m0, classify = "TREATMENT", average = c("REP"))
    # pred_m1<- predict(m1, classify = "TREATMENT")
    # ER0.1 <- (pred_m0$avsed^2/pred_m1$avsed^2)*100
    # 
    # ## M0 LATICE vs MELHOR MODELO
    # pred_m0<- predict(m0, classify = "TREATMENT", average = c("REP"))
    # pred_b<- predict(m_b, classify = "TREATMENT")
    # ER0.b <- (pred_m0$avsed^2/pred_b$avsed^2)*100
    # 
    # ## M1 LINHA-COLUNA VS MELHOR MODELO
    # ER1.b <- (pred_m1$avsed^2/pred_b$avsed^2)*100
    # 
    # avsed_models <- c(pred_m0$avsed,pred_m1$avsed,pred_b$avsed) ##Average Standard Error of the Difference
    # 
    # ER <- c(ER0.1,ER0.b,ER1.b)
    # names(ER) <- c("ER0.1","ER0.b","ER1.b")
    # 
    # ##ANOVA Modelo 0 Latice
    # # summ0<-summary(m0)
    # # vc0<-summ0$varcomp
    # # aov_m0 <- wald.asreml(m0)
    # 
    # ## ANOVA Modelo 1 linha x coluna
    # # summ1<-summary(m1)
    # # vc1<-summ1$varcomp
    # # aov_m1 <- wald.asreml(m1)
    # 
    # cat("Analise Espacial - Variavel:", nomamb[j], colnames(dados)[i], "\n")
    # if (best.model=="m0") print(paste("Melhor model ---> ", best.model,"-->","Modelo base --Latice",sep = ''))
    # if (best.model=="m1") print(paste("Melhor model ---> ", best.model,"-->","Modelo base --Linha-Coluna",sep = ''))
    # if (best.model=="m2") print(paste("Melhor model ---> ", best.model,"-->","Modelo Linha-Coluna AR1xAR1",sep = ''))
    # 
    # print("Eficiencia Relativa")
    # print(ER)
    
    # ## ANOVA Melhor modelo baseado no BIC
    # summ_b<-summary(m_b)
    # print(vc_b<-summ_b$varcomp)
    # print(aov_b <- wald.asreml(m_b))
    
    ## ANOVA Melhor modelo baseado no BIC
    summ_b<-summary(m0)
    print(vc_b<-summ_b$varcomp)
    print(aov_b <- wald.asreml(m0))
    
    #Medias fenotípicas ajustadas e SE
    # write.csv(pred_b$pvals,file = paste("M_",colnames(dados)[i],"_",nomamb[j],"_",best.model,".csv",sep = ""))
    write.csv(pred_b$pvals,file = paste("M_",colnames(dados)[i],"_",nomamb[j],"_m0.csv",sep = ""))
    
  }
  
}

####Second stage####
#Computing the weights  
env1 <- read.csv(file = "env1.csv",h = T)
env2 <- read.csv(file = "env2.csv",h = T)
env3 <- read.csv(file = "env3.csv",h = T)
env4 <- read.csv(file = "env4.csv",h = T)
env5 <- read.csv(file = "env5.csv",h = T)

# BLUEs_SE <- rbind(env1,env2,env3,env4,env5)
# BLUEs_SE$envf <- rep(1:5,each=50)
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
# 
# MatW <- list(w1,w2,w3,w4,w5)
# library(cIRT)
# W = solve(direct_sum(MatW))

BLUEs_SE <- rbind(env1,env2,env3,env4,env5)
BLUEs_SE$envf <- rep(1:5,each=50)
BLUEs_SE <- BLUEs_SE[,c(-1,-5)]
BLUEs_SE$std.error <- 1/(BLUEs_SE$std.error^2)
BLUEs_SE$Wsi <- sqrt(BLUEs_SE$std.error)

#Second stage
library(asreml)
data1 <- BLUEs_SE[with(BLUEs_SE, order(envf)), ]
data1$envf <- as.factor(data1$envf)
data1$trt_names <- as.factor(data1$trt_names)
str(data1)
mod3 <- asreml(fixed= predicted.value ~ envf,
               random= ~us(envf):vm(trt_names,K),
               weights = std.error,
               family = asr_gaussian(dispersion=1),
               data=data1, maxiter=100)
mod3 <- update.asreml(mod3)
bb <- summary(mod3)$varcomp

library(sommer)
data1 <- BLUEs_SE[with(BLUEs_SE, order(envf)), ]
data1$envf <- as.factor(data1$envf)
data1$trt_names <- as.factor(data1$trt_names)

mod4 <- mmer(fixed= predicted.value ~ envf,
             random= ~vs(us(envf), trt_names, Gu = K),
             weights = Wsi,
             data=data1, verbose=T)
cc <- summary(mod4)$varcomp



#### Análise Espacial - Sommer ####

i=11

for (i in 10:ncol(dados))
{
  cat("------------------------------------------------------------------", "\n")
  cat("ANAVA POR AMBIENTE", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  dados.i <- dados[,c(1:9,i)]
  dados.i <- na.omit(dados.i)
  nomamb <- levels(droplevels(dados.i)$AMBIENTE)
  
  statsamb <- matrix(0, nlevels(droplevels(dados.i)$AMBIENTE),6)
  rownames(statsamb) <- levels(droplevels(dados.i)$AMBIENTE)
  colnames(statsamb) <- c("QME", "CVe", "Acuracia", "Fc Genot", "Pr > Fc", "Shapiro_P-value")
  
  j=5
  for (j in 1:nlevels(droplevels(dados.i)$AMBIENTE))
  {
    cat("------------------------------------------------------------------", "\n")
    cat("Anova - Variavel:", nomamb[j], colnames(dados)[i], "\n")
    cat("------------------------------------------------------------------", "\n")
    dados.j <- droplevels(dados.i[dados.i$AMBIENTE==nomamb[j],])
    str(dados.j)
    colnames(dados.j)[ncol(dados.j)] <- "y"
    dados.j$ROWn <- as.numeric(dados.j$ROW)
    dados.j$COLn <- as.numeric(dados.j$COL)
    dados.j <- dados.j[with(dados.j,order(ROW,COL)),]
    
    mix <- mmer(y ~ REP + TREATMENT,
                random= ~ REP:IBLOCK,
                rcov= ~ units,
                data = dados.j)
    summary(mix)$varcomp
    p.mix <- predict.mmer(mix,D="TREATMENT")$pvals
    
    mix1 <- mmer(y ~ TREATMENT,
                random= ~ ROW + COL +
                  spl2Da(ROWn,COLn),
                rcov= ~ units,
                data = dados.j)
    summary(mix1)$varcomp
    p.mix1 <- predict.mmer(mix1, D="TREATMENT")$pvals
    
    
    mix2 <- mmer(y ~ TREATMENT,
                 random= ~ ROW + COL +
                   spl2Db(ROWn,COLn),
                 rcov= ~ units,
                 data = dados.j)
    summary(mix2)$varcomp
    mix2$data
    
    p.mix2 <- predict.mmer(mix2,D="TREATMENT")$pvals
    
    rbind(mean(p.mix$std.error),mean(p.mix1$std.error),mean(p.mix2$std.error))
    
    mix <- mmec(dados.i[dados.i$AMBIENTE==nomamb[j],ncol(dados.j)]~ REP + TREATMENT,
                random= ROWF + COLF +
                  #spl2Dc(ROWF,COLF)+ 
                  (1|IBLOCK),
                rcov=~units,
                data=dados.i[dados.i$AMBIENTE==nomamb[j],])
    
  }
  
}


