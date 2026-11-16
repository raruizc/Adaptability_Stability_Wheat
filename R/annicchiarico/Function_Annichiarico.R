###########################################################################
#### M?todo Annichiarico de avalia??o de estabilidade e adaptabilidade ####
###########################################################################

#Discentes:
  #Carlos Henrique Pereira
  #Marcia Leite dos Santos

rm(list=ls())
# setwd("C:/Jose Airton/UFLA/Disciplinas/Pos-Graduacao/Emprego de softwares Gen?tica/Fun??es R")
dados<-read.table("R/annicchiarico/ex_dados.txt",h=T)
head(dados)
str(dados)

#Annichiarico baseado na m?dia do ambiente e da testemunha
annichiarico <- function(data, alpha, check, graph, lines, columns)
{
  genotype <- matrix(t(data[,1]))
  data <- data[,-1]
  y<-ncol(data)
  x<-nrow(data)
  
  data1 <- matrix(0,x,y)
  data2 <- matrix(0,x,y)
  pi1 <- matrix(0,1,x)
  sd1 <- matrix(0,1,x)
  resultado1 <- matrix(0,1,x)
  pi2 <- matrix(0,1,x)
  sd2 <- matrix(0,1,x)
  resultado2 <- matrix(0,1,x)
  
  z <- qnorm(alpha,0,1,lower.tail = F)

  #based on the environment mean
  for(i in 1:x)
  {
    for(j in 1:y)
    {
      data1[i,j] <- (data[i,j]/(sum(data[,j])/x))*100
      sd1[i] <- sd(data1[i,])
      pi1[i] <- (sum(data1[i,])/y)
      resultado1[i] <- pi1[i]-(z*(sd1[i]))
    }
  }
  
  #based on the check mean
  for(i in 1:x)
  {
    for(j in 1:y)
      {
      data2[i,j]<-(data[i,j]/data[check,j])*100
      pi2[i]<-(sum(data2[i,])/y)
      sd2[i]<-sd(data2[i,])
      resultado2[i]<-pi2[i]-(z*(sd2[i]))
      }
  }
      
  
  #jpeg('rplot.jpg')
  
  if (graph==T)
  {
    require(fmsb)
    par(mfrow=c(lines,columns))
    par(mai = rep(0.25, 4))
    w1 <- ceiling(max(data1))
    w2 <- ceiling(max(data2))
    w <- max(w1,w2)
    
    #somePDFPath = "C:/Jose Airton/UFLA/Disciplinas/Pos-Graduacao/Emprego de softwares Gen?tica/Fun??es R/some.pdf"
    #pdf(file=somePDFPath)
    
    for(i in 1:x)  
    {
        tabgraf <- data.frame(rbind(rep(w,y),rep(0,y),data1[i,],apply(data1,2,mean),data2[i,]))
        colnames(tabgraf) <- colnames(data)
        grafico <- radarchart(tabgraf, axistype = 0, seg = 3, cglty = 3, plty = 1,
                              pty = 32, cglcol = 1, plwd = 2, pcol = c(1,2,4), axislabcol = 3,centerzero=T,
                              title=paste("G",i,collapse="."))
    }
    #dev.off()
    
  }
  #dev.off()
  
  pi1 <- t(pi1)
  sd1 <- t(sd1)
  resultado1 <- t(resultado1)
  resultado2 <- t(resultado2)
  
  #Sa?da
  resumo <- as.data.frame(cbind(genotype, pi1, sd1,resultado1,resultado2))
  colnames(resumo)<-c("Gen?tipo", "M?dia", "DP", "I_M?dia", "I_Check")
  cat("?ndice de Confiabilidade de Annichiarico", "\n")
  resumo
}


annichiarico(data = dados, alpha = 0.25, check = 6, graph = T, lines = 3, columns = 3)


#Annichiarico baseado na m?dia do ambiente
annichiarico.mean <- function(data, alpha, graph, lines, columns)
{
  genotype <- matrix(t(data[,1]))
  data <- data[,-1]
  y<-ncol(data)
  x<-nrow(data)
  
  data1 <- matrix(0,x,y)
  data2 <- matrix(0,x,y)
  pi1 <- matrix(0,1,x)
  sd1 <- matrix(0,1,x)
  resultado1 <- matrix(0,1,x)
  z <- qnorm(alpha,0,1,lower.tail = F)
  
  #based on the environment mean
  for(i in 1:x)
  {
    for(j in 1:y)
    {
      data1[i,j] <- (data[i,j]/(sum(data[,j])/x))*100
      sd1[i] <- sd(data1[i,])
      pi1[i] <- (sum(data1[i,])/y)
      resultado1[i] <- pi1[i]-(z*(sd1[i]))
    }
  }
  

  #jpeg('rplot.jpg')
  
  if (graph==T)
  {
    require(fmsb)
    par(mfrow=c(lines,columns))
    par(mai = rep(0.25, 4))
    w1 <- ceiling(max(data1))
    
    #somePDFPath = "C:/Jose Airton/UFLA/Disciplinas/Pos-Graduacao/Emprego de softwares Gen?tica/Fun??es R/some.pdf"
    #pdf(file=somePDFPath)
    
    for(i in 1:x)  
    {
      tabgraf <- data.frame(rbind(rep(w1,y),rep(0,y),data1[i,],apply(data1,2,mean)))
      colnames(tabgraf) <- colnames(data)
      grafico <- radarchart(tabgraf, axistype = 0, seg = 3, cglty = 3, plty = 1,
                            pty = 32, cglcol = 1, plwd = 2, pcol = c(1,2), axislabcol = 3,centerzero=T,
                            title=paste("G",i,collapse="."))
    }
    #dev.off()
    
  }
  #dev.off()
  
  pi1 <- t(pi1)
  sd1 <- t(sd1)
  resultado1 <- t(resultado1)
  
  #Sa?da
  resumo <- as.data.frame(cbind(genotype, pi1, sd1,resultado1))
  colnames(resumo)<-c("Genótipo", "Média", "DP", "I_Média")
  cat("Índice de Confiabilidade de Annichiarico", "\n")
  resumo
}

annichiarico.mean(data = dados, alpha = 0.25, graph = T, lines = 3, columns = 3)


#Annichiarico baseado na m?dia da testemunha
annichiarico.check <- function(data, alpha, check)
{
  genotype <- matrix(t(data[,1]))
  data <- data[,-1]
  y<-ncol(data)
  x<-nrow(data)
  
  data2 <- matrix(0,x,y)
  pi2 <- matrix(0,1,x)
  sd2 <- matrix(0,1,x)
  resultado2 <- matrix(0,1,x)
  
  z <- qnorm(alpha,0,1,lower.tail = F)
  
  #based on the check mean
  for(i in 1:x)
  {
    for(j in 1:y)
    {
      data2[i,j]<-(data[i,j]/data[check,j])*100
      pi2[i]<-(sum(data2[i,])/y)
      sd2[i]<-sd(data2[i,])
      resultado2[i]<-pi2[i]-(z*(sd2[i]))
    }
  }

  pi2 <- t(pi2)
  sd2 <- t(sd2)
  resultado2 <- t(resultado2)
  
  #Sa?da
  resumo <- as.data.frame(cbind(genotype, pi2, sd2,resultado2))
  colnames(resumo)<-c("Genótipo", "Média", "DP", "I_Check")
  cat("Índice de Confiabilidade de Annichiarico", "\n")
  resumo
}

annichiarico.check (data = dados, alpha = 0.25, check = 6)


