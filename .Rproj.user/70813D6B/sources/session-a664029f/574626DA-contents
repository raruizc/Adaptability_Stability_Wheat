#Start####
rm(list=ls(all=T))

## TABLE HTML 
print_tbl <- function(table, digits = 3, n = 10, ...){
  if(!is.null(n)){
    knitr::kable(head(table, n = n), booktabs = TRUE, digits = digits, ...)
  } else{
    knitr::kable(table, booktabs = TRUE, digits = digits, ...)
  }
}

#### Import data ####
ALT <- read.csv(file = "Medias_GxA_ALT.csv",h = T)
PH <- read.csv(file = "Medias_GxA_PH.csv",h = T)
PMS <- read.csv(file = "Medias_GxA_PMS.csv",h = T)
PROD <- read.csv(file = "Medias_GxA_PROD.csv",h = T)
IPPH <- read.csv(file = "Medias_GxA_IPPH.csv",h = T)
colnames(ALT)[4] <- "ALT" 
colnames(PH)[4] <- "PH" 
colnames(PMS)[4] <- "PMS" 
colnames(PROD)[4] <- "PROD"
colnames(IPPH)[4] <- "IPPH"

### Data ###
dados <- data.frame(ALT$TREATMENT, ALT$envf, ALT$ALT, PH$PH, 
                    PMS$PMS, PROD$PROD, IPPH$IPPH)
colnames(dados) <- c("TREATMENT", "ENV", "ALT", "PH",
                     "PMS", "PROD", "IPPH")

str(dados)
dados <- transform(dados, ENV =factor(ENV),
                   TREATMENT = factor(TREATMENT))

### Estability with Metan ###
library(metan)
library(rio)
library(emmeans)


i=3

for(i in 3:ncol(dados)){
  a <- ge_plot(dados, ENV, TREATMENT, colnames(dados)[i])
  #b <- ge_plot(datos, ENV, TREATMENT, ALT, type = 2)
  #arrange_ggplot(a, b, tag_levels = "a")
  print(a)
}

### TREATMENT WINNER BY ENVIROMENT###
ganhadores <- ge_winners(dados, ENV, TREATMENT, resp = everything(),
                         better = c("l, h, h, h, h")) 
ganhadores

### RANKING BY RESPONSE###
winners <- 
  ge_winners(dados, ENV, TREATMENT,
             resp = everything(),
             type = "ranks",
             better = c("l, h, h, h, h"))
winners

### GROUP OF ENVIROMENTS ###
i=3

for(i in 3:ncol(dados)){
  d1 <- ge_cluster(dados, ENV, TREATMENT, colnames(dados)[i], nclust = 3)
  plot(d1, nclust = 3, main = colnames(dados)[i])
}


#### GGE BIPLOT ####

str(dados)

##### Biplot tipo 1: Biplot Básico #####

gge_model <- gge(dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, 
                                                 PROD, IPPH))

head(dados)
i=1
for (i in 1:5) {
  tiff(filename = paste(paste("Biplot_1_",colnames(dados)[i+2],"Basico",".tiff", sep = "_")), 
       width = 5, height = 5, units = "in", res = 300, compression = "lzw")
  
  print(plot(gge_model, var = i,
       col.gen = "black",
       col.env = "blue",
        size.text.env = 4,
        size.text.gen = 4,
       title = F,
       plot_theme = theme_metan (grid = "both")))
  
  dev.off()
  
}



##### Biplot tipo 2: Desempenho médio vs. estabilidade #####
gge_model <- gge(dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD, IPPH), 
                 svp = "genotype")

## Geral
i=1
for (i in 1:5) {
  tiff(filename = paste(paste("Biplot_2_",colnames(dados)[i+2],
                              "Desempenho_vs_Estabilidade",".tiff", sep = "_")), 
       width = 5, height = 5, units = "in", res = 300, compression = "lzw")
  
  print(plot(gge_model, var = i,
             type = 2,
             col.gen = "black",
             col.env = "blue",
             axis_expand = 1.5,
             size.text.env = 2,
             size.text.gen = 3,
             title = F,
             plot_theme = theme_metan_minimal()))
  
  dev.off()
  
}

## Expand
i=2
for (i in 1:5) {
  tiff(filename = paste(paste("Biplot_2_",colnames(dados)[i+2],
                              "Desempenho_vs_Estabilidade_Expand",".tiff", sep = "_")), 
       width = 5, height = 5, units = "in", res = 300, compression = "lzw")
  
  print(plot(gge_model, var = i,
             type = 2,
             col.gen = "black",
             col.env = "blue",
             axis_expand = 0.5,
             size.text.env = 2,
             size.text.gen = 3,
             title = F,
             plot_theme = theme_metan_minimal()))
  
  dev.off()
  
}


##### Biplot tipo 3: Qual-Ganhou-Onde #####

gge_model <- gge(dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD, IPPH), 
                  svp = "symmetrical")

i=1
for (i in 1:5) {
  tiff(filename = paste(paste("Biplot_3_",colnames(dados)[i+2],
                              "Qual_Ganhou",".tiff", sep = "_")), 
       width = 5, height = 5, units = "in", res = 300, compression = "lzw")
  
  print(plot(gge_model, var = i,
             type = 3,
             #size.shape.win = 1,
             size.text.env = 3,
             size.text.gen = 2,
             #large_label = 3,
             col.gen = "black",
             col.env = "blue",
             title = F,
             plot_theme = theme_metan(color.background = transparent_color())))
  
  dev.off()
  
}


##### Biplot tipo 4: Discriminatividade vs. representatividade ####
gge_model <- gge(dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD, IPPH),
                 svp = "environment")

i =1
for (i in 1:5) {
  tiff(filename = paste(paste("Biplot_4_",colnames(dados)[i+2],
                              "Discri_Repres",".tiff", sep = "_")), 
       width = 5, height = 5, units = "in", res = 300, compression = "lzw")
  
  print(plot(gge_model, var = i,
             type = 4,
             size.text.env = 3,
             size.text.gen = 2,
             col.gen = "black",
             col.env = "blue",
             title = F,
             plot_theme = theme_metan_minimal()))
  
  dev.off()
  
}

##### Biplot tipo 5: Examinar um ambiente ####
gge_model <- gge (dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD), 
                  svp = "symmetrical")
levels(dados$ENV)

tiff(filename = paste("Biplot_5_","Um_Ambiente",".tiff", sep = "_"), 
     width = 5, height = 5, units = "in", res = 300, compression = "lzw")

plot(gge_model,
     type = 5,
     sel_env = "ITUTINGA-2023",
     col.gen = "black",
     col.env = "black",
     size.text.env = 3,
     size.text.gen = 2,
     axis_expand = 1.5)

dev.off()

##### Biplot tipo 6: Ranqueamento de ambientes ####

gge_model <- gge(dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD))

tiff(filename = paste("Biplot_6_","Ranking_Amb",".tiff", sep = "_"), 
     width = 5, height = 5, units = "in", res = 300, compression = "lzw")

plot(gge_model,
     type = 6,
     col.line = "red",
     col.gen = "black",
     col.env = "black",
     col.circle = "blue",
     col.alpha.circle = 1,
     size.text.env = 4,
     axis_expand = 1.5,
     size.shape = 4,
     plot_theme = theme_metan(color.background = "white"))


dev.off()

##### Biplot tipo 7: Examinar um genotipo ####

gge_model <- gge(dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD), 
                 svp = "genotype")

tiff(filename = paste("Biplot_7_","Um_Genot",".tiff", sep = "_"), 
     width = 5, height = 5, units = "in", res = 300, compression = "lzw")

plot(gge_model, var = 4,
          type = 7,
          sel_gen = "BRS 264",
          col.gen = "black",
          col.env = "black",
          size.text.env = 10,
          axis_expand = 1.8,
          plot_theme = theme_metan (grid = "both"))


dev.off()

##### Biplot tipo 8: Classificacao genotipos ####


gge_model <- gge(dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD), 
                 svp = "genotype")

tiff(filename = paste("Biplot_8_","Classi_Genotipos",".tiff", sep = "_"), 
     width = 5, height = 5, units = "in", res = 300, compression = "lzw")


plot(gge_model, var =4,
     type = 8,
     col.gen = "black",
     col.env = "gray",
     size.text.gen = 6,
     size.text.env = 5,
     axis_expand = 1.5,
     plot_theme = theme_metan_minimal())

dev.off()

##### Biplot tipo 9: Comparar dois genótipos ####

gge_model <- gge (dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD), 
                  svp = "symmetrical")

tiff(filename = paste("Biplot_9_","Comparar_Gen",".tiff", sep = "_"), 
     width = 5, height = 5, units = "in", res = 300, compression = "lzw")

plot(gge_model,
          type = 9,
          sel_gen1 = "BRS 264",
          sel_gen2 = "TBIO ATON",
          col.gen = "black",
          size.text.gen = 1.5,
          size.text.win = 4,
          title = FALSE,
          plot_theme = theme_metan_minimal())

dev.off()

### Biplot tipo 10: Relação entre ambientes

gge_model <- gge(dados, ENV, TREATMENT, resp = c(ALT, PH, PMS, PROD, IPPH))

tiff(filename = paste("Biplot_10_","Relacao_Amb",".tiff", sep = "_"), 
     width = 5, height = 5, units = "in", res = 300, compression = "lzw")

plot(gge_model, var = 4,
     type = 10,
     col.gen = "black",
     título = T)

dev.off()

# GT biplot for all numeric variables
mod <- gtb(dados, TREATMENT, resp = c(ALT, PH, PMS, PROD, IPPH), scaling = 0)
plot(mod,
     axis_expand = 0.8,
     title = FALSE)

#### Método Annichiarico de avaliação de estabilidade e adaptabilidade ####
rm(list=ls(all=T))

### Import data ###
ALT <- read.csv(file = "Tabela_GxA_ALT.csv",h = T)
PH <- read.csv(file = "Tabela_GxA_PH.csv",h = T)
PMS <- read.csv(file = "Tabela_GxA_PMS.csv",h = T)
PROD <- read.csv(file = "Tabela_GxA_PROD.csv",h = T)
IPPH <- read.csv(file = "Tabela_GxA_IPPH.csv",h = T)

dados <- IPPH
dados <- dados[,-1]
colnames(dados) <- c("CULTIVARES", "A1", "A2", "A3",
                     "A4", "A5", "A6", "A7", "A8", "A9")

####Annichiarico baseado na média do ambiente e da testemunha####
annichiarico <- function(data, alpha, check, graph, lines, columns)
{
  genotype <- matrix(t(data[,1]))
  nomecultivar <- droplevels(data)$CULTIVARES
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
  
  
   # tiff(filename = paste("Annichiarico_Testemunha_Ambiente_IPPH_",
   #                      nomecultivar[check],".tiff", sep = "_"), 
   #       width = 6, height = 5, units = "in", res = 300, compression = "lzw")
   
  #jpeg('Annichiarico_Testemunha_Ambiente_BRS264.jpg')
  
  print(if (graph==T)
  {
    require(fmsb)
    par(mfrow=c(lines,columns))
    par(mai = rep(0.25, 4))
    w1 <- ceiling(max(data1))
    w2 <- ceiling(max(data2))
    w <- max(w1,w2)
    
    #somePDFPath = "C:/Users/Ricardo Ruiz/Desktop/Mestrado GMP/Scripts_Mestrado_Ricardo/Annichiarico_Testemunha_Ambiente_IPPH_BRS264.pdf"
    somePDFPath = "C:/Users/Ricardo Ruiz/Desktop/Mestrado GMP/Scripts_Mestrado_Ricardo/Annichiarico_Testemunha_Ambiente_IPPH_Tbio_Aton.pdf"
    pdf(file=somePDFPath)
    
    for(i in 1:x)  
    {
      tabgraf <- data.frame(rbind(rep(w,y),rep(0,y),data1[i,],apply(data1,2,mean),data2[i,]))
      colnames(tabgraf) <- colnames(data)
      grafico <- radarchart(tabgraf, axistype = 0, seg = 3, cglty = 3, plty = 1,
                            pty = 32, cglcol = 1, plwd = 2, pcol = c(1,2,4), axislabcol = 3,centerzero=T,
                            #title=paste("G",i,collapse="."))
                            title=paste(nomecultivar[i],collapse="."))
    }
    dev.off()
    
  })
  #dev.off()
  
  pi1 <- t(pi1)
  sd1 <- t(sd1)
  resultado1 <- t(resultado1)
  resultado2 <- t(resultado2)
  
  #Saida
  resumo <- as.data.frame(cbind(genotype, pi1, sd1,resultado1,resultado2))
  colnames(resumo)<-c("Genótipo", "Média", "DP", "I_Média", "I_Check")
  cat("Índice de Confiabilidade de Annichiarico", "\n")
  resumo
}


## Testemunha BRS 264
annichiarico(data = dados, alpha = 0.25, check = 2, graph = T, lines = 4, columns = 4)

## Tbio Aton
annichiarico(data = dados, alpha = 0.25, check = 12, graph = T, lines = 4, columns = 4)


####Annichiarico baseado na média do ambiente ####
annichiarico.mean <- function(data, alpha, graph, lines, columns)
{
  genotype <- matrix(t(data[,1]))
  nomecultivar <- droplevels(data)$CULTIVARES
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
  
  tiff(filename = paste("Annichiarico_Ambiente_IPPH_",".tiff", sep = "_"),
       width = 6, height = 5, units = "in", res = 300, compression = "lzw")
                        
  
  #jpeg('annichiarico_ambiente.jpg')
  
  if (graph==T)
  {
    require(fmsb)
    par(mfrow=c(lines,columns))
    par(mai = rep(0.25, 4))
    w1 <- ceiling(max(data1))
    
    
    # somePDFPath = "C:/Users/Ricardo Ruiz/Desktop/Mestrado GMP/Scripts_Mestrado_Ricardo/Annichiarico_Ambiente_Alt.pdf"
    # pdf(file=somePDFPath)
    
    for(i in 1:x)  
    {
      tabgraf <- data.frame(rbind(rep(w1,y),rep(0,y),data1[i,],apply(data1,2,mean)))
      colnames(tabgraf) <- colnames(data)
      grafico <- radarchart(tabgraf, axistype = 0, seg = 3, cglty = 3, plty = 1,
                            pty = 32, cglcol = 1, plwd = 2, pcol = c(1,2), axislabcol = 3,centerzero=T,
                            #title=paste("G",i,collapse="."))
                            title=paste(nomecultivar[i],collapse="."))
      
    }
    #dev.off()
    
  }
  dev.off()
  
  pi1 <- t(pi1)
  sd1 <- t(sd1)
  resultado1 <- t(resultado1)
  
  #Saida
  resumo <- as.data.frame(cbind(genotype, pi1, sd1,resultado1))
  colnames(resumo)<-c("Genótipo", "Média", "DP", "I_Média")
  cat("Índice de Confiabilidade de Annichiarico", "\n")
  resumo
}

annichiarico.mean(data = dados, alpha = 0.25, graph = T, lines = 4, columns = 4)


####Annichiarico baseado na media da testemunha####
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
  
  #Saida
  resumo <- as.data.frame(cbind(genotype, pi2, sd2,resultado2))
  colnames(resumo)<-c("Genótipo", "Média", "DP", "I_Check")
  cat("Índice de Confiabilidade de Annichiarico", "\n")
  resumo
}

## Testemunha BRS 264
annichiarico.check (data = dados, alpha = 0.25, check = 2)

## Tbio Aton
annichiarico.check (data = dados, alpha = 0.25, check = 12)

#### SK ####
rm(list=ls(all=T))
library(ggplot2)
source("sk.r")

ALT <- read.csv(file = "Medias_G_ALT.csv",h = T)
PH <- read.csv(file = "Medias_G_PH.csv",h = T)
PMS <- read.csv(file = "Medias_G_PMS.csv",h = T)
PROD <- read.csv(file = "Medias_G_PROD.csv",h = T)
IPPH <- read.csv(file = "Medias_G_IPPH.csv",h = T)
colnames(ALT)[3] <- "ALT" 
colnames(PH)[3] <- "PH" 
colnames(PMS)[3] <- "PMS" 
colnames(PROD)[3] <- "PROD"
colnames(IPPH)[3] <- "IPPH"

##### ALTURA ####
dados <- ALT
dados <- transform(dados, TREATMENT=factor(TREATMENT))
dados$lower.CL <- dados$ALT - qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error
dados$upper.CL <- dados$ALT + qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error

df_b <- data.frame(dados$TREATMENT, dados$ALT, dados$lower.CL, dados$upper.CL)
colnames(df_b) <- c("TREATMENT","ALTURA", "lower.CL", "upper.CL")

str(df_b)
x.j <- cbind(df_b[,1],df_b[,2])
nobs <- 3
mult <- matrix(1,nobs,1)
xk.j <- kronecker(x.j,mult)

GLe.i <- 201.10
SSe.i <- 3273

(CV <- sqrt(16.3)/mean(dados$ALT))

#Using a function SKT - Scott-Knott at the end of this file
result <- SK(xk.j[,2], xk.j[,1], round(GLe.i,digits=0), SSe.i, alpha = 0.05, debug=TRUE)
result$Treatments <- as.character(result$Treatments)
result$Treatments <- as.numeric(result$Treatments)
result <- result[order(result$Treatments),]

df_b$SK <- result$Groups
(df_b)

tiff(filename = paste(paste(colnames(dados)[3],".tiff", sep = "_")), width = 4, height = 5, units = "in", res = 300, compression = "lzw")


color_pallete_function <- colorRampPalette(
  colors = c("black", "blue", "red"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)
num_colors <- length(unique(df_b$SK))
SK_colors <- color_pallete_function(num_colors)

print(ggplot(df_b, aes(y=reorder(TREATMENT,ALTURA), x=ALTURA, color = SK)) + #sort the hybrids by the values of the BLUPs
        labs (x = "Altura (cm)", y = "Cultivares") +
        geom_point() +
        geom_vline(xintercept = mean(dados[,3], na.rm = T), col = "red") +
        geom_errorbarh(aes(xmin=lower.CL,
                           xmax=upper.CL), height=0.5) +
        theme(axis.text.x = element_text(color="black"),
              axis.text.y = element_text(color="black")) +
        theme(legend.position = "none"))

dev.off()

##### PH ####
dados <- PH
dados <- transform(dados, TREATMENT=factor(TREATMENT))
dados$lower.CL <- dados$PH - qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error
dados$upper.CL <- dados$PH + qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error

df_b <- data.frame(dados$TREATMENT, dados$PH, dados$lower.CL, dados$upper.CL)
colnames(df_b) <- c("TREATMENT","PH", "lower.CL", "upper.CL")

str(df_b)
x.j <- cbind(df_b[,1],df_b[,2])
nobs <- 3
mult <- matrix(1,nobs,1)
xk.j <- kronecker(x.j,mult)

GLe.i <- 197.90
SSe.i <- 1421.4

(CV <- sqrt(7.18)/mean(dados$PH))

#Using a function SKT - Scott-Knott at the end of this file
result <- SK(xk.j[,2], xk.j[,1], round(GLe.i,digits=0), SSe.i, alpha = 0.05, debug=TRUE)
result$Treatments <- as.character(result$Treatments)
result$Treatments <- as.numeric(result$Treatments)
result <- result[order(result$Treatments),]

df_b$SK <- result$Groups
(df_b)

tiff(filename = paste(paste(colnames(dados)[3],".tiff", sep = "_")), width = 4, height = 5, units = "in", res = 300, compression = "lzw")

color_pallete_function <- colorRampPalette(
  colors = c("black", "blue", "red"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)
num_colors <- length(unique(df_b$SK))
SK_colors <- color_pallete_function(num_colors)

print(ggplot(df_b, aes(y=reorder(TREATMENT,PH), x=PH, color = SK)) + #sort the hybrids by the values of the BLUPs
        labs (x = "PH (kg/hL)", y = "Cultivares") +
        geom_point() +
        geom_vline(xintercept = mean(dados[,3], na.rm = T), col = "red") +
        geom_errorbarh(aes(xmin=lower.CL,
                           xmax=upper.CL), height=0.5) +
        theme(axis.text.x = element_text(color="black"),
              axis.text.y = element_text(color="black")) +
        theme(legend.position = "none"))

dev.off()

##### PMS ####
dados <- PMS
dados <- transform(dados, TREATMENT=factor(TREATMENT))
dados$lower.CL <- dados$PMS - qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error
dados$upper.CL <- dados$PMS + qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error

df_b <- data.frame(dados$TREATMENT, dados$PMS, dados$lower.CL, dados$upper.CL)
colnames(df_b) <- c("TREATMENT","PMS", "lower.CL", "upper.CL")

str(df_b)
x.j <- cbind(df_b[,1],df_b[,2])
nobs <- 3
mult <- matrix(1,nobs,1)
xk.j <- kronecker(x.j,mult)

GLe.i <- 206.40
SSe.i <- 429.94

(CV <- sqrt(2.08)/mean(dados$PMS))

#Using a function SKT - Scott-Knott at the end of this file
result <- SK(xk.j[,2], xk.j[,1], round(GLe.i,digits=0), SSe.i, alpha = 0.05, debug=TRUE)
result$Treatments <- as.character(result$Treatments)
result$Treatments <- as.numeric(result$Treatments)
result <- result[order(result$Treatments),]

df_b$SK <- result$Groups
(df_b)

tiff(filename = paste(paste(colnames(dados)[3],".tiff", sep = "_")), width = 4, height = 5, units = "in", res = 300, compression = "lzw")

color_pallete_function <- colorRampPalette(
  colors = c("black", "blue", "red"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)
num_colors <- length(unique(df_b$SK))
SK_colors <- color_pallete_function(num_colors)

print(ggplot(df_b, aes(y=reorder(TREATMENT,PMS), x=PMS, color = SK)) + #sort the hybrids by the values of the BLUPs
        labs (x = "PMS (g)", y = "Cultivares") +
        geom_point() +
        geom_vline(xintercept = mean(dados[,3], na.rm = T), col = "red") +
        geom_errorbarh(aes(xmin=lower.CL,
                           xmax=upper.CL), height=0.5) +
        theme(axis.text.x = element_text(color="black"),
              axis.text.y = element_text(color="black")) +
        theme(legend.position = "none"))

dev.off()

##### PROD ####
dados <- PROD
dados <- transform(dados, TREATMENT=factor(TREATMENT))
dados$lower.CL <- dados$PROD - qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error
dados$upper.CL <- dados$PROD + qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error

df_b <- data.frame(dados$TREATMENT, dados$PROD, dados$lower.CL, dados$upper.CL)
colnames(df_b) <- c("TREATMENT","PROD", "lower.CL", "upper.CL")

str(df_b)
x.j <- cbind(df_b[,1],df_b[,2])
nobs <- 3
mult <- matrix(1,nobs,1)
xk.j <- kronecker(x.j,mult)

GLe.i <- 176
SSe.i <- 23112366.7

(CV <- sqrt(131320.2653)/mean(dados$PROD))

#Using a function SKT - Scott-Knott at the end of this file
result <- SK(xk.j[,2], xk.j[,1], round(GLe.i,digits=0), SSe.i, alpha = 0.05, debug=TRUE)
result$Treatments <- as.character(result$Treatments)
result$Treatments <- as.numeric(result$Treatments)
result <- result[order(result$Treatments),]

df_b$SK <- result$Groups
(df_b)

tiff(filename = paste(paste(colnames(dados)[3],".tiff", sep = "_")), width = 4, height = 5, units = "in", res = 300, compression = "lzw")


color_pallete_function <- colorRampPalette(
  colors = c("black", "blue", "red"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)
num_colors <- length(unique(df_b$SK))
SK_colors <- color_pallete_function(num_colors)

print(ggplot(df_b, aes(y=reorder(TREATMENT,PROD), x=PROD, color = SK)) + #sort the hybrids by the values of the BLUPs
        labs (x = "Produção de grãos (kg/ha)", y = "Cultivares") +
        geom_point() +
        geom_vline(xintercept = mean(dados[,3], na.rm = T), col = "red") +
        geom_errorbarh(aes(xmin=lower.CL,
                           xmax=upper.CL), height=0.5) +
        theme(axis.text.x = element_text(color="black"),
              axis.text.y = element_text(color="black")) +
        theme(legend.position = "none"))

dev.off()

##### IPPH ####
dados <- IPPH
dados <- transform(dados, TREATMENT=factor(TREATMENT))
dados$lower.CL <- dados$IPPH - qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error
dados$upper.CL <- dados$IPPH + qt(1 - ((1 - 0.95) / 2), 25) * dados$std.error

df_b <- data.frame(dados$TREATMENT, dados$IPPH, dados$lower.CL, dados$upper.CL)
colnames(df_b) <- c("TREATMENT","IPPH", "lower.CL", "upper.CL")

str(df_b)
x.j <- cbind(df_b[,1],df_b[,2])
nobs <- 3
mult <- matrix(1,nobs,1)
xk.j <- kronecker(x.j,mult)

GLe.i <- 173
SSe.i <- 25811322.4

(CV <- sqrt(149198.3954)/mean(dados$IPPH))

#Using a function SKT - Scott-Knott at the end of this file
result <- SK(xk.j[,2], xk.j[,1], round(GLe.i,digits=0), SSe.i, alpha = 0.05, debug=TRUE)
result$Treatments <- as.character(result$Treatments)
result$Treatments <- as.numeric(result$Treatments)
result <- result[order(result$Treatments),]

df_b$SK <- result$Groups
(df_b)

tiff(filename = paste(paste(colnames(dados)[3],".tiff", sep = "_")), width = 4, height = 5, units = "in", res = 300, compression = "lzw")


color_pallete_function <- colorRampPalette(
  colors = c("black", "blue", "red"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)
num_colors <- length(unique(df_b$SK))
SK_colors <- color_pallete_function(num_colors)

print(ggplot(df_b, aes(y=reorder(TREATMENT,IPPH), x=IPPH, color = SK)) + #sort the hybrids by the values of the BLUPs
        labs (x = "IPPH", y = "Cultivares") +
        geom_point() +
        geom_vline(xintercept = mean(dados[,3], na.rm = T), col = "red") +
        geom_errorbarh(aes(xmin=lower.CL,
                           xmax=upper.CL), height=0.5) +
        theme(axis.text.x = element_text(color="black"),
              axis.text.y = element_text(color="black")) +
        theme(legend.position = "none"))

dev.off()

## INDICE OLIVOTO 2019 ####
remove.packages("Matrix")
install.packages("Matrix")
remove.packages("lme4")
install.packages("lme4")
library(lme4)

library(metan)
ALT <- read.csv(file = "Medias_GxA_ALT.csv",h = T)
PH <- read.csv(file = "Medias_GxA_PH.csv",h = T)
PMS <- read.csv(file = "Medias_GxA_PMS.csv",h = T)
PROD <- read.csv(file = "Medias_GxA_PROD.csv",h = T)
IPPH <- read.csv(file = "Medias_GxA_IPPH.csv",h = T)
colnames(ALT)[4] <- "ALT" 
colnames(PH)[4] <- "PH" 
colnames(PMS)[4] <- "PMS" 
colnames(PROD)[4] <- "PROD"
colnames(IPPH)[4] <- "IPPH"

### Data ###
dados <- data.frame(ALT$TREATMENT, ALT$envf, ALT$ALT, PH$PH, 
                    PMS$PMS, PROD$PROD, IPPH$IPPH)
colnames(dados) <- c("TREATMENT", "ENV", "ALT", "PH",
                     "PMS", "PROD", "IPPH")

dados <- transform(dados, ENV =factor(ENV),
                   TREATMENT = factor(TREATMENT))

nomamb <- levels(droplevels(dados$ENV))
nomcul <- levels(droplevels(dados$TREATMENT))

x.j <- cbind(dados[,1], dados[,2], dados[,3], dados[,4],
             dados[,5], dados[,6], dados[,7])
typeof(x.j)
nobs <- 3
mult <- matrix(1,nobs,1)
xk.j <- kronecker(x.j,mult)
xk.j <- data.frame(xk.j)
#ambiente <- data.frame(gl(9,3, label =nomamb))
ambiente <- gl(9,3, label =nomamb)
ambiente <- data.frame(rep(ambiente, times = 16))
colnames(ambiente) <- "ENV"
cultivares <- data.frame(gl(16,27, label = nomcul))
colnames(cultivares) <- "GEN"
bloco <- data.frame(rep(c(1, 2, 3), times = 144)) 
colnames(bloco) <- "BLOCO"

dados_rep <- data.frame(ambiente, cultivares, bloco,xk.j$X3,
                        xk.j$X4, xk.j$X5, xk.j$X6, xk.j$X7)
colnames(dados_rep) <- c("ENV", "GEN", "BLOCO", "ALT", "PH",
                     "PMS", "PROD", "IPPH")
str(dados_rep)
dados <- transform(dados_rep, ENV =factor(ENV),
                   GEN = factor(GEN),
                   BLOCO = factor(BLOCO))


model_waasb <- 
  waasb(dados_rep,
        env = ENV,
        gen = GEN,
        rep = BLOCO,
        resp = c(ALT, PH, PMS, PROD, IPPH), 
        mresp = c("l, h, h, h, h"), # aumentar, aumentar, reduzir
        wresp = c(75, 50, 50, 75, 75), # definir pesos
        verbose = FALSE)

# índice WAASB
waasb_ind <- gmd(model_waasb, "WAASB")
print_tbl(waasb_ind)

### WAASBY

# A seleção simultânea para desempenho médio e estabilidade
# é baseada no índice WAASBY @Olivoto2019a.
# Este índice considera a estabilidade (*WAASB*) e 
# o desempenho médio (variável dependente, *Y*) 
# para classificação de genótipos 

waasby_ind <- gmd(model_waasb, what = "WAASBY")
print_tbl(waasby_ind, digits = 2)
plot_waasby(model_waasb, var = "PROD")

scenarios <- wsmp(model_waasb)
plot(scenarios,
     type = 2,
     var = "PROD")

#### Multitrait Stability Index (MTSI) ####

mtsi_model <- mtsi(model_waasb,
                   verbose = FALSE)
tiff(filename = paste(paste("Índice de estabilidade multitrait (MTSI)"
                            ,".tiff", sep = "_")), 
     width = 8, height = 5, units = "in", res = 300, compression = "lzw")

plot(mtsi_model, radar = T)
dev.off()
# Autovalores e variância explicada
print_tbl(mtsi_model$PCA)

# Diferencial de seleção para estabilidade
print_tbl(mtsi_model$sel_dif_stab)

# Diferencial de seleção para performance
print_tbl(mtsi_model$sel_dif_trait)


### Índice MGIDI
mod <- gamem(dados_rep,
             gen = GEN,
             rep = BLOCO,
             resp = everything())
## Erro
mgidi_index <-
  mgidi(mod,
        ideotype = c("l", "h", "h", "h", "h"),
        SI = 15)

plot(mgidi_index,
     SI = 15) # padrão

# ganhos de seleção
gmd(mgidi_index)

# radar plot
plot(mgidi_index)

# pontos fortes e fracos
plot(mgidi_index, type = "contribution")


