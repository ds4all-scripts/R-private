# Artigo Densidade  -------------------------------------------------------

# Pacotes -----------------------------------------------------------------

library(devtools)
library(labestData)
library(gmodels)
library(reshape)
library(dplyr)
library(lubridate)
library(stats)
library(tseries)
library(rugarch)
library(fpp)
library(forecast)
library(car)
library(nortest)
require(graphics)
library(mFilter)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(randomForest)
library(readxl)
library(GGally)
library(ggalt)
library(faraway)
library(MASS)
library(lattice)
library(daewr)

# Diretório ---------------------------------------------------------------

local = c("C:/Users/User/Documents/Artigo Samylla/Densidade/DoE")
setwd(local)



# Base de dados -----------------------------------------------------------

# Sempre utilizar a Opção 1!
#Opção 1: usando o dataset "ValorMedio.txt" (usar sep = ";")
new.data = read.table("ValorMedio.txt",sep = ";")
head(new.data,3)

#Opção 2: usando o dataset "Dataset2.xlsx"
newdata = read_xlsx("Dataset2.xlsx")
newdata = as.data.frame(newdata)

#Preditor Block
newdata$Block = as.factor(ifelse(newdata$Amostra == 1 |
                                 newdata$Amostra == 4,"B1",
                                  ifelse(newdata$Amostra == 2 |
                                         newdata$Amostra == 5, "B2", "B3")))

#Preditor Outros: Soma dos outros filos
newdata$Outros = rowSums(newdata[, c(5:8, 10,11)])

#Preditor tratamento
newdata$Treat = as.factor(ifelse(newdata$Amostra =="1"|
                                   newdata$Amostra =="2"|
                                   newdata$Amostra =="3","Treat","Ctrl"))

#Preditor Numero de reatores em operação
                        #0 h             #72 h        #128 h
newdata$Reatores = c(0,0,0,0,0,0, 15,15,15,0,0,0, 15,15,15,0,0,0,
                        #336 h          #504          #720 h
                     14,15,14,0,0,0, 4,15,7,0,0,0, 2,8,3,0,0,0 )

#bck
#write.table(newdata[,c(4,1,2,19,3,18,16,9,17,12:15)],
#            file = "ValorMedio.txt",
#            sep = ";")

#write.table(newdata[,c(4,1,2,19,3,18,16,9,17,12:15,5:8, 10,11)],
#            file = "ValorMedioGeral.txt",
#            sep = ";")

# Seleção dos dados para análise ------------------------------------------

#Utilizando a opção 1
d2= new.data
head(d2,3)

#Utilizado a Opção 2
d2 = newdata[newdata$Block != "B2",c(4,1,2,19,3,18,16,9,17,12:15)]
head(d2,3)

#Formatando as variáveis de d2
d2$Amostra = as.factor(d2$Amostra)
d2$h = as.factor(d2$h)
d2$Tempo = as.factor(d2$Tempo)
d2$Percentual = as.factor(d2$Percentual)
d2$Percentual = relevel(d2$Percentual,"0")
d2$Treat = as.factor(d2$Treat)
d2$Block = as.factor(d2$Block)


#Verificação
summary(d2)
str(d2)


# Analise: Blocos fatoriais completamente aleatórios ----------------------

#Instruções:
#           dataset ser um 'dataframe' com nome d2;
#           As variáveis respostas deve estar na coluna 8 até 13;
#           Manipular as funções png() e dev.off() ou locator() analises específicas;

#Foi considerado:
#               1 - Dois blocos (B1 e B3), B2 foi excluído devido ao
#                   funcionamento anormal do reator desse mesocosmos;
#               2 - Além do Blocos, dois fatores foram analisados:
#                   Fator 1: Tratamento: 0 - Ctrl e 1 - Treat;
#                   Fator 2: Tempo: 0, 7 ,14 , 21, 28 e 35 dias;
#               3 - A interação entre Fator 1 e Fator 2 foi considerada;

# Post-hoc
#               4 - O fator tempo é uma variável quantitativa e pro isso
#                   foi possível ajustar uma regressão para verificar a
#                   existência de tendência siginificativa linear ou
#                   polinomial de grau até 6-1 (6 devido ao n°de nível
#                   do Fator de tempo) pelo método dos polinômios ortogonais
#               5 - Tratamento como 2 níveis, então a propria saída da ANova
#                   Fornecesse a resposta;
#

par(mfrow = c(2,3))
for (i in 8:13) {
  mod3 <- aov((d2[, i]) ~ Block + Tempo * Treat , data = d2)
  cat("Imprimindo....",colnames(d2)[i],"\n")
  print(summary(mod3))
  cat("..........................","\n","\n")

  #Contraste e ajuste via regressão

  #Tempo
  contrasts(d2$Tempo) <- contr.poly(6)
  mod2 <- aov(d2[, i] ~ Block + Tempo * Treat ,
              data = d2)
  cat("Coef. da regressão para o tempo | Filo:",colnames(d2)[i],"\n")
  print(summary.aov(mod2, split = list(
    Tempo = list(
      "Linear" = 1,
      "Quadratic" = 2,
      "Cubic" = 3,
      "Quartic" = 4,
      "Penta" = 5))))
  cat("..........................","\n","\n")

  #Grafico
  options(scipen = 999)
  png(filename = paste0("2 -Box   ",colnames(d2)[i],".png"),
      width = 15,
      height =7,
      res =600,
      units = "in")
  par(mfrow = c(2,3))
  R <- do.call("cbind", split(d2[d2$Treat != "Treat", i],
                              d2$Block[d2$Treat != "Treat"]))
  y <- apply(R, 1, mean)
  x <- as.double(levels(d2$Tempo))
  plot(
    x,
    y,
    xlab = expression("Dia"),
    ylab = ifelse(i==9|i==8,
                  expression("Densidade" ~ (Cel ~ mL ^ {
                    -1
                  })),""),
    ylim = c(min(R), max(R)),
    main = paste("Controle", colnames(d2)[i]),
    pch = 17,
    xaxt = "n")
  axis(1,
       x,
       c(0,  7, 14, 21, 28, 35))
  xx <- seq(0.0, 35, .1)
  d2.Linear = lm(y ~ poly(x, 1))
  lines(xx, predict(d2.Linear, data.frame(x = xx)), lty = 2)
  R <- do.call("cbind", split(d2[d2$Treat == "Treat", i],
                              d2$Block[d2$Treat == "Treat"]))
  y <- apply(R, 1, mean)
  x <- as.double(levels(d2$Tempo))
  plot(
    x,
    y,
    xlab = expression("Dia"),
    ylab = ifelse(i==9 |i==8,
                  expression("Densidade" ~ (Cel ~ mL ^ {-1})),""),
    ylim = c(min(R), max(R)),
    main = paste("Tratamento", colnames(d2)[i]),
    pch = 15,
    xaxt = "n"
  )
  axis(1,
       x,
       c(0,  7, 14, 21, 28, 35))
  xx <- seq(0.0, 35, .1)
  d2.Linear = lm(y ~ poly(x, 1))
  lines(xx, predict(d2.Linear,
                    data.frame(x = xx)),
        lty = 1)
  R <- do.call("cbind", split(d2[, i],
                              d2$Block))
  y <- apply(R, 1, mean)
  x <- c(1,0, 1,0, 1,0, 1,0, 1,0, 1,0)
  color = ifelse(i == 9 | i==8,
                 "gray",
                 c("gray","white"))
  box <- boxplot(y ~ as.factor(x),
                 col = c("gray","white"),
                 xlab = "",
                 ylab = ifelse(i==9|i==8,
                               expression("Densidade" ~ (Cel ~ mL ^ {-1  })),
                               ""),
                 ylim = c(min(R), max(R)),
                 xaxt = "n")
  title("Efeito do tratamento")
  mn.t <- tapply(y,as.factor(x), mean)
  xi <- seq(box$n)
  points(xi, mn.t, col = "black", pch = 8)
  axis(1,
       1:2,
       c("Controle", "Tratamento"))
  plot(mod3, c(1:2, 5))
  locator(1)
  cat("...........................................","\n","\n")
  dev.off()
}


# Gráfico dos filos -----------------------------------------------

#Base de dados
dataset <- read_excel("dataset.xlsx",
                      sheet = "densidade")
d = as.data.frame(dataset[,1:9])

#Preditor tratamento
d$Type = ifelse(d$`Ponto de Coleta` =="1"|
                  d$`Ponto de Coleta` =="2"|
                  d$`Ponto de Coleta` =="3","Treatment",
                ifelse(d$`Ponto de Coleta`=="4"|
                         d$`Ponto de Coleta`=="5"|
                         d$`Ponto de Coleta`=="6","Control",
                       d$`Ponto de Coleta`))
#Preditor Cianobactéria
d$Marcador = ifelse(d$Phylum=="Cyanobacteria",1,0)

#Acrescentar o novo fator do Tempo
d$h = d$Tempo
d$Tempo = as.factor(d$Tempo)
levels(d$Tempo) = c(0, 7, 14, 21, 28, 35)

# Criação dos blocos
d$Block = as.factor(ifelse(d$`Ponto de Coleta` == 1 |
                           d$`Ponto de Coleta` == 4 ,"B1",
                              ifelse(d$`Ponto de Coleta` == 2 |
                                     d$`Ponto de Coleta` == 5 , "B2",
                                          ifelse(d$`Ponto de Coleta` == "RW",
                                                 "RW","B3"))))

#Utilizando apenas os valores médios
e = d[d$Tipo =="Média" & d$`Ponto de Coleta`!="RW",]
e$Phylum = as.factor(e$Phylum)
levels(e$Phylum)[1:3] =c("Bacillariophyta",
                         "Charophyta",
                         "Chlorophyta" )


summary(e)
str(e)

# Geral - Ciano+ourtos  --------------------------------------------------

g = h = NA
for (j in unique(e$Tempo)) {
  for (i in unique(e$`Ponto de Coleta`)) {
    f = (e[e$Tempo == j & e$`Ponto de Coleta` == i,])
    g = data.frame("Tempo" = j,
                   "Amostra" = i,
                   t(tapply(f$Valor, f$Phylum , sum)))
    h = na.omit(rbind(h, g))
  }
};h

grafico = function(x,
                   n = "Percentual Filo ",
                   inicio.numeric = 2,
                   fim.numeric = NA,
                   block = 1 ,
                   pos.leg = "left",
                   tit.leg = "Genus",
                   tit.pos = "left"){
  h=x
  for (i in 1:inicio.numeric) {
    x[,i] = as.factor(x[,i])
  }
  h$Soma = as.numeric(rowSums(h[,-c(1:inicio.numeric)]))
  g = cbind(h[,c(1:inicio.numeric)],
            round((h[,-c(1:inicio.numeric,ncol(h))]/h$Soma),3))

  cat("Total...", rowSums(round((h[,-c(1:inicio.numeric,NCOL(h))]/h$Soma),3)),"\n","\n")

  j = (melt(g,id.vars = c("Tempo", "Amostra")))

  for (i in unique(j$Tempo)) {
    g = j[j$Tempo==i,]
    png(filename = paste0("1 ",n,i," h.png"),
        width =12,
        height =6,
        res = 300,
        units = "in")
    print(
      ggplot(g, aes(x = Amostra,
                    y = value,
                    fill = variable))+
        geom_bar(stat = "identity")+
        ylab("Percentual")+
        xlab(i)+
        facet_grid(~Amostra,scales = "free")+
        guides(fill = guide_legend(title = tit.leg,
                                   label.position = pos.leg,
                                   title.position = tit.pos))+ # Cpom legenda

        theme(legend.text = element_text(face = "italic"),
              panel.background = element_rect(fill = "white",
                                              colour = "black",
                                              size = .5,
                                              linetype = 1)))
    dev.off()

    ds.graph = as.data.frame(j)
  }
  return(ds.graph)
}

#Para dataset h: Retirar RW e usando média

a = grafico(h[,-7],n = "Percentual geral médio sem Rw ")

#Para dataset d3: Retirar RW, B2 e usando média

d3 = read.table("ValorMedioGeral.txt",sep = ";")
head(d3)
d3 = d3[d3$Block!="B3", c(3,1,14:19,8)]
d3$Tempo = as.factor(d3$Tempo)
levels(d3$Tempo) =c("0 dias",  "7 dias",  "14 dias", "21 dias", "28 dias", "35 dias")
d3$Amostra = as.factor(d3$Amostra)
levels(d3$Amostra) =c( "M1", "M3", "M4", "M6")

#Total geral com ciano
b = grafico(d3,n = "Percentual geral médio sem Rw e B2 ")

#Total geral sem ciano
b = grafico(d3[,-9],n = "Percentual médio sem ciano sem Rw e B2 ")


#Gêneros de ciano
e = d[d$Tipo =="Média" &
        d$`Ponto de Coleta`!="RW" &
        d$Marcador==1,]

g = h = NA
for (j in unique(e$Tempo)) {
  for (i in unique(e$`Ponto de Coleta`)) {
    (f = e[e$Tempo ==j & e$`Ponto de Coleta` == i,])
    g = data.frame("Tempo" = j,
                   "Amostra" = i,
                   t(tapply(f$Valor, f$ORGANISMOS , sum)))
    h = na.omit(rbind(h, g))
  }
};h

h$Tempo = as.factor(h$Tempo)
levels(h$Tempo) =c("0 dia",  "7 dias",  "14 dias ", "21 dias", "28 dias", "35 dias")
h$Amostra = as.factor(h$Amostra)
levels(h$Amostra) =c( "M1","M2", "M3", "M4", "M5", "M6")
a=grafico(h,n = "Percentual médio genero ciano sem RW ")


# Gráfico definitivo  -----------------------------------------------------


#Colocar o fator block
a$Bock =as.factor(ifelse(a$Amostra == 1 |
                           a$Amostra == 4 ,"B1",
                         ifelse(a$Amostra == 2 |
                                a$Amostra == 5 , "B2","B3")))

#Configurar o fator Tempo
a$Tempo = as.factor(a$Tempo)
levels(a$Tempo) =c("0 dia",  "7 dias",  "14 dias", "21 dias", "28 dias", "35 dias")

#Configurar o fator Amostro
a$Amostra = as.factor(a$Amostra)
levels(a$Amostra) = c( "M1", "M2", "M3", "M4","M5", "M6")


#Instrução da função:
#       1 - Formato do dataset da saído da função gráfico();
#       2 - Similar a "grafico()", escolher os parâmetros:
#           - pos.leg
#           - tit.pos
#           - tit.pos
grafico2 = function(g,
                    pos.leg = "left",
                    tit.leg = " ",
                    tit.pos = "left"){

ggplot(g, aes(x = Amostra,
              y = value,
              fill = variable))+
  geom_bar(stat = "identity")+
  ylab("Percentual")+
  xlab("")+
  facet_grid(Tempo~Bock,scales = "free")+
  guides(fill = guide_legend(title = tit.leg,
                             label.position = pos.leg,
                             title.position = tit.pos))+ # Cpom legenda

  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1))
}


grafico2(a,tit.pos = "bottom")

attach(a)
ggplot(a, aes(x = (Tempo),
              y = value,
              group = variable,fill=variable))+
  #geom_line()+ geom_point()+
  geom_area()+
  ylab("Percentual")+
  xlab("")+
  facet_grid(~Amostra,scales = "free")+
  guides(fill = guide_legend(title = tit.leg,
                             label.position = pos.leg,
                             title.position = tit.pos))+ # Cpom legenda

  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1))
