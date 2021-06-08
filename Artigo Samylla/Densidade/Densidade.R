# Artigo 1  ---------------------------------------------------------------
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
# Diretorios e datasets --------------------------------------------------

Local = "C:\\Users\\User\\Documents\\Artigo Samylla\\Densidade"
setwd(Local)


dataset <- read_excel("dataset.xlsx",
                      sheet = "densidade")

# Análise Explotatória dos dados ------------------------------------------

d = as.data.frame(dataset[,1:9])
d$Type = ifelse(d$`Ponto de Coleta` =="1"|
                d$`Ponto de Coleta` =="2"|
                d$`Ponto de Coleta` =="3","Treatment",
                  ifelse(d$`Ponto de Coleta`=="4"|
                         d$`Ponto de Coleta`=="5"|
                         d$`Ponto de Coleta`=="6","Control",
                                       d$`Ponto de Coleta`))
d$Marcador = ifelse(d$Phylum=="Cyanobacteria",1,0)
e = d[d$Tipo =="Média" & d$`Ponto de Coleta`!="RW",]
e$Phylum = as.factor(e$Phylum)
levels(e$Phylum)[1:3] =c("Bacillariophyta",
                         "Charophyta",
                         "Chlorophyta" )


d
## Geral - Ciano+ourtos  --------------------------------------------------

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
                   tit.graph1 = "Percentual.png",
                   tit.graph2 = "Percentual Area.png"){

h=x
h$Soma = as.numeric(rowSums(h[,-c(1:inicio.numeric)]))
g = cbind(h[,c(1:inicio.numeric)],
               round((h[,-c(1:inicio.numeric,ncol(h))]/h$Soma),3))

cat("Total...", rowSums(round((h[,-c(1:inicio.numeric,NCOL(h))]/h$Soma),3)),"\n","\n")

j = (melt(g,id.vars = c("Tempo", "Amostra")))

for (i in unique(j$Tempo)) {
  g = j[j$Tempo==i,]
  print(
    ggplot(g, aes(x = Amostra,
                  y = value,
                  fill = variable))+
      geom_bar(stat = "identity")+
      ylab("Percentual")+
      xlab(i)+
      facet_grid(~Amostra,scales = "free")+
      guides(fill = guide_legend(title = "",
                                 label.position = "left",
                                 title.position = "left"))+ # Cpom legenda

      theme(legend.text = element_text(face = "italic"),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = .5,
                                            linetype = 1)))
  ggsave(filename =paste("Time",i," -",tit.graph1),device = "png",
          width = 15, height = 8,units = "in",limitsize = F )



  ds.graph = as.data.frame(j)
}
return(ds.graph)
}

a = grafico(h,
            n = "Percentual geral sem RW",
            tit.graph1 = "Geral com ciano.png",
            inicio.numeric = 2)

h.melt = melt(h,id.vars = c("Tempo", "Amostra"))
str(h.melt)
h.melt$Tempo = as.factor(h.melt$Tempo)
levels(h.melt$Tempo) = c("0 h",   "72 h",  "168 h", "336 h", "504 h", "720 h")
h.melt$Amostra = as.factor(h.melt$Amostra)
levels(h.melt$Amostra) = c("M1", "M2", "M3", "M4", "M5", "M6")


library(RColorBrewer)

G = ggplot(a, aes(x = as.factor(Tempo),
              y = (value),
              group = variable,
              fill=variable,
              alpha = .5,
              colour = variable))+
  geom_area()+
  #geom_bar(stat = "identity")+
  ylab(expression("Percentual"))+
  xlab("Tempo")+
  facet_grid(~Amostra,scales = "free")+
  guides(fill = guide_legend(title = "",
                             label.position = "left",
                             title.position = "left"),
         colour = "none",
         alpha = "none")+
     scale_fill_manual(values =brewer.pal(n = 6,name = "Dark2"))+
      theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1))+
  theme_cowplot();G

ggsave(filename = "Fito sem Ciano.png",plot = G,device = "png",
       width = 20, height = 10,units = "in",dpi = "retina")


# Cianobacterias ----------------------------------------------------------

g = h.ciano = NA
e.ciano = e[e$Marcador==1,]

for (j in unique(e.ciano$Tempo)) {
  for (i in unique(e.ciano$`Ponto de Coleta`)) {
    f = (e.ciano[e.ciano$Tempo == j &
                   e.ciano$`Ponto de Coleta` == i,])
    g = data.frame("Tempo" = j,
                   "Amostra" = i,
                    t(tapply(f$Valor,
                             f$ORGANISMOS,
                             function(X) {round(sum(X),0)})))
    h.ciano = na.omit(rbind(h.ciano, g))
  }
};h.ciano
colnames(h.ciano)[3:20] = a
a = str_c(str_sub(a,end = -5),
          "sp.",
          sep = " ")
colnames(h.ciano)[3:20] = a
a = grafico(h.ciano,
            n = "sem RW em",
            tit.graph1 = "Ciano.png",
            inicio.numeric = 2)


h.melt = melt(h.ciano,id.vars = c("Tempo", "Amostra"))
str(h.melt)
levels(h.melt$variable)[14] = "Planktothrix sp."
levels(h.melt$variable)[7]  = "Raphidiopsis sp."
levels(h.melt$variable)[17] = "Snowella sp."
a = str_c(str_sub(levels(h.melt$variable),end = -5),
                                "sp.",
                                sep = " ")
levels(h.melt$variable) = a
h.melt$variable = as.factor(as.character(h.melt$variable))
h.melt$Tempo = as.factor(h.melt$Tempo)
levels(h.melt$Tempo) = c("0 h",   "72 h",  "168 h", "336 h", "504 h", "720 h")
h.melt$Amostra = as.factor(h.melt$Amostra)
levels(h.melt$Amostra) = c("M1", "M2", "M3", "M4", "M5", "M6")

G = ggplot(h.melt, aes(x = Tempo,
                       y = sqrt(value),
                       group = variable,
                       fill=variable,
                       alpha = .5,
                       colour = variable))+
  geom_area()+
  ylab(expression(sqrt("Densidade")))+
  xlab("Tempo")+
  facet_grid(~Amostra,scales = "free")+
  guides(fill = guide_legend(title = "",
                             label.position = "left",
                             title.position = "left"),
         colour = "none",
         alpha = "none")+
  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1))+
  theme_cowplot();G

ggsave(filename = "Ciano.png",plot = G,device = "png",
       width = 20, height = 10,units = "in",dpi = "retina")


# outros ------------------------------------------------------------------

g = h = NA
for (j in unique(e$Tempo)) {
  for (i in unique(e$`Ponto de Coleta`)) {
    f = (e[e$Tempo == j & e$`Ponto de Coleta` == i,])
    g = data.frame("Tempo" = j,
                   "Amostra" = i,
                   "Percentual" = unique(e[e$Tempo == j &
                                          e$`Ponto de Coleta` == i,4]),
                   t(tapply(f$Valor, f$Phylum , sum)))
    h = na.omit(rbind(h, g))
  }
};h #Retirar a coluna 3 (Percentual) e ciano (8)
h = h[,-c(3,8)]

grafico(h,
        n = "1 ")

h.melt = melt(h,id.vars = c("Tempo", "Amostra"))
str(h.melt)
h.melt$Tempo = as.factor(h.melt$Tempo)
levels(h.melt$Tempo) = c("0 h",   "72 h",  "168 h", "336 h", "504 h", "720 h")
h.melt$Amostra = as.factor(h.melt$Amostra)
levels(h.melt$Amostra) = c("M1", "M2", "M3", "M4", "M5", "M6")

G = ggplot(h.melt, aes(x = Tempo,
                       y = sqrt(value),
                       group = variable,
                       fill=variable,
                       alpha = .5,
                       colour = variable))+
  geom_area()+
  ylab(expression(sqrt("Densidade")))+
  xlab("Tempo")+
  facet_grid(~Amostra,scales = "free")+
  guides(fill = guide_legend(title = "",
                             label.position = "top",
                             title.position = "left"),
         colour = "none",
         alpha = "none")+
  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1))+
  theme_cowplot();G

ggsave(filename = "Fito Sem Ciano.png",plot = G,device = "png",
       width = 20, height = 10,units = "in",dpi = "retina")


h$Amostra = ifelse(h$Amostra == "1" |
                    h$Amostra == "2" |
                     h$Amostra == "3", "Treatment","Control")

grafico(h,n = "Sem Rw ciano tratamento e controle percentual em ")



ggplot(g, aes(x = Amostra,
              y = value,
              fill = variable))+
  geom_boxplot()+
  #scale_fill_grey(start = 0.2, end = .8)+
  ylab("Percentual")+
  xlab(i)+
  facet_grid(~Amostra,scales = "free")+
  guides(fill = guide_legend(title = "Philo",
                             label.position = "top",
                             title.position = "left"))+ # Cpom legenda

  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1))


# Poisson -----------------------------------------------------------------
# Formatação da base de dados ---------------------------------------------
d = as.data.frame(dataset[,1:9])


#Preditor Marcador
d$Marcador = ifelse(d$Phylum=="Cyanobacteria",1,0)

#Corte da base de dados para utilização do valor médio
e = d[d$Tipo =="Contagem 1" & d$`Ponto de Coleta`!="RW",]
#unique(d$Tipo)
#Converter Phylum em factor
e$Phylum = as.factor(e$Phylum) #Atenção: fatores duplicados!

#Correção do erro de duplicidade de fatores
levels(e$Phylum)[1:3] =c("Bacillariophyta",
                         "Charophyta",
                         "Chlorophyta" )

#Rearranjando o dataset
g = h = NA
for (j in unique(e$Tempo)) {
  for (i in unique(e$`Ponto de Coleta`)) {
    for (k in unique(e$Tipo)) {
      f = (e[e$Tempo == j &
               e$`Ponto de Coleta` == i &
                  e$Tipo == k , ])
      g = data.frame(
        "Tempo" = j,
        "Amostra" = i,
        "Percentual" = unique(e[e$Tempo == j &
                                  e$`Ponto de Coleta` == i, 4]),
        t(tapply(f$Valor, f$Phylum , sum))
      )
      h = na.omit(rbind(h, g))
    }
  }
};h


#Formato final do dataset
data = data.frame(cbind(h[,1:3], round(h[,-c(1:3)],0)))
#data = data[data$Amostra!=2 &
#              data$Amostra!=5 &
#                data$Tempo!=0,]

for (i in 4:10) {
  cat(colnames(data)[i],"\n")
  print(shapiro.test(sqrt(data[,i]+.5)))
  cat("------","\n")

}


#r= c(15,15,15,15,15,15,15,15,14,14,14,14,4,4,7,7,2,2,3,3)
d2$Percentual = r
#Exportação do dataset
write.csv(data,file = "tabela.csv",col.names = T)


# Verificação de superdisperssão  -----------------------------------------
round(apply(h[-c(1:3,7)], 2, mean),2)
round(apply(h[-c(1:3,7)], 2, var),2)
# Indicios de superdispersao, pois E(u)< var(u)

# Abordagem 1 -------------------------------------------------------------
#Não apresentou bons ajuste e foge da escopo do artigo
b=data.frame()
for (i in 4:10) {
  for (j in unique(data$Amostra)){
    x = data[data$Amostra == j, c(1, 3, i)]
    x[, 2] = as.factor(x[, 2])
    x[, 2] = relevel(x[, 2], ref = "0")
    attach(x, warn.conflicts = F)
    #Ajuste linear
    print(paste("Meso ",j, "\n", "Filo: ", colnames(x)[3], "\n"))
    m1 = lm(x[, 3] ~ x[, 1] + x[, 2], data = x)
    print(plot(x[, 3] ~ x[, 1], main = colnames(x)[3]))
    print(abline(lm(x[, 3] ~ x[, 1], data = x)))
    print(summary(m1))
    print(plot(m1, 1))
    #Ajuste Poisson
    modp <- glm(x[, 3] ~ x[, 1] + x[, 2], family = poisson, x)
    a = as.data.frame(faraway::sumary(modp)[["coefficients"]])
    a$Deviance = faraway::sumary(modp)[["deviance"]]
    a$Null.deviance = faraway::sumary(modp)[["null.deviance"]]
    a$Percentual = 1 - a$Deviance / a$Null.deviance
    a = round(a, 4)
    a$Meso = j
    a$Filo = colnames(x)[3]
    b = rbind(a, b)
  }
}


# Abordagem 2 -------------------------------------------------------------

#Utilizando 3 preditores: x1[Tempo -> Quantitativa: 0, 72, ..., 720)]
#                         x2[Tratamento: categorica: Sim/Não]
#                         x3[Percental: categorica: 0, <50, <70, 100]


data$Treat = as.factor(ifelse(data$Amostra =="1"|
                              data$Amostra =="2"|
                              data$Amostra =="3","Treat","Ctrl"))
d1 = data[,c(2,1,11,3,4:10)];d1

d1$Treat =as.factor(d1$Treat)
d1$Percentual = as.factor(d1$Percentual)
d1$Percentual = relevel(d1$Percentual, ref = "0")
d2 = d1[d1$Amostra!="2" & d1$Amostra!="5",]

k=e=b=data.frame()
for (i in 4:10) {
#Subset do dataset d1
    x = d1[d1$Treat=="Treat",c(1:3,i)]
    #x[, 3] = as.factor(x[, 3])
    #x[, 3] = relevel(x[, 3], ref = "0")
    attach(x, warn.conflicts = F)

    #Ajuste linear
    cat("Filo: ",colnames(x)[4],"...............","\n")
    m1 = lm(x[, 4] ~ x[, 1]+x[,3], data = x)
    #print(plot(x[, 4] ~ x[, 1], main = colnames(x)[4]))
    #print(abline(lm(x[, 4] ~ x[, 1], data = x)))
    #print(summary(m1))
    #print(plot(m1, 1))


    #Ajuste Poisson
    modp <- glm(x[, 4] ~ x[, 1] +x[,3], family = poisson, x)
    a = as.data.frame(summary(modp)[["coefficients"]])
    a$Deviance = summary(modp)[["deviance"]]
    a$Null.deviance = summary(modp)[["null.deviance"]]
    a$Percentual = 1 - a$Deviance / a$Null.deviance
    a = round(a, 4)
    a$Filo = colnames(x)[4]
    #dp = sum(residuals(modp,type="pearson")^2)/modp$df.res
    #print(dp)
    cat("Modelo de Poisson para",colnames(x)[4],"...............","\n")
    print(summary(modp))
    b = rbind(a, b)

    #quasi-poisson
    modd <- glm(x[, 4] ~ x[, 1]+x[, 3], family=quasipoisson, x)
    c = as.data.frame(summary(modd)[["coefficients"]])
    c$Deviance = summary(modd)[["deviance"]]
    c$Null.deviance = summary(modd)[["null.deviance"]]
    c$Percentual = 1 - c$Deviance / c$Null.deviance
    c = round(c, 3)
    c$Filo = colnames(x)[4]
    cat("Modelo de Quasi-poisson para",i,"\n")
    print(summary(modd))
    e = rbind(c, e)


    #BN
    modbn <- MASS::glm.nb(x[, 4] ~ x[, 1]+x[, 3], x)
    g = as.data.frame(summary(modbn)[["coefficients"]])
    g$Deviance = summary(modbn)[["deviance"]]
    g$Null.deviance = summary(modbn)[["null.deviance"]]
    g$Percentual = 1 - g$Deviance / g$Null.deviance
    g = round(g, 3)
    c$Filo = colnames(x)[4]
    cat("Modelo de Binomial Negativo para",colnames(x)[4],"...............","\n")
    print(summary(modbn))
    k = rbind(g, k)

    #Grafico de ajuste
    par(mfrow = c(1,2))
    halfnorm(residuals(modp))
    plot(log(fitted(modp)),log((x[,4]-fitted(modp))^2),
         xlab= expression(hat(mu)),
         ylab=expression((y-hat(mu))^2),
         main = paste(colnames(x)[4],
                      round(sum(residuals(modp,type="pearson")^2)/modp$df.res),2))
    #abline(0,2)
locator(1)
  }
#write.table(b, file = "poisson.txt",sep = ";",col.names = T)
#Qp = e


