# Objetivos

## QUESTION I.	(OR1) Nos organismos remanescente, houve um maior impacto na
## integridade celular das cianobactérias do que das algas ao longo do tempo de tratamento?
###  R-> Isso ser? verificando calculando-se:
###      Odds[Algas]/Odds[Cyano], tr?s interpreta??es poss?veis
###              >1 -> Bom!! : ) ->  Cianobact?rias s?o mais atingidas,pois as algas s?o menos afetadas;
###              <1 -> M?dio :|  ->  Algas s?o mais atingidas do que ciano, mas n?o implica que ciano n?o sejam
###              =1 -> N?o h? direfen?a entre esses filos, ambas s?o impactadas da mesma forma.

## QUESTION II. (OR2)	O isolamento por si s? diminui a chance de c?lulas
## ?ntegras de em rela??o a ?gua bruta coletada no in?cio do tratamento?

###  R-> Isso será verificando calculando-se:
###      Odds[Control]/Odds[Raw Water], tr?s interpreta??es poss?veis
###              >1 -> M?dio :| -> O isomaento ? ben?fico para os organismos,pois a chance de c?lulas ?ntegras no Conotrole ? maior do que na ?gua bruta
###              <1 -> M?dio :| -> O isomaento ? prejudicial aos organismos, pois a chance de c?lulas ?ntegras no Conotrole ? menor do que na ?gua bruta
###              =1 -> Bom!! :) -> o isolamento n?o influencia em nada


## QUESTION III.	(OR3) O fotocat?lise diminui as chances de c?lulas ?ntegras
## dos organismos remanescente em rela??o ao controle?

###  R-> Isso ser? verificando calculando-se:
###      Odds[photo]/Odds[control], tr?s interpreta??es poss?veis
###              >1 -> Ruim!! :( -> A fotocat?lise ? ben?fica para os organismos, pois existe mais c?lulas ?ntegras  fotocat?lise
###              <1 -> as c?lulas do photo s?o mais afetadas, portanto o photo foi prejudicial a integridade
###              =1 -> o isolamento n?o influencia na integriadade


getwd()
local = "C:\\Users\\User\\Documents\\Artigo Samylla\\Samylla Meso\\Samylla"
setwd(local)
### Pacotes necess?rios --------------------------------------------------
library(bnstruct)
library(DMwR)  # algor?timo Knn para imputa??o de dados
library(readxl)
library(hnp)
library(ggplot2)
library(cowplot)
library(Factoshiny)


### Fun??o res?duo ----------------------------------------------------------
Residuos2 = function( modelo, main = ""){
  installed.packages("hnp")
  library(hnp)
  par(mfrow =c(2,2))

  #Estat???stica dos res???duos de Person
  rpears = residuals(modelo, type='pearson')

  QP<-sum(rpears^2)
  p2<-1-pchisq(QP,modelo$df.residual)
  Resulado_Pearson = cbind(QP,p2)

  #Gr???ficos
  plot(rpears, pch=16,
       ylim=c(-3.5,3.5), #Limites m???ximos
       ylab="Pearson residuals",
       xlab = "Observation",main = main)
  abline(h=0, lty=1)
  abline(h=-3, lty=3)
  abline(h= 3, lty=3)
  hnp(modelo,
      sim = 100,
      conf = 0.95,
      resid.type = "pearson",
      halfnormal = T,
      scale = F,
      plot.sim = T,
      verb.sim = F,
      warn = F,
      how.many.out = F,
      print.on = F,
      paint.out = F,
      col.paint.out = "blue",
      newclass = F)



  dev = residuals(modelo, type='deviance')
  QL = sum(dev^2)
  p1 = 1-pchisq(QL,modelo$df.residual)
  Resulado_deviance = cbind(QL,p1)


  plot(dev, pch=16,
       ylim=c(-3.5,3.5), #Limites m???ximos
       ylab="Deviance residuals",
       xlab = "Observation")
  abline(h=0, lty=1)
  abline(h=-3, lty=3)
  abline(h= 3, lty=3)
  hnp(modelo,
      sim = 100,
      conf = 0.95,
      resid.type = "deviance",
      halfnormal = T,
      scale = F,
      plot.sim = T,
      verb.sim = F,
      warn = F,
      how.many.out = F,
      print.on = F,
      paint.out = F,
      col.paint.out = "blue",
      newclass = F)

  saida = list(Resulado_deviance = Resulado_deviance,
               Resulado_Pearson = Resulado_Pearson)

  return(saida)
}


### Bases de dados --------------------------------------------------------

#### 4 bases seram utilizadas:

##### Compara??o entre ciano x algas (Base.xlsx)
Median <- read_excel("base.xlsx", sheet = "Median")

##### Compara??o entre Filamentosas x algas (arquivo Base.xlsx)
Fila <- read_excel(choose.files(), sheet = "Fila")

##### Compara??o entre Planktothrix x ciano
pkx_cyn <- read_excel(choose.files(), sheet = "pkx_vs_cyn")


##### Compara??o entre Planktothrix x algas
pkx_algas <- read_excel(choose.files(), sheet = "pkx_vs_algae")



#### Transformar o objeto `tiblle` em `dataframe`(Opcioanl)

##### Comparar  ciano x algas

b1=as.data.frame(Median)
#b1=as.data.frame(Fila)
#b1=as.data.frame(pkx_cyn)
#b1=as.data.frame(pkx_algas)
#
##### Compara??o entre Filamentosas x algas
#b1=as.data.frame(Fila)

##### Compara??o entre Planktothrix x ciano
#b1=as.data.frame(pkx_cyn)

##### Compara??o entre Planktothrix x algas
#b1=as.data.frame(pkx_algas)



### Formata?ao do dataset ------------------------------------------------

colnames(b1) = c("Sample","Hours", "Phylum", "Yes", "No")
str(b1)

b1$Sample = factor(as.factor(b1$Sample),levels = c("Raw Water", "Control","8 g/PDO" ))
relevel(b1$Sample,ref = "Raw Water")

#### Comparar ciano x algas
b1$Phylum = factor(as.factor(b1$Phylum),levels = c("Cyano","Algae"))
levels(b1$Phylum) =c("Cyano", "Algae")
relevel(b1$Phylum,ref = "Cyano")

##### Compara??o entre Filamentosas x algas
#b1$Phylum = factor(as.factor(b1$Phylum),levels = c("Cyano Fil","Algae"))
#levels(b1$Phylum) =c("Filamentous", "Algae")
#relevel(b1$Phylum,ref = "Filamentous")
#
###### Compara??o entre Planktothrix x ciano
#b1$Phylum = factor(as.factor(b1$Phylum),levels = c("P. agardhii","Cyano"))
#levels(b1$Phylum) =c("P. agardhii","Cyano")
#relevel(b1$Phylum,ref = "P. agardhii")

#pkx_algas
##### Compara??o entre Planktothrix x algas
#b1$Phylum = factor(as.factor(b1$Phylum),levels = c("P. agardhii","Algae"))
#levels(b1$Phylum) =c("P. agardhii","Algae")
#relevel(b1$Phylum,ref = "P. agardhii")
##


b1$Hours = as.factor(b1$Hours)
levels(b1$Hours) = c("0 h", "168 h", "336 h", "504 h", "72 h", "720 h")
b1$Hours = factor(as.factor(b1$Hours),levels = c("0 h", "72 h","168 h", "336 h", "504 h", "720 h"))
relevel(b1$Hours,ref = "0 h")
str(b1)

b1$Yes = round(b1$Yes, 0)
b1$No = round(b1$No, 0)


#Corre??o do tempo controle na ?gua bruta

b1[b1$Sample =="Raw Water"&b1$Hours!="0 h"&b1$Phylum=="Cyano",c(4,5)] =
  b1[b1$Sample =="Raw Water"&b1$Hours=="0 h"&b1$Phylum=="Cyano",c(4,5)]

b1[b1$Sample =="Raw Water"&b1$Hours!="0 h"&b1$Phylum!="Cyano",c(4,5)] =
  b1[b1$Sample =="Raw Water"&b1$Hours=="0 h"&b1$Phylum!="Cyano",c(4,5)];b1

### An?lise dos dados ----------------------------------------------------


### In?cio OR ------------------------------------------------------------



ANODEV = list()
Lista_Odds =  list()
Lista_Coef =  list()
OR1_p = OR2_p = OR3_p = OR2 = OR1 = OR3 = data.frame("Lower" = NA,
                                                     "OR"    = NA,
                                                     "Upper" = NA)
classe = "cian vs alg"
#classe = "fila vs alg"
#classe = "pktx vs cia"
#classe = "pktx vs alg"
for (i in levels(b1$Hours)) {
  print(paste("Inciando com",i))

#Banco de dados
b0 = b1[b1$Hours == i,-2 ]
b0 = b0[order(b0$Sample,decreasing = F),]

#Modelo Nulo
mod0 = glm(as.matrix(b0[,c(3,4)])~1, family=binomial(link="logit"), data=b0)

#Modelo Saturado
m1 = glm(formula = as.matrix(b0[, c(3,4)]) ~ Phylum + Sample + Phylum:Sample,
         family = binomial(link = "logit"), data = b0)
anova(m1, test="Chisq")
#Modelo escolhido
m2 = glm(formula = as.matrix(b0[, c(3,4)]) ~ Phylum + Sample,
         family = binomial(link = "logit"), data = b0)

#Modelo Somente Phylum
m3 = glm(formula = as.matrix(b0[, c(3,4)]) ~ Phylum,
         family = binomial(link = "logit"), data = b0)

#Modelo Somente Sample
m4 = glm(formula = as.matrix(b0[, c(3,4)]) ~ Sample,
         family = binomial(link = "logit"), data = b0)
anova(m4, test="Chisq")

# Imprimindo o diagn?stico dos res?duos
print(paste("Análises dos Resíduos [tempo",i,"]"))
Nomeplot = paste0(classe,"Análises dos Resíduos [tempo",i,"].png")

par(mfrow = c(2,2))
png(filename = Nomeplot, width = 800, height = 600,)
print(Residuos2(m2,main = i))
dev.off()



attach(b0)

# Tabela ANODEV
T0 = as.data.frame(anova(m1, test="Chisq"));T0
T0$AIC = c(
  AIC(mod0),
  AIC(m3),
  AIC(m2),
  AIC(m1)
)

T0.1 = as.data.frame(anova(m4, test="Chisq"));T0.1
T0.1$AIC =  c(AIC(mod0),
              AIC(m4))

Tab.ANODEV = rbind(T0.1 ,T0[-1,])
Tab.ANODEV$Deviance = formatC(Tab.ANODEV$Deviance, digits = 2, format = "f")
Tab.ANODEV$`Resid. Dev` = formatC(Tab.ANODEV$`Resid. Dev`,format = "f", digits = 2)
Tab.ANODEV$`Pr(>Chi)` = formatC( Tab.ANODEV$`Pr(>Chi)`,format = "f", digits = 4)
Tab.ANODEV$AIC = formatC( Tab.ANODEV$AIC,format = "f", digits = 1);Tab.ANODEV
row.names(Tab.ANODEV) = c(1:nrow(Tab.ANODEV))
Tab.ANODEV$Model =  c( "NULL",
                       "Sample",
                       "Phylum",
                       "Sample+Phylum",
                       "Sample+Phylum+Phylum|Sample")
Tab.ANODEV = Tab.ANODEV[,c(7,1:6)]

print(paste("tabela ANODEV - Tempo", i))
print(Tab.ANODEV )

ANODEV[i] = list(i = Tab.ANODEV )
print(ANODEV)
path=paste0("C:\\Users\\User\\Documents\\Artigo Samylla\\Samylla Meso\\Samylla\\ANODEV",classe, i ,".txt")
write.table(as.data.frame(ANODEV[i] ),file = path  ,sep = ";")

#coeficientes dos modelos
Coef = as.data.frame(round(cbind(coefficients(m2),confint(m2)),3))
Coef = Coef[,c(2,1,3)]
colnames(Coef) = c("Lower", "Coefficients", "Upper")
Lista_Coef[i] = list(i = Coef )

#odds
Odds = as.data.frame(round(exp(model.matrix(m2)%*%cbind(coefficients(m2),confint(m2))),3))
rownames(Odds) = c("S1","S2","S3","S4","S5","S6")
Odds = Odds[,c(2,1,3)]
colnames(Odds) = c("Lower", "Odds", "Upper")
Lista_Odds[i] = list(i = Odds )


#Odds Ratio decimal
#Algas x cianobact?rias

OR1[i,] =   data.frame( "Lower" = round(exp(Coef[3,])[1],2),
                          "OR"    = round(exp(Coef[3,])[2],2),
                          "Upper" = round(exp(Coef[3,])[3],2))

OR2[i,] =   data.frame( "Lower" = round(exp(Coef[4,]- Coef[3,])[1],1),
                          "OR"    = round(exp(Coef[4,]- Coef[3,])[2],1),
                          "Upper" = round(exp(Coef[4,]- Coef[3,])[3],1))


OR3[i,] =   data.frame( "Lower" = round(exp(Coef[2,])[1],2),
                        "OR"    = round(exp(Coef[2,])[2],2),
                        "Upper" = round(exp(Coef[2,])[3],2))

}

(Od = as.data.frame(cbind(
  Lista_Odds$`0 h`,
  Lista_Odds$`72 h`,
  Lista_Odds$`168 h`,
  Lista_Odds$`336 h`,
  Lista_Odds$`504 h`,
  Lista_Odds$`720 h`
)))


(Coeficientes = as.data.frame(cbind(
  Lista_Coef$`0 h`,
  Lista_Coef$`72 h`,
  Lista_Coef$`168 h`,
  Lista_Coef$`336 h`,
  Lista_Coef$`504 h`,
  Lista_Coef$`720 h`
)))


OR = OR1[-1,]
OR$Time =c(0,72,168,336,504,720)
OR$Sig = ifelse(OR$Lower<=1 & OR$Upper>=1,"No",ifelse(OR$OR ==1,"No", "Yes"))
OR = as.data.frame(OR[-1,])
write.table(OR,file = "C:\\Users\\User\\Documents\\Artigo Samylla\\Samylla Meso\\Samylla\\OR1.txt", sep = ";")
attach(OR)
Plot_OR1 = ggplot(OR)+aes(x = as.factor(Time), y = OR, color = Sig)+
  geom_point()+
  geom_errorbar(ymin=OR$Lower,ymax=OR$Upper)+
  ylim(c(0,5))+
  geom_hline(yintercept = 1)+
  theme_bw()+
  scale_color_grey(start = .7, end = .01, guide = "t")+
  xlab("Time (h)")+
  ylab(expression("OR"[1]))+
  theme(legend.position = "")+
  guides(color=guide_legend(title="Significant "));Plot_OR1

Plot_OR1.1 = ggplot(OR)+aes(x = as.factor(Time), y = OR, color = Sig)+
  geom_point()+
  geom_errorbar(ymin=OR$Lower,ymax=OR$Upper)+
  ylim(c(0,5))+
  geom_hline(yintercept = 1)+
  theme_bw()+
  scale_color_grey(start = .7, end = .01, guide = "t")+
  xlab("Time (h)")+
  ylab(expression("OR"[1]))+
  theme(legend.position = "left")+
  guides(color=guide_legend(title="Significant "));Plot_OR1.1


OR = OR2[-1,]
OR$Time =c(0,72,168,336,504,720)
OR$Sig = ifelse(OR$Lower<=1 & OR$Upper>=1,"No",ifelse(OR$OR ==1,"No", "Yes"))
OR = as.data.frame(OR[-1,])
write.table(OR,file = "C:\\Users\\User\\Documents\\Artigo Samylla\\Samylla Meso\\Samylla\\OR2.txt", sep = ";")

attach(OR)

Plot_OR2 = ggplot(OR)+aes(x = as.factor(Time), y = OR, color = Sig)+
  geom_point()+
  geom_errorbar(ymin=OR$Lower,ymax=OR$Upper)+
  ylim(c(0,5))+
  geom_hline(yintercept = 1)+
  theme_bw()+
  scale_color_grey(start = .01, end = .7)+
  xlab("Time (h)")+
  ylab(expression("OR"[2]))+
  theme(legend.position = "")+
guides(color=guide_legend(title="Significant "));Plot_OR2


Plot_OR2.1 = ggplot(OR)+aes(x = as.factor(Time), y = OR, color = Sig)+
  geom_point()+
  geom_errorbar(ymin=OR$Lower,ymax=OR$Upper)+
  ylim(c(0,5))+
  geom_hline(yintercept = 1)+
  theme_bw()+
  scale_color_grey(start = .01, end = .7)+
  xlab("Time (h)")+
  ylab(expression("OR"[2]))+
  theme(legend.position = "")+
  guides(color=guide_legend(title="Significant "));Plot_OR2.1


OR = OR3[-1,]
OR$Time =c(0,72,168,336,504,720)
OR$Sig = ifelse(OR$Lower<=1 & OR$Upper>=1,"No",ifelse(OR$OR ==1,"No", "Yes"))
OR = as.data.frame(OR[-1,])
write.table(OR,file = "C:\\Users\\User\\Documents\\Artigo Samylla\\Samylla Meso\\Samylla\\or3.txt", sep = "@", row.names =T, col.names = T)
attach(OR)
Plot_OR3 = ggplot(OR)+aes(x = as.factor(Time), y = OR, color = Sig)+
  geom_point()+
  geom_errorbar(ymin=OR$Lower,ymax=OR$Upper)+
  ylim(c(0,16))+
  geom_hline(yintercept = 1)+
  theme_bw()+
  scale_color_grey(start = .7, end = .01, guide = "t")+
  xlab("Time (h)")+
  ylab(expression("OR"[3]))+
  theme(legend.position = "")+
  guides(color=guide_legend(title="Significant "));Plot_OR3

#Ciano vs Algas

NomeplotOR = paste0(classe,"1 dataset corrigio (1000 x 400).png")
png(filename = NomeplotOR, width = 1000, height = 400,)
cian_vs_alg= plot_grid(Plot_OR1,Plot_OR2,Plot_OR3,labels = "AUTO", nrow = 1);cian_vs_alg
dev.off()
#
NomeplotOR = paste0("DEF1",classe," - dataset corrigio (1000 x 400).png")
png(filename = NomeplotOR, width = 10, height = 4,units = "in",res = 400)
  cian_vs_alg2= plot_grid(Plot_OR1.1,Plot_OR2,Plot_OR3,nrow = 1);cian_vs_alg2
dev.off()


#NomeplotOR = paste0(classe,"3 - dataset corrigio (1000 x 400).png")
#png(filename = NomeplotOR, width = 10, height = 4, res = 400, units = "in")
#fila_vs_alg= plot_grid(Plot_OR1,Plot_OR2,Plot_OR3,labels = "AUTO", nrow = 1);fila_vs_alg
#dev.off()


NomeplotOR = paste0("Def 2",classe,"Dataset corrigio (1000 x 400).png")
png(filename = NomeplotOR, width = 10, height = 4, res = 400, units = "in")
fila_vs_alg2= plot_grid(Plot_OR1.1,Plot_OR2,Plot_OR3, nrow = 1);fila_vs_alg2
dev.off()

NomeplotOR = paste0(classe," dataset corrigio (1000 x 400).png")
png(filename = NomeplotOR, width = 10, height = 4,res = 400, units = "in")
pktx_vs_cia= plot_grid(Plot_OR1,Plot_OR2,Plot_OR3,labels = "AUTO", nrow = 1);pktx_vs_cia
dev.off()
#
#
NomeplotOR = paste0(classe," 2 - dataset corrigio (1000 x 400).png")
png(filename = NomeplotOR, width = 10, height = 4,res = 400, units = "in")
pktx_vs_cia2= plot_grid(Plot_OR1.1,Plot_OR2,Plot_OR3, nrow = 1);pktx_vs_cia2
dev.off()
#

NomeplotOR = paste0(classe," dataset corrigio (800 x 600).png")
png(fiename = NomeplotOR, width = 800, height = 600,)
pktx_vs_alg= plot_grid(Plot_OR1,Plot_OR2,Plot_OR3,labels = "AUTO", nrow = 1);pktx_vs_alg
dev.off()


NomeplotOR = paste0("Def 3",classe," 2 - dataset corrigio (800 x 600).png")
png(filename = NomeplotOR, width = 800, height = 600,)
pktx_vs_alg2= plot_grid(Plot_OR1.1,Plot_OR2,Plot_OR3, nrow = 1);pktx_vs_alg2
dev.off()



NomeplotOR = paste0("GERAL(800 x 600).png")
png(filename = NomeplotOR, width = 1000, height = 1200,)
plot_grid(cian_vs_alg2,
          fila_vs_alg2,
          pktx_vs_cia2,
          pktx_vs_alg2,
          labels = "AUTO",
          nrow = 4)
dev.off()



### Fim da an?lise  do dados : OR ----------------------------------------


### In?cio PCA -----------------------------------------------------------

### Base de dados ###
library(Factoshiny)
BasePCA <- read_excel("Samylla Meso/PCA/BasePCA.xlsx", sheet = "PCA")

df.pca = as.data.frame(BasePCA)


str(df.pca)

#Testando procedimento para retirar o dado faltante

df.pca$Fluoride = df.pca$Fluoride1

df.pca$Fluoride1 = as.numeric(ifelse(df.pca$Fluoride =="N.D.", NA, df.pca$Fluoride ))

library(DMwR)  # algor?timo Knn para imputa??o de dados

df.pca1 <- knnImputation(df.pca[,-c(1,2,15)])

df.pca$Fluoride1 == df.pca1$Fluoride1

df.pca2 =cbind(df.pca[,c(1,2)],df.pca1) # banco sem missing data

str(df.pca2)

#Formantando os fatores
df.pca2$Time  = as.factor(df.pca2$Time)
levels(df.pca2$Time) = c("0 h",   "72 h",  "168 h", "336 h", "504 h", "720 h")

df.pca2$Sample = as.factor(df.pca2$Sample)
levels(df.pca2$Sample)= c("Mesocosm 1", "Mesocosm 2", "Mesocosm 3", "Mesocosm 4", "Mesocosm 5", "Mesocosm 6", "Raw water")


# analise geral
Factoshiny::PCAshiny(df.pca2)

# A compara??o total n?o foi boa,pois as duas primeiras componentes
# s? descrevem pouco menos de 38% dos dados

PCAshiny(df.pca2[,c(1:5,9,10)])   # Organol?ptico

PCAshiny(df.pca2[,c(1,2,6, 8,9, 11:13)]) #

PCAshiny(df.pca2[,c(1,2,6, 11:14)]) #

PCAshiny(df.pca2[,c(1,2,3,4,9,8,13)])

PCAshiny(df.pca2[,c(1,2,3,4,9,8,13)])



# FIM TESTE PCA -----------------------------------------------------------



# Nova fase (12/05/2020) ---------------------------------------------------

getwd()
setwd("C:/Users/allan/Documents/Samylla Nova")

dados_fq  =  read_excel("TiO2 Results table _DATASET_FQ (1).xlsx",sheet = "DATASETFQ")



dados.ctrl = as.data.frame(dados_fq[dados_fq$MESO =="Mesocosmo 4" |
                                    dados_fq$MESO =="Mesocosmo 5" |
                                    dados_fq$MESO =="Mesocosmo 6" ,-2])
dados.ctrl1 =  knnImputation(dados.ctrl[,-c(1,2)]) #imputa??o dos dados

dados.ctrl = cbind(dados.ctrl[,1],dados.ctrl1)

PCAshiny(dados.ctrl) #teste 1 ( todas as vari?veis)
PCAshiny(dados.ctrl[,c(1,2,3,7,14,16)]) #teste 2 apenas as mais relevantes


dados_fq  =  read_excel("TiO2 Results table _DATASET_FQ (1).xlsx",sheet = "DATASETFQ")



dados.trat = as.data.frame(dados_fq[dados_fq$MESO =="Mesocosmo 1" |
                                      dados_fq$MESO =="Mesocosmo 2" |
                                      dados_fq$MESO =="Mesocosmo 3" ,-2])
dados.trat1 =  knnImputation(dados.trat[,-c(1,2)]) #imputa??o dos dados

dados.trat = cbind(dados.trat[,1],dados.trat1)
View(dados.ctrl)
View(dados.trat)

PCAshiny(dados.trat) #teste 1 ( todas as vari?veis)
PCAshiny(dados.trat[,c(1,2,3,7,14,16)]) #teste 2 apenas as mais relevantes
