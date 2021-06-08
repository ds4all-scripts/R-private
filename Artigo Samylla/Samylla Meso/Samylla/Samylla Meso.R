library(ggplot2)
library(ISLR)
library(dplyr)
library(readxl)


DATASET_KNN = read_excel("C:/Users/allan/Google Drive/DS4All/ATENEU/DATASET.KNN.xlsx")

class(DATASET_KNN)
df = as.data.frame(DATASET_KNN)

str(df)

df$GENUS = as.factor(df$GENUS)

summary(df)

ggplot(df, aes(x = df$d, y = df$L, color = df$GENUS))+
  geom_point(shape =df$GENUS)+
  ylab("Length (µm)")+
  xlab("Diameter(µm)")+
  theme_classic()+
  scale_color_grey(start = .01, end = .5)
  

library(caTools)

set.seed(1)
div = sample.split(df$GENUS, SplitRatio = 0.80)

table(div)

length(div)/length(df$GENUS)
table(div)/sum(table(div))



df_treinamento = subset(df, div==TRUE) #treinamento
df_teste <- subset(df, div==FALSE) # teste

library(class)

set.seed(1)
prev = knn(train = df_treinamento[,-ncol(df_treinamento)],# dados do treinamento(sem rótulo) 
           test   = df_teste[,-3], # dados do teste (novamente sem rótulo)
           cl= df_treinamento[,3],

           k=50)
                 

sum(prev==df_teste[,3])/NROW(df_teste[,3]) #Acurácia 

mean(df_teste[,3] != prev) # Error

Ac = 0
Erro = 0
for (i in 1:100) {
  set.seed(1)
  prev = knn(train = df_treinamento[,-ncol(df_treinamento)],# dados do treinamento(sem rótulo) 
             test   = df_teste[,-3], # dados do teste (novamente sem rótulo)
             cl= df_treinamento[,3],
             k=i)
  Ac[i] = sum(prev==df_teste[,3])/NROW(df_teste[,3])
  Erro[i] = mean(df_teste[,3] != prev)
}

plot(Ac, ylim = c(0,1),
     main = "Teste1")
lines(Erro)



df_norm = as.data.frame(scale(df[,-3]))

summary(df_norm)

summary(df_norm)
df_norm$Genus = df[,3]


df_norm2= as.data.frame(apply(df[,-3], 2,function (z) (z-min(z))/(max(z)-min(z))))
df_norm2$Genus = df[,3]
summary(df_norm2)


set.seed(1)
div = sample.split(df$GENUS, SplitRatio = 0.80)

df_treinamento1 = subset(df_norm, div==TRUE) #treinamento
df_teste1       = subset(df_norm, div==FALSE) # teste



df_treinamento2 = subset(df_norm2, div==TRUE) #treinamento
df_teste2       = subset(df_norm2, div==FALSE) # teste






for (i in 1:nrow(df_teste)) {
  set.seed(1)
  prev = knn(train = df_treinamento1[,-ncol(df_treinamento)],# dados do treinamento(sem rótulo) 
             test   =       df_teste1[,-3], # dados do teste (novamente sem rótulo)
             cl=      df_treinamento1[,3],
             k=i)
  Ac[i] = sum(prev==df_teste1[,3])/NROW(df_teste1[,3])
  Erro[i] = mean(df_teste1[,3] != prev)
}

plot(Ac, ylim = c(0,1),main = "Teste2")
lines(Erro)


for (i in 1:nrow(df_teste)) {
  set.seed(1)
  prev = knn(train =  df_treinamento2[,-ncol(df_treinamento)],# dados do treinamento(sem rótulo) 
             test   =       df_teste2[,-3], # dados do teste (novamente sem rótulo)
             cl=      df_treinamento2[,3],
             k=i)
  Ac[i] = sum(prev==df_teste2[,3])/NROW(df_teste2[,3])
  Erro[i] = mean(df_teste2[,3] != prev)
}

plot(Ac, ylim = c(0,1),main = "Teste3")
lines(Erro)



