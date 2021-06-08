## Dataset, pacotes e diretórios  ----------------------------------------
setwd("C:/Users/allan/Documents/Samylla Tricoma")
library(readxl)
library(ggplot2)
library(stringr)
library(nortest)
library(cowplot) 

Trichomes <- read_excel("Trichomes.xlsx", sheet = "Dataset")
t.ds = as.data.frame(Trichomes)


# Fim ( parte 1) ----------------------------------------------------------


# Configuração Prévia -----------------------------------------------------

#visão geral 
summary(t.ds)
str(t.ds)
colnames(t.ds) = c("Time",    "Sample",  "cpt",     "Species" , "Sample1")

Trichomes$Time
# Configuração  "Time"
# t.ds$Time1 = str_sub(t.ds$Time, end=-14)
# t.ds$Time = (str_remove(t.ds$Time1, "T")) # Retirar o "T"
# t.ds$Time = (str_remove(t.ds$Time1, " ")) # Retira os espaços 
# t.ds$Time = as.factor(t.ds$Time)
t.ds$Time = as.factor(t.ds$Time)
levels(t.ds$Time)
t.ds$Time = factor(t.ds$Time,levels = c("0 h","72 h", "168 h", "336 h", "504 h", "720 h"))

#Configuração Sample e Sample1

t.ds$Sample1 = as.factor(ifelse(t.ds$Sample=="Mesocosmo 1" |
                      t.ds$Sample=="Mesocosmo 2" |
                      t.ds$Sample=="Mesocosmo 3","Treatment",
                      ifelse(t.ds$Sample == "Raw water","Raw water","Control")))

t.ds$Sample1 = factor(t.ds$Sample1, levels = c("Raw water","Control","Treatment"))
#Configuração Species

t.ds$Species = as.factor(t.ds$Species)


# Fim ( Parte 2) ----------------------------------------------------------


# Identificação das Diferenças  -------------------------------------------

lillie.test(t.ds$cpt[t.ds$Sample == "Raw water" & t.ds$Species == "Dolichospermum sp."]) # Não normal 

#### Porque não é normal (p<0.001):
#### 1 - Dados de contagem
#### 2 - Tamanho da amostra não foi suficiente  
#### 3 - Distribuição de Poisson

# teste 1 - Análise geral 
pairwise.wilcox.test(t.ds$cpt,t.ds$Sample1,
                     paired = F,
                     alternative = "t",
                     p.adjust.method = "bonferroni")

ggplot(t.ds,aes(y = t.ds$cpt, x = t.ds$Sample1))+geom_boxplot()

#Conclusão: necessidade de analise a nível de espécie

# Análise por organismo:
pairwise.wilcox.test(t.ds$cpt[t.ds$Species =="Dolichospermum sp."],
                     t.ds$Sample1[t.ds$Species =="Dolichospermum sp."],
                     paired = F,
                     alternative = "t",
                     p.adjust.method = "bonferroni")
#Conclusão: necessidade de verificação a nível de especie em cada tempo



#Verificação em nível de espécie em cada tempo

# teste dolicho por tempo

for (i in levels(t.ds$Time)) {
  
print(i)
print(pairwise.wilcox.test(    t.ds$cpt[t.ds$Species =="Dolichospermum sp."&t.ds$Time == i],
                           t.ds$Sample1[t.ds$Species =="Dolichospermum sp."&t.ds$Time == i],
                     paired = F,
                     alternative = "t",
                     p.adjust.method = "bonferroni"))
  }



for (i in levels(t.ds$Time)) {
  
  print(i)
  print(pairwise.wilcox.test(     t.ds$cpt[t.ds$Species =="Planktothrix agardhii"&t.ds$Time == i],
                              t.ds$Sample1[t.ds$Species =="Planktothrix agardhii"&t.ds$Time == i],
                                 paired = F,
                                 alternative = "t",
                                 p.adjust.method = "bonferroni"))
}

for (i in levels(t.ds$Time)) {
  
  print(i)
  print(pairwise.wilcox.test(     t.ds$cpt[    t.ds$Species =="Pseudoanabaena sp."&t.ds$Time == i],
                                  t.ds$Sample1[t.ds$Species =="Pseudoanabaena sp."&t.ds$Time == i],
                                  paired = F,
                                  alternative = "t",
                                  p.adjust.method = "bonferroni"))
}

for (i in levels(t.ds$Time)) {
  
  print(i)
  print(pairwise.wilcox.test(     t.ds$cpt[    t.ds$Species =="R. raciborskii"&t.ds$Time == i],
                                  t.ds$Sample1[t.ds$Species =="R. raciborskii"&t.ds$Time == i],
                                  paired = F,
                                  alternative = "t",
                                  p.adjust.method = "bonferroni"))
}


png(filename = "TRICOMA.png",width = 15,height = 8,units = "in",res = 500)
ggplot(t.ds,aes(y = cpt,
                x = Sample1))+
  ylab("Cells per trichome")+
  xlab("Samples")+
  geom_boxplot()+
  theme_bw()+
  facet_grid(t.ds$Species~t.ds$Time, scales = "free")
dev.off()


