### Base de dados ###
getwd()
local = "D:/Documents/Projects/R-private/Artigo Samylla/Samylla Meso/PCA"
setwd(local)
library(readxl)
BasePCA <- read_excel("BaseFinalPca.xlsx",
                      sheet = "PCA")

df.pca = as.data.frame(BasePCA)
str(df.pca)
summary(df.pca)

#Imputação dos dados Por Random Forest
df.na= df.pca

df.imputed = randomForest::rfImpute(as.factor(df.na$Sample) ~ .,
                                    df.na,
                                    iter=5,
                                    ntree=500)

# Em 23/11 decidimos não utilizar CT (col. 19) e CDT (col. 20)
# Pois mesmo após a imputação os dados não foram conclusivos

df.pca = df.imputed[,-c(20,21)]
summary(df.pca)

# Variáveis qualitativas

#Apenas Mesocosmo
library(FactoInvestigate)
library(Factoshiny)
library(xfun)
str(df.pca)

# Com água bruta = não apresentou bons resultados
Factoshiny::PCAshiny(df.pca)

#Sem água bruta (6 variáveis pré-selecionadas)
PCAshiny(df.pca[df.pca$`as.factor(df.na$Sample)`!="Raw water",
])
#Como justificar a utilização de apenas 6 variáveis ?
b = PCA(df.pca[,-1])
c = as.data.frame(round(b$var$contrib[,c(1,2)],2))

write.table(c,
            file = "Contribuição para cada componete.txt",
            sep = ";")
#3 variáveis PC1
c[order(c$Dim.1, decreasing = T),1:2][1:3,1]
#3 variáveis PC2
c[order(c$Dim.2, decreasing = T),1:2][1:3,2]


# Três variáveis com maior representação para cada componete
a = df.pca[df.pca$`as.factor(df.na$Sample)`!="Raw water",
           c(2,5,4,10,18,19)]

#Nova PCA
PCAshiny(a)



# Hierarquização ----------------------------------------------------------


a$Meso = rep(c("M1", "M2", "M3", "M4", "M5", "M6"),6)

row.names(a) = stringr::str_c(a$Meso,"-",a$Time)
a = a[,-7]

#calculo de distâncias
#Euclidiana
dist = dist(a)

#maximum
dist = dist(a,method = "maximum")

#manhattan
dist = dist(a,method = "manhattan")


#canberra (melhor distância)
dist = dist(a,method = "canberra")


#Método de hierarquização e dendograma

#complete
h = hclust(dist)
plot(h,cex = 0.6, hang = -1)
rect.hclust(h, k = 4, border = 1:6)

par(mfrow = c(1,1))
#"ward.D" (+ camberra)
h = hclust(dist, method = "ward.D")
plot(h,cex = 0.6, hang = -1)
h3 = rect.hclust(h, k = 3, border = 3)
rect.hclust(h, k = 4, border = 4)
rect.hclust(h, k = 8, border = 2)

#"ward.D2"
h = hclust(dist, method = "ward.D2")
plot(h,cex = 0.6, hang = -1)
rect.hclust(h, k = 4, border = 1:6)

#"single"
h = hclust(dist, method = "single")
plot(h,cex = 0.6, hang = -1)
rect.hclust(h, k = 4, border = 1:6)

#"average"
h = hclust(dist, method = "average")
plot(h,cex = 0.6, hang = -1)
rect.hclust(h, k = 4, border = 1:6)

#"mcquitty"
h = hclust(dist, method = "mcquitty")
plot(h,cex = 0.6, hang = -1)
rect.hclust(h, k = 4, border = 1:6)

#"median"
h = hclust(dist, method = "median")
plot(h,cex = 0.6, hang = -1)
rect.hclust(h, k = 4, border = 1:6)

#"centroid"
h = hclust(dist, method = "centroid")
plot(h,cex = 0.6, hang = -1)
rect.hclust(h, k = 4, border = 1:6)
summary(h)

#Criação dos clusters para verificação de diferenças significativas
a$h3 = as.factor(cutree(h, 3))
levels(a$h3) = c("0 %","100 %","< 70 %")
a$h3 = factor(a$h3,levels = c("0 %","< 70 %","100 %"))

a$h4 = as.factor( cutree(h, 4))
levels(a$h4) = c(   "0 % [0 h]",
                  "100 % [72 and 168 h]",
                 "< 70 % [336 and 504 h]",
                 "< 50 % [720 h]")
#English
a$h4 = factor(a$h4,levels = c("0 % [0 h]",
                              "100 % [72 and 168 h]",
                             "< 70 % [336 and 504 h]",
                              "< 50 % [720 h]"))

#Portugues
a$h4_p =a$h4
levels(a$h4_p) = c("0 % [0 h]",
                   "100 % [72 e 168 h]",
                   "< 70 % [336 e 504 h]",
                   "< 50 % [720 h]")
str(a)

a$Amostra = rep(rep(c("Tratamento","Controle"),c(3,3)),6)
a = a[,c(10,1:9)]

# Grafico turbidez --------------------------------------------------------

library(ggplot2)
library(cowplot)

ylabel= "Turbidity (uT)"
ylabel.p = "Turbidez (uT)"
xlabel=""
xlabel.p = "\nPercentual de iluminação [Tempo]"

Tb4.1 = ggplot(a, aes(x = h4_p, y = Turbidity, fill = Amostra))+
  geom_boxplot()+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p);Tb4.1

Tb4.2 = ggplot(a, aes(x = h4_p, y = Turbidity))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab("")+
  xlab(xlabel.p)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(7.3,7.35,5.2,5.9),
           label = c("(A)","(A,C)","(B)","(B,C)"));Tb4.2
Tb4 = plot_grid(Tb4.1,Tb4.2, labels = "AUTO");Tb4
ggsave(filename = "graphics/Turbidez_def.png",plot = Tb4,
         device = "png",
       width = 18, height = 7,units = "in")

pairwise.wilcox.test(a$`Turbidity (uT)`,
                     a$h4,
                     p.adjust.method = "bonferroni",
                     paired = F,
                     alternative = "t",
                     exact = F)



# Grafico Transparencia ---------------------------------------------------

ylabel = "Transparency (cm)"
ylabel.p = "Transparência (cm)"

Tb4.1 = ggplot(a, aes(x = h4_p, y = Transparency, fill = Amostra))+
  geom_boxplot()+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p);Tb4.1

Tb4.2 = ggplot(a, aes(x = h4_p, y = Transparency))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab("")+
  xlab(xlabel)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(53.3,61.3,97.2,60.9),
           label = c("(A)","(A,C)","(B,C)","(B,C)"));Tb4.2

Tb4 = plot_grid(Tb4.1,Tb4.2, labels = "AUTO");Tb4
ggsave(filename = "graphics/Transparency_def.png",plot = Tb4,
       device = "png",
       width = 18, height = 7,units = "in")


Tb4 = ggplot(a, aes(x = h4_p, y = `Transparency (cm)`))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab(ylabel)+
  xlab(xlabel)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(53.3,61.3,97.2,60.9),
           label = c("(A)","(A,C)","(B,C)","(B,C)"));Tb4

ggsave(filename = "Transparêncy_new_english.png",plot = Tb4,
       device = "png",scale = 2,
       width = 5,height = 3,units = "in")

Tb4 = ggplot(a, aes(x = a$h4_p, y = a$`Transparency (cm)`))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(53.3,61.3,97.2,60.9),
           label = c("(A)","(A,C)","(B,C)","(B,C)"));Tb4


ggsave(filename = "Transparência_new.png",plot = Tb4,
       device = "png",scale = 2,
       width = 5,height = 3,units = "in")



pairwise.wilcox.test(a$`Transparency (cm)`,
                     a$h4,
                     p.adjust.method = "bonferroni",
                     paired = F,
                     alternative = "t",
                     exact = F)


# Grafico Oxigencio dissolvido --------------------------------------------

ylabel = "Dissolved oxygen (mg/L)"
ylabel.p = "Oxigênio dissolvido (mg/L)"

Tb4.1 = ggplot(a, aes(x = h4_p, y = DO, fill = Amostra))+
  geom_boxplot()+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p);Tb4.1

Tb4.2 = ggplot(a, aes(x = h4_p, y = DO))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab("")+
  xlab(xlabel)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(9,5,5.2,4.75),
           label = c("(A)","(B)","(B)","(B)"));Tb4.2


Tb4 = plot_grid(Tb4.1,Tb4.2, labels = "AUTO");Tb4
ggsave(filename = "graphics/DO_def.png",plot = Tb4,
       device = "png",
       width = 18, height = 7,units = "in")


Tb4 = ggplot(a, aes(x = a$h4_p, y = a$`Dissolved oxygen  (mg/L)`))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(9,5,5.2,4.75),
           label = c("(A)","(B)","(B)","(B)"));Tb4
ggsave(filename = "Oxigênio dissolvido.png",plot = Tb4,
       device = "png",scale = 2,
       width = 5,height = 3,units = "in")

pairwise.wilcox.test(a$`Dissolved oxygen  (mg/L)`,
                     a$h4,
                     p.adjust.method = "bonferroni",
                     paired = F,
                     alternative = "t",
                     exact = F)


# Grafico Carbono organico dissolvido -------------------------------------

ylabel= "Dissolved Organic Carbon (mg/L)"
ylabel.p = "Carbono orgânico dissolvido (mg/L)"

Tb4.1 = ggplot(a, aes(x = h4_p, y = DOC, fill = Amostra))+
  geom_boxplot()+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p);Tb4.1


Tb4.2 = ggplot(a, aes(x = h4_p, y = DOC ))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab("")+
  xlab(xlabel)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(16.8, 12, 17.5, 17.5),
           label = c("(A)","(B)","(A)","(A)"));Tb4.2

Tb4 = plot_grid(Tb4.1,Tb4.2, labels = "AUTO");Tb4
ggsave(filename = "graphics/DOC_def.png",plot = Tb4,
       device = "png",
       width = 18, height = 7,units = "in")


Tb4 = ggplot(a, aes(x = a$h4_p, y = a$`Dissolved Organic Carbon (mg/L)`))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(16.8, 12, 17.5, 17.5),
           label = c("(A)","(B)","(A)","(A)"));Tb4
ggsave(filename = "Carbono orgânico dissolvido.png",plot = Tb4,
       device = "png",scale = 2,
       width = 5,height = 3,units = "in")

pairwise.wilcox.test(a$`Dissolved Organic Carbon (mg/L)`,
                     a$h4,
                     p.adjust.method = "bonferroni",
                     paired = F,
                     alternative = "t",
                     exact = F)


# Gráfico Carbono organico total ------------------------------------------

ylabel= "Total Organic Carbon (mg/L)"
ylabel.p="Carbono orgânico total (mg/L)"

Tb4.1 = ggplot(a, aes(x = h4_p, y = TOC, fill = Amostra))+
  geom_boxplot()+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p);Tb4.1

Tb4.2 = ggplot(a, aes(x = h4_p, y = TOC))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab(ylabel)+
  xlab(xlabel)+
  annotate("text",
         x = c(1.25,4.25,2.25,3.25),
         y = c(18, 12.5, 19.5, 19.5),
         label = c("(A)","(B)","(A)","(A)"));Tb4.2

Tb4 = plot_grid(Tb4.1,Tb4.2, labels = "AUTO");Tb4
ggsave(filename = "graphics/TOC_def.png",plot = Tb4,
       device = "png",
       width = 18, height = 7,units = "in")
Tb4 = ggplot(a, aes(x = a$h4_p, y = a$`Total Organic Carbon (mg/L)`))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  theme_cowplot()+
  ylab(ylabel.p)+
  xlab(xlabel.p)+
  annotate("text",
           x = c(1.25,4.25,2.25,3.25),
           y = c(18, 12.5, 19.5, 19.5),
           label = c("(A)","(B)","(A)","(A)"));Tb4

Tb4 = plot_grid(Tb4.1,Tb4.2, labels = "AUTO");Tb4
ggsave(filename = "graphics/DOC_def.png",plot = Tb4,
       device = "png",
       width = 18, height = 7,units = "in")

pairwise.wilcox.test(a$`Total Organic Carbon (mg/L)`,
                     a$h4,
                     p.adjust.method = "bonferroni",
                     paired = F,
                     alternative = "t",
                     exact = F)

