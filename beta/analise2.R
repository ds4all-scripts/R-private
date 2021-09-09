# Libraries, work directory and data set  ---------------------------------
require(ggplot2)
require(cowplot)
require(gridExtra)
require(betareg)
require(hnp)
require(xtable)
require(readxl)
rm(list = ls())
setwd("D:/Documents/Projects/R-private/beta")
data=read_excel("Datasets/Dataset2.xlsx")


# Exploratory analysis and  data visualization --------------------------

head(data)
summary(data)
data$Treatment=as.factor(ifelse(data$Percentual==0,0,1))
data$Day=as.factor(data$Day)
data$Sample=as.factor(data$Sample)

dataset=cbind.data.frame(data$Day,
                      data$Treatment,
                      data$Sample,
                      data$Others,
                      data$Cyanobacteria,
                      data$Isimp.,
                      data$Ishan.,
                      data$ID,
                      data$IE)

colnames(dataset)=c("Day", "Treatment", "Sample", "Others", "Cyanobacteria"
                   ,"Isimp", "Ishan", "ID", "IE")

# Day 0 adjustment
dataset$Treatment[1:3]=1

data_control=dataset[dataset$Treatment==0,]
data_treatment=dataset[dataset$Treatment!=0,]

# Cyanobacteria data visualization ----------------------------------------

ggplot(dataset)+
  aes(Cyanobacteria)+
  geom_histogram(fill="white",color="black")+
  theme_cowplot()

summary(dataset$Cyanobacteria)

# Boxplot
(ggplot(dataset)+
    aes(Treatment, Cyanobacteria)+
    geom_boxplot(fill="white",color="black")+
    theme_classic()+
    stat_summary(fun=mean,
                 geom="point",
                 shape= 8,
                 size=2))

ggplot(dataset)+
  aes(Cyanobacteria)+
  geom_histogram(fill="white",
                 color="black",
                 bins = 30)+
  theme_cowplot()+
  facet_grid(~Treatment)

wilcox.test(data_treatment$Cyanobacteria,data_control$Cyanobacteria)

# Lines comparing means in relation
# to time and treatment
(ggplot()+
  stat_summary(data=data_control,
                aes(x=Day,
                    y=Cyanobacteria,
                    linetype="Control"),
                fun = mean, geom="line",
                group=1)+
  stat_summary(data=data_treatment,
               aes(x=Day,
                   y=Cyanobacteria,
                   linetype="Treatment"),
               fun = mean,
               geom="line",
               group=1)+
  theme_classic()+
  scale_linetype_manual("Sample",
                        values=c("Treatment"=2,
                                 "Control"=1)))
dataset$Treatment2 = as.factor(ifelse(dataset$Treatment==1,"Yes","No"))

# Others data visualization -------------------------------------------------------------

ggplot(dataset)+
  aes(Others)+
  geom_histogram(fill="white",color="black")+
  theme_classic()
summary(dataset$Others)

# Box plot
(ggplot(dataset)+
  aes(Treatment2, Others)+
  geom_boxplot(fill="white",color="black")+
  theme_classic()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2))

wilcox.test(data_control$Others,data_treatment$Others)

# Lines comparing means in relation
# to time and treatment

(ggplot()+
  stat_summary(data=data_control,
                aes(x=Day,
                    y=Others,
                    linetype="Control"),
                fun = mean,
                geom="line",
                group=1)+
  stat_summary(data=data_treatment,
               aes(x=Day,
                   y=Others,
                   linetype="Treatment"),
               fun = mean,
               geom="line",
               group=1)+
  theme_classic()+
  scale_linetype_manual("Sample",
                        values=c("Treatment"=2,
                                 "Control"=1)))


# Isimp data visualization ------------------------------------------------

hist1=ggplot(dataset)+
  aes(Isimp)+
  geom_histogram(fill="white",
                 color="black")+
  theme_classic()
hist1
summary(dataset$Isimp)

# Box plot
b1 = (ggplot(dataset)
  +aes(Treatment2, Isimp,fill=Treatment)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none")
  +xlab("Treatment"))
b1
wilcox.test(data_control$Isimp,data_treatment$Isimp)


# Lines comparing means in relation
# to time and treatment

l1 = (ggplot()+
    stat_summary(data=data_control,
                 aes(x=Day,
                     y=Isimp,
                     linetype="Control"),
                 fun = mean,
                 geom="line",
                 group=1)+
    stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=Isimp,
                     linetype="Treatment"),
                 fun = mean,
                 geom="line",
                 group=1)+
    theme_classic()+
    scale_linetype_manual("Sample",
                          values=c("Treatment"=2,
                                   "Control"=1)))

# Ishan data visualization -------------------------------------------------------------------------

hist2=ggplot(dataset)+
  aes(Ishan)+
  geom_histogram(fill="white",
                 color="black")+
  theme_classic()
hist2
summary(dataset$Ishan)

# Box plot
b2 = (ggplot(dataset)
  +aes(Treatment2, Ishan,fill=Treatment)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none")
  +xlab("Treatment"))
b2
wilcox.test(data_control$Ishan,data_treatment$Ishan)


# Lines comparing means in relation
# to time and treatment

l2 = (ggplot()+
    stat_summary(data=data_control,
                 aes(x=Day,
                     y=Ishan,
                     linetype="Control"),
                 fun = mean,
                 geom="line",
                 group=1)+
    stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=Ishan,
                     linetype="Treatment"),
                 fun = mean,
                 geom="line",
                 group=1)+
    theme_classic()+
    scale_linetype_manual("Sample",
                          values=c("Treatment"=2,
                                   "Control"=1)))


# ID data visualization ----------------------------------------------------------------------

hist3=ggplot(dataset)+
  aes(ID)+
  geom_histogram(fill="white",
                 color="black")+
  theme_classic()
hist3
summary(dataset$ID)

# Box plot
b3 = (ggplot(dataset)
  +aes(Treatment2, ID,fill=Treatment)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none")
  +xlab("Treatment"))
b3
wilcox.test(data_control$ID,data_treatment$Ishan)


# Lines comparing means in relation
# to time and treatment

l3 = (ggplot()+
    stat_summary(data=data_control,
                 aes(x=Day,
                     y=ID,
                     linetype="Control"),
                 fun = mean,
                 geom="line",
                 group=1)+
    stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=ID,
                     linetype="Treatment"),
                 fun = mean,
                 geom="line",
                 group=1)+
    theme_classic()+
    scale_linetype_manual("Sample",
                          values=c("Treatment"=2,
                                   "Control"=1)))


# IE data visualization ---------------------------------------------------

hist4=ggplot(dataset)+
  aes(IE)+
  geom_histogram(fill="white",
                 color="black")+
  theme_classic()
hist4
summary(dataset$Ishan)

# Box plot
b4 = (ggplot(dataset)
  +aes(Treatment2, IE,fill=Treatment2)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none")
  +xlab("Treatment"))
b4
wilcox.test(data_control$Ishan,data_treatment$Ishan)


# Lines comparing means in relation
# to time and treatment

l4 = (ggplot()+
    stat_summary(data=data_control,
                 aes(x=Day,
                     y=IE,
                     linetype="Control"),
                 fun = mean,
                 geom="line",
                 group=1)+
    stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=Ishan,
                     linetype="Treatment"),
                 fun = mean,
                 geom="line",
                 group=1)+
    theme_classic()+
    scale_linetype_manual("Sample",
                          values=c("Treatment"=2,
                                   "Control"=1)))


# Overview ----------------------------------------------------------------


#histogram
h = plot_grid(hist1,hist2,hist3,hist4, labels = "AUTO")
h
ggsave(filename = "graphics/histogram_overview.png",
       plot = h,
       device = "png",
       width = 18, height = 7,units = "in")

#boxplot

b = plot_grid(b1,b2,b3,b4, labels = "AUTO")
b
ggsave(filename = "graphics/boxplot_overview.png",
       plot = b,
       device = "png",
       width = 18, height = 7,units = "in")


#lines

l = plot_grid(l1,l2,l3,l4, labels = "AUTO")
l
ggsave(filename = "graphics/lines_overview.png",
       plot = l,
       device = "png",
       width = 18, height = 7,units = "in")



#  Beta regression --------------------------------------------------------

#backup: "dataset2"
summary(dataset)
dataset2=dataset

#adjust for beta regression: transformation to scale 0.1
# to 0.9 to avoid problems at the extremes

dataset2$Ishan=0.1+(0.9-0.1)*(dataset2$Ishan-min(dataset2$Ishan))/(max(dataset2$Ishan)-min(dataset2$Ishan))

# effect of transformation on Ishan
summary(dataset2$Ishan)
par(mfrow = c(1,2))
hist(dataset2$Ishan,breaks = sqrt(length(dataset2$Ishan)))
hist(dataset$Ishan,breaks = sqrt(length(dataset2$Ishan)))

par(mfrow = c(1,1))
(plot(dataset$Ishan,
     type = "l",
     bty = "l",
     ylim = c(0,2),
     ylab = "Ishan",
     xlab = "Observation")+
lines(dataset2$Ishan, col = 2)+
abline(h = c(.1,.9),lty = 2)+
legend(5,2.20,
       legend = c("Normal","Transformed"),
       bty = "n",ncol = 2,lty = 1, col =c(1,2)))


# Model 1 - Isham ------------------------------------------------------------

modelo_ishan=betareg(Ishan~Day*Treatment,link="logit",data=dataset2)
coef_ishan=summary(modelo_ishan)
xtable(coef_ishan$coefficients$mean,digits=5)

confint(modelo_isimp)

r1=residuals(modelo_ishan,type="deviance")
r2=residuals(modelo_ishan,type="sweighted2")
r3=residuals(modelo_ishan,type="pearson")
#Graph residuals
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Indice",y="Residuos Deviance"))

r=(ggplot(dataset2)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Indices",y="Resíduos Padronizado tipo 2"))
r
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Pearson"))

pred_obs=(ggplot(dataset2)+geom_point(aes(x=modelo_ishan$y,y=predict(modelo_ishan)))
          +theme_classic()
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observados",y="y preditos"))
pred_obs
plot(modelo_ishan)

res=hnp(r2)


dados_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(dados_res)=c("x","lower","upper","median","residuals")
head(dados_res)

hnp_res_ggplot=ggplot(data = dados_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed")+
  theme_classic()

grid.arrange(r, hnp_res_ggplot, pred_obs, nrow=3)


#?ndice Isimp
summary(dataset2$Isimp)
hist(dataset2$Isimp)
modelo_isimp=betareg(Isimp~Day*Treatment,link="logit",data=dataset2)
model_isimp=summary(modelo_isimp)
model_isimp$coefficients$mean
xtable(model_isimp$coefficients$mean,digits=5)
confint(modelo_isimp)

r1=residuals(modelo_isimp,type="deviance")
r2=residuals(modelo_isimp,type="sweighted2")
r3=residuals(modelo_isimp,type="pearson")
#Gr?fico dos res?duos
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
+geom_hline(yintercept = c(-2,2),linetype="dashed")
+labs(x="?ndices",y="Res?duos Deviance"))

r=(ggplot(dataset2)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Padronizado tipo 2"))

(ggplot(dataset2)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Pearson"))

pred_obs=(ggplot(dataset2)+geom_point(aes(x=modelo_isimp$y,y=predict(modelo_isimp)))
 +theme_classic()
  +geom_abline(intercept = 0, slope = 1)
  +labs(x="y observados",y="y preditos"))

#plot(modelo_isimp)
res=hnp(r2)


dados_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(dados_res)=c("x","lower","upper","median","residuals")
head(dados_res)

hnp_res_ggplot=ggplot(data = dados_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed")+
  theme_classic()

grid.arrange(r, hnp_res_ggplot, pred_obs, nrow=3)

#?ndice ID
summary(dataset2$ID)
hist(dataset2$ID)
modelo_id=betareg(ID~Day*Treatment,link="logit",data=dataset2)
model_id=summary(modelo_id)
xtable(model_id$coefficients$mean,digits=5)

r1=residuals(modelo_id,type="deviance")
r2=residuals(modelo_id,type="sweighted2")
r3=residuals(modelo_id,type="pearson")
#Gr?fico dos res?duos
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Deviance"))

r=(ggplot(dataset2)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Padronizado tipo 2"))

(ggplot(dataset2)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Pearson"))

pred_obs=(ggplot(dataset2)+geom_point(aes(x=modelo_id$y,y=predict(modelo_id)))
  +theme_classic()
  +geom_abline(intercept = 0, slope = 1)
  +labs(x="y observados",y="y preditos"))

#plot(modelo_isimp)
hnp(r1)
res=hnp(r2)
hnp(r3)

dados_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(dados_res)=c("x","lower","upper","median","residuals")
head(dados_res)

hnp_res_ggplot=ggplot(data = dados_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed")+
  theme_classic()

grid.arrange(r,hnp_res_ggplot, pred_obs, nrow=3)
#?ndice IE
summary(dataset2$IE)
hist(dataset2$IE)
modelo_ie=betareg(IE~Day*Treatment,link="logit",data=dataset2)
modelo_ie
model_ie=summary(modelo_ie)
xtable(model_ie$coefficients$mean,digits=5)

confint(modelo_ie)
r1=residuals(modelo_ie,type="deviance")
r2=residuals(modelo_ie,type="sweighted2")
r3=residuals(modelo_ie,type="pearson")
#Gr?fico dos res?duos
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Deviance"))

r=(ggplot(dataset2)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Padronizado tipo 2"))

(ggplot(dataset2)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="?ndices",y="Res?duos Pearson"))

pred_obs=(ggplot(dataset2)+geom_point(aes(x=modelo_ie$y,y=predict(modelo_ie)))
  +theme_classic()
  +geom_abline(intercept = 0, slope = 1)
  +labs(x="y observados",y="y preditos"))

#plot(modelo_isimp)
hnp(r1)
res=hnp(r2)
hnp(r3)

dados_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(dados_res)=c("x","lower","upper","median","residuals")
head(dados_res)

res_hnp_ggplot=ggplot(data = dados_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed")+
  theme_classic()

grid.arrange(r,res_hnp_ggplot,pred_obs,nrow=3)


#Juntando gr?ficos antes da modelagem
hist4=ggplot(dataset2)+aes(Ishan)+geom_histogram(fill="white",color="black")+theme_classic()
hist4
grid.arrange(hist1,hist2,hist3,hist4,nrow=2)

write.csv(dados2, file="\\Users\\Alberto\\Desktop\\Trabalhos\\projetos\\artigo_allan_1\\dados2.csv",row.names=F)
?write.csv
