# Libraries, work directory and data set  ---------------------------------
require(ggplot2)
require(cowplot)
require(gridExtra)
require(betareg)
citation("betareg")
require(hnp)
require(xtable)
require(readxl)
rm(list = ls())
setwd("D:/Documents/Projects/R-private/beta")

# Photocatalysis data set  ------------------------------------------------
data2=read_excel("Datasets/Dataset2.xlsx",sheet = "data2")
data2 = data2[order(data2$Treatment, decreasing = T),]
# Exploratory analysis and  data visualization --------------------------

head(data2)
summary(data2)
data2$Treatment=as.factor(data2$Treatment)
data2$Day=as.factor(data2$Day)
levels(data2$Day) = c("0","1","3","5")
#Confirmation
str(data2)

# Observations:
#             - An observation of Day "3" and Treatment
#               "No" Control was imputed -> (Mean);
#             - On Day "0" the 3 observations regarding
#               the Treatment were duplicates of the
#               Control;
#

data2 = data2[order(data2$Day,decreasing = F),]
data2


# Shapiro-Wilk Normality Test ------------------------------------------------------------

dataset3 = data2 #Bkp

#backup: "dataset4"

dataset4=dataset3

#adjust for beta regression: transformation to scale 0.1
# to 0.9 to avoid problems at the extremes

dataset4$Ishan=0.1+(0.9-0.1)*(dataset3$Ishan-min(dataset3$Ishan))/(max(dataset3$Ishan)-min(dataset3$Ishan))


# Shapiro-Wilk Normality Test ---------------------------------------------

shapiro.test(dataset3$IE)
shapiro.test(dataset3$Ishan)


data_control = dataset3[dataset3$Treatment=="No",]
shapiro.test(data_control$IE)
shapiro.test(data_control$Ishan)

data_treatment=dataset3[dataset3$Treatment!="No",]
shapiro.test(data_treatment$IE)
shapiro.test(data_treatment$Ishan)

data_control2   = dataset4[dataset4$Treatment=="No",]
data_treatment2 = dataset4[dataset4$Treatment!="No",]


# Ishan data visualization ------------------------------------------------

summary(dataset3$Ishan)
sd(dataset3$Ishan)/mean(dataset3$Ishan)
shapiro.test(dataset3$Ishan)

H1 = ggplot(dataset3)+
  aes(Ishan)+
  geom_histogram(aes(y = stat(count)/sum(count),
                     fill = Treatment),
                 color="black")+
  ylab("")+
  theme_cowplot()+
  scale_fill_grey(start = .4,end = .8 )+
  theme(legend.position = "n")+
  ylim(0,.25)+ ylab("Relative frequency")
H1

#histogram transformed data

H3 = ggplot(dataset4)+
  aes(Ishan)+
  geom_histogram(aes(y = stat(count)/sum(count),
                     fill = Treatment),
                 color="black")+
  ylab("")+
  theme_cowplot()+
  scale_fill_grey(start = .4,end = .8 )+
  theme(legend.position = "n")+
  ylim(0,.25)+xlab("Ishan transformed")
H3



# Non-parametric modelling  -----------------------------------------------

# factor: Treatment

#Ishan

wilcox.test(data_treatment$Ishan,data_control$Ishan,exact = F)
wilcox.test(data_treatment2$Ishan,data_control2$Ishan,exact = F)

B1 = (ggplot(dataset3)+
        aes(Treatment, Ishan)+
        geom_boxplot(fill= c("darkgray","white"),color="black")+
        theme_classic()+
        xlab("Treatment")+
        stat_summary(fun=mean,
                     geom="point",
                     shape= 8,
                     size=2))

B1

#Ishan Transformed
attach(dataset4)
B2 = (ggplot(dataset4)+
        aes(Treatment, Ishan)+
        geom_boxplot(fill=c("darkgray","white"),
                     color="black")+
        theme_classic()+
        labs(x = "Treatment",y = "Ishan transformed")+
        stat_summary(fun=mean,
                     geom="point",
                     shape= 8,
                     size=2))

B2

#IE
pairwise.wilcox.test(dataset3$IE,
                     dataset3$Treatment,
                     p.adjust.method = "bonferroni",
                     paired = F,alternate = "t",exact = F)


B3 = (ggplot(dataset4)+
        aes(Treatment, IE)+
        geom_boxplot(fill=c("darkgray","white"),
                     color="black")+
        theme_classic()+
        labs(x = "Treatment",y = "IE")+
        stat_summary(fun=mean,
                     geom="point",
                     shape= 8,
                     size=2))

B3

# factor: Day

#Ishan
pairwise.wilcox.test(dataset3$Ishan,
                     dataset3$Day,
                     p.adjust.method = "bonferroni",
                     paired = F,alternate = "t",exact = F)

B4 = (ggplot(dataset3)+
        aes(Day, Ishan)+
        geom_boxplot(fill=c("darkgray","white",
                            "white","white"),color="black")+
        theme_classic()+
        xlab("Day")+
        stat_summary(fun=mean,
                     geom="point",
                     shape= 8,
                     size=2))

B4

#Ishan transformed
pairwise.wilcox.test(dataset4$Ishan,
                     dataset4$Day,
                     p.adjust.method = "bonferroni",
                     paired = F,alternate = "t",exact = F)

B5 = (ggplot(dataset4)+
        aes(Day, Ishan)+
        geom_boxplot(fill=c("darkgray","white",
                            "white","white"),color="black")+
        theme_classic()+
        xlab("Day")+
        labs(y = "Ishan transformed")+
        stat_summary(fun=mean,
                     geom="point",
                     shape= 8,
                     size=2))

B5


#IE

pairwise.wilcox.test(dataset3$IE,
                     dataset3$Day,
                     p.adjust.method = "bonferroni",
                     paired = F,alternate = "t",exact = F)

B6 = (ggplot(dataset3)+
        aes(Day, IE)+
        geom_boxplot(fill=c("darkgray","white",
                            "darkgray",
                            "darkgray"),color="black")+
        theme_classic()+
        xlab("Day")+
        stat_summary(fun=mean,
                     geom="point",
                     shape= 8,
                     size=2))

B6


#Final result

B7 = plot_grid(B4,B5,B1,B2,B6,B3,
               labels = c("A",NA,"B",
                          NA,"C","D"),ncol = 2)
B7

ggsave("graphics/box_plot_overview.png",
       plot = B7,width = 6,height = 7,units = "in",
       dpi = 10000,limitsize = F)

ggplot(dataset3)+
  aes(Ishan)+
  geom_histogram(fill="darkgray",
                 color="black",
                 bins = 30)+
  theme_cowplot()+
  facet_grid(~Treatment)

# Lines comparing means in relation
# to time and treatment
L1 = (ggplot()+
        stat_summary(data=data_control,
                     aes(x=Day,
                         y=Ishan,
                         linetype="Control"),
                     fun = mean, geom="line",
                     group=1)+
        stat_summary(data=data_treatment,
                     aes(x=Day,
                         y=Ishan,
                         linetype="Treatment"),
                     fun = mean,
                     geom="line",
                     group=1)+
        stat_summary(data=data_control,
                     aes(x=Day,
                         y=Ishan),
                     fun = mean, geom="point",
                     group=1)+
        stat_summary(data=data_treatment,
                     aes(x=Day,
                         y=Ishan),
                     fun = mean,
                     geom="point",
                     group=1)+
        theme_classic()+
        scale_linetype_manual("Sample",
                              values=c("Treatment"=2,
                                       "Control"=1)))
L1
# IE data visualization ------------------------------------------------

H2 = ggplot(dataset3)+
  aes(IE)+
  geom_histogram(aes(y = stat(count)/sum(count),
                     fill = Treatment),
                 #fill="darkgray",
                 color="black")+

  ylab("")+
  theme_cowplot()+
  scale_fill_grey(start = .4,end = .8 )+
  ylim(0,.25)+ ylab("Relative frequency")
H2
summary(dataset3$IE)
sd(dataset3$IE)/mean(dataset3$IE)
shapiro.test(dataset3$IE)
# Box plot


wilcox.test(data_treatment$IE,data_control$IE)

B2 = (ggplot(dataset3)+
        aes(Treatment, IE)+
        geom_boxplot(fill="white",color="black")+
        theme_classic()+
        xlab("Treatment")+
        stat_summary(fun=mean,
                     geom="point",
                     shape= 8,
                     size=2))

B2

ggplot(dataset3)+
  aes(IE)+
  geom_histogram(fill="darkgray",
                 color="black",
                 bins = 30)+
  theme_cowplot()+
  facet_grid(~Treatment)


# Lines comparing means in relation
# to time and treatment
L2 = (ggplot()+
        stat_summary(data=data_control,
                     aes(x=Day,
                         y=IE,
                         linetype="Control"),
                     fun = mean, geom="line",
                     group=1)+
        stat_summary(data=data_treatment,
                     aes(x=Day,
                         y=IE,
                         linetype="Treatment"),
                     fun = mean,
                     geom="line",
                     group=1)+
        stat_summary(data=data_control,
                     aes(x=Day,
                         y=IE),
                     fun = mean, geom="point",
                     group=1)+
        stat_summary(data=data_treatment,
                     aes(x=Day,
                         y=IE),
                     fun = mean,
                     geom="point",
                     group=1)+
        theme_classic()+
        scale_linetype_manual("Sample",
                              values=c("Treatment"=2,
                                       "Control"=1)))
L2

L3 = (ggplot()+
        stat_summary(data=data_control2,
                     aes(x=Day,
                         y=Ishan,
                         linetype="Control"),
                     fun = mean, geom="line",
                     group=1)+
        stat_summary(data=data_treatment2,
                     aes(x=Day,
                         y=Ishan,
                         linetype="Treatment"),
                     fun = mean,
                     geom="line",
                     group=1)+
        stat_summary(data=data_control2,
                     aes(x=Day,
                         y=Ishan),
                     fun = mean, geom="point",
                     group=1)+
        stat_summary(data=data_treatment2,
                     aes(x=Day,
                         y=Ishan),
                     fun = mean,
                     geom="point",
                     group=1)+
        theme_classic()+
        scale_linetype_manual("Sample",
                              values=c("Treatment"=2,
                                       "Control"=1)))+
  ylab("Ishan transformed")
L3


# Overview ----------------------------------------------------------------


#Transformation effect

Trf = ggplot(dataset3,aes(x = 1:24,
                          y = Ishan))+
  geom_line(size = 1.2,
            color = gray(.4))+ylim(0,5.5)+
  geom_line(aes(y = dataset4$Ishan),
            size = 1.2)+
  theme_cowplot()+
  annotate("text",
           x = 15 ,
           y = c(5.5,1.75),
           label = c("Non transformed data",
                     "Transformed data"), color = c(gray(.40),"black"))+
  geom_hline(yintercept = 1,linetype = 3,size = 1)
Trf
#histogram
H3 = H3 + xlab("Ishan tansformed")
H3 = H3 + ylab("Relative frequency")
H3

#With density
H1+ylim(0,.7)+geom_density()
H2+geom_density()+geom_density()+ylim(0,6)
H3+geom_density()+ylim(0,2.5)
Trf = Trf+xlab("Observations")
H = plot_grid(H1,H2,H3, Trf, ncol = 2, labels = "AUTO")
H


ggsave(filename = "graphics/Histogram_overview_H202.png",
       plot = H,
       device = "png",dpi = 1000,
       width = 13, height = 6,units = "in",limitsize = F)

#box plot

B = plot_grid(B1,B2, labels = "AUTO")
B
ggsave(filename = "graphics/Boxplot_overview_h2o2.png",
       plot = B,
       device = "png",dpi = 1000,limitsize = F,
       width = 6, height = 4,units = "in")






#lines
L1 = L1+ theme(legend.position = "n")+ylab("Ishan")
L3 = L3+ theme(legend.position = "n")

L = plot_grid(L1,L3, L2, labels = "AUTO",ncol = 3)
L
ggsave(filename = "graphics/Lines_overview_h2O2.png",
       plot = L,scale = 1.1,
       device = "png",dpi = 1000,limitsize = F,
       width = 12, height = 4,units = "in")

# Beta regression (H2O2) --------------------------------------------------------
# Model without interaction -----------------------------------------------
# Model 1 - Isham (H2O2) (without interaction) ------------------------------------------------------------

model_ishan=betareg(Ishan~Day+Treatment,link="logit",
                    data=dataset4)
confint(model_ishan)

#Table of results
tab_ishan=summary(model_ishan)
xtable(tab_ishan$coefficients$mean,digits=50)


# Residuals analysis
r1=residuals(model_ishan,type="deviance")
r2=residuals(model_ishan,type="sweighted2")
r3=residuals(model_ishan,type="pearson")

#graphic of residuals analysis
r=(ggplot(dataset4)+geom_point(aes(x=1:24,y=r1))+ylim(-3,3)+
     theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Index",y="Deviance residuals"))
r = r+annotate("text",
               x = 12.5,
               y = 2.3,
               label = "Ishan transformed")
r
(ggplot(dataset4)+geom_point(aes(x=1:24,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Standard residuals (type 2)"))

(ggplot(dataset4)+geom_point(aes(x=1:24,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

pred_obs=(ggplot(dataset4)+geom_point(aes(x=model_ishan$y,y=predict(model_ishan)))
          +theme_classic()
          +ylim(0,1)
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observed",y="y predicted"))
pred_obs

# Half-Normal Plots with Simulation Envelopes

#porque r2 ??? Não seria melhor escolher r1 ou r3?
res=hnp(r1)

dados_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(dados_res)=c("x","lower","upper","median","residuals")
head(dados_res)

hnp_res_ggplot=ggplot(data = dados_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed")+
  theme_classic()+labs(x = "Theoretical quantiles",
                       y = "Residuals")

g1 = grid.arrange(r, hnp_res_ggplot, pred_obs, nrow=3)
g1
#Others residuals
plot(model_ishan)


# Model 2 - IE (H2O2) (without interaction) ------------------------------------------------------------
model_ie=betareg(IE~Day+Treatment,
                 link="logit",
                 data=dataset4)
confint(model_ie)

summary(model_ie)
#Table of results
tab_ie=summary(model_ie)
xtable(tab_ie$coefficients$mean,digits=5)

print(citation("betareg"),
      bibtex=TRUE)
# Residuals analysis
r1=residuals(model_ie,type="deviance")
r2=residuals(model_ie,type="sweighted2")
r3=residuals(model_ie,type="pearson")

#graphic of residuals analysis
r=(ggplot(dataset4)+geom_point(aes(x=1:24,y=r1))+ylim(-3,3)+
     theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Index",y="Deviance residuals"))+
  annotate(geom = "text",
           x = 12.5,y = 2.25,
           label = "IE")
r

(ggplot(dataset4)+geom_point(aes(x=1:24,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Standard residuals (type 2)"))
(ggplot(dataset4)+geom_point(aes(x=1:24,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

pred_obs=(ggplot(dataset4)+geom_point(aes(x=model_ie$y,y=predict(model_ie)))
          +theme_classic()
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observed",y="y predicted")
          +ylim(0,1))
pred_obs

# Half-Normal Plots with Simulation Envelopes


res=hnp(r1)

dados_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(dados_res)=c("x","lower","upper","median","residuals")
head(dados_res)


hnp_res_ggplot=ggplot(data = dados_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed")+
  theme_classic()+labs(x = "Theoretical quantiles",
                       y = "Residuals")
hnp_res_ggplot
g3 = grid.arrange(r, hnp_res_ggplot, pred_obs, nrow=3)
g3
g = plot_grid(g1, g3, labels = "AUTO")
g


# Other link functions ----------------------------------------------------


link_func = c("logit","cauchit","probit",
              "cloglog", "log", "loglog")
List_plot = list()
for (i in link_func) {
cat("Link ----->", i, "\n")
model_ie=betareg(IE~Day+Treatment,
                 link= i,
                 data=dataset4)
print(summary(model_ie))
r1=residuals(model_ie,type="deviance")
res=hnp(r1)
data_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(data_res)=c("x","lower","upper","median","residuals")

print(head(data_res))
hnp = ggplot(data = data_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed") +
  theme_classic() + labs(x = "Theoretical quantiles",
                         y = "Residuals")+
  annotate("text", x = .5, y = 3,label = i)
print(hnp)
List_plot[[i]] = list(i = hnp)

cat("------------------------------------------ end","\n")
}

hnp1 = plot_grid(List_plot$logit$i,
                 List_plot$cauchit$i,
                 List_plot$probit$i,
                 List_plot$cloglog$i,
                 List_plot$loglog$i,
                 List_plot$log$i,
                 ncol = 3,
                 labels = "AUTO")

hnp1
ggsave(filename = "graphics/hnp_otherlinks_overview_h2O2.png",
       plot = hnp1,limitsize = F,
       device = "png",dpi = 1000,
       width = 6, height = 4,units = "in")

#The best link: Cauchit

model_ie_cauchit=betareg(IE~Day+Treatment,
                 link="cauchit",
                 data=dataset4)

summary(model_ie_cauchit)

#Table of results
tab_ie_cauchit=summary(model_ie_cauchit)
xtable(tab_ie_cauchit$coefficients$mean,digits=5)



# Models with interaction ----------------------------------------------------


# Model 3 -  Ishan (H2O2 ) with interaction -------------------------------

model_ishan=betareg(Ishan~Day*Treatment,
                 link="logit",
                 data=dataset4)

summary(model_ishan)
#Table of results
tab_ishan=summary(model_ishan)
xtable(tab_ishan$coefficients$mean,digits=5)


# Residuals analysis
r1=residuals(model_ishan,type="deviance")
r2=residuals(model_ishan,type="sweighted2")
r3=residuals(model_ishan,type="pearson")

#graphic of residuals analysis


r = (ggplot(dataset4)+geom_point(aes(x=1:24,y=r1))+ylim(-3,3)+
     theme_classic()
   +geom_hline(yintercept = c(-3,3),linetype="dashed")
   +labs(x="Index",y="Deviance residuals"))+
  annotate(geom = "text",
           x = 12.5,y = 2.25,
           label = "Ishan")
r
(ggplot(dataset4)+geom_point(aes(x=1:24,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Standard residuals (type 2)"))

(ggplot(dataset4)+geom_point(aes(x=1:24,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

# Half-Normal Plots with Simulation Envelopes

res=hnp(r1)

dados_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(dados_res)=c("x","lower","upper","median","residuals")
head(dados_res)


hnp_res_ggplot=ggplot(data = dados_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed")+
  theme_classic()+labs(x = "Theoretical quantiles",
                       y = "Residuals")


pred_obs=(ggplot(dataset4)+geom_point(aes(x=model_ie$y,y=predict(model_ie)))
          +theme_classic()
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observed",y="y predicted")
          +ylim(0,1))
pred_obs

G1 = grid.arrange(r, hnp_res_ggplot, pred_obs, nrow=3)
G1

# Model 4 -  Ishan (H2O2 ) with interaction -------------------------------

model_ie=betareg(IE~Day*Treatment,
                    link="logit",
                    data=dataset4)

summary(model_ie)
#Table of results
tab_ie=summary(model_ie)
xtable(tab_ie$coefficients$mean,digits=5)


# Residuals analysis
r1=residuals(model_ie,type="deviance")
r2=residuals(model_ie,type="sweighted2")
r3=residuals(model_ie,type="pearson")

#graphic of residuals analysis


r = (ggplot(dataset4)+geom_point(aes(x=1:24,y=r1))+ylim(-3,3)+
       theme_classic()
     +geom_hline(yintercept = c(-3,3),linetype="dashed")
     +labs(x="Index",y="Deviance residuals"))+
  annotate(geom = "text",
           x = 12.5,y = 2.25,
           label = "IE")

r


(ggplot(dataset4)+geom_point(aes(x=1:24,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Standard residuals (type 2)"))

(ggplot(dataset4)+geom_point(aes(x=1:24,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))


# Half-Normal Plots with Simulation Envelopes

res=hnp(r1)

dados_res= data.frame(res$x, res$lower, res$upper, res$median, res$residuals)
colnames(dados_res)=c("x","lower","upper","median","residuals")
head(dados_res)


hnp_res_ggplot=ggplot(data = dados_res, aes(x)) +
  geom_point(aes(y = residuals)) +
  geom_line(aes(y = lower)) +
  geom_line(aes(y = upper)) +
  geom_line(aes(y = median), linetype = "dashed")+
  theme_classic()+labs(x = "Theoretical quantiles",
                       y = "Residuals")


pred_obs=(ggplot(dataset4)+geom_point(aes(x=model_ie$y,y=predict(model_ie)))
          +theme_classic()
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observed",y="y predicted")
          +ylim(0,1))
pred_obs

G2 = grid.arrange(r, hnp_res_ggplot, pred_obs, nrow=3)
G2

# All residuals
G = grid.arrange(G1,G2, nrow=1)
G

ggsave(filename = "graphics/Residuals_interaction_h2O2.png",
       plot = G,scale = 1.1,
       device = "png",dpi = 1000,limitsize = F,
       width = 4, height = 7,units = "in")

# Photocatalysis data set  ------------------------------------------------

data=read_excel("Datasets/Dataset2.xlsx",sheet = "data")

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

shapiro.test(dataset$Isimp)
shapiro.test(dataset$Ishan)
shapiro.test(dataset$ID)
shapiro.test(dataset$IE)



data_control=dataset[dataset$Treatment==0,]
shapiro.test(data_control$Isimp)
shapiro.test(data_control$Ishan)
shapiro.test(data_control$ID)
shapiro.test(data_control$IE)

data_treatment=dataset[dataset$Treatment!=0,]
shapiro.test(data_treatment$Isimp)
shapiro.test(data_treatment$Ishan)
shapiro.test(data_treatment$ID)
shapiro.test(data_treatment$IE)


dataset$Treatment2 = as.factor(ifelse(dataset$Treatment==1,"Yes","No"))

# Cyanobacteria data visualization ----------------------------------------

ggplot(dataset)+
  aes(Cyanobacteria)+
  geom_histogram(fill="darkgray",color="black")+
  theme_cowplot()

summary(dataset$Cyanobacteria)

# Box plot

wilcox.test(data_treatment$Cyanobacteria,data_control$Cyanobacteria)

(ggplot(dataset)+
    aes(Treatment2, Cyanobacteria)+
    geom_boxplot(fill="white",color="black")+
    theme_classic()+
    xlab("Treatment")+
    stat_summary(fun=mean,
                 geom="point",
                 shape= 8,
                 size=2))

ggplot(dataset)+
  aes(Cyanobacteria)+
  geom_histogram(fill="darkgray",
                 color="black",
                 bins = 30)+
  theme_cowplot()+
   facet_grid(~Treatment2)

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
    stat_summary(data=data_control,
                 aes(x=Day,
                     y=Cyanobacteria),
                 fun = mean, geom="point",
                 group=1)+
    stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=Cyanobacteria),
                 fun = mean,
                 geom="point",
                 group=1)+
  theme_classic()+
  scale_linetype_manual("Sample",
                        values=c("Treatment"=2,
                                 "Control"=1)))

# Others data visualization -------------------------------------------------------------

ggplot(dataset)+
  aes(Others)+
  geom_histogram(fill="darkgray",color="black")+
  theme_classic()
summary(dataset$Others)

# Box plot
wilcox.test(data_control$Others,data_treatment$Others)

(ggplot(dataset)+
  aes(Treatment2, Others)+
  geom_boxplot(fill="white",color="black")+
  theme_classic()+
  xlab("Treatment")+
  stat_summary(fun=mean, geom="point", shape= 8, size=2))


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
    stat_summary(data=data_control,
                 aes(x=Day,
                     y=Others),
                 fun = mean,
                 geom="point",
                 group=1)+
    stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=Others),
                 fun = mean,
                 geom="point",
                 group=1)+
    theme_classic()+
  scale_linetype_manual("Sample",
                        values=c("Treatment"=2,
                                 "Control"=1)))


# Isimp data visualization ------------------------------------------------

summary(dataset$Isimp) # [0,1] OK!

hist1=ggplot(dataset)+
  aes(Isimp)+
  geom_histogram(fill="darkgray",
                 color="black")+
  theme_classic()
hist1

# Box plot
wilcox.test(data_control$Isimp,data_treatment$Isimp)

b1 = (ggplot(dataset)
  +aes(Treatment2, Isimp,fill=Treatment)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none")
  +xlab("Treatment"))
b1

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
      stat_summary(data=data_control,
                   aes(x=Day,
                       y=Isimp),
                   fun = mean,
                   geom="point",
                   group=1)+
      stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=Isimp,
                     linetype="Treatment"),
                 fun = mean,
                 geom="line",
                 group=1)+
      stat_summary(data=data_treatment,
                   aes(x=Day,
                       y=Isimp),
                   fun = mean,
                   geom="point",
                   group=1)+
      theme_classic()+
    scale_linetype_manual("Sample",
                          values=c("Treatment"=2,
                                   "Control"=1)))
l1
# Ishan data visualization -------------------------------------------------------------------------

summary(dataset$Ishan) # interval [0,1] exceeded, transform the data!

hist2=ggplot(dataset)+
  aes(Ishan)+
  geom_histogram(fill="darkgray",
                 color="black")+
  theme_classic()
hist2

# Box plot
wilcox.test(data_control$Ishan,data_treatment$Ishan)

b2 = (ggplot(dataset)
  +aes(Treatment2, Ishan,fill=Treatment)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none")
  +xlab("Treatment"))
b2


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
      stat_summary(data=data_control,
                   aes(x=Day,
                       y=Ishan),
                   fun = mean,
                   geom="point",
                   group=1)+
    stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=Ishan,
                     linetype="Treatment"),
                 fun = mean,
                 geom="line",
                 group=1)+
      stat_summary(data=data_treatment,
                   aes(x=Day,
                       y=Ishan),
                   fun = mean,
                   geom="point",
                   group=1)+
          theme_classic()+
    scale_linetype_manual("Sample",
                          values=c("Treatment"=2,
                                   "Control"=1)))
l2

# ID data visualization ----------------------------------------------------------------------
summary(dataset$ID)

hist3=ggplot(dataset)+
  aes(ID)+
  geom_histogram(fill="darkgray",
                 color="black")+
  theme_classic()
hist3

# Box plot
wilcox.test(data_control$ID,data_treatment$Ishan)

b3 = (ggplot(dataset)
  +aes(Treatment2, ID,fill=Treatment)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none")
  +xlab("Treatment"))
b3


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
      stat_summary(data=data_control,
                   aes(x=Day,
                       y=ID,
                       linetype="Control"),
                   fun = mean,
                   geom="point",
                   group=1)+
    stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=ID,
                     linetype="Treatment"),
                 fun = mean,
                 geom="line",
                 group=1)+
      stat_summary(data=data_treatment,
                   aes(x=Day,
                       y=ID),
                   fun = mean,
                   geom="point",
                   group=1)+
    theme_classic()+
    scale_linetype_manual("Sample",
                          values=c("Treatment"=2,
                                   "Control"=1)))
l3

# IE data visualization ---------------------------------------------------

summary(dataset$IE)

hist4=ggplot(dataset)+
  aes(IE)+
  geom_histogram(fill="darkgray",
                 color="black")+
  theme_classic()
hist4

# Box plot
wilcox.test(data_control$Ishan,data_treatment$Ishan)

b4 = (ggplot(dataset)
  +aes(Treatment2, IE,fill=Treatment2)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none")
  +xlab("Treatment"))
b4

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
      stat_summary(data=data_control,
                   aes(x=Day,
                       y=IE),
                   fun = mean,
                   geom="point",
                   group=1)+
      stat_summary(data=data_treatment,
                 aes(x=Day,
                     y=Ishan,
                     linetype="Treatment"),
                 fun = mean,
                 geom="line",
                 group=1)+
      stat_summary(data=data_treatment,
                   aes(x=Day,
                       y=Ishan),
                   fun = mean,
                   geom="point",
                   group=1)+
                  theme_classic()+
    scale_linetype_manual("Sample",
                          values=c("Treatment"=2,
                                   "Control"=1)))

l4
# Overview ----------------------------------------------------------------

#histogram
h = plot_grid(hist1,hist2,hist3,hist4, labels = "AUTO")
h
ggsave(filename = "graphics/histogram_overview.png",
       plot = h,
       device = "png",
       width = 10, height = 8,units = "in")

#box plot

b = plot_grid(b1,b2,b3,b4, labels = "AUTO")
b
ggsave(filename = "graphics/boxplot_overview.png",
       plot = b,
       device = "png",
       width = 10, height = 8,units = "in")


#lines
l1 = l1+theme(legend.position = "n")
l2 = l2+theme(legend.position = "n")
l3 = l3+theme(legend.position = "n")

l = plot_grid(l1,l2,l3,l4, labels = "AUTO")
l
ggsave(filename = "graphics/lines_overview.png",
       plot = l,
       device = "png",
       width = 13, height = 10,units = "in")

# Beta regression --------------------------------------------------------

#backup: "dataset2"
summary(dataset)
dataset2=dataset

#adjust for beta regression: transformation to scale 0.1
# to 0.9 to avoid problems at the extremes

dataset2$Ishan=0.1+(0.9-0.1)*(dataset2$Ishan-min(dataset2$Ishan))/(max(dataset2$Ishan)-min(dataset2$Ishan))

# effect of transformation on Ishan
summary(dataset2$Ishan)
png("graphics/hist_Ishan_trans_vs_nonTrans.png",
    width = 12,height = 6, res = 600,units = "in")
par(mfrow = c(1,2))
hist(dataset2$Ishan,
     breaks = sqrt(length(dataset2$Ishan)),
     main = "",xlab = "Transformed")
hist(dataset$Ishan,
     breaks = sqrt(length(dataset2$Ishan)),
     main = "", xlab = "Non transformed")
dev.off()
png("graphics/lines_trans_vs_nonTrans.png",
    width = 7,height = 4, res = 600,units = "in")
par(mfrow = c(1,1))
plot(dataset$Ishan,
     type = "l",
     bty = "l",
     ylim = c(0,2),
     ylab = "Ishan", xlab = "Observation")
lines(dataset2$Ishan, col = 2)
abline(h = c(.1,.9),lty = 2)
legend(5,2.20,
       legend = c("Non transformed","Transformed"),
       bty = "n",ncol = 2,lty = 1, col =c(1,2))
dev.off()

# Model 1 - Isham ------------------------------------------------------------

model_ishan=betareg(Ishan~Day*Treatment,link="logit",data=dataset2)
confint(model_ishan)

#model without iterations
model_ishan2 = betareg(Ishan~Day+Treatment,link="logit",data=dataset2)
confint(model_ishan2)


#Table of results with iterations
tab_ishan=summary(model_ishan)
xtable(tab_ishan$coefficients$mean,digits=5)


#Table of results without iterations
tab_ishan=summary(model_ishan2)
xtable(tab_ishan$coefficients$mean,digits=5)

# Residuals analysis
r1=residuals(model_ishan,type="deviance")
r2=residuals(model_ishan,type="sweighted2")
r3=residuals(model_ishan,type="pearson")

#graphic of residuals analysis
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+
    theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Deviance residuals"))

r=(ggplot(dataset2)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Index",y="Standard residuals (type 2)"))
r
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

pred_obs=(ggplot(dataset2)+geom_point(aes(x=model_ishan$y,y=predict(model_ishan)))
          +theme_classic()
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observed",y="y predicted"))
pred_obs

# Half-Normal Plots with Simulation Envelopes


res=hnp(r1)

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

#Others residuals
plot(model_ishan)

# Model 2 - Isimp  --------------------------------------------------------

model_isimp=betareg(Isimp~Day*Treatment,link="logit",data=dataset2)
confint(model_isimp)

#table of results
tab_isimp=summary(model_isimp)
tab_isimp$coefficients$mean
xtable(tab_isimp$coefficients$mean,digits=5)

# Residuals analysis
r1=residuals(model_isimp,type="deviance")
r2=residuals(model_isimp,type="sweighted2")
r3=residuals(model_isimp,type="pearson")

# Graphic of residuals analysis
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Deviance residuals"))

r=(ggplot(dataset2)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Standard residuals (type 2)"))

(ggplot(dataset2)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

pred_obs = (
  ggplot(dataset2) + geom_point(aes(
    x = model_isimp$y, y = predict(model_isimp)
  ))
  + theme_classic()
  + geom_abline(intercept = 0, slope = 1)
  + labs(x = "y observados", y = "y preditos")
)
pred_obs


# Half-Normal Plots with Simulation Envelopes
res=hnp(r1)

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

#Others residuals
plot(model_ishan)


# Model 3 - Ie ------------------------------------------------------------

model_id=betareg(ID~Day*Treatment,link="logit",data=dataset2)
confint(model_id)

#table of results
tab_id=summary(model_id)
tab_id$coefficients$mean
xtable(tab_id$coefficients$mean,digits=5)

# Residuals analysis
r1=residuals(model_id,type="deviance")
r2=residuals(model_id,type="sweighted2")
r3=residuals(model_id,type="pearson")

# Graphic of residuals analysis
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Deviance residuals"))

r=(ggplot(dataset2)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Index",y="Standard residuals (type 2)"))

(ggplot(dataset2)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

pred_obs = (
  ggplot(dataset2) + geom_point(aes(
    x = model_id$y, y = predict(model_id)
  ))
  + theme_classic()
  + geom_abline(intercept = 0, slope = 1)
  + labs(x = "y observados", y = "y preditos")
)
pred_obs


# Half-Normal Plots with Simulation Envelopes
res=hnp(r1)

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

#Others residuals
plot(model_id)

# Model 4 - IE ------------------------------------------------------------

model_ie=betareg(IE~Day*Treatment,link="logit",data=dataset2)
confint(model_ie)

#table of results
tab_ie=summary(model_ie)
tab_ie$coefficients$mean
xtable(tab_ie$coefficients$mean,digits=5)

# Residuals analysis
r1=residuals(model_ie,type="deviance")
r2=residuals(model_ie,type="sweighted2")
r3=residuals(model_ie,type="pearson")

# Graphic of residuals analysis
(ggplot(dataset2)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Deviance residuals"))

r=(ggplot(dataset2)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Index",y="Standard residuals (type 2)"))

(ggplot(dataset2)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

pred_obs = (
  ggplot(dataset2) + geom_point(aes(
    x = model_ie$y, y = predict(model_ie)
  ))
  + theme_classic()
  + geom_abline(intercept = 0, slope = 1)
  + labs(x = "y observados", y = "y preditos")
)
pred_obs

# Half-Normal Plots with Simulation Envelopes
res=hnp(r1)

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

#Others residuals
plot(model_ie)



