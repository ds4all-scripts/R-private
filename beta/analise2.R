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
# Photocatalysis data set  ------------------------------------------------

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

#Table of results
tab_ishan=summary(model_ishan)
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


# Model 3 - ID ------------------------------------------------------------

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



# Photocatalysis data set  ------------------------------------------------
setwd("D:/Documents/Projects/R-private/beta")
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

# Ishan data visualization ------------------------------------------------


dataset3 = data2 #Bkp
data_control=dataset3[dataset3$Treatment=="No",]
data_treatment=dataset3[dataset3$Treatment!="No",]


summary(dataset3$Ishan)
H1 = ggplot(dataset3)+
  aes(Ishan)+
  geom_histogram(fill="darkgray",color="black")+
  theme_cowplot()
H1

# Box plot

wilcox.test(data_treatment$Ishan,data_control$Ishan)

B1 = (ggplot(dataset3)+
    aes(Treatment, Ishan)+
    geom_boxplot(fill="white",color="black")+
    theme_classic()+
    xlab("Treatment")+
    stat_summary(fun=mean,
                 geom="point",
                 shape= 8,
                 size=2))

B1
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
  geom_histogram(fill="darkgray",color="black")+
  theme_cowplot()
H2
summary(dataset3$IE)

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


# Overview ----------------------------------------------------------------

#histogram
H = plot_grid(H1,H2, labels = "AUTO")
H
ggsave(filename = "graphics/Histogram_overview_H202.png",
       plot = H,
       device = "png",
       width = 10, height = 8,units = "in")

#box plot

B = plot_grid(B1,B2, labels = "AUTO")
B
ggsave(filename = "graphics/Boxplot_overview_h2o2.png",
       plot = B,
       device = "png",
       width = 10, height = 8,units = "in")


#lines
L1 = L1+theme(legend.position = "n")
L2

L = plot_grid(L1,L2, labels = "AUTO")
L
ggsave(filename = "graphics/Lines_overview_h2O2.png",
       plot = L,
       device = "png",
       width = 13, height = 10,units = "in")

# Beta regression (H2O2) --------------------------------------------------------

#backup: "dataset4"

dataset4=dataset3

#adjust for beta regression: transformation to scale 0.1
# to 0.9 to avoid problems at the extremes

dataset4$Ishan=0.1+(0.9-0.1)*(dataset3$Ishan-min(dataset3$Ishan))/(max(dataset3$Ishan)-min(dataset3$Ishan))

# effect of transformation on Ishan
summary(dataset4$Ishan)
png("graphics/hist_Ishan_trans_vs_nonTrans_h202.png",
    width = 12,height = 6, res = 600,units = "in")
par(mfrow = c(1,2))
hist(dataset4$Ishan,
     breaks = sqrt(length(dataset2$Ishan)),
     main = "",xlab = "Transformed")
hist(dataset3$Ishan,
     breaks = sqrt(length(dataset2$Ishan)),
     main = "", xlab = "Non transformed")
dev.off()
png("graphics/lines_trans_vs_nonTrans_h202.png",
    width = 7,height = 4, res = 600,units = "in")
par(mfrow = c(1,1))
plot(dataset4$Ishan,
     type = "l",
     bty = "l",
     ylim = c(0,6),
     ylab = "Ishan", xlab = "Observation")
lines(dataset3$Ishan, col = 2)
abline(h = c(.1,.9),lty = 2)
legend(5,6.20,
       legend = c("Non transformed","Transformed"),
       bty = "n",ncol = 2,lty = 1, col =c(1,2))
dev.off()

# Model 5 - Isham (H2O2) ------------------------------------------------------------

model_ishan=betareg(Ishan~Day*Treatment,link="logit",
                    data=dataset4)
confint(model_ishan)

#Table of results
tab_ishan=summary(model_ishan)
xtable(tab_ishan$coefficients$mean,digits=5)


# Residuals analysis
r1=residuals(model_ishan,type="deviance")
r2=residuals(model_ishan,type="sweighted2")
r3=residuals(model_ishan,type="pearson")

#graphic of residuals analysis
(ggplot(dataset4)+geom_point(aes(x=1:24,y=r1))+ylim(-3,3)+
    theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Deviance residuals"))

r=(ggplot(dataset4)+geom_point(aes(x=1:24,y=r2))+ylim(-3,3)+theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Index",y="Standard residuals (type 2)"))
r
(ggplot(dataset4)+geom_point(aes(x=1:24,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

pred_obs=(ggplot(dataset4)+geom_point(aes(x=model_ishan$y,y=predict(model_ishan)))
          +theme_classic()
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observed",y="y predicted"))
pred_obs

# Half-Normal Plots with Simulation Envelopes

#porque r2 ??? Não seria melhor escolher r1 ou r3?
res=hnp(r3)

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

# Model 6 - IE (H2O2) ------------------------------------------------------------

model_ie=betareg(IE~Day*Treatment,link="logit",
                    data=dataset4)
confint(model_ie)

#Table of results
tab_ie=summary(model_ie)
xtable(tab_ie$coefficients$mean,digits=5)


# Residuals analysis
r1=residuals(model_ie,type="deviance")
r2=residuals(model_ie,type="sweighted2")
r3=residuals(model_ie,type="pearson")

#graphic of residuals analysis
(ggplot(dataset4)+geom_point(aes(x=1:24,y=r1))+ylim(-3,3)+
    theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Deviance residuals"))

r=(ggplot(dataset4)+geom_point(aes(x=1:24,y=r2))+ylim(-3,3)+theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Index",y="Standard residuals (type 2)"))
r
(ggplot(dataset4)+geom_point(aes(x=1:24,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Index",y="Pearson residuals"))

pred_obs=(ggplot(dataset4)+geom_point(aes(x=model_ie$y,y=predict(model_ie)))
          +theme_classic()
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observed",y="y predicted"))
pred_obs

# Half-Normal Plots with Simulation Envelopes

#porque r2 ??? Não seria melhor escolher r1 ou r3?
res=hnp(r3)

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

