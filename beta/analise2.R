require(ggplot2)
require(gridExtra)
require(betareg)
require(hnp)
require(xtable)

dados=read.csv(file.choose())
head(dados)
summary(dados)
dados$Tratamento=as.factor(ifelse(dados$Percentual==0,0,1))
dados$Outros=dados$Bacillariophyta+dados$Charophyta+dados$Chlorophyta+
  dados$Cryptophyta+dados$Euglenozoa+dados$Ochrophyta
dados$Tempo=as.factor(dados$Tempo)
dados$Amostra=as.factor(dados$Amostra)


dados2=cbind.data.frame(dados$Tempo, dados$Tratamento, dados$Amostra
                        ,dados$Outros, dados$Cyanobacteria, dados$Isimp., dados$Ishan., dados$ID
                        , dados$IE)
#dados2=dados2[-which(dados$Amostra==2 | dados$Amostra==5),]
colnames(dados2)=c("Tempo", "Tratamento", "Amostra", "Outros", "Cyanobacteria"
                   ,"Isimp", "Ishan", "ID", "IE")

dados2$Tratamento[1:3]=1
dados_controle=dados2[dados2$Tratamento==0,]
dados_tratamento=dados2[dados2$Tratamento==1,]


#####Variável Cyanobacteria

ggplot(dados2)+aes(Cyanobacteria)+geom_histogram(fill="white",color="black")+theme_classic()

summary(dados2$Cyanobacteria)

#Boxplot entre os tratamentos, realizar teste de medianas e médias.
(ggplot(dados2)
+aes(Tratamento, Cyanobacteria)+geom_boxplot(fill="white",color="black")
+theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2))

ggplot(dados2)+aes(Cyanobacteria)+geom_histogram(fill="white",color="black")+theme_classic()+facet_grid(~Tratamento)

wilcox.test(dados_controle$Cyanobacteria,dados_tratamento$Cyanobacteria)

#Linhas comparando as médias em relação ao tempo e tratamento
(ggplot()
  +stat_summary(data=dados_controle, aes(x=Tempo,y=Cyanobacteria, 
              linetype="Controle"), fun = mean, geom="line",group=1)
  +stat_summary(data=dados_tratamento, aes(x=Tempo,y=Cyanobacteria, 
              linetype="Tratamento"), fun = mean, geom="line",group=1)
  +theme_classic()
  +scale_linetype_manual("Tipo de tratamento", values=c("Tratamento"=2,"Controle"=1)))

#####Variável Outros

ggplot(dados2)+aes(Outros)+geom_histogram(fill="white",color="black")+theme_classic()
summary(dados2$Outros)

#Boxplot entre os tratamentos, realizar teste de medianas e médias.
(ggplot(dados2)
  +aes(Tratamento, Outros)+geom_boxplot(fill="white",color="black")
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2))

wilcox.test(dados_controle$Outros,dados_tratamento$Outros)
#Linhas comparando as médias em relação ao tempo e tratamento
(ggplot()
  +stat_summary(data=dados_controle, aes(x=Tempo,y=Outros, 
                     linetype="Controle"), fun = mean, geom="line",group=1)
  +stat_summary(data=dados_tratamento, aes(x=Tempo,y=Outros, 
                     linetype="Tratamento"), fun = mean, geom="line",group=1)
  +theme_classic()
  +scale_linetype_manual("Tipo de tratamento", values=c("Tratamento"=2,"Controle"=1)))

#####Variável Isimp

hist1=ggplot(dados2)+aes(Isimp)+geom_histogram(fill="white",color="black")+theme_classic()
hist1
summary(dados2$Isimp)

#Boxplot entre os tratamentos, realizar teste de medianas e médias.
(ggplot(dados2)
  +aes(Tratamento, Isimp,fill=Tratamento)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none"))

wilcox.test(dados_controle$Isimp,dados_tratamento$Isimp)

#Linhas comparando as médias em relação ao tempo e tratamento
(ggplot()
  +stat_summary(data=dados_controle, aes(x=Tempo,y=Isimp, 
                     linetype="Controle"), fun = mean, geom="line",group=1)
  +stat_summary(data=dados_tratamento, aes(x=Tempo,y=Isimp, 
                     linetype="Tratamento"), fun = mean, geom="line",group=1)
  +theme_classic()
  +scale_linetype_manual("Tipo de tratamento", values=c("Tratamento"=2,"Controle"=1)))

#####Variável Ishan

ggplot(dados2)+aes(Ishan)+geom_histogram(fill="white",color="black")+theme_classic()
summary(dados2$Ishan)

#Boxplot entre os tratamentos, realizar teste de medianas e médias.
(ggplot(dados2)
  +aes(Tratamento, Ishan,fill=Tratamento)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none"))

wilcox.test(dados_controle$Ishan,dados_tratamento$Ishan)
#Linhas comparando as médias em relação ao tempo e tratamento
(ggplot()
  +stat_summary(data=dados_controle, aes(x=Tempo,y=Ishan, 
                       linetype="Controle"), fun = mean, geom="line",group=1)
  +stat_summary(data=dados_tratamento, aes(x=Tempo,y=Ishan, 
                       linetype="Tratamento"), fun = mean, geom="line",group=1)
  +theme_classic()
  +scale_linetype_manual("Tipo de tratamento", values=c("Tratamento"=2,"Controle"=1)))

#####Variável ID

hist2=ggplot(dados2)+aes(ID)+geom_histogram(fill="white",color="black")+theme_classic()
hist2
summary(dados2$ID)

#Boxplot entre os tratamentos, realizar teste de medianas e médias.
(ggplot(dados2)
  +aes(Tratamento, ID,fill=Tratamento)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none"))

wilcox.test(dados_controle$ID,dados_tratamento$ID)
#Linhas comparando as médias em relação ao tempo e tratamento
(ggplot()
  +stat_summary(data=dados_controle, aes(x=Tempo,y=ID, 
                       linetype="Controle"), fun = mean, geom="line",group=1)
  +stat_summary(data=dados_tratamento, aes(x=Tempo,y=ID, 
                       linetype="Tratamento"), fun = mean, geom="line",group=1)
  +theme_classic()
  +scale_linetype_manual("Tipo de tratamento", values=c("Tratamento"=2,"Controle"=1)))

#####Variável IE

hist3=ggplot(dados2)+aes(IE)+geom_histogram(fill="white",color="black")+theme_classic()
hist3
summary(dados2$Cyanobacteria)

#Boxplot entre os tratamentos, realizar teste de medianas e médias.
(ggplot(dados2)
  +aes(Tratamento, IE, fill=Tratamento)+geom_boxplot()
  +theme_classic()+stat_summary(fun=mean, geom="point", shape= 8, size=2)
  +scale_fill_manual(values=c("white","gray"))
  +theme(legend.position="none"))

wilcox.test(dados_controle$IE, dados_tratamento$IE)
#Linhas comparando as médias em relação ao tempo e tratamento
(ggplot()
  +stat_summary(data=dados_controle, aes(x=Tempo,y=IE, 
                                         linetype="Controle"), fun = mean, geom="line",group=1)
  +stat_summary(data=dados_tratamento, aes(x=Tempo,y=IE, 
                                           linetype="Tratamento"), fun = mean, geom="line",group=1)
  +theme_classic()
  +scale_linetype_manual("Tipo de tratamento", values=c("Tratamento"=2,"Controle"=1)))





#Regressão Beta
summary(dados2)


dados3=dados2
dados3$Ishan=0.1+(0.9-0.1)*(dados3$Ishan-min(dados3$Ishan))/(max(dados3$Ishan)-min(dados3$Ishan))

#ìndice Ishan
summary(dados3$Ishan)
hist(dados3$Isimp)
modelo_ishan=betareg(Ishan~Tempo*Tratamento,link="logit",data=dados3)
coef_ishan=summary(modelo_ishan)
xtable(coef_ishan$coefficients$mean,digits=5)

confint(modelo_isimp)

r1=residuals(modelo_ishan,type="deviance")
r2=residuals(modelo_ishan,type="sweighted2")
r3=residuals(modelo_ishan,type="pearson")
#Gráfico dos resíduos
(ggplot(dados3)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Deviance"))

r=(ggplot(dados3)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
   +geom_hline(yintercept = c(-2,2),linetype="dashed")
   +labs(x="Índices",y="Resíduos Padronizado tipo 2"))
r
(ggplot(dados3)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Pearson"))

pred_obs=(ggplot(dados3)+geom_point(aes(x=modelo_ishan$y,y=predict(modelo_ishan)))
          +theme_classic()
          +geom_abline(intercept = 0, slope = 1)
          +labs(x="y observados",y="y preditos"))
pred_obs
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


#ìndice Isimp
summary(dados3$Isimp)
hist(dados3$Isimp)
modelo_isimp=betareg(Isimp~Tempo*Tratamento,link="logit",data=dados3)
model_isimp=summary(modelo_isimp)
model_isimp$coefficients$mean
xtable(model_isimp$coefficients$mean,digits=5)
confint(modelo_isimp)

r1=residuals(modelo_isimp,type="deviance")
r2=residuals(modelo_isimp,type="sweighted2")
r3=residuals(modelo_isimp,type="pearson")
#Gráfico dos resíduos
(ggplot(dados3)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
+geom_hline(yintercept = c(-2,2),linetype="dashed")
+labs(x="Índices",y="Resíduos Deviance"))

r=(ggplot(dados3)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Padronizado tipo 2"))

(ggplot(dados3)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Pearson"))

pred_obs=(ggplot(dados3)+geom_point(aes(x=modelo_isimp$y,y=predict(modelo_isimp)))
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

#ìndice ID
summary(dados3$ID)
hist(dados3$ID)
modelo_id=betareg(ID~Tempo*Tratamento,link="logit",data=dados3)
model_id=summary(modelo_id)
xtable(model_id$coefficients$mean,digits=5)

r1=residuals(modelo_id,type="deviance")
r2=residuals(modelo_id,type="sweighted2")
r3=residuals(modelo_id,type="pearson")
#Gráfico dos resíduos
(ggplot(dados3)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Deviance"))

r=(ggplot(dados3)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Padronizado tipo 2"))

(ggplot(dados3)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Pearson"))

pred_obs=(ggplot(dados3)+geom_point(aes(x=modelo_id$y,y=predict(modelo_id)))
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
#ìndice IE
summary(dados3$IE)
hist(dados3$IE)
modelo_ie=betareg(IE~Tempo*Tratamento,link="logit",data=dados3)
modelo_ie
model_ie=summary(modelo_ie)
xtable(model_ie$coefficients$mean,digits=5)

confint(modelo_ie)
r1=residuals(modelo_ie,type="deviance")
r2=residuals(modelo_ie,type="sweighted2")
r3=residuals(modelo_ie,type="pearson")
#Gráfico dos resíduos
(ggplot(dados3)+geom_point(aes(x=1:36,y=r1))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Deviance"))

r=(ggplot(dados3)+geom_point(aes(x=1:36,y=r2))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Padronizado tipo 2"))

(ggplot(dados3)+geom_point(aes(x=1:36,y=r3))+ylim(-3,3)+theme_classic()
  +geom_hline(yintercept = c(-2,2),linetype="dashed")
  +labs(x="Índices",y="Resíduos Pearson"))

pred_obs=(ggplot(dados3)+geom_point(aes(x=modelo_ie$y,y=predict(modelo_ie)))
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


#Juntando gráficos antes da modelagem
hist4=ggplot(dados3)+aes(Ishan)+geom_histogram(fill="white",color="black")+theme_classic()
hist4
grid.arrange(hist1,hist2,hist3,hist4,nrow=2)

write.csv(dados2, file="\\Users\\Alberto\\Desktop\\Trabalhos\\projetos\\artigo_allan_1\\dados2.csv",row.names=F)
?write.csv
