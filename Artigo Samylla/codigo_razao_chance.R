dados=read.csv(file.choose())# indicar o camanho do banco de dados 
#Pré-processamento
colnames(dados)=c("Species","PAC","Gradient","Time","Yes","No")
dados$Species=ifelse(dados$Species=="Dolicho","D. circinale","M. aeruginosa")
dados$Species=factor(dados$Species,levels=c("D. circinale","M. aeruginosa"))
dados$PAC=ifelse(dados$PAC=="Sim","Yes","No")
dados$PAC=factor(dados$PAC,levels=c("No","Yes"))
dados$Gradient=factor(dados$Gradient,levels=c("G1","G2","G3"))
dados$Time=factor(dados$Time,levels=c("0s","10s","15s","30s","45s","60s"))
head(dados)
dados.alterado = dados
relevel(dados.alterado$Species,ref = "D. circinale")
#Verificando as referências
unique(dados$Species) # Confirmação dos fatores 
unique(dados$PAC)
unique(dados$Time)
unique(dados$Gradient)

str(dados)  # Confirmar se todas as variáveis são fatores e seus respectivos níveis
modelo=glm(as.matrix(dados[,c(5,6)])~Species+Time+Gradient+Species*PAC*Gradient+PAC*Time*Gradient,
             data=dados,family=binomial(link = "logit"))           

dados2 = rbind(dados[1:36,],dados[55:72,], dados[37:54,])
summary(dados2)
(modelo2=glm(as.matrix(dados[,c(5,6)])~Species+Time+Gradient+Species*PAC*Gradient+PAC*Time*Gradient,
           data=dados2,family=binomial(link = "logit")))           
summary(modelo)

#Tabela anodev
anova(modelo,test="Chisq")
modelo_nulo=glm(as.matrix(dados[,c(5,6)])~1,family=binomial(link="logit"),data=dados)
AIC(modelo)
step(modelo_nulo,~Species+Time+Gradient+Species*PAC*Gradient+PAC*Time*Gradient,direction = "both")

#Razão de chance e intervalos de confiaça 
# O Tempo
summary(modelo)
razao_chance=t(t(exp(c(modelo$coefficients[3],modelo$coefficients[4],modelo$coefficients[5],modelo$coefficients[6],modelo$coefficients[7]))))
razao_chance
#Intervalo de confiança para razão de chance
intervalo_razoes_chance=exp(confint(modelo,level=0.95))[3:7,]
intervalo_razoes_chance
matriz_razao_chance=cbind.data.frame(c("10 s","15 s","30 s","45 s","60 s"),razao_chance,intervalo_razoes_chance)
colnames(matriz_razao_chance)=c("Times","OR","Lower limit","Upper limit")
matriz_razao_chance

#Gradient 

#Razão de chance
razao_chance= t(t(exp(modelo$coefficients[c(8:9)])))
razao_chance
#Intervalo de confiança para razão de chance
intervalo_razoes_chance=exp(confint(modelo,level=0.95))[8:9,]
intervalo_razoes_chance
matriz_razao_chance2=cbind.data.frame(razao_chance,intervalo_razoes_chance);matriz_razao_chance2



#Espécie
#Razão de chance
razao_chance=t(t(exp(modelo$coefficients[2])))
razao_chance
#Intervalo
intervalo_razoes_chance=exp(confint(modelo,level=0.95))[2,]
intervalo_razoes_chance
matriz_razao_chance3=c(razao_chance,intervalo_razoes_chance);matriz_razao_chance3
dados[dados$Gradient != "G1" & dados$Species != "D. circinale", ]
#Chances
matriz_x=model.matrix(modelo)
vetor_chances=exp(matriz_x%*%modelo$coefficients)
vetor_chances
write.csv(matriz_x,file="matrix_x.csv",row.names = F)

require(ggplot2)

#Sem carvão e Specie D.c
a=data.frame(vetor_chances[19:36,],c(rep("G1",6),rep("G2",6),rep("G3",6)),rep(c("0 s","10 s","15 s","30 s","45 s","60 s"),3))
colnames(a)=c("x","Gradient","Time")
a
str(a)
cc=(ggplot(a)+aes(rep(1:6,3),x,color=Gradient)+geom_point()+geom_line()+theme_classic()+
  labs(x="0 s          10 s          15 s          30 s          45 s          60 s"))
vetor_chances[3]/vetor_chances[1]
class(matriz_x)

#Com carvão e Specie D.c
a=data.frame(vetor_chances[1:18,],c(rep("G1",6),rep("G2",6),rep("G3",6)),rep(c("0 s","10 s","15 s","30 s","45 s","60 s"),3))
colnames(a)=c("x","Gradient","Time")
a
str(a)
sc=(ggplot(a)+aes(rep(1:6,3),x,color=Gradient)+geom_point()+geom_line()+theme_classic()+
    labs(x="0 s          10 s          15 s          30 s          45 s          60 s"))
vetor_chances
class(matriz_x)
require(cowplot)
plot_grid(sc,cc,ncol=1)
###Resolvendo a matriz das chances, divindindo as chances em diferentes condições (interações).
matriz_chances_dif=matrix(NA,nrow=12,ncol=5)
matriz_chances=matrix(vetor_chances,ncol=6,byrow=T);matriz_chances
for(i in 1:12)
{
for(j in 1:5)
{
matriz_chances_dif[i,j]=matriz_chances[i,j+1]/matriz_chances[i,1]
}
}
matriz_chances_dif
#As três primeiras linhas alteram os Gradient's na presença de PAC e a espécie d.c e 
#alterando os tempos de 10s a 60s.


#Estimativas das razões de chance inventadas
log_chance=matriz_x[,-1]%*%modelo$coefficients[-1]
modelo$coefficients[5]+modelo$coefficients[17]
matriz_cov=matriz_x[,-1]%*%vcov(modelo)[-1,-1]%*%t(matriz_x[,-1])

for(i in 1:72)
{
  print(exp(c(log_chance[i]-qnorm(0.975)*sqrt(matriz_cov[i,i]),log_chance[i]+qnorm(0.975)*sqrt(matriz_cov[i,i]))))
}

log_chance[2]
exp(c(modelo$coefficients[5]+modelo$coefficients[16]-qnorm(0.975)*sqrt(matriz_cov[2,2]),modelo$coefficients[5]+modelo$coefficients[16]+modelo$coefficients[10]+qnorm(0.975)*sqrt(matriz_cov[2,2])))
exp(c(modelo$coefficients[6]+modelo$coefficients[17]-qnorm(0.975)*sqrt(matriz_cov[3,3]),modelo$coefficients[5]+modelo$coefficients[16]+modelo$coefficients[10]+qnorm(0.975)*sqrt(matriz_cov[2,2])))
matriz_chances_dif

#Ajeitar a matriz x para fazer as razões de chances alteradas a situações diferentes.
posicoes=seq(1,73,6);posicoes
matriz_salvadora=matrix(NA,nrow(matriz_x),ncol(matriz_x));matriz_salvadora
dados
for(i in 1:length(posicoes))
{
  if(i!=length(posicoes))
  {
  for(j in 1:ncol(matriz_x))
  {
    w=posicoes[i]
    for(k in seq(posicoes[i],(posicoes[i+1])-1))
    {
      if(k!=w)
          {
          if(matriz_x[w,j]==1 && matriz_x[k,j]==1)
             {
               matriz_salvadora[k,j]=0
          }
          else
          {
            matriz_salvadora[k,j]=matriz_x[k,j]
          }
      }
      else
        {
        matriz_salvadora[k,j]=matriz_x[k,j]
        }
    }
    
  }
}
}
matriz_salvadora
#Todos estão preenchidos
sum(is.na(matriz_salvadora))

#Razões de chance inventadas
matriz_coef_inventados=matriz_salvadora%*%modelo$coefficients
exp(matriz_coef_inventados)

razoes_invetadadas=exp(matriz_coef_inventados);razoes_invetadadas
#Intervalos das razões de chance inventadas
cov_coef_inventada=matriz_salvadora%*%vcov(modelo)%*%t(matriz_salvadora)

intervalo_razoes_inventadas=data.frame();intervalo_razoes_inventadas
for(i in 1:length(matriz_coef_inventados))
{
 b=(exp(c(matriz_coef_inventados[i]-qnorm(0.975)*sqrt(cov_coef_inventada[i,i]),matriz_coef_inventados[i]+qnorm(0.975)*sqrt(cov_coef_inventada[i,i])))) 
 intervalo_razoes_inventadas=rbind(intervalo_razoes_inventadas,b)
}
intervalo_razoes_inventadas
matriz_samylla=cbind(exp(matriz_coef_inventados),intervalo_razoes_inventadas)
colnames(matriz_samylla)=c("Razões inventadas","Limite Inferior","Limite Superior")
matriz_samylla

#Gráficos de razões inventadas
a=data.frame(c(matriz_chances_dif[1,],matriz_chances_dif[2,],matriz_chances_dif[3,],matriz_chances_dif[4,],
               matriz_chances_dif[5,],matriz_chances_dif[6,],matriz_chances_dif[7,],matriz_chances_dif[8,]
               ,matriz_chances_dif[9,],matriz_chances_dif[10,],matriz_chances_dif[11,],matriz_chances_dif[12,]),
             rep(rep(c("G1","G2","G3"),c(5,5,5)),4))
a
colnames(a)=c("razoes","Gradient")
a
b=ggplot(a[1:15,])+aes(rep(1:5,3),razoes,color=Gradient)+geom_point()+geom_line();b
c=ggplot(a[16:30,])+aes(rep(1:5,3),razoes,color=Gradient)+geom_point()+geom_line();c
d=ggplot(a[31:45,])+aes(rep(1:5,3),razoes,color=Gradient)+geom_point()+geom_line();d
e=ggplot(a[46:60,])+aes(rep(1:5,3),razoes,color=Gradient)+geom_point()+geom_line();e
plot_grid(b,c,d,e,ncol=2)
matriz_chances_dif
View(matriz_x)

View(matriz_salvadora)



df.mtz.samylla = as.data.frame(cbind(dados[,c(1:(NCOL(dados)-2))],matriz_samylla))
Gf.mtz.samylla = df.mtz.samylla[df.mtz.samylla$Time !="0s",]

matriz_x2 = model.matrix(modelo2)
matriz_salvadora2 = matrix(data = NA , nrow = 36, ncol = NCOL(matriz_x))
for (i in 1: nrow(matriz_x)/4) 
  {
    for (j in 1:ncol(matriz_x)) 
      {
       matriz_salvadora2[i,j] =    ifelse(matriz_x2[i,j]==1    &    matriz_x2[i+18,j]==1,0,matriz_x2[i,j] ) 
       matriz_salvadora2[i+18,j] = ifelse(matriz_x2[i+36,j]==1 &    matriz_x2[i+54,j]==1,0,matriz_x2[i+36,j] ) 
      }
  }

apply(matriz_salvadora2,2, sum)
View(matriz_salvadora2)
matriz_coe_2 = matriz_salvadora2%*%modelo2$coefficients
matriz_cov_2 = matriz_salvadora2%*%vcov(modelo2)%*%t(matriz_salvadora2)


data.PAC = data.frame("Odds_PAC" = exp(matriz_coe_2),
                      "lower" = exp(matriz_coe_2 - qnorm(1-.001/2)*diag(matriz_cov_2)),
                      "Upper" = exp(matriz_coe_2 + qnorm(1-.001/2)*diag(matriz_cov_2)),
                      "Species"  = as.factor(rep(c("D. CIRCINALE","M. AERUGINOSA"), c(18,18))),
                      "Gradient" = as.factor(rep(rep(c("G1","G2", "G3"),c(6,6,6)),2)),
                      "Time"     = as.factor(rep(c("0 s", "10 s", "15 s", "30 s", "45 s", "60 s"),6)));data.PAC
data.PAC.2 = data.PAC[data.PAC$Time !="0 s",];data.PAC.2


 
require(ggplot2)
require(cowplot) 

 
data.PAC.dc = data.PAC[data.PAC$Species == "D. CIRCINALE" & data.PAC$Time != "0 s" & data.PAC$Gradient !="G2" &data.PAC$Gradient !="G3",]
data.PAC.Rr = data.PAC[data.PAC$Species != "D. CIRCINALE" & data.PAC$Time != "0 s" & data.PAC$Gradient !="G2" &data.PAC$Gradient !="G3",]
G1.PAC.dc=  ggplot(data = data.PAC.dc,
        aes(x = data.PAC.dc$Time, 
            y = data.PAC.dc$Odds_PAC,
            color=data.PAC.dc$Gradient))+# ylim(0,9)+
   geom_errorbar(aes(ymax = data.PAC.dc$lower,
                     ymin = data.PAC.dc$Upper), size = .25, color = "gray50")+
   geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed")+ 
   geom_point(size = 3.5) +
   theme(legend.position="none")+
   scale_color_manual(values=c("black", "gray35", "gray70"),
                      name="Interval of velocity gradient :",
                      labels=c("G1", "G2", "G3"))+ facet_grid(Gradient~Species, scales = "free")+
   
   ylab("Odds ratio") + ylim(0,5.7)+
  xlab("");G1.PAC.dc
 

 
 G1.PAC.Rr =  ggplot(data = data.PAC.Rr,
        aes(x = data.PAC.Rr$Time, 
            y = data.PAC.Rr$Odds_PAC,
            color=data.PAC.Rr$Gradient))+# ylim(0,9)+
   geom_errorbar(aes(ymax = data.PAC.Rr$lower,
                     ymin = data.PAC.Rr$Upper), size = .25, color = "gray50")+
   geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed")+ 
   geom_point(size = 3.5) +
   theme(legend.position="none")+
   scale_color_manual(values=c("black", "gray35", "gray70"))+ facet_grid(Gradient~Species, scales = "free")+
   
   ylab("") +  ylim(0,5.7)+
   xlab("");G1.PAC.Rr
 
PAC.Geral.g1 = plot_grid(G1.PAC.dc, G1.PAC.Rr, nrow = 1) 
PAC.Geral.g1

data.PAC.dc = data.PAC[data.PAC$Species == "D. CIRCINALE" & data.PAC$Time != "0 s" & data.PAC$Gradient !="G1" &data.PAC$Gradient !="G3",]
data.PAC.Rr = data.PAC[data.PAC$Species != "D. CIRCINALE" & data.PAC$Time != "0 s" & data.PAC$Gradient !="G1" &data.PAC$Gradient !="G3",]
G2.PAC.dc=   ggplot(data = data.PAC.dc,
                   aes(x = data.PAC.dc$Time, 
                       y = data.PAC.dc$Odds_PAC,
                       color=data.PAC.dc$Gradient))+# ylim(0,9)+
  geom_errorbar(aes(ymax = data.PAC.dc$lower,
                    ymin = data.PAC.dc$Upper), size = .25, color = "gray50")+
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed")+ 
  geom_point(size = 3.5) +
  theme(legend.position="none")+
  scale_color_manual(values=c("black", "gray35", "gray70"),
                     name="Interval of velocity gradient :",
                     labels=c("G1", "G2", "G3"))+ facet_grid(Gradient~Species, scales = "free")+
  
  ylab("Odds ratio") + ylim(0,8)+
  xlab("");G2.PAC.dc



G2.PAC.Rr =  ggplot(data = data.PAC.Rr,
                    aes(x = data.PAC.Rr$Time, 
                        y = data.PAC.Rr$Odds_PAC,
                        color=data.PAC.Rr$Gradient))+# ylim(0,9)+
  geom_errorbar(aes(ymax = data.PAC.Rr$lower,
                    ymin = data.PAC.Rr$Upper), size = .25, color = "gray50")+
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed")+ 
  geom_point(size = 3.5) +
  theme(legend.position="none")+
  scale_color_manual(values=c("black", "gray35", "gray70"))+ facet_grid(Gradient~Species, scales = "free")+
  
  ylab("") +  ylim(0,8)+
  xlab("");G2.PAC.Rr

PAC.Geral.g2 = plot_grid(G2.PAC.dc, G2.PAC.Rr, nrow = 1) 
PAC.Geral.g2


data.PAC.dc = data.PAC[data.PAC$Species == "D. CIRCINALE" & data.PAC$Time != "0 s" & data.PAC$Gradient !="G2" &data.PAC$Gradient !="G1",]
data.PAC.Rr = data.PAC[data.PAC$Species != "D. CIRCINALE" & data.PAC$Time != "0 s" & data.PAC$Gradient !="G2" &data.PAC$Gradient !="G1",]
G3.PAC.dc=   ggplot(data = data.PAC.dc,
                    aes(x = data.PAC.dc$Time, 
                        y = data.PAC.dc$Odds_PAC,
                        color=data.PAC.dc$Gradient))+# ylim(0,9)+
  geom_errorbar(aes(ymax = data.PAC.dc$lower,
                    ymin = data.PAC.dc$Upper), size = .25, color = "gray50")+
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed")+ 
  geom_point(size = 3.5) +
  theme(legend.position="none")+
  scale_color_manual(values=c("black", "gray35", "gray70"),
                     name="Interval of velocity gradient :",
                     labels=c("G1", "G2", "G3"))+ facet_grid(Gradient~Species, scales = "free")+
  
  ylab("Odds ratio") + ylim(0,1.5)+
  xlab("");G3.PAC.dc



G3.PAC.Rr =  ggplot(data = data.PAC.Rr,
                    aes(x = data.PAC.Rr$Time, 
                        y = data.PAC.Rr$Odds_PAC,
                        color=data.PAC.Rr$Gradient))+# ylim(0,9)+
  geom_errorbar(aes(ymax = data.PAC.Rr$lower,
                    ymin = data.PAC.Rr$Upper), size = .25, color = "gray50")+
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed")+ 
  geom_point(size = 3.5) +
  theme(legend.position="none")+
  scale_color_manual(values=c("black", "gray35", "gray70"))+ facet_grid(Gradient~Species, scales = "free")+
  
  ylab("") +  ylim(0,1.5)+
  xlab("");G3.PAC.Rr

PAC.Geral.g3 = plot_grid(G3.PAC.dc, G3.PAC.Rr, nrow = 1) 
PAC.Geral.g3

PAC.Geral.TOTAL = plot_grid(PAC.Geral.g1, PAC.Geral.g2, PAC.Geral.g3, ncol = 1)
PAC.Geral.TOTAL

