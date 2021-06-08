# Samylla Artigo 2 `Peróxido`

## Base de dados: Proporção da Mediana 


# início base proporção mediana -------------------------------------------



### 1 - Importar os dados dos Excel

library(readxl)
base <- read_excel("~/Samylla/base.xlsx", sheet = "Prop_Median")
class(base)

### 2 - Transformar o objeto `tiblle` em `dataframe`(Opcioanl)


b1=as.data.frame(base)

### 3 - Definir os tipos de variáveis### 3 - Definir os tipos de variáveis

str(b1)

#Variável Sample 
b1$Sample = as.factor(b1$Sample) #tipo Factor 
b1$Sample = factor(b1$Sample, levels = c("Raw Water", "Control","8 g/PDO" )) # Ordenar corretamente
relevel(b1$Sample,ref = "Raw Water") # Def. categoria de referência

#Variável Day ( substituir por Hours)

b1$Hours = base$Day
(b1 = b1[,c(1,6, 3:5)]) #obs.: Pode-se utilizar a função `colnames()`
b1$Hours = as.factor(b1$Hours) #tipo Factor 
levels(b1$Hours) = c("0 h", "168 h", "336 h", "504 h", "72 h",  "720 h") # Renomear os fatores
relevel(b1$Hours,ref = "0 h") # Def. categoria de referência

#Variável Species ( substituir por Phylum)

colnames(b1) = c("Sample",  "Hours",   "Phylum", "Yes",  "No")
b1$Phylum= as.factor(b1$Phylum) #tipo Factor 
levels(b1$Phylum) = c("Other", "Cyano")
relevel(b1$Phylum,ref = "Other") # Def. categoria de referência

# Variáveis "Yes" e "No"

b1$Yes =round(b1$Yes*100,0)
b1$No = 100-b1$Yes 
table(b1$Yes+b1$No) # Conferindo os dados!

str(b1) # Verificação final


### 4 - Início da análise

#### 4.1 Analisando o link `logit`

#### 4.1.1 Escolher o modelo por meio do Algoritmo Stepwise (baseado no AIC ) utilizando a função `step()`  

#Modelo Nulo

mod0 = glm(as.matrix(b1[,c(4,5)])~1, family=binomial(link="logit"), data=b1)

# Stepwise
attach(b1)
step(mod0,~Sample+Hours+Phylum + Sample*Hours*Phylum, data = b1, direction = c("backward"))
step(mod0,~Sample+Hours+Phylum + Sample*Hours*Phylum, data = b1, direction = c("forward"))
step(mod0,~Sample+Hours+Phylum + Sample*Hours*Phylum, data = b1, direction = c("both"))

#### 4.1.2 Testar os 2 melhores ajustes utilizando critérios estatísticos (AIC) e fenomenológicos


# modelo M1:  
m1 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours + 
           Sample:Hours + Phylum:Sample + Phylum:Hours + Phylum:Sample:Hours, 
         family = binomial(link = "logit"), data = b1)
anova(m1, test="Chisq")
summary(m1)

# Os coef de M1 não foram significativos

m2 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours + 
           Sample:Hours + Phylum:Sample + Phylum:Hours, 
         family = binomial(link = "logit"), data = b1)

anova(m2, test="Chisq")
summary(m2)

m3 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours + 
           + Phylum:Sample, 
         family = binomial(link = "logit"), data = b1)


anova(m3, test="Chisq")
summary(m3)

b1[b1$Sample !="Raw Water",  ]
m4 = glm(formula = as.matrix(b1[b1$Sample !="8 g/PDO", c(4, 5)]) ~ Phylum + Sample + Hours, 
         family = binomial(link = "logit"), data = b1[b1$Sample !="8 g/PDO",  ])


anova(m4, test="Chisq")
summary(m4)
Residuos(m4)


m5 = glm(formula = as.matrix(b1[, c(4, 5)]) ~Phylum+Sample+Hours+Phylum*Sample  , 
         family = binomial(link = "logit"), data = b1)
anova(m5, test="Chisq")
summary(m5)
Residuos(m5)


#### 4.1.3 Testar a qualidade dos resíduos

##### 4.1.3.1 - Estatística dos resíduos da deviance e de Pearson 

dev = residuals(m5, type='deviance')
QL = sum(dev^2)
p1 = 1-pchisq(QL,m5$df.residual)
cbind(QL,p1)


rpears<-residuals(m5, type='pearson')
rpears
QP<-sum(rpears^2)
p2<-1-pchisq(QP,(m5$df.null-m5$df.residual))
cbind(QP,p2)


##### 4.1.3.2 - Analise gráfica: (a) Resíduos da deviance, (b) Residos de Pearson e (c)envelope simulado


# Funçoes -----------------------------------------------------------------
#Função para criar o envelope siulado (Giolo, 2018)
envelope=function(modelo, Index_X = "(c)",ylad = "Deviance residuals" ){
  dados=na.omit(modelo$data)
  nsim=100
  n=modelo$df.null+1
  r1=sort(rstandard(modelo,type='deviance'))
  m1=matrix(0,nrow=n,ncol=nsim)
  a2=simulate(modelo,nsim=nsim)
  for (i in 1:nsim){
    dados$y=a2[,i]
    aj=update(modelo,y~.,data=dados)
    m1[,i]=sort(rstandard(aj,type='deviance'))}
  li=apply(m1,1,quantile,0.025)
  m=apply(m1,1,quantile,0.5)
  ls=apply(m1,1,quantile,0.975)
  quantis=qnorm((1:n-0.5)/n)
  plot(rep(quantis,2),c(li,ls),type='n',xlab=paste("N(0,1) quantiles\n",Index_X),
       ylab= ylad)
  
  title("")
  lines(quantis,li,type='l')
  lines(quantis,m,type='l',lty=2)
  lines(quantis,ls,type='l')
  points(quantis,r1,pch=16,cex=0.75)
}


Residuos = function( modelo){

installed.packages("hnp")
library(hnp)
par(mfrow =c(2,2))

#Estatística dos resíduos de Person
rpears = residuals(modelo, type='pearson')

QP<-sum(rpears^2)
p2<-1-pchisq(QP,modelo$df.residual)
Resulado_Pearson = cbind(QP,p2)

#Gráficos
plot(rpears, pch=16, 
     ylim=c(-3.5,3.5), #Limites máximos
     ylab="Pearson residuals", 
     xlab = "Observation")
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
      ylim=c(-3.5,3.5), #Limites máximos
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

# Fim Funções -------------------------------------------------------------


Residuos(m5) # Resíduos não atenderam os pré supostos


##### 4.2 Analisando outros links `probit`, `cloglog` e `cauchiy` no melhor modelo (M5)


m5 = glm(formula = as.matrix(b1[, c(4, 5)]) ~Phylum+Sample+Hours+Phylum*Sample  , 
         family = binomial(link = "probit"), data = b1)
anova(m5, test="Chisq")
summary(m5);Residuos(m5)

m5 = glm(formula = as.matrix(b1[, c(4, 5)]) ~Phylum+Sample+Hours+Phylum*Sample  , 
         family = binomial(link = "cloglog"), data = b1)
anova(m5, test="Chisq")
summary(m5);Residuos(m5)


m5 = glm(formula = as.matrix(b1[, c(4, 5)]) ~Phylum+Sample+Hours+Phylum*Sample  , 
         family = binomial(link = "cauchiy"), data = b1)
anova(m5, test="Chisq")
summary(m5);Residuos(m5)

## Conclusão: `base proporção mediana` não apresentau bons resultados, presseguir com o teste


# Fim  base proporção mediana ---------------------------------------------




## Base de dados: Mediana 

### 1 - Importar os dados dos Excel

library(readxl)

base <- read_excel("Samylla/base.xlsx", sheet = "Median")
View(base)

### 2 - Transformar o objeto `tiblle` em `dataframe`(Opcioanl)


b1=as.data.frame(base)

### 3 - Definir os tipos de variáveis### 3 - Definir os tipos de variáveis

str(b1)

#Variável Sample 
b1$Sample = as.factor(b1$Sample) #tipo Factor 
b1$Sample = factor(b1$Sample, levels = c("Raw Water", "Control","8 g/PDO" )) # Ordenar corretamente
relevel(b1$Sample,ref = "Raw Water") # Def. categoria de referência

#Variável Day ( substituir por Hours)

b1$Hours = base$Day
(b1 = b1[,c(1,6, 3:5)]) #obs.: Pode-se utilizar a função `colnames()`
b1$Hours = as.factor(b1$Hours) #tipo Factor 
levels(b1$Hours) = c("0", "168", "336", "504", "72",  "720") # Renomear os fatores
relevel(b1$Hours,ref = "0") # Def. categoria de referência
b1$Hours = as.numeric(as.character(b1$Hours))
#Variável Species ( substituir por Phylum)
str(b1)
colnames(b1) = c("Sample",  "Hours",   "Phylum", "Yes",  "No")
b1$Phylum= as.factor(b1$Phylum) #tipo Factor 
levels(b1$Phylum) = c("Other", "Cyano")
relevel(b1$Phylum,ref = "Other") # Def. categoria de referência

# Variáveis "Yes" e "No" ok!

str(b1) # Verificação final


### 4 - Início da análise

#### 4.1 Analisando o link `logit`

#### 4.1.1 Escolher o modelo por meio do Algoritmo Stepwise (baseado no AIC ) utilizando a função `step()`  

#Modelo Nulo

mod0 = glm(as.matrix(b1[,c(4,5)])~1, family=binomial(link="logit"), data=b1)

# Stepwise
attach(b1)
step(mod0,~Sample+Hours+Phylum + Sample*Hours*Phylum, data = b1, direction = c("backward"))
step(mod0,~Sample+Hours+Phylum + Sample*Hours*Phylum, data = b1, direction = c("forward"))
step(mod0,~Sample+Hours+Phylum + Sample*Hours*Phylum, data = b1, direction = c("both"))

#### 4.1.2 Testar os 2 melhores ajustes utilizando critérios estatísticos (AIC) e fenomenológicos


# modelo M1:  
m1 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours + Phylum*Sample*Hours, 
         family = binomial(link = "logit"), data = b1)
anova(m1, test="Chisq")
summary(m1);Residuos(m1)

# Os coef de M1 não foram significativos

m2 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours + 
           Sample:Hours + Phylum:Sample + Phylum:Hours, 
         family = binomial(link = "logit"), data = b1)

anova(m2, test="Chisq")
summary(m2);Residuos(m2)

m3 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours + 
           + Phylum:Sample, 
         family = binomial(link = "logit"), data = b1)


anova(m3, test="Chisq")
summary(m3);Residuos(m3)

m4 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours, 
         family = binomial(link = "logit"), data = b1)


anova(m4, test="Chisq")
summary(m4);Residuos(m4)


m5 = glm(formula = as.matrix(b1[, c(4, 5)]) ~Phylum+Sample+Hours+Phylum*Sample  , 
         family = binomial(link = "logit"), data = b1)
anova(m5, test="Chisq")
summary(m5);Residuos(m5)



##### 4.2 Analisando outros links `probit`, `cloglog` e `cauchiy` no melhor modelo (M5)


m2 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours , 
         family = binomial(link = "probit"), data = b1)

anova(m2, test="Chisq")
summary(m2);Residuos(m2)

m2 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours + 
           Sample:Hours + Phylum:Sample + Phylum:Hours, 
         family = binomial(link = "cloglog"), data = b1)

anova(m2, test="Chisq")
summary(m2);Residuos(m2)

m2 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + Hours, 
         family = binomial(link = "cauchit"), data = b1)

anova(m2, test="Chisq")
summary(m2);Residuos(m2)
## Conclusão: `base proporção mediana` não apresentau bons resultados, presseguir com o teste



b2 = b1[b1$Sample!= "Raw Water",]
b2$Sample = factor(b2$Sample, levels = c("Control","8 g/PDO"))
relevel(b2$Sample, ref = c("Control"))

m2 = glm(formula = as.matrix(b1[, c(4, 5)]) ~ Phylum + Sample + as.factor(Hours)+Phylum*Sample, 
         family = binomial(link = "logit"), data = b1)

anova(m2, test="Chisq")
summary(m2)
Residuos(m2)


m2 = glm(formula = as.matrix(b2[, c(4, 5)]) ~ Phylum + Sample+ Phylum:Sample, 
         family = binomial(link = "logit"), data = b2)

anova(m2, test="Chisq")
summary(m2)
Residuos(m2)


### Considerando cada tempo ----------------------------------------------



#### Tempo 0 - Inicio ----------------------------------------------------

colnames(b1) = c("Sample","Hours", "Phylum", "Yes", "No")
str(b1)
b1$Sample = factor(as.factor(b1$Sample),levels = c("Raw Water", "Control","8 g/PDO" ))
relevel(b1$Sample,ref = "Raw Water")

b1$Phylum = factor(as.factor(b1$Phylum),levels = c("Cyano","Algae"))
relevel(b1$Phylum,ref = "Cyano")


b0 = b1[b1$Hours == 0,-2 ]

#Modelo Nulo
mod0 = glm(as.matrix(b0[,c(3,4)])~1, family=binomial(link="logit"), data=b0)

mod0 = glm(as.matrix(b0[,c(3,4)])~1, family=binomial(link="logit"), data=b0)

# Stepwise
attach(b0)
step(mod0,~Sample+Phylum + Sample:Phylum , data = b0, direction = c("both"))

#### 4.1.2 Testar os melhores ajustes utilizando critérios estatísticos (AIC) e fenomenológicos

# modelo M1:  
# modelo M1:

mod0 = glm(as.matrix(b0[,c(3,4)])~1, family=binomial(link="logit"), data=b0)


m1 = glm(formula = as.matrix(b0[, c(3,4)]) ~ Phylum + Sample + Phylum:Sample,
         family = binomial(link = "logit"), data = b0)
anova(m1, test="Chisq")
summary(m1)
Residuos(m1)


m2 = glm(formula = as.matrix(b0[, c(3,4)]) ~ Phylum + Sample,
         family = binomial(link = "logit"), data = b0)
anova(m2, test="Chisq")
summary(m2)
Residuos(m2)



m3 = glm(formula = as.matrix(b0[, c(3,4)]) ~ Phylum,
         family = binomial(link = "logit"), data = b0)
anova(m3, test="Chisq")
summary(m3)
Residuos(m3)


m4 = glm(formula = as.matrix(b0[, c(3,4)]) ~ Sample,
         family = binomial(link = "logit"), data = b0)
anova(m4, test="Chisq")
summary(m4)
Residuos(m4)


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
format(T72$`Resid. Dev`, digits = 3, nsmall = 1)
format(T72$`Resid. Dev`, digits = 3, nsmall = 6) # small não influencia
Tab.ANODEV$`Resid. Dev` = formatC(Tab.ANODEV$`Resid. Dev`,format = "f", digits = 2)
Tab.ANODEV$`Pr(>Chi)` = formatC( Tab.ANODEV$`Pr(>Chi)`,format = "f", digits = 4)
Tab.ANODEV$AIC = formatC( Tab.ANODEV$AIC,format = "f", digits = 1);Tab.ANODEV
row.names(Tab.ANODEV) = c(1:nrow(Tab.ANODEV))
Tab.ANODEV$Model =  c( "NULL",
                       "Sample",
                       "Phylum",
                       "Sample+Phylum",
                       "Sample+Phylum+Phylum|Sample")
Tab.ANODEV.0 = Tab.ANODEV[,c(7,1:6)]



#odds
y = summary(m2)
Odds = data.frame("Tempo 0 h" = (model.matrix(m2))%*%as.matrix(y$coefficients[,1]))
row.names(Odds) = c("S1", "S2", "S3", "S4", "S5", "S6")

#Odds Ratio

( OR1 =   data.frame("Time"  = 0 ,
                      "Lower" = round(exp(coefficients(m2)[2]- 1.96*y$coefficients[2,2])*100,1),
                      "OR1" = round(exp(coefficients(m2)[1])*100,1),
                      "Upper" = round(exp(coefficients(m2)[1]+ 1.96*y$coefficients[2,2])*100,1)))

(OR2 =     data.frame("Time"  = 0,
                      "Lower" = round(exp(coefficients(m2)[3]- 1.96*y$coefficients[3,2])*100,1),
                      "OR2"   = round(exp(coefficients(m2)[3])*100,1), 
                      "Upper" = round(exp(coefficients(m2)[3]+ 1.96*y$coefficients[3,2])*100,1)))

(OR3 = data.frame("Time"  = 0,
                  "Lower" = round(exp(coefficients(m2)[4]/coefficients(m2)[3]- 
                                   1.96*y$coefficients[4,2]/y$coefficients[3,2])*100,1),
                  "OR3"   = round(exp(coefficients(m2)[4]/coefficients(m2)[3])*100,1), 
                  "Upper" = round(exp(coefficients(m2)[4]/coefficients(m2)[3]+ 
                                   1.96*y$coefficients[4,2]/y$coefficients[3,2])*100,1)))




#### Fim tempo 0 ---------------------------------------------------------

model.matrix(m1)


b72 = b1[b1$Day  == "72 hrs",-2]

colnames(b72) = c("Sample","Phylum", "Yes","No")
str(b72)
b72$Sample = factor(as.factor(b72$Sample),levels = c("Raw Water", "Control","8 g/PDO" ))
relevel(b72$Sample,ref = "Raw Water")

b72$Phylum = factor(as.factor(b72$Phylum),levels = c("Cyano","Algae"))
relevel(b72$Phylum,ref = "Cyano")

#Modelo Nulo

mod0 = glm(as.matrix(b72[,c(3,4)])~1, family=binomial(link="logit"), data=b72)


#### 4.1.2 Testar os melhores ajustes utilizando critérios estatísticos (AIC) e fenomenológicos

# modelo M1:  
m1 = glm(formula = as.matrix(b72[, c(3,4)]) ~ Phylum + Sample + Phylum:Sample,
         family = binomial(link = "logit"), data = b72)
anova(m1, test="Chisq")
summary(m1)
Residuos(m1)


m2 = glm(formula = as.matrix(b72[, c(3,4)]) ~ Phylum + Sample,
         family = binomial(link = "logit"), data = b72)
   anova(m2, test="Chisq")
 summary(m2)
Residuos(m2)



m3 = glm(formula = as.matrix(b72[, c(3,4)]) ~ Phylum,
         family = binomial(link = "logit"), data = b72)
   anova(m3, test="Chisq")
 summary(m3)
Residuos(m3)


m4 = glm(formula = as.matrix(b72[, c(3,4)]) ~ Sample,
         family = binomial(link = "logit"), data = b72)
   anova(m4, test="Chisq")
 summary(m4)
 Residuos(m4)
 

# Verificação no Step 
attach(b72)
step(mod0,~Sample+Phylum + Sample:Phylum , data = b72, direction = c("both"))


# Tabela ANODEV
attach(b72)

# Tabela ANODEV
T72 = as.data.frame(anova(m1, test="Chisq"));T72
T72$AIC = c(
  AIC(mod0),
  AIC(m3),
  AIC(m2),
  AIC(m1)
)

T72.1 = as.data.frame(anova(m4, test="Chisq"));T72.1
T72.1$AIC =  c(AIC(mod0),
               AIC(m4))

Tab.ANODEV = rbind(T72.1 ,T72[-1,])
Tab.ANODEV$Deviance = formatC(Tab.ANODEV$Deviance, digits = 2, format = "f") 
format(T72$`Resid. Dev`, digits = 3, nsmall = 1)
format(T72$`Resid. Dev`, digits = 3, nsmall = 6) # small não influencia
Tab.ANODEV$`Resid. Dev` = formatC(Tab.ANODEV$`Resid. Dev`,format = "f", digits = 2)
Tab.ANODEV$`Pr(>Chi)` = formatC( Tab.ANODEV$`Pr(>Chi)`,format = "f", digits = 4)
Tab.ANODEV$AIC = formatC( Tab.ANODEV$AIC,format = "f", digits = 1);Tab.ANODEV
row.names(Tab.ANODEV) = c(1:nrow(Tab.ANODEV))
Tab.ANODEV$Model =  c( "NULL",
                       "Sample",
                       "Phylum",
                       "Sample+Phylum",
                       "Sample+Phylum+Phylum|Sample")
Tab.ANODEV.72 = Tab.ANODEV[,c(7,1:6)]

###### Dentre os modelos de Regressão logística avaliados na `Tab.ANODEV.72` com as variáveis
###### `Sample` e `Phylum`, o modelo contendo apenas a variável `Sample` (linha 2) apresentou significância
###### estatística (p<0.01), além de AIC= 31.9, menor patamar desse parâmetro dentre os modelos
###### passíveis de utilização (exceto Modelo Nulo - linha 1 e Saturado - linha 5). Assim, apenas a variável 
###### `Sample` exeplica substancialmente a variabilidade da probabilidade de lise, evidenciando sua
###### associação ao fenômeno estudo. Portanto, independente do filo, em 72 h tanto o isolamento quanto 
###### o tratamento impactam os organismos.

#### 5 Estimativa dos coeficientes  do modelo selecionado:

m4$coefficients
y = summary(m4)
y$coefficients[,1:2]
x= round(model.matrix(m4)%*%as.matrix(cbind(m4$coefficients)),1);x
anova(m4, test = "Chisq")
as.data.frame(summary(m4))
round(x[3,1]/x[1,1]*100,1)
round(x[5,1]/x[1,1]*100,1)

qnorm(0.975)


(ic = round(c(exp(coefficients(m4)[1]), 
              exp(coefficients(m4)[1]- qnorm(0.975)*y$coefficients[1,2]),
              exp(coefficients(m4)[1]+ qnorm(0.975)*y$coefficients[1,2]))*100,1))

(ic = round(c(exp(coefficients(m4)[2]), 
              exp(coefficients(m4)[2]- qnorm(0.975)*y$coefficients[2,2]),
              exp(coefficients(m4)[2]+ qnorm(0.975)*y$coefficients[2,2]))*100,1))

(ic = round(c(exp(coefficients(m4)[3]), 
              exp(coefficients(m4)[3]- qnorm(0.975)*y$coefficients[3,2]),
              exp(coefficients(m4)[3]+ qnorm(0.975)*y$coefficients[3,2]))*100,1))

###### com base na tabela das chances, tem-se que  

# ### Fim 72 h ------------------------------------------------------------






b168 = b1[b1$Day  == "168 hrs" ,-2]
colnames(b168) = c("Sample","Phylum", "Yes","No")
str(b168)
b168$Sample = factor(as.factor(b168$Sample),levels = c("Raw Water", "Control","8 g/PDO" ))
relevel(b168$Sample,ref = "Raw Water")

b168$Phylum = factor(as.factor(b168$Phylum),levels = c("Cyano","Algae"))
relevel(b168$Phylum,ref = "Cyano")



#Modelo Nulo

mod0 = glm(as.matrix(b168[,c(3,4)])~1, family=binomial(link="logit"), data=b168)



#### 4.1.2 Testar os melhores ajustes utilizando critérios estatísticos (AIC) e fenomenológicos

# modelo M1:  

m1 = glm(formula = as.matrix(b168[, c(3,4)]) ~ Phylum + Sample + Phylum*Sample,
         family = binomial(link = "logit"), data = b168)
anova(m1, test="Chisq")
summary(m1)
Residuos(m1)


m2 = glm(formula = as.matrix(b168[, c(3,4)]) ~ Phylum + Sample,
         family = binomial(link = "logit"), data = b168)
anova(m2, test="Chisq")
summary(m2)
Residuos(m2)



m3 = glm(formula = as.matrix(b168[, c(3,4)]) ~ Phylum,
         family = binomial(link = "logit"), data = b168)
anova(m3, test="Chisq")
summary(m3)
Residuos(m3)


m4 = glm(formula = as.matrix(b168[, c(3,4)]) ~ Sample,
         family = binomial(link = "logit"), data = b168)
anova(m4, test="Chisq")
summary(m4)
Residuos(m4)

# Verificação no Step 
attach(b168)
step(mod0,~Sample+Phylum + Sample:Phylum , data = b168, direction = c("both"))


# Tabela ANODEV
attach(b168)

# Tabela ANODEV
T168 = as.data.frame(anova(m1, test="Chisq"));T168
T168$AIC = c(
  AIC(mod0),
  AIC(m3),
  AIC(m2),
  AIC(m1)
)

T168.1 = as.data.frame(anova(m4, test="Chisq"));T168.1
T168.1$AIC =  c(AIC(mod0),
               AIC(m4))

Tab.ANODEV.168 = rbind(T168.1 ,T168[-1,])
Tab.ANODEV.168$Deviance = formatC(Tab.ANODEV.168$Deviance, digits = 2, format = "f") 
Tab.ANODEV.168$`Resid. Dev` = formatC(Tab.ANODEV.168$`Resid. Dev`,format = "f", digits = 2)
Tab.ANODEV.168$`Pr(>Chi)` = formatC( Tab.ANODEV.168$`Pr(>Chi)`,format = "f", digits = 4)
Tab.ANODEV.168$AIC = formatC( Tab.ANODEV$AIC,format = "f", digits = 1);Tab.ANODEV.168
row.names(Tab.ANODEV.168) = c(1:nrow(Tab.ANODEV.168))
Tab.ANODEV.168$Model =  c( "NULL",
                       "Sample",
                       "Phylum",
                       "Sample+Phylum",
                       "Sample+Phylum+Phylum|Sample")
Tab.ANODEV.168 = Tab.ANODEV.168[,c(7,1:6)]

###### Quando se analisa 168 h, conforme pode ser visto na `Tab.ANODEV.168`, as variáveis `Sample` e `Phylum` apresentaram significância
###### estatística (p<0.01), indicando que com o passar do tempo, o `Phylum` passa a influenciar na probabilidade de células íntegras. 
###### Sendo assim, o modelo da linha 4 contendo  as duas variáveis foi escolhido pois além do `AIC = 32,0` a `Resid. Dev.` apresentou o menor 
###### valor (8.61) desse parâmetro dentre os modelos passíveis de utilização (exceto Modelo Nulo - linha 1 e Saturado - linha 5). Assim,  
###### com o passar do  tempo, cianos  exeplica substancialmente a variabilidade da probabilidade de lise, evidenciando sua
###### associação ao fenômeno estudo. Portanto, independente do filo, em 72 h tanto o isolamento quanto 
###### o tratamento impactam os organismos.


m2$coefficients

x = round(model.matrix(m2)%*%as.matrix(y$coefficients[,1]),1);x

round(x[3,1]/x[1,1]*100,1)
round(x[5,1]/x[1,1]*100,1)



y = summary(m2)
y$coefficients[,1:2]
(ic = round(c(exp(coefficients(m2)[1]), 
              exp(coefficients(m2)[1]- 1.96*y$coefficients[1,2]),
              exp(coefficients(m2)[1]+ 1.96*y$coefficients[1,2]))*100,1))

(ic = round(c(exp(coefficients(m2)[2]),  
              exp(coefficients(m2)[2]- 1.96*y$coefficients[2,2]),
              exp(coefficients(m2)[2]+ 1.96*y$coefficients[2,2]))*100,1))

(ic = round(c(exp(coefficients(m2)[3]),  
              exp(coefficients(m2)[3]- 1.96*y$coefficients[3,2]),
              exp(coefficients(m2)[3]+ 1.96*y$coefficients[3,2]))*100,1))

(ic = round(c(exp(coefficients(m2)[4]),  
              exp(coefficients(m2)[4]- 1.96*y$coefficients[4,2]),
              exp(coefficients(m2)[4]+ 1.96*y$coefficients[4,2]))*100,1))


