

# Modelo de regressão de  poisson -----------------------------------------
# modelando taxa de mortalidade



deaths = c(4,5,16,31,68,206,
           6,14,22,39,87,122)
pop = c(39219,37761,35573,24621,16366,16366,
        35577,32342,28286,17984,10188,10188)
age = as.factor(c(seq(30,80,10),seq(30,80,10)))
age2 = (c(seq(30,80,10),seq(30,80,10)))

sex = as.factor(rep(c("female","male"),each=6))

#a taxa é lambda/pop, logo pode ser um numero menor do que 1, pois 
# lambda é uma contagem (sempre positivo e maior ou igual a 0 e 
# a população "pop" geralmente é um numero grande. Dessa forma, Para 
# contornar esse problema faz =  log(lambda/pop) = log(lambda) - log(pop).
# passado o log(pop) para o outro lado da equação tem0-se:
# log(lambda) = sex+age+log(pop), esse termo "log(pop)" é chamado de offset
# 

model = glm(deaths~offset(log(pop))+sex+age,
            family=poisson)

summary(model)
cov.beta = vcov(model)

#mULHER DE 40
# X = c(b0,b1,...,b7)
  x = c(1,0,1,0,0,0,0)
b = model$coefficients
previsto = x%*%b
#para 100.000 habitantes, TEM-SE
100000*exp(previsto) #23,4 A CADA 100MIL MULHERES

# PARA A MARGEM DE ERRO FICA: V(b0+b2) = V(b0)+v(b2)+2Cov(b0,b2)
# com base na matriz "cov.beta"
var.previsto = cov.beta[1,1]+cov.beta[3,3]+2*cov.beta[1,3]
#erro-padrão
ep.previsto = sqrt(var.previsto)

#Intervalo de confiança
limites = c(previsto-1.96*ep.previsto,
            previsto,
            previsto+1.96*ep.previsto)

#Considerando 100 mil mulheres a taxa de mortalidade fica:
100000*exp(limites)

#HOMEM DE 60 ANOS

# X = c(b0,b1,...,b7)
x = c(1,1,0,0,1,0,0)
previsto2 = x%*%b
# PARA A MARGEM DE ERRO FICA: V(b0+b1+b4) = V(b0)+v(b1)+v(b4)+2Cov(b0,b1)+
#                                           2Cov(b0,b2)+2Cov(b2,b1)
# com base na matriz "cov.beta"
var.previsto2 = cov.beta[1,1]+cov.beta[2,2]+cov.beta[5,5]+
               2*cov.beta[1,2]+2*cov.beta[1,5]+2*cov.beta[2,5]
#erro-padrão
ep.previsto2 = sqrt(var.previsto2)

#Intervalo de confiança
limites2 = c(previsto2-1.96*ep.previsto2,
            previsto2,
            previsto2+1.96*ep.previsto2)


#Considerando 100 mil homens a taxa de mortalidade fica:
100000*exp(limites2)


prev=predict(model, type = "response",se.fit = T)
(prev$fit) # as estimativas com base nos valores de "pop"
exp(previsto)*pop[2]
(prev$fit)[2]

# usando age como numeric -------------------------------------------------
model = glm(deaths~offset(log(pop))+sex+age2,
            family=poisson)

summary(model)
cov.beta2 = vcov(model)

#Estimar com a margem de erro a taxa para mulher de qualquer idade
#PADRÃO DAS COVARIÁVEIS
x = cbind(rep(1,6),rep(0,6),age2[1:6])
b=model$coefficients
#previsão eta
previsto3 = x%*%b
#na escala do dados em 100 mil
100000*exp(previsto3)

#Variância da previsão
# V(b0+b2Age2), pois b1 = 0, pois é para mulheres com qualquer iddade. 
#V(b0+b2Age2) = V(b0)+ V(b2Age2)+2Cov(b0,b2Age2) = V(b0)+(Age2)²*V(b2)+2*Age2*Cov(b0,b2)

var.previsto3 = cov.beta2[1,1]+cov.beta2[3,3]*((age2[1:6])^2) +2*(age2[1:6])*cov.beta2[1,3]

#erro padrão
ep.previsto3 = sqrt(var.previsto3)

#Margem de erro
LS = previsto3+1.96*ep.previsto3
LI = previsto3-1.96*ep.previsto3

#Na escala dos dados (mu)
100000*exp(LS)
100000*exp(LI)

#plotando o grafico

plot(age2[1:6], 10e5*exp(previsto3),type = "l",
     col = 2,xlab = "Idade",ylab = "Taxa de Mortalidade")
lines(age2[1:6], 10e5*exp(LS),lty = 3,col = 2)
lines(age2[1:6], 10e5*exp(LI),lty = 3,col = 2)


#Estimar com a margem de erro a taxa para homem de qualquer idade
#PADRÃO DAS COVARIÁVEIS
x = cbind(rep(1,6),rep(1,6),age2[1:6])
b=model$coefficients
#previsão eta
previsto4 = x%*%b
#na escala do dados em 100 mil
100000*exp(previsto4)

#Variância da previsão
# V(b0+b1+b2Age2). 
#V(b0+b1+b2Age2) = V(b0)+ V(b1)+ V(b2Age2)+2Cov(b0,b2Age2)+ 2Cov(b1,b2Age2)+2Cov(b0,b1)
#                = V(b0)+ V(b1)+(Age2)²*V(b2)+2*Age2*Cov(b0,b2) + 2*Age2*Cov(b1,b2) + 2Cov(b0,b1)

var.previsto4 = cov.beta2[1,1]+cov.beta2[2,2]+cov.beta2[3,3]*((age2[1:6])^2)+2*(age2[1:6])*cov.beta2[1,3]+2*(age2[1:6])*cov.beta2[2,3]+2*cov.beta2[1,2]

#erro padrão
ep.previsto4 = sqrt(var.previsto4)

#Margem de erro
LS = previsto4+1.96*ep.previsto4
LI = previsto4-1.96*ep.previsto4

#Na escala dos dados (mu)
100000*exp(LS)
100000*exp(LI)

#plotando o grafico
lines(age2[1:6], 10e5*exp(previsto4),lty = 1,col = 3)
lines(age2[1:6], 10e5*exp(LS),lty = 3,col = 3)
lines(age2[1:6], 10e5*exp(LI),lty = 3,col = 3)

# taxas observadas
taxa = 10e5*deaths/pop
taxa.m = taxa[1:6]
taxa.h = taxa[7:12]
points(age2[1:6],taxa.m, col =2,pch = 16, cex =0.5)
points(age2[1:6],taxa.h, col =3,pch = 16, cex =0.5)
