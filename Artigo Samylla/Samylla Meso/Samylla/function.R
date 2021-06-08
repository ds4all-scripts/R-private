
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