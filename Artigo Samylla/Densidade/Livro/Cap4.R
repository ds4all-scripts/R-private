
# Livro: Analysis od categorical data with R ------------------------------


# #Cap 4  ---------------------------------------------------------------


# Read in data
setwd("C://Users//User//Documents//Artigo Samylla//Densidade")
stoplight <- read.csv(file  = "Livro//stoplight.csv")
head(stoplight)


# Compare to Poisson

# Summary statistics
mean(stoplight$vehicles)
var(stoplight$vehicles)

# Frequencies
table(stoplight$vehicles) #Note that y = 0, 1, ..., 8 all have positive counts
rel.freq <- table(stoplight$vehicles)/length(stoplight$vehicles)
rel.freq2 <- c(rel.freq, rep(0, times = 7))

# Poisson calculations
y <- 0:15
prob <- round(dpois(x = y, lambda = mean(stoplight$vehicles)), 4)

# Observed and Poisson
data.frame(y, prob, rel.freq = rel.freq2)

# Plot

plot(x = y - 0.1, y = prob, type = "h", ylab = "Probability", xlab = "Number of vehicles", lwd = 2,
     xaxt = "n")
axis(side = 1, at = 0:15)
lines(x = y + 0.1, y = rel.freq2, type = "h", lwd = 2, lty = "solid", col = "red")
abline(h = 0)
legend(x = 9, y = 0.15, legend = c("Poisson", "Observed"), lty = c("solid", "solid"), lwd = c(2,2), col = c("black", "red"), bty = "n")


# B/W Version

plot(x = y - 0.1, y = prob, type = "h", ylab = "Probability", xlab = "Number of vehicles", lwd = 2,
     xaxt = "n")
axis(side = 1, at = 0:15)
lines(x = y + 0.1, y = rel.freq2, type = "h", lwd = 2, lty = "solid", col = "gray70")
abline(h = 0)
legend(x = 9, y = 0.15, legend = c("Poisson", "Observed"), lty = c("solid", "solid"), lwd = c(2,2), col = c("black", "gray70"), bty = "n")


# NOTE: should not use dashed lines due to the type of plot -
#       will not know exactly where top of bar is


########################################################################
# CI

alpha <- 0.05
n <- length(stoplight$vehicles)
mu.hat <- mean(stoplight$vehicles)

# Wald
mu.hat + qnorm(p = c(alpha/2, 1 - alpha/2))*sqrt(mu.hat/n)

# Score - original used with published book
(mu.hat + qnorm(p = c(alpha/2, 1 - alpha/2))/(2*n)) + qnorm(p = c(alpha/2, 1 - alpha/2)) * sqrt((mu.hat + qnorm(p = 1 - alpha/2)/(4*n))/n)
# Score - corrected after book published (missing ^2)
(mu.hat + qnorm(p = 1 - alpha/2)^2/(2*n)) + qnorm(p = c(alpha/2, 1 - alpha/2)) *
  sqrt((mu.hat + qnorm(p = 1 - alpha/2)^2/(4*n))/n)

# Exact
qchisq(p = c(alpha/2, 1 - alpha/2), df = c(2*n*mu.hat, 2*(n*mu.hat + 1)))/(2*n)
# Other code for exact
# qgamma(p = c(alpha/2, 1-alpha/2), shape = c(n*mu.hat, n*mu.hat+1), scale = 1)/n
# c(qchisq(p = alpha/2, df = 2*n*mu.hat),qchisq(p = 1-alpha/2, df = 2*(n*mu.hat+1)))/(2*n)

# Exact using poisson.test in stats package
poisson.test(x = mu.hat*n)$conf.int / n

# Usual t-distribution based interval
t.test(x = stoplight$vehicles, conf.level = 0.95)

#######################################################################
# Repeating CI calculations using functions from epitools package.
#  For each function, x=sum of data (using n*mu.hat here) and
#  pt = sample size in our applications.  See help(pois.conf.int).

library(epitools)

pois.exact(x = mu.hat*n, pt = n, conf.level = 0.95)
pois.approx(x = mu.hat*n, pt = n, conf.level = 0.95)




# Livro: Extending the linear model with R --------------------------------


# Cap 5 -------------------------------------------------------------------
library(faraway)
barplot(dpois(0:5,0.5),xlab="y",ylab="Probability",names=0:5,main="mean = 0.5")
barplot(dpois(0:10,2),xlab="y",ylab="Probability",names=0:10,main="mean = 2")
barplot(dpois(0:15,5),xlab="y",ylab="Probability",names=0:15,main="mean = 5")
data(gala, package="faraway")
gala <- gala[,-2]
modl <- lm(Species ~ . , gala)
plot(modl, 1)
modt <- lm(sqrt(Species) ~ . , gala)
plot(modt, 1)
library(faraway)
summary(modt)
modp <- glm(Species ~ ., family=poisson, gala)
sumary(modp)
summary(modp)
halfnorm(residuals(modp))
plot(log(fitted(modp)),log((gala$Species-fitted(modp))^2), xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2))
abline(0,1)
(dp <- sum(residuals(modp,type="pearson")^2)/modp$df.res)
sumary(modp,dispersion=dp)
modd <- glm(Species ~ ., family=quasipoisson, gala)
drop1(modd,test="F")
data(dicentric, package="faraway")
round(xtabs(ca/cells ~ doseamt+doserate, dicentric),2)
with(dicentric,interaction.plot(doseamt, doserate, ca/cells, legend=FALSE))
lmod <- lm(ca/cells ~ log(doserate)*factor(doseamt), dicentric)
summary(lmod)$adj
plot(residuals(lmod) ~ fitted(lmod),xlab="Fitted",ylab="Residuals")
abline(h=0)
dicentric$dosef <- factor(dicentric$doseamt)
pmod <- glm(ca ~ log(cells)+log(doserate)*dosef,  family=poisson,dicentric)
sumary(pmod)
rmod <- glm(ca ~ offset(log(cells))+log(doserate)*dosef,  family=poisson,dicentric)
sumary(rmod)
data(solder, package="faraway")
modp <- glm(skips ~ . , family=poisson, data=solder)
c(deviance(modp), df.residual(modp))
modp2  <- glm(skips ~ (Opening +Solder + Mask + PadType + Panel)^2 ,  family=poisson, data=solder)
deviance(modp2)
pchisq(deviance(modp2),df.residual(modp2),lower=FALSE)
library(MASS)
modn <- glm(skips ~ .,negative.binomial(1),solder)
modn
modn <- glm.nb(skips ~ .,solder)
summary(modn)
library(pscl)
modp <- glm(art ~ ., data=bioChemists, family=poisson)
sumary(modp)
ocount <- table(bioChemists$art)[1:8]
pcount <- colSums(predprob(modp)[,1:8])
plot(pcount,ocount,type="n",xlab="Predicted",ylab="Observed")
text(pcount,ocount, 0:7)
modh <- hurdle(art ~ ., data=bioChemists)
summary(modh)
modz <- zeroinfl(art ~ ., data=bioChemists)
summary(modz)
plot(fitted(modh), fitted(modz), xlab="Hurdle predictions", ylab="ZIP predictions")
abline(0,1)
modz2 <- zeroinfl(art ~ fem+kid5+ment | ment, data=bioChemists)
summary(modz2)
(lrt <- 2*(modz$loglik-modz2$loglik))
1-pchisq(6.1728,6)
exp(coef(modz2))
newman <- data.frame(fem="Men",mar="Single",kid5=0,ment=6)
predict(modz2, newdata=newman, type="prob")
predict(modz2, newdata=newman, type="zero")
