# Planejamento e controle de experimentos ---------------------------------
#install.packages("daewr")
#install.packages("gmodels")
## Intalar do repositório de desenvolvimento no GitLab.
#3install_git(url = "https://gitlab.c3sl.ufpr.br/pet-estatistica/labestData.git",
#branch = "master", build_vignettes = F)

library(devtools)
library(labestData)
library(gmodels)

# Delineamento Inteiramente Casualizado -----------------------------------

str(ZimmermannTb3.5)
# Nomes curtos dão mais agilidade.
zim <- ZimmermannTb3.5

# Atribui novos rótulos para os níveis.
levels(zim$adub) <- c("100/0/0", "50/50/0", "16/84/0", "16/42/42")

# Produção média.
aggregate(prod ~ adub, data = zim, FUN = mean)

library(tables)

# Tabela com medidas descritivas: média, desvio-padrão e número de obs.
tabular(adub + 1 ~ prod * (mean + sd + length), data = zim)

library(lattice)

xyplot(
        prod ~ reorder(adub, prod),
        data = zim,
        type = c("p", "a"),
        ylab = expression("Produção de grãos de arroz" ~ kg ~ ha ^ {
                -1
        }),
        xlab = expression("Percentual aplicado de 80" ~
                                  kg ~ ha ^ {
                                          -1
                                  } ~ "(Plantio/40 DAE/60 DAE)")
)

# Fator de 4 níveis.
x <- gl(4, 1)

# Nome dos contrastes.
ctr <- paste0("contr.", c("treatment", "SAS", "sum", "helmert"))

# Obtendo a matriz de contrantes para cada tipo.
L <- lapply(
        ctr,
        FUN = function(type) {
                cbind(1, contrasts(C(x, type)))
        }
)
names(L) <- ctr
L

# Obtendo qual função da média cada parâmetro corresponde.
lapply(L, function(x)
        MASS::fractions(solve(x)))

# Ajuste do modelo.
m0 <- lm(prod ~ adub, data = zim)

# Quadro de análise de variância.
anova(m0)
# Estimativas dos efeitos.
summary(m0)

ctr <- paste0("contr.", c("treatment", "SAS", "sum", "helmert"))

sapply(
        ctr,
        FUN = function(type) {
                m1 <- update(m0,
                             contrasts = list(adub = type))
                K <- cbind("(Intercept)" = 1,
                           contrasts(C(zim$adub, type)))
                K %*% coef(m1)
        }
)
par(mfrow = c(2, 2))
plot(m0)
layout(1)





# Cap 2 - Livro -----------------------------------------------------------

set.seed(7638)
f <- factor(rep(c(35, 40, 45), each = 4))
fac <- sample(f, 12)
eu <- 1:12
plan <- data.frame(loaf = eu, time = fac)

library(daewr)
str(bread)
shapiro.test(bread$height)
#Notar que 'time' é um fator!
mod0 <- lm(height ~ time, data = bread)
summary(mod0)

#Os mesmos coeficientes podem ser obtidos:

# Vetor y
y = c(4.5, 5.0, 5.5, 6.75, 6.5, 6.5, 10.5, 9.5, 9.75, 8.75, 6.5, 8.25)

#Matrix X
X = matrix(c(rep(1, 12),
             rep(c(0, 1), each = 4),
             rep(0, 4),
             rep(0, 8),
             rep(1, 4)),
           byrow = F,
           nrow = 12)

#Matriz quadrada X'X
t(X) %*% X


#Matriz coluna X'y
t(X) %*% y

#Matriz inv X'X
solve(t(X) %*% X) #usando a função solve
MASS::ginv(t(X) %*% X) #usando a função ginv() do MASS

# Estimativa de Beta-> inv(X'X)X'y
betachapeu = (solve(t(X) %*% X)) %*% (t(X) %*% y)

#usando ginv()->(MASS::ginv(t(X)%*%X))%*%(t(X)%*%y)

#fit.contrast( mod0, "time", c(-1, 0, 1) ) Não entendi

mod1 <- aov(height ~ time, data = bread)
summary(mod1)
par(mfrow = c(2, 2))
plot(mod1, which = 5)
plot(mod1, which = 1)
plot(mod1, which = 2)
plot(
        residuals(mod1) ~ loaf,
        main = "Residuals vs Exp. Unit",
        font.main = 1,
        data = bread
)
abline(h = 0, lty = 2)

#Verificação dos pressupostos
library(MASS)
bc <- boxcox(mod1)
lambda <- bc$x[which.max(bc$y)]
lambda


#Transformação box-cox
tbread <- transform(bread, theight = height ^ (-.5050505))
mod2 <- aov(theight ~ time, data = tbread)
summary(mod2)
# Melhorar o valor de F e a qualidade dos residuos


par(mfrow = c(2, 2))
plot(mod2, which = 5)
plot(mod2, which = 1)
plot(mod2, which = 2)
plot(
        residuals(mod2) ~ loaf,
        main = "Transf Residuals vs Exp. Unit",
        font.main = 1,
        data = tbread
)
abline(h = 0, lty = 2)

#Minimos quadrados ponderados
with(bread, {
        std <- tapply(bread$height, bread$time, sd)
        weights <- rep(1 / std, each = 4)
        mod3 <- lm(height ~ time, weights = weights, data = bread)
        anova(mod3)
})

library(daewr)
library(MASS)
modf <- polr(score ~ method, weight = count, data = teach)
modr <- polr(score ~ 1, weight = count, data = teach)
anova(modf, modr)

#Poder do teste
library(daewr)
rmin <- 2 #smallest number of replicates considered
rmax <- 6 # largest number of replicates considered
alpha <- rep(0.05, rmax - rmin + 1)
sigma <- sqrt(2.1)
nlev <- 3
nreps <- rmin:rmax
Delta <- 3
power <- Fpower1(alpha, nlev, nreps, Delta, sigma)
power

#Comparações planejadas

library(daewr)
mod4 <- aov(yield ~ treat, data = sugarbeet)
con <- matrix(c(1,-1 / 3,-1 / 3,-1 / 3, 0, 1,-1, 0,
                0, 0, 1,-1), 4, 3)
L <- t(con)
rownames(L) <- c("-fertilizer effect",
                 "-plowed vs. broadcast"
                 ,
                 "-January vs. April")
L
options(digits = 3)
library(gmodels)
fit.contrast(mod4, "treat", L)


contrasts(bread$time) <- contr.poly(3)
contrasts(bread$time)
mod3 <- aov(height ~ time, bread)
summary.lm(mod3)

#Comparações não planejadas

mod4 <- aov(yield ~ treat, data = sugarbeet)
mod4.tukey <- TukeyHSD(mod4, ordered = T)
mod4.tukey


library(agricolae)
compare <- SNK.test(mod4, "treat", alpha = 0.05)
print(compare)


library(multcomp)
sugar.dun <- glht(mod4,
                  linfct = mcp(treat = "Dunnett"),
                  alternative = "less")
summary(sugar.dun)


# Cap 3 - Livro ------------------------------------------------------------------
head(data)
View(data[order(data$Amostra, decreasing = F), ])
#OUTRA FORMA DA BASE DE DADOS
d2f = melt(data, id.vars = c("Tempo", "Amostra", "Percentual"))
View(d2f[orde])
d2f$Sample = as.factor(d2f$Sample)
d2f$Treatment = as.factor(d2f$Treatment)
d2f$Treatment = relevel(d2f$Treatment, ref = "0")
d2f$EU = as.numeric(d2f$EU)
str(d2f)


#FUNÇÃO PARA CRIAR PLANO FATORIAL
D <- expand.grid(BW = c(3.25, 3.75, 4.25), WL = c(4, 5, 6))
E = expand.grid(Sample = levels(as.factor(data$Tempo)),
                Treatment = levels(as.factor(data$Percentual)))
#UTILIZAR:
#       rbind() para cada replicata
#       sample() pra randomizar o experimento

library(daewr)
mod1 <- aov(CO ~ Eth * Ratio, data = COdata)
summary(mod1)
str(COdata)
shapiro.test(COdata$CO)

# Formatação do dataset
data = data[order(data$Amostra, decreasing = F), ]
data$Tempo = as.factor(data$Tempo)
data$Amostra = as.factor(data$Amostra)
data$Percentual = as.factor(data$Percentual)
data$Percentual = relevel(data$Percentual, "0")
str(data)




# Cap 4 - Livro -----------------------------------------------------------
#Diretório
local = c("C:/Users/User/Documents/Artigo Samylla/Densidade/DoE")
setwd(local)

#base com os valores médios

newdata = read_xlsx("Dataset2.xlsx")
newdata = as.data.frame(newdata)

d3 = data[order(data$Tempo, decreasing = F),]
d3$Isimp. = newdata$Isimp.
d3$Ishan. = newdata$Ishan.
d3$ID = newdata$ID
d3$IE = newdata$IE
colnames(newdata)[3] = "Amostra"
newdata$Block = as.factor(ifelse(
        newdata$Amostra == 1 |
                newdata$Amostra == 4,
        "B1",
        ifelse(newdata$Amostra == 2 |
                       newdata$Amostra == 5, "B2", "B3")
))
newdata$Outros = rowSums(newdata[, c(4:7, 9,10)])
newdata$Treat = as.factor(ifelse(newdata$Amostra =="1"|
                                 newdata$Amostra =="2"|
                                 newdata$Amostra =="3","Treat","Ctrl"))

                        #0 h             #72 h        #128 h
newdata$Reatores = c(0,0,0,0,0,0, 15,15,15,0,0,0, 15,15,15,0,0,0,
                        #336 h          #504          #720 h
                     14,15,14,0,0,0, 4,15,7,0,0,0, 2,8,3,0,0,0 )
newdata[,c(3,1,18,2,17,15,8,16,11:14)]

#Formatação da base de dados

data$Block = as.factor(ifelse(
        data$Amostra == 1 |
                data$Amostra == 4,
        "B1",
        ifelse(data$Amostra == 2 |
                       data$Amostra == 5, "B2", "B3")
))

d2 = newdata[newdata$Block != "B2",c(3,1,18,2,17,15,8,16,11:14)]

d2$Amostra = as.factor(d2$Amostra)
d2$Tempo = as.factor(d2$Tempo)
d2$Percentual = as.factor(d2$Percentual)
d2$Percentual = relevel(d2$Percentual,"0")
summary(d2)
str(d2)


for (i in 7:12) {
        mod3 <- aov((d2[, i]) ~ Block + Tempo * Treat , data = d2)
        cat("Imprimindo....",colnames(d2)[i],"\n")
        print(summary(mod3))
        cat("..........................","\n","\n")

        #Contraste e ajuste via regressão

        #Tempo
        contrasts(d2$Tempo) <- contr.poly(6)
        mod2 <- aov(d2[, i] ~ Block + Tempo * Treat ,
                    data = d2)
        cat("Coef. da regressão para o tempo | Filo:",colnames(d2)[i],"\n")
        print(summary.aov(mod2, split = list(
                Tempo = list(
                        "Linear" = 1,
                        "Quadratic" = 2,
                        "Cubic" = 3,
                        "Quartic" = 4,
                        "Penta" = 5))))
        cat("..........................","\n","\n")

        #Tratamento
        contrasts(d2$Treat) <- contr.poly(2)
        mod4 <- aov(d2[,i] ~ Block + Tempo * Treat , data = d2)
        cat("Coef. da regressão para o Tratamento | Filo:",colnames(d2)[i],"\n")
        print(summary.aov(mod4, split = list(Treat = list("Linear" = 1))))
        cat("..........................","\n","\n")
        #Grafico
        options(scipen = 999)
        png(filename = paste0(colnames(d2)[i],".png"),
            width = 15,
            height =7,
            res =600,
            units = "in")
        par(mfrow = c(2,3))
        R <- do.call("cbind", split(d2[d2$Treat != "Treat", i],
                                    d2$Block[d2$Treat != "Treat"]))
        y <- apply(R, 1, mean)
        x <- as.double(levels(d2$Tempo))
        plot(
                x,
                y,
                xlab = expression("Tempo" ~ (h)),
                ylab = ifelse(i==7 |i==8,
                              expression("Densidade" ~ (Cel ~ mL ^ {
                        -1
                })),""),
                ylim = c(min(R), max(R)),
                main = paste("Controle", colnames(d2)[i]),
                pch = 17,
                xaxt = "n")
        axis(1,
             x,
             c(0,  72, 168, 336, 504, 720))
        xx <- seq(0.0, 720.0, .1)
        d2.Linear = lm(y ~ poly(x, 1))
        lines(xx, predict(d2.Linear, data.frame(x = xx)), lty = 2)
        R <- do.call("cbind", split(d2[d2$Treat == "Treat", i],
                                    d2$Block[d2$Treat == "Treat"]))
        y <- apply(R, 1, mean)
        x <- as.double(levels(d2$Tempo))
        plot(
                x,
                y,
                xlab = expression("Tempo" ~ (h)),
                ylab = ifelse(i==7 |i==8,
                              expression("Densidade" ~ (Cel ~ mL ^ {-1})),""),
                ylim = c(min(R), max(R)),
                main = paste("Tratamento", colnames(d2)[i]),
                pch = 15,
                xaxt = "n"
        )
        axis(1,
             x,
             c(0,  72, 168, 336, 504, 720))
        xx <- seq(0.0, 720.0, .1)
        d2.Linear = lm(y ~ poly(x, 1))
        lines(xx, predict(d2.Linear,
                          data.frame(x = xx)),
              lty = 1)
        R <- do.call("cbind", split(d2[, i],
                                    d2$Block))
        y <- apply(R, 1, mean)
        x <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
        plot(
                x,
                y,
                xlab = "",
                ylab = ifelse(i==7 |i==8,
                              expression("Densidade" ~ (Cel ~ mL ^ {-1  })),""),
                ylim = c(min(R), max(R)),
                main = "",
                pch = 16,
                xaxt = "n"
        )
        axis(1,
             0:1,
             c("Controle", "Tratamento"))
        xx <- seq(0.0, 1, .1)
        d2.Linear = lm(y ~ poly(x, 1))
        lines(xx, predict(d2.Linear, data.frame(x = xx)), lty = 3)
        plot(mod3, c(1:2, 5))
        locator(1)
        cat("...........................................","\n","\n")
        dev.off()
}





