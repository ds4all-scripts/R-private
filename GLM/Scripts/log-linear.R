
# Modelos log-lineares  ---------------------------------------------------


# Definições: -------------------------------------------------------------

# São modelos baseados na distribuição Poisson com
# função de ligação ln produzindo preditores lineares
# do tipo:

# E[Yi] = n.Theta = Mi
# ln E[Yi] = ln n + ln thata = ln n + ln thata , sendo n const.
# ln mi = ln n + beta0 + beta1Xi + ...+betaNXi, como n não varia,
# const = ln n + beta
# ln E[Yi] = const + t(Xi)BETA, X e BETA matrizes de planejamento e
# de preditor

# O preditor linear é idêntico ao preditor MLG Poisson, exceto pelo
# fato de que "n" é constante. Na prática, na MLG Poisson "ln n" era
# conhecido como offset e "n" variava para cada observação, pois
# representava a quantidade de expostos, ou seja:

#   nº de mortes (contagem, variável Poisson)
# ------------------
#  População exposta (offset, varia para cada observação)

# Nos modelos log-lineares os totais marginais são fixos!

# Aqui Theta[jk] é interpretado como a probabilidade de que um
# observaçãoocorra na celula da linha j e coluna k. Sendo assim,
# modela-se as frequências da tabela de contingência.

# Não é claro quem são as variáveis dependentes e independentes.
# No R, geralmente a frequencia é definida como dependente e a
# interpretação fica:  o fator está associado ou não à frequência

# Geralmente, compara-se dois ajustes: Um modelo aditivo, apenas
# com a soma simples dos fatores sem interação e outro acrescido
# dos termos de interação. Após os ajustes, faz-se o teste da
# das diferença da deviance numa Qui-quadrado com o nº de graus de
# liberdade igual a diferença dos graus de liberdade do modelo aditivo
# menos o modelo com interação. Se significante, existe evidência de
# que os fatores não são independentes, isto é, os valores estimados
# estão associados a interação dos fatores.

# Para quantificar a associação por meio do odds ratio deve-se
# utilizar um modelo um modelo multinomial ou binomial.


# Ex 1: Local da Úlcera vs uso de aspirina: estudo caso controle ----------

freq = c(62,39,53,49,6,25,8,8)
gd = c(0, 0, 1, 1, 0, 0, 1, 1)# Se gastrite duodenal gd = 1, 0 caso contrario
cc = c(1, 0, 1, 0, 1, 0, 1, 0)# Se control cc = 1, 0 caso contrario
ap = c(0, 0, 0, 0, 1, 1, 1, 1)# Se uso aspirina ap = 1, 0 caso contrario

#Modelo Nulo
minimal = glm(freq ~1, family = "poisson")
s1 = summary(minimal)

exp(3.44202)
sum(freq)
# interpretação: exp(3.44202) = 31.25 = n Tetha
# "a cada sum(freq) = 250 pessoas selecionadas aleatoriamente
# espera-se tem úlcera, espera-se que aproximadamente 31 pessoas
# usam asperina no caso, 31 teriam úlcera gastrica e seriam
# do controle, a mesma quantidade seria de pessoas que não
# usam as asperina, e assim por diante."

# Esse modelo não tem aplicação prática, apesar de ser
# significativo.

#Deviance Modelo Nulo
s1$deviance

#Graus de liberdade
#8 observações - 1 (valor ajustado do intersepto)
s1$df.residual

#Modelo Aditivo
aditivo = glm(freq ~gd+cc+ap, family = "poisson")
s2 = summary(aditivo)

#Deviance Modelo Aditivo
s2$deviance

#Graus de liberdade
#8 observações - 4 (valor ajustado de ap, gd, cc e intersepto)
s2$df.residual

#Modelo Saturado
maximal = glm(freq ~gd*cc*ap, family = "poisson")
s3 = summary(maximal)

#Deviance Modelo Saturado
s3$deviance #Zero!!!

#Graus de Saturado
# 8 observações - 8 (valor ajustado para ap, gd, cc, suas interações
# e intersepto)
s3$df.residual

#Analises:

# M1 - Interação entre local da gastrite (gd) e Caso-Controle (CC)

m1 = glm(freq ~gd*cc,family = "poisson")
s4 = summary(m1);s4
1-pchisq(s4$deviance,s4$df.residual) #modelo não ajustado

1-pchisq(s1$deviance -   s4$deviance,    # p> 0.05 não há evidencia que
         s1$df.residual -s4$df.residual )# o m4 seja melhor do que m1

# Porém, como o modelo nulo não tem aplicação prática, assume-se m4
# valido

# M2 - Interação entre local da gastrite (gd) e Caso-Controle (CC) e
# uso ou não de aspirina

m2= glm(freq ~gd*cc + ap,family = "poisson")
s5 = summary(m2);s5
1-pchisq(s5$deviance,s5$df.residual) #modelo não ajustado

# M3 - 2 Interações: entre local da gastrite (gd) e Caso-Controle (CC)
# e uso ou não de aspirina e caso controle
m3= glm(freq ~gd*cc + ap*cc,family = "poisson")
s6 = summary(m3);s6
1-pchisq(s6$deviance,s6$df.residual) #modelo não ajustado


# M4 - 2 Interações: entre local da gastrite (gd) e Caso-Controle (CC)
# e uso ou não de aspirina e caso controle
m4= glm(freq ~gd*cc + ap*cc + ap*gd,family = "poisson")
s7 = summary(m4);s7
1-pchisq(s7$deviance,s7$df.residual) #Ajustado a 1%!

#Concusões:

# Apesar dos modelos não significativos, de M2 a M4, pela
# diferença de deviance, concluimos que m4 é melhor que m3,
# que é melhor que m2. Todavia, como m4 é ajustado a 1% e
# considerando que em MLG os ajustes são aproximados e
# assintóticos, pode-se utiliza esse modelo.


# Uso de aspirina pode ser um fator de risco para úlceras,
# conforme visto em m3. Nesse modelo a inclusão de "ap" reduziu
# a diviance de 127 para um pouca mais que 10.
# Com menor evidência, também pode estar associada com o local
# de ocorrência da úlcera, segundo resultados observados em m4.

# # Ex 2: Importancia do ar condicionado ----------------------------------

freq = c(26,9,5,40,17,8,12,21,14,17,15,15,7,15,41,8,12,18)
sex = c(0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1) #Se mulher = 1, 0 caso contrario
rate = c(rep("no",6), rep("imp",6), rep("Vimp",6))
age = c("18", "24", "40", "18", "24", "40", "18", "24", "40", "18", "24", "40", "18", "24", "40", "18", "24", "40")
# "18", "24", "40",representam as classes de idade.

m1 = glm(freq~sex+age, family = "poisson")

m2 = glm(freq~sex*age, family = "poisson")

m3 = glm(freq~sex*age+rate, family = "poisson")

m3 = glm(freq~sex*age+rate, family = "poisson")
