
# Multinomial  ------------------------------------------------------------

#pkg
install.packages("nnet")
library(nnet)

#Ex1
y = as.factor(c(rep(1,6), rep(2,6), rep(3,6))) #  1:-Import, 2: Import e 3:+Import
x1 = c(rep(0,3),rep(1,3),rep(0,3),rep(1,3),rep(0,3),rep(1,3)) #x1=1 p/Homens e 0 c.c.
#Mulher é a referencia
x2 = c(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0) #x2 = 1 p/Idade 24-40 e 0 c.c.
x3 = c(0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1)#x3 = 1 p/Idade >40 e 0 c.c.
#Idade <24 é a referência
freq = c(26,9,5,40,17,8,12,21,14,17,15,15,7,15,41,8,12,18)

summary(multinom(y~x1+x2+x3, weights = freq))

#Interpretações para comparação entre "Impor" (PI2) vs "-Import"(PI1):

#Coefficients:
#B02            #B12      #B22    #B32
#  (Intercept)         x1       x2       x3
#2  -0.5907992 -0.3881301 1.128268 1.587709
#Std. Errors:
#  (Intercept)        x1        x2        x3
#2   0.2839756 0.3005115 0.3416449 0.4028997

# B02 (beta 02) - Não usual, prefere-se trabahar com as prob. estimadas
exp(-0.5907992) #odds de B02
exp(-0.5907992)-1 #o sinal indica redução!
exp(-0.5907992+1.96*0.2839756) # Lim. Sup. do IC de B02
exp(-0.5907992-1.96*0.2839756) # Lim. Inf. do IC de B02

# A probabilidade de uma mulher com menos de 24 anos atribua o grau
# "Import." (PI2) é 45% menor do que a probabilidade de atribuir o grau
# "-Import." (PI1).



# B12 (beta 12)
exp(-0.3881) #odds de B12
exp(-0.3881)-1 #o sinal indica redução!
exp(-0.3881+1.96*0.3005115) # Lim. Sup. do IC de B12
exp(-0.3881-1.96*0.3005115) # Lim. Inf. do IC de B12

# As chances de atribuição do grau "Import." (PI2) para Homens são
# 32% menores do que para as mulheres, em relação ao grau
# "-Import." (PI1)

# B22 (beta 22)

exp(1.128268) #odds de B22
exp(1.128268)-1 #o sinal indica aumento!
exp(1.128268+1.96*0.3416449) # Lim. Sup. do IC de B22
exp(1.128268-1.96*0.3416449) # Lim. Inf. do IC de B22

# As chances de atribuição do grau "Import." (PI2) para pessoas
# com idade entre 24 e 40 anos são 209% maiores do que para pessoas
# menores que 24 anos, em relação ao grau "-Import"(PI1)




#Interpretações para comparação entre "+Impor" (PI3) vs "-Import"(PI1):

#Coefficients:
#     B03            #B13      #B23    #B33
#  (Intercept)         x1       x2       x3
#3  -1.0390726 -0.8130202 1.478104 2.916757
#Std. Errors:
#  (Intercept)        x1        x2        x3
#3   0.3305014 0.3210382 0.4009256 0.4229276

# B03 (beta 03)
exp(-1.0390726) #odds de B03
exp(-1.0390726)-1 #o sinal indica redução!
exp(-1.0390726+1.96*0.3305014) # Lim. Sup. do IC de B03
exp(-1.0390726-1.96*0.3305014) # Lim. Inf. do IC de B03

# A probabilidade de uma mulher com menos de 24 anos atribua o grau
# "+Import." (PI3) é 65% menor do que a probabilidade de atribuir o grau
# "-Import." (PI1).

# B13 (beta 13)
exp(-0.8130202) #odds de B13
exp(-0.8130202)-1 #o sinal indica redução!
exp(-0.8130202+1.96*0.3210382) # Lim. Sup. do IC de B13
exp(-0.8130202-1.96*0.3210382) # Lim. Inf. do IC de B13

# As chances de atribuição do grau "+Import." (PI3) para Homens são
# 56% menores do que para as mulheres, em relação ao grau
# "-Import." (PI1)

# B23 (beta 23)

exp(1.478104) #odds de B23
exp(1.478104)-1 #o sinal indica aumento!
exp(1.478104+1.96*0.4009256) # Lim. Sup. do IC de B23
exp(1.478104-1.96*0.4009256) # Lim. Inf. do IC de B23

# As chances de atribuição do grau "+Import." (PI3) para pessoas
# com idade entre 24 e 40 anos são 338% maiores do que para pessoas
# menores que 24 anos, em relação ao grau "-Import"(PI1)


#Trabalhando com as probabilidades estimadas
# Para um melhor interpretação dos B02 e B03, é melhor calcular
# PI1, PI2 e PI3, fazendo:

#log(PI2/PI1) = B02, ou seja, mulher (x1 = 0) com idade <24 (x2 = x3=0)

#PI2 = exp(B02)*PI1, e de maneira similar,
#PI3 = exp(B03)*PI1
#PI1+PI2+PI3 = 1
#PI1(1+exp(B02)+exp(B03)) = 1

PI1 = 1/(1+exp(-0.5907992)+exp(-1.0390726))
PI1 # Probabilidade de mulheres menores que 24 anos atribuir "-Import."
PI2 = PI1*exp(-0.5907992)
PI2 # Probabilidade de mulheres menores que 24 anos atribuir "Import."
PI3 = PI1*exp(-1.0390726)
PI3 # Probabilidade de mulheres menores que 24 anos atribuir "+Import."


#Qualidade do ajuste
m1 = multinom(y~x1+x2+x3, weights = freq)


#Modelo Maximal ou Saturado
M_Max = multinom(y~x1*x2*x3, weights = freq)
L_max =logLik(M_Max)
L_max[1]

#Modelo Ajustado
L_m1 = logLik(m1)
L_m1[1]

#Modelo Mininal ou nulo

M_nulo = multinom(y~1, weights = freq)
L_nulo = logLik(M_nulo)
L_nulo[1]

#Deviance
D = 2*(L_max[1]-L_m1[1])
D

#graus de liberdade para o teste Chisquared (m-p)

#m: nº de observações em cada classe X nº de classes
6*3

#p:nº de parametros estimados
m1$edf

df = (6*3-m1$edf)

#Teste

1-pchisq(D,df) #Modelo valido!
# Não existe evidência contra a hipótese de que m1 seja
# tão bom quanto o modelo satarado, logo o modelo está ok!


#Residuos de pearson

fitted.prob = as.matrix(m1[["fitted.values"]][1:6,])
fitted.values = fitted.prob* c(45,45,60,65,44,41)

Res = (matrix(m1$weights,nrow = 6,ncol = 3)-fitted.values)/sqrt(fitted.values)
RP = round(sum(Res^2),3);RP

#Os graus de liberdade são os mesmo da deviance
1-pchisq(RP,df)

#Teste da razão de verossimilhança do modelo ajustado vs o modelo Nulo
C= 2*(L_m1[1]-L_nulo[1])
C
df_C =m1$edf - M_nulo$edf
1-pchisq(C,df_C)

# Mesmo sendo mais complexo, as evidências apontam que m1 apresenta
# ajuste diferente do modelo nulo, logo como m1 está validado, é
# rocomendável utilizá-lo.


#Pseudo R²

1-L_m1[1]/L_nulo[1]



# Outra opção menos complexa
x2 = rep(c(1:3),times = 6) #nesse caso, idade é não será um fator
summary(multinom(y~x1+x2, weights = freq))


View(a)
# Ex 2: Resp. do Tumor ao tratamento --------------------------------------

# Y com 4 categorias, sendo a piora a referência
# 1:-Piora, 2: Indiferente,
# 3:Melhora parcial e 4: Completa remissão
# O nº de linhas da tabela de contigência informa a
# quantidade de repetição de cada nível de Y

y = as.factor(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4)))

#Trat: Presença de tratamento, 1 se Sequential, 0 c.c.
trat = rep(c(rep(1,2),rep(0,2)),4)

#sex: sexo, 1 p/Homens e 0 c.c.
sex = rep(1:0,8)

#Frequencia
freq = c(28,04,41,12,
         45,12,44,07,
         29,05,20,03,
         26,02,20,01)
model = multinom(y~sex+trat, weights = freq)
a = summary(model)
coef=a$coefficients
se = a$standard.errors


#   (Intercept)        sex      trat
#           B02        B12       B13
# 2   -0.112579 0.09023528 0.6164483

#  B13 (beta 13)

exp(coef[1,2]) #odds de B13
exp(coef[1,2])-1 #o sinal indica redução!
exp(coef[1,2]+1.96*se[1,2]) # Lim. Sup. do IC de B13
exp(coef[1,2]-1.96*se[1,2]) # Lim. Inf. do IC de B13

# As chances de atribuição do grau "+Import." (PI3) para Homens são
# 56% menores do que para as mulheres, em relação ao grau
# "-Import." (PI1)





