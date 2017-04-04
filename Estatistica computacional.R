rm(list=ls())
## Questão 7
#############################################
n      <- 1e6
rn     <- runif(n,0,pi/3)
sinrn  <- sin(rn)
estMC  <- ((pi/3)-0)*mean(sinrn)
print(estMC)

vv <- 1/2  ## valor verdadeiro

## Comparando os valores

erro = abs(estMC - vv)
print(erro)
## 2.891086e-06

##############################################

rm(list=ls())
## Questão 9
#############################################
n          <- 500000
rn         <- runif(n)
integral   <- ((exp(-rn))/(1+(rn^2)))
MCconv     <-  mean(integral) ##metodo convencional
varconv    <- (1/n) * sum((MCconv-integral)^2)

rn1        <- 1-rn
int1       <- integral
int2       <- ((exp(-rn1))/(1+(rn1^2)))
MCant      <- mean((int1 + int2)/2)  
varant     <- var(int1 + int2) ## Metodo das variaveis antiteticas

## Comparando os valores

vargain = ((varconv - varant)/varconv)
print(vargain)
## 0.927129 reducao de 92.71% da variancia

##############################################

