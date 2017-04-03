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

