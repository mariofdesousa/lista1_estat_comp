rm(list=ls())
## Questão 1
#############################################

# função densidade Pareto(a,b)
d_pareto = function(x,a,b) a*((b/x)^a)/x
# função para gerar variáveis aleatórias com distribuição Pareto(a,b)
r_pareto = function(n,a,b){
  vu = runif(n)
  x = b/(vu^(1/a))
  return(x)
}

# histrograma de uma amostra da Pareto(2,2)
hist(r_pareto(100000,2,2),prob=TRUE)
# Adicionando a densidade verdadeira ao histograma
vxseq = seq(2,1000,.1)
vdens = d_pareto(vxseq,2,2)
lines(vxseq,vdens,type="l",col=2)
# Vale destacar que a variância para a Pareto(2,2) é infinita

rm(list=ls())
## Questão 3
#############################################
# densidade Epanechnikov
d_epach = function(x) 3*(1 - x^2)/4
# função para gerar variáveis aleatórias com distribuição Epanechnikov
r_epach = function(n){
  v = rep(0,n)
  for(i in 1:n){
    u1 = runif(1,-1,1); u2 = runif(1,-1,1); u3 = runif(1,-1,1);
    if((abs(u3)>abs(u2))&&(abs(u3)>abs(u1))){
      v[i] = u2
    }else{
      v[i] = u3
    }
  }
  return(v)
}

# histograma de uma amostra da Epanechnikov e a densidade verdadeira
hist(r_epach(100000),prob=TRUE)
curve(d_epach, xlim=c(-1,1),add=TRUE,col=2)


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

