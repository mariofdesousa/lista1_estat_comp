rm(list=ls())
## Questão 1
#############################################


#Gerar numeros aleatorios da distribuicao de pareto(2,2) e comparar com a densidade verdadeira

d_pareto <- function(x,a,b){(a*(b^a))/x^(a+1)}
r_pareto <- function(n,a,b){
  u    <- runif(n)
  rpar <- b/((1-u)^(1/a))
}

hist(r_pareto(1000,2,2),prob=TRUE)
vseq <- seq(2,1000,.1)
vdens <- d_pareto(vseq,2,2)
lines(vseq,vdens,col="red")


# histrograma de uma amostra da Pareto(2,2)
hist(r_pareto(100000,2,2),prob=TRUE,breaks=100,main="Histograma - Pareto(2,2)",
     ylab="Densidade",xlab="")
# Adicionando a densidade verdadeira ao histograma
vxseq = seq(2,1000,.1)
vdens = d_pareto(vxseq,2,2)
lines(vxseq,vdens,type="l",col=2,lwd=2)
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
hist(r_epach(100000),prob=TRUE,main="Histograma - núcleo de Epanechnikov",
     ylim=c(0,1),ylab="Densidade",xlab="")
curve(d_epach, xlim=c(-1,1),add=TRUE,col=2,lwd=2)
###############################################
rm(list=ls())
## Questão 4
n  <- 50
p2 <- numeric(n)
rep <- 5000
G   <- numeric(rep)
k <- 1
xdec <- seq(0,1,.1)
######################################
while(k < rep){ 
    x <- rlnorm(n,mean=10,sdlog = 1)
   p1    <- (1/((n^2)*mean(x)))
  xs <- sort(x)
    for(i in 1:n){
    p2[i] <- ((2*i) - n - 1)*xs[i]    
    }
   k <- k+1
G[k] <- p1*sum(p2)
}

Gstatln <- rbind(mean(G),median(G))
quantile(G, probs = seq(0, 1, 0.1), na.rm = FALSE,
          names = TRUE, type = 7)
print(Gstatln)
#######################################
rm(k)
k <- 1

while(k < rep){ 
  x <- runif(n,min=10,max = 20)
  p1    <- (1/((n^2)*mean(x)))
  xs <- sort(x)
  for(i in 1:n){
    p2[i] <- ((2*i) - n - 1)*xs[i]    
  }
  k <- k+1
  G[k] <- p1*sum(p2)
}
Gstatunif <- rbind(mean(G),median(G))
quantile(G, probs = seq(0, 1, 0.1), na.rm = FALSE,
         names = TRUE, type = 7)
print(Gstatunif)
############################################  
rm(k)
k <- 1

while(k < rep){ 
  x <- rbinom(n,size=1,prob = 0.1)
  p1    <- (1/((n^2)*mean(x)))
  xs <- sort(x)
  for(i in 1:n){
    p2[i] <- ((2*i) - n - 1)*xs[i]    
  }
  G[k] <- p1*sum(p2)
  
  if(is.nan(G[k])==FALSE){
    k <- k+1
    G[k] <- p1*sum(p2)
  }
  
else{
   k <- k
}
}

Gstatbern <- rbind(mean(G),median(G))
quantile(G, probs = seq(0, 1, 0.1), na.rm = FALSE,
         names = TRUE, type = 7)
print(Gstatbern)

###############################################
rm(list=ls())
## Questão 6
#############################################
n      <- 20
rep    <- 100000
alpha  <- 0.05

#a)chi^2
ttestchi  <-  replicate(rep,{
  x = rchisq(n, 1)
  (mean(x)-1)/(sd(x)/sqrt(n))
})
Ind <- ttestchi > qt(1-alpha, n-1)
mean(Ind)

#b)
ttestunif  <-  replicate(rep,{
  x = runif(n,0,2)
  (mean(x)-1)/(sd(x)/sqrt(n))
})
Ind <- ttestunif > qt(1-alpha, n-1)
mean(Ind)

#c)
ttestexp  <-  replicate(rep,{
  x = rexp(n,1)
  (mean(x)-1)/(sd(x)/sqrt(n))
})
Ind <- ttestexp > qt(1-alpha, n-1)
mean(Ind)

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
#Questão 10

# Na primeira resolução iremos supor que sabemos gerar de uma normal truncada e fazer o método de importância
rm(list=ls())

MCNormTruncX2 <- function(n,lower=1,upper=Inf,media=0,dp=1){

if("truncnorm" %in% rownames(installed.packages())==FALSE){install.packages("truncnorm");library(truncnorm)}  else library(truncnorm)
  
rtn      <- rtruncnorm(n=n,a=lower,b=upper,mean=media,sd=dp)
int1     <- ((rtn^2)*dnorm(x=rtn,mean=media,sd=dp))/dtruncnorm(x=rtn,a=lower,b=upper,mean=media,sd=dp)
impMC1   <- mean(int1)
varinf1  <- var(int1)/n

result   <- list(thetahat = impMC1,
                 varhat   = varinf1,
                 int      = int1)
return(result)
}

rbind(MCNormTruncX2(n=100),MCNormTruncX2(n=1e4),MCNormTruncX2(n=1e6))


# Agora vamos utilizar uma distribuicao exponencial truncada
# Para gerar da exponencial truncada sera utilizado o metodo da inversao


