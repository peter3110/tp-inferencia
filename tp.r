
library(ggplot2)
library(rjags)
library(mcmcplots)

# EJ1a
jagsscript = cat(" 
model {
    # Priors
    A ~ dbeta(priornocargada,priornocargada)  # no cargada
    B ~ dbeta(priorcargada,priorcargada)  # cargada
    alpha ~ dunif(0,cantMonedas)
    for(i in 1:cantMonedas) {
      p[i] = (alpha < i && i <= alpha + 1)
      theta[i] = p[i] * B + (1 - p[i]) * A
    }

    # Likelihood
    for(i in 1:cantMonedas) {
      m[i] ~ dbinom(theta[i],cantLanzamientos)
    }
}                  
", file="modelo1a.txt")

# EJ1b
jagsscript = cat("
model {
   dMonedaCargada ~ dbeta(priorcargada,priorcargada) # ---> para extraer la distrib posterior de la moneda cargada
   alpha ~ dcat(probs_cargada_categorica)   # ---> vector con (1/cantMonedas, 1/cantMonedas, ...) 
   for (i in 1:cantMonedas) {
     theta[i] = (alpha==i) * dMonedaCargada + (1 - (alpha==i)) * x[i] 
     x[i] ~ dbeta(priornocargada,priornocargada) # variable auxiliar
   }
   # Likelihood
   for(i in 1:cantMonedas) {
    m[i] ~ dbinom(theta[i], cantLanzamientos)
   }
}       
", file = "modelo1b.txt")

# EJ2a
jagsscript = cat("
model {
  # Priors
  alpha ~ dbern(1./2.) # 1 sii cada moneda esta cargada
  for (i in 1:cantMonedas) {
    x[i] ~ dbeta(priorcargada,priorcargada)     # var auxiliar
    y[i] ~ dbeta(priornocargada,priornocargada) # var auxiliar
    theta[i] = (alpha==1) * x[i] + (alpha!=1) * y[i]
  }
  # Likelihood
  for(i in 1:cantMonedas) {
    m[i] ~ dbinom(theta[i], cantLanzamientos)
  }
}
", file = "modelo2a.txt")

# ejemplos (con nchains = 5)
# { resultados = c(5,3,10), priorcargada=.5, priornocargada=100}
# { resultados = c(5,3,0), priorcargada=.5, priornocargada=100}
# { resultados = c(4,6,3), priorcargada=.5, priornocargada=100}
# { resultados = c(4,6,3), priorcargada=.5, priornocargada=10}
# { resultados = c(4,6,3), priorcargada=.001, priornocargada=10}
# { resultados = c(4,6,1), priorcargada=.001, priornocargada=10}
# Experimentacion
niter <- 5000
nchains <- 3
cantMonedas <- 3
cantLanzamientos <- 10
resultados <- c(4,6,1)
priorcargada <- .01
priornocargada <- 10

# Modelos
mod_1a = jags.model('modelo1a.txt', data = list('m' = resultados, 'cantMonedas' = cantMonedas,
                    'cantLanzamientos' = cantLanzamientos, 'priorcargada' = priorcargada,
                    'priornocargada' = priornocargada), 
                    n.chains = nchains)
samples.1a <- coda.samples(mod_1a, c('theta'), n.iter = niter)
plot(samples.1a)

####################

mod_1b = jags.model('modelo1b.txt', data = list('m' = resultados, 'cantMonedas' = cantMonedas,
                    'cantLanzamientos' = cantLanzamientos, 'priorcargada' = priorcargada,
                    'priornocargada' = priornocargada,
                    'probs_cargada_categorica' = rep(1./cantMonedas, cantMonedas) ),
                    n.chains = nchains)
samples.1b <- coda.samples(mod_1b, c('theta'), n.iter = niter)
plot(samples.1b)

####################

mod_2a = jags.model('modelo2a.txt', data=list('m' = resultados, 'cantMonedas' = cantMonedas,
                    'cantLanzamientos' = cantLanzamientos, 'priorcargada' = priorcargada,
                    'priornocargada' = priornocargada),
                    n.chains = nchains)
samples.2a <- coda.samples(mod_2a, c('theta'), n.iter = niter)
plot(samples.2a)
