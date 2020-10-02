rm(list = ls())

library(tidyverse)
library(lme4)
t <- 30
a <- 0.25
s <- 1000 
cutoff <- 0
R <- rnorm(t)
beta <- 0.01
m <- rnorm( s, 0.1, 0.2)
t_start <- ceiling( runif(s, 1, t-2) )
t_end <- ceiling( runif( s, t_start, t))
b <- runif( s, 0.1, 0.9)
X <- matrix( NA, s, t )

for( i in 1:s){ 
  X[ i , t_start[i] ] <- m[i]
}

for ( i in 2:t ) { 
  for( j in 1:s ) {   
    temp <- rnorm(1,  a + b[j]*X[j,i-1] + beta*R[i-1], 0.1)
    temp <- ifelse( i > t_end[j] , NA, temp )
    X[j, i] <- ifelse( !is.na(temp), temp, X[j, i])
  }
}


par(mfrow = c(2, 1))
plot( X[1, ], type = 'l', col = 1, ylim = c(0, 3))

for ( i in 2:s) { 
  points( X[i, ], type = 'l' , col = i )
}

plot( R, type = 'l', lty = 2)
par(mfrow = c(1,1))

sig_obs <- 0.1 

dat <- 
  data.frame(X = t(X) ) %>% 
  gather( plant, size, starts_with('X')) %>% 
  mutate( plant = as.numeric(factor( plant ))) %>% 
  group_by( plant) %>% 
  mutate( t = row_number()) %>% 
  rowwise() %>% 
  mutate( size_obs = rnorm( 1, size, sig_obs )) %>% 
  mutate( size_obs = max( size_obs, cutoff)) %>% 
  group_by( plant) %>% 
  arrange( plant, t) %>% 
  mutate( size0 = lag(size), size_obs0 = lag(size_obs)) %>% 
  mutate( R0 = lag(R), R1 = lag(R, 2)) 

m1 <- lm( data = dat, size ~ size0 + R0 + R1)
summary(m1)

m2 <- lm( data = dat, size_obs ~ size_obs0 + R0 + R1)
summary(m2)


mer <- lmer( data = dat, size ~ size0 + R0 + R1 + (size0|plant) )
summary( mer )


