
xbar <- 284.3
s <- 127.5
alfa <- .05
n <-52
# H0 mu=3000
# H1 mu <> 300

zcalc <-  (xbar - 300)/(s/sqrt(n))

zcalc
(ztablas <- qnorm(.025,lower.tail = F))

# Como zcalc < ztablas entonces NO rechazamos H0

#######
tvida <- c(2.0, 1.3, 6.0, 1.9, 5.1, 0.4, 1.0, 5.3,
           15.7, 0.7, 4.8, 0.9, 12.2, 5.3, 0.6, 3.6,
           10.2, 5.3, 8.5, 9.4, 12.8, 14.0, 9.6, 4.5,
           10.4, 4.5, 8.7, 11.5, 14.5, 6.5, 15.0, 7.8)

t.test(x=tvida,
       mu=10,
       alternative = "two.sided",
       conf.level = .90)
# hotdogs

hotdogs= c(25.2, 21.3, 22.8, 17, 29.8, 21, 25.5, 
           16, 20.9, 19.5)
t.test(hotdogs, 
       alternative= "two.sided", 
       conf.level=.95, 
       mu = 15 )
# Proporciones

prop.test(x=80,
          n=8640,
          p=.01,
          alternative = "less",
          conf.level = .95)
# varianza

(edades_al_morir <- c(80,90,85,85,75,58,70,84,87,81,87,61,73,84,85,
                      70,78,95,77,52))

if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/stests', force=TRUE)
#library(stests)
stests::var.test(edades_al_morir,null.value = 100, 
                 alternative = "greater", conf.level=0.95) 

# medias de dos poblaciones

mortero_modificado =c(16.85, 16.40, 17.20, 16.35, 16.52,
                      17.04, 16.96, 17.15, 16.59, 16.57)
mortero_no_modificado = c(17.50, 17.63, 18.25, 18.00, 17.86,
                          17.75, 18.22, 17.90, 17.96, 18.15)
t.test(mortero_modificado,
       mortero_no_modificado,
       alternative = "two.sided")


tablamortero<-rbind(cbind(1,mortero_modificado),
cbind(2,mortero_no_modificado))
colnames(tablamortero)<- c("indicadora","resistencia")


t.test(resistencia ~ indicadora,
       data=tablamortero,
       alternative = "two.sided")
# pareados
FONDO <- c(0.430, 0.266, 0.567, 0.531, 0.707, 0.716)
SUPERFICIE<- c( 0.415, 0.238, 0.390, 0.410, 0.605, 0.609)
t.test(FONDO,SUPERFICIE,
       alternative = "two.sided",
       paired = TRUE,conf.level = .99)
