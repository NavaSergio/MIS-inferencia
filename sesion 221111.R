
install.packages("infer")
library(infer) 


(ttablas = qt(.975,9))
 
 
 (tcalculado = (21.9-15)/1.31)
 zcalculado =  (promedio.muestra - promedio.hipotetico) / (sd.muestra/sqrt(n))
 
 
 dt
 pt
 qt
 
 
 curve(dt(x,df=9),-5,5)
 curve(pt(x,df=9),-5,5)
 curve(qt(x,df=9),0,1)
 
 
 hotdogs= c(25.2, 21.3, 22.8, 17, 29.8, 21, 25.5, 16, 20.9, 19.5)
 t.test(hotdogs, alternative= "two.sided", conf.level=.95, mu = 15 )
 #############################
 (promedio.muestra = 80/8640)
 (sd.p = sqrt(promedio.muestra *(1-promedio.muestra)))
 
  (promedio.muestra - 0.01)/(sd.p/sqrt(8640))
 prop.test(80,8640, p=.01, alternative = "less")
 
 qnorm(.05)
 
curve(dnorm(x),-5,5)
 
 zcalculado =  (promedio.muestra - promedio.hipotetico) / (sd.muestra/sqrt(n))
 ########################3
 
 (edades_al_morir <- c(80,90,85,85,75,58,70,84,87,81,87,61,73,84,85,
                       70,78,95,77,52))

 (estadistico  = (length(edades_al_morir)-1)*var(edades_al_morir)/100)
 
 (chi.tablas = qchisq(df=19, .95))
 
 curve(dchisq(x,df=19),0,40)
 
 if (!require('devtools')) install.packages('devtools')
 devtools::install_github('fhernanb/stests', force=TRUE)
 library(stests)
 stests::var.test(edades_al_morir,null.value = 100, 
                  alternative = "greater", conf.level=0.95) 
1-pchisq(estadistico,df=19)
pchisq(estadistico,df=19,lower.tail = F) 


####################
mortero_modificado =c(16.85, 16.40, 17.20, 16.35, 16.52,
                      17.04, 16.96, 17.15, 16.59, 16.57)
mortero_no_modificado = c(17.50, 17.63, 18.25, 18.00, 17.86,
                          17.75, 18.22, 17.90, 17.96, 18.15)
t.test(mortero_modificado,mortero_no_modificado,
       alternative = "two.sided")

t.test(mortero_modificado,mortero_no_modificado,
       alternative = "two.sided",paired = TRUE)

#########################

peso.inicial <-c(165, 143, 175, 135, 148, 155, 158, 140,
                 172, 164, 178, 182, 190, 169, 157)
peso.final <- c(145, 137, 170, 136, 141, 138, 137, 125,
                161, 156, 165, 170, 176, 154, 143)
t.test(peso.inicial, peso.final,paired = T)
t.test(peso.inicial-peso.final)

##############################
mortero_modificado =c(16.85, 16.40, 17.20, 16.35, 16.52,
                      17.04, 16.96, 17.15, 16.59, 16.57)
mortero_no_modificado = c(17.50, 17.63, 18.25, 18.00, 17.86,
                          17.75, 18.22, 17.90, 17.96, 18.15)
var.test(mortero_no_modificado,mortero_modificado)

