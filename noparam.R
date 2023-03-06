muestraX <- c( 1.1, 3.4, 4.3, 2.1, 7.0 , 2.5 )
muestraY <- c( 7.0, 8.0, 3.0, 5.0, 6.2 , 4.4 )


MX= data.frame(x="X",Datos=muestraX)

MY= data.frame(x="Y",Datos=muestraY)
df = rbind(MX,MY)
library(tidyverse)

df <-df %>% mutate(rango=rank(ties.method = "average",Datos))


wilcox.test(x = muestraX, y = muestraY, alternative = "two.sided", mu = 0,
            paired = FALSE, conf.int = 0.95)


wilcox.test(data=df, Datos~x, alternative = "two.sided", mu = 0,
            paired = FALSE, conf.int = 0.95)


#####
datos <- data.frame(
  condicion = c(rep("condicion1", 18), rep("condicion2", 18), rep("condicion3", 18)),
  n_huevos = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 27, 28, 29, 30, 51, 52, 53, 342, 40,
               41, 42, 43, 44, 45, 46, 47, 48, 67, 88, 89, 90, 91,92, 93, 94, 293,
               19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 25, 36, 37, 58, 59, 60, 71, 72)
)
head(datos)



datos <-datos %>% mutate(rango=rank(ties.method = "average",n_huevos))

plot(datos$n_huevos,datos$condicion)


datos %>% ggplot(aes(x=as.factor(condicion),y=n_huevos)) +
  geom_boxplot()


aggregate(n_huevos ~ condicion, data = datos, FUN = median)


datos %>% group_by(condicion) %>%
  summarise(med=median(n_huevos))
  


datos %>% group_by(condicion) %>%
  summarise(sumrank=sum(rango))

kruskal.test(n_huevos ~ condicion, data = datos)



  