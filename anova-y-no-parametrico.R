insectos <- c(16,11,20,21,14,7,37,32,15,25,39,41,21,12,14,17,13,17,45,59,48,46,38,47) 
colores <- as.factor(c(rep(c("azul", "verde", "blanco", "amarillo"), each =6)))
datos=data.frame(insectos,colores)
boxplot(insectos ~ colores, col = c("yellow", "blue", "white","green"), ylab = "Número de insectos atrapados")
tapply(insectos, colores, mean)
modelo = lm(insectos~colores, data=datos)
anova(modelo)
modelo.aov=aov(modelo)
summary(modelo.aov)
par(mfrow=c(2,2))
plot(modelo.aov)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(modelo)
par(mfrow=c(1,1))


TUKEY <- TukeyHSD(x=modelo.aov, 'colores', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")




library(MASS)
dt=immer
head(immer)

wilcox.test(immer$Y1,immer$Y2,paired=TRUE)
summary(dt)
mean(dt$Y1>dt$Y2)


wilcox.test(mpg ~ am, data=mtcars)

ggplot (mtcars,aes(x=mpg,fill=factor(am)))+
  geom_density(alpha=0.4)

kruskal.test(Ozone ~ Month, data=airquality)
ggplot (airquality,aes(x=Ozone,fill=factor(Month)))+
  geom_density(alpha=0.4)



valoracion <- c( 9, 5, 2, 6, 3, 1, 5, 5, 5, 11, 5, 1, 8, 4, 3, 10, 4, 1, 7, 3, 4 ) 
hora <- factor( rep( c( "mañana", "tarde", "noche" ), 7 ) ) 
sujeto <- factor( rep( 1:7, each = 3 ) ) 
datos <- data.frame( valoracion, hora, sujeto )
head(datos)
by(data = datos$valoracion, INDICES = datos$hora, FUN = median)
friedman.test(valoracion, hora, sujeto)
friedman.test(valoracion ~ hora | sujeto ,data=datos)
ggplot (datos,aes(x=valoracion,fill=factor(hora)))+
  geom_density(alpha=0.4)
