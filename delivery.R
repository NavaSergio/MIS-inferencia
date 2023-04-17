# regresión lineal múltiple

install.packages("robustbase")

data(delivery,package = "robustbase")
delivery2 <-delivery
delivery2$distance <- delivery$distance/10
str(delivery)
plot(delivery)



modelo <- lm(delTime ~ n.prod + distance,data= delivery)
modelo
summary(modelo)
anova(modelo)

summary(lm(delTime ~ n.prod + distance,data= delivery2))

X <- as.matrix(cbind(rep(1,25),delivery[,1:2]))
y <- delivery$delTime

solve(t(X)%*%X)  %*%t(X) %*% y


completo <- lm(delTime ~ n.prod + distance,data= delivery)
completo
summary(completo)
anova(completo)


reducido <- lm(delTime ~ n.prod ,data= delivery)
reducido
summary(reducido)
anova(reducido)


anova(reducido,completo)

confint(completo)
datosnuevos <- data.frame(n.prod=8,distance=275)
predict(completo,datosnuevos)
predict(completo,datosnuevos,interval = "confidence")



confint(reducido)
datosnuevos.reducido <- data.frame(n.prod=8)
predict(reducido,datosnuevos.reducido)
predict(reducido,datosnuevos.reducido,interval = "confidence")

#### Predicción

predict(completo,datosnuevos,interval = "prediction")
predict(reducido,datosnuevos.reducido,interval = "prediction")
