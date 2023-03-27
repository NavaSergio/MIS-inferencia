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
