# regresión lineal múltiple

install.packages("robustbase")

data(delivery,package = "robustbase")

str(delivery)
modelo <- lm(delTime ~ n.prod + distance,data= delivery)
modelo
summary(modelo)
