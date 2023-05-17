data(mtcars)
# Ajustar modelo lineal sin transformación
modelo1 <- lm(mpg ~ wt, data = mtcars)

# Imprimir resumen del modelo
summary(modelo1)
# Aplicar transformación Box-Cox
bc_mpg <- MASS::boxcox(mpg ~ wt, data = mtcars)

# Seleccionar la transformación óptima
lambda <- bc_mpg$x[which.max(bc_mpg$y)]

# Ajustar modelo lineal con transformación
modelo2 <- lm((mpg^lambda - 1) / lambda ~ wt, data = mtcars)

# Imprimir resumen del modelo
summary(modelo2)
plot(modelo1,which =1)
plot(modelo2,which =1)
