simula_datos <- function(n, b0, b1, varianza){
  set.seed(123) # Fijar semilla para reproducibilidad
  
  x <- runif(n, min = 0, max = 10) # Generar valores aleatorios para X
  y <- b0 + b1*x + rnorm(n, mean = 0, sd = varianza) # Simular Y
  
  # Ajustar modelo de regresión lineal simple
  modelo <- lm(y ~ x)
  
  # Devolver lista con resultados
  return(list(x = x, y = y, modelo = modelo))
}
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))+
    theme_classic()
}

ui <- fluidPage(
  titlePanel("Regresión lineal simple"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Número de puntos", value = 50, min = 10, max = 1000),
      numericInput("b0", "Intercepto", value = 0, min = -10, max = 10),
      numericInput("b1", "Pendiente", value = 1, min = -10, max = 10),
      numericInput("varianza", "Varianza", value = 1, min = 1, max = 30, step = 1),
      selectInput("funcion", "Selecciona una función:",
                  choices = c("X", "ln(X)", "exp(X)", "X^2", "sin(X)")),
      downloadButton('downloadData', 'Descargar archivo .CSV')
    ),
    mainPanel(
      plotOutput("dispersion"),
      tableOutput("summary"),
      tableOutput("anova"),
      plotOutput("diag")
    )
  )
)


server <- function(input, output){
  # Simular datos y ajustar modelo
  datos <- reactive({
    #simula_datos(input$n, input$b0, input$b1, input$varianza)
    set.seed(123) # Fijar semilla para reproducibilidad
    n <- input$n
    b0 <- input$b0
    b1 <- input$b1
    varianza <- input$varianza
    x <- runif(n, min = 1.1, max = 10) # Generar valores aleatorios para X
    if (input$funcion == "X^2") {
      y <- (b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(varianza)))^2
    } else if (input$funcion == "ln(X)") {
      y <- log(b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(varianza)))
    } else if (input$funcion == "exp(X)") {
      y <- exp(b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(varianza)))
    } else if (input$funcion == "X") {
      y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(varianza)) # Simular Y
    } else if (input$funcion == "sin(X)") {
      y <- sin(b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(varianza)))
    }
    
    
    # Ajustar modelo de regresión lineal simple
    modelo <- lm(y ~ x)
     return(list(x = x, y = y, modelo = modelo))
  })
  
  # Mostrar diagrama de dispersión con la línea ajustada
  output$dispersion <- renderPlot({
    datos <- datos()
    #plot(datos$x, datos$y, main = "Diagrama de dispersión")
    #abline(datos$modelo, col = "red")
    ggplotRegression(datos$modelo)
  })
  output$diag <- renderPlot({
    datos <- datos()
    par(mfrow=c(2,2))
    plot(datos$modelo)
    par(mfrow=c(1,1))
  })
  
  
  # Mostrar tabla del ANOVA del modelo
  output$anova <- renderTable({
    datos <- datos()
    d <- anova(datos$modelo)
    d
  },digits=3,rownames = TRUE,na = "")
  output$summary <- renderTable({
    datos <- datos()
    d <- summary(datos$modelo)$coefficients
    d
  },digits=3,rownames = TRUE)
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('Datos', '.csv', sep='') 
    },
    content = function(file) {
      misdatos <- data.frame(x=datos()$x,y=datos()$y)
      write.csv(misdatos, file,na="",row.names = FALSE)
    }
  )
}


shinyApp(ui = ui, server = server)

