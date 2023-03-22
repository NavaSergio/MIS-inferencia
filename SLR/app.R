simula_datos <- function(n, b0, b1, varianza){
  set.seed(123) # Fijar semilla para reproducibilidad
  
  x <- runif(n, min = 0, max = 10) # Generar valores aleatorios para X
  y <- b0 + b1*x + rnorm(n, mean = 0, sd = varianza) # Simular Y
  
  # Ajustar modelo de regresión lineal simple
  modelo <- lm(y ~ x)
  
  # Devolver lista con resultados
  return(list(x = x, y = y, modelo = modelo))
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
                  choices = c("X^2", "ln(X)", "exp(X)", "X", "sin(X)"))
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
      y <- (b0 + b1*x + rnorm(n, mean = 0, sd = varianza))^2
    } else if (input$funcion == "ln(X)") {
      y <- log(b0 + b1*x + rnorm(n, mean = 0, sd = varianza))
    } else if (input$funcion == "exp(X)") {
      y <- exp(b0 + b1*x + rnorm(n, mean = 0, sd = varianza))
    } else if (input$funcion == "X") {
      y <- b0 + b1*x + rnorm(n, mean = 0, sd = varianza) # Simular Y
    } else if (input$funcion == "sin(X)") {
      y <- sin(b0 + b1*x + rnorm(n, mean = 0, sd = varianza))
    }
    
    
    # Ajustar modelo de regresión lineal simple
    modelo <- lm(y ~ x)
     return(list(x = x, y = y, modelo = modelo))
  })
  
  # Mostrar diagrama de dispersión con la línea ajustada
  output$dispersion <- renderPlot({
    datos <- datos()
    plot(datos$x, datos$y, main = "Diagrama de dispersión")
    abline(datos$modelo, col = "red")
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
    names <- rownames(d)
    rownames(d) <- NULL
    data <- cbind(names,d)
    data
  })
  output$summary <- renderTable({
    datos <- datos()
    d <- summary(datos$modelo)$coefficients
    names <- rownames(d)
    rownames(d) <- NULL
    data <- cbind(names,d)
    data
    
  })
  
}


shinyApp(ui = ui, server = server)

