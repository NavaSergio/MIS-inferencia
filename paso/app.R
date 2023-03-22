library(shiny)
library(ggplot2)

# Define las funciones a graficar
x2 <- function(x) { x^2 }
logx <- function(x) { log(x) }
expx <- function(x) { exp(x) }
x <- function(x) { x }
sinx <- function(x) { sin(x) }

# Define la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Gráfico de X vs Función"),
  
  sidebarLayout(
    sidebarPanel(
      # Añade un selectInput para seleccionar la función
      selectInput(inputId = "funcion", label = "Seleccione la función:",
                  choices = c("X^2", "ln(X)", "exp(X)", "X", "sin(X)"))
    ),
    
    mainPanel(
      # Añade un plotOutput para mostrar el gráfico
      plotOutput(outputId = "grafico")
    )
  )
)

# Define el servidor
server <- function(input, output) {
  # Crea un vector de valores de x
  x <- seq(-10, 10, length.out = 100)
  
  # Crea una reactividad para actualizar el gráfico cuando se seleccione una función
  funcion_react <- reactive({
    switch(input$funcion,
           "X^2" = x2(x),
           "ln(X)" = logx(x),
           "exp(X)" = expx(x),
           "X" = x(x),
           "sin(X)" = sinx(x))
  })
  
  # Renderiza el gráfico con ggplot2
  output$grafico <- renderPlot({
    ggplot(data.frame(x = x, y = funcion_react()), aes(x = x, y = y)) +
      geom_line()
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
