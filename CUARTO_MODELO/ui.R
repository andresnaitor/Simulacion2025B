library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel(
    fluidRow(
      column(width=2),
      column(width=10, h1("Aplicativo Web - Andrés Báez - Christian Guanoluisa"))
    )
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=3,
                 numericInput("num",
                              "Número de variables:",
                              min=1,
                              max=500,
                              value=20),
                 radioButtons("metodo",
                              "seleccione el método:",
                              c("Congruencial Multiplicativo",
                                "Congruencial Mixto",
                                "Cuadrados Medios",
                                "Lehmer")),
                 hr(),
                 h3("Parámetros"),
                 numericInput("x0", "Semilla inicial:", min=99, max= 2^31-1, value=151),
                 numericInput("a", "Constante a:", min=1, max= 2^31-1, value=31),
                 numericInput("m", "Constante m:", min=1, max= 2^31-1, value=73),
                 numericInput("c", "Constante c:", min=1, max= 2^31-1, value=97),
                 hr(),
                 actionButton("simular", "Simular", icon=icon("circle-play"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      navbarPage(icon("book"),
                 tabPanel("Números Aleatorios",
                          verbatimTextOutput("code"),
                          plotOutput("grafico"),
                          verbatimTextOutput("resumen"),
                          hr(),
                          
                          conditionalPanel(condition = "input.simular == 1",
                                           h3("Cálculo de Integrales"),
                                           fluidRow(
                                             column(4,textInput("func", "Ingrese la función que desea evaluar")),
                                             column(4,numericInput("linf", "Límite inferior", min = 0, max = 10, value = 0)),
                                             column(4,numericInput("lsup", "Límite superior", min = 0, max = 10, value = 1))
                                           ),
                                           fluidRow(
                                             column(6, selectInput("tipo_integral", "Tipo de intervalo:",
                                                                   choices = c("Finito", "[0, ∞)", "(-∞, ∞)", "[a, ∞)")))
                                           ),
                                           plotOutput("graficofun"),
                                           h3("El resultado de la integral es:"),
                                           verbatimTextOutput("integral")
                          )
                 ),
                 tabPanel("Variables Aleatorias Discretas",
                          h3("Generación por Transformada Inversa"),
                          fluidRow(
                            column(6, textInput("vals", "Valores separados por comas:", "1,2,3")),
                            column(6, textInput("probs", "Probabilidades separadas por comas:", "0.2,0.5,0.3"))
                          ),
                          fluidRow(
                            column(6, numericInput("nval", "Número de simulaciones:", min=1, max=1000, value=20)),
                            column(6, actionButton("sim_discretas", "Simular Inversa", icon=icon("dice")))
                          ),
                          verbatimTextOutput("discretas_out"),
                          plotOutput("discretas_plot"),
                          
                          hr(),
                          h3("Generación de Variables Geométricas"),
                          fluidRow(
                            column(6, numericInput("p_geom", "Probabilidad de éxito p:", min=0.01, max=1, value=0.2)),
                            column(6, numericInput("n_geom", "Número de simulaciones:", min=1, max=1000, value=20)),
                            column(6, actionButton("sim_geom", "Simular Geométrica", icon=icon("dice")))
                          ),
                          verbatimTextOutput("geom_out"),
                          plotOutput("geom_plot"),
                          
                          hr(),
                          h3("Generación de Variables Poisson"),
                          fluidRow(
                            column(6, numericInput("lambda", "Valor de λ (lambda):", min=0.01, max=100, value=2)),
                            column(6, numericInput("n_poisson", "Número de simulaciones:", min=1, max=1000, value=20)),
                            column(6, actionButton("sim_poisson", "Simular Poisson", icon=icon("dice")))
                          ),
                          verbatimTextOutput("poisson_out"),
                          plotOutput("poisson_plot"),
                          
                          hr(),
                          h3("Generación de Variables Binomiales"),
                          fluidRow(
                            column(6, numericInput("n_binom", "Número de ensayos n:", min=1, max=100, value=10)),
                            column(6, numericInput("p_binom", "Probabilidad de éxito p:", min=0.01, max=1, value=0.5)),
                            column(6, numericInput("nval_binom", "Número de simulaciones:", min=1, max=1000, value=20)),
                            column(6, actionButton("sim_binom", "Simular Binomial", icon=icon("dice")))
                          ),
                          verbatimTextOutput("binom_out"),
                          plotOutput("binom_plot")
                 ),
                 tabPanel("Variables Aleatorias Continuas")) #Menu navegable
    )
  )
)
