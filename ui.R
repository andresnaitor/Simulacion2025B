library(shiny)

# Define UI for application that draws a histogram
fluidPage(



      titlePanel(fluidRow(
          column(3,tags$img(src="foto01.png", width="90px", height="80px"),
                 tags$p("Andres Báez y Cristian Guanoluisa",
                        style="color:#461273; font-size:0.8em; font-family:medieval; margin-top:5px;")),
          column(9,h1("Trabajo aplicativo en grupo",
                    style="text-align:right; color:#461273; padding:18px; font-size:1.1em; font-family:medieval")
        ))),


    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Cargar archivo Excel", accept = ".xlsx"),
        uiOutput("sheet_ui"),
        actionButton("analizar", "Carga datos"),
        hr(),
        selectInput("colx", "Seleccione el color:", c("Cyan" = "cyan", "Morado" = "purple", "Amarillo" = "yellow")),
        hr(),
        downloadButton("descargar_pdf", "Informe PDF")
      ),

      mainPanel(
        tags$style("h1 {color: #1D8B91; font-family: medieval; font-size:1.7em}"),
        navbarPage("Análisis",
                   tabPanel("Datos",
                            h1("Registros de la base de datos"),
                            tableOutput("TablaVariable"),
                            hr(),
                            h1("Fuente de ingresos y total de gastos por Rango de edad"),
                            tableOutput("CreditosXProv"),
                            hr(),
                            h1("Información sobre créditos"),
                            tableOutput("Tabla3")
                   ),
                   tabPanel("Gráficos",
                            fluidRow(
                              column(12,
                                     h1("Gráfico de barras"),
                                     uiOutput("VarCualitativa"),
                                     plotOutput("Grafico01")
                              ),
                              column(12,
                                     h1("Gráfico Dispersión")
                              ),
                              column(6, uiOutput("VarCuantitativa")),
                              column(6, uiOutput("VarCuantitativa2")),
                              column(12,
                                     plotOutput("Grafico02")
                              ),
                              column(12,
                                     h1("Gráfico de pastel"),
                                     uiOutput("VarCualitativa"),
                                     plotOutput("Grafico03")
                              )
                            )
                   ),
                   
                   tabPanel("Informe")
        )
      )
    )
)
