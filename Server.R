library(shiny)
library(tidyverse)
library(readxl)
library(kableExtra)
library(formattable)
library(quarto)

function(input, output, session) {
  
  # ---- Carga de datos ----
  archivo <- reactive({
    req(input$file)
    path <- input$file$datapath
    excel_sheets(path)
  })
  
  output$sheet_ui <- renderUI({
    req(archivo())
    selectInput("hoja", "Seleccionar hoja:", choices = archivo())
  })
  
  datos <- eventReactive(input$analizar, {
    req(input$file, input$hoja)
    read_excel(input$file$datapath, sheet = input$hoja)
  })
  
  # ---- Variables dinámicas ----
  output$VarCualitativa <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "character") %>% pull(Variable)
    selectInput("VarCual", "Seleccione la variable:", choices = vars)
  })
  
  output$VarCuantitativa <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "numeric") %>% pull(Variable)
    selectInput("VarCuan", "Seleccione la variable:", choices = vars)
  })
  
  output$VarCuantitativa2 <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "numeric") %>% pull(Variable)
    selectInput("VarCuan2", "Seleccione la variable:", choices = vars)
  })
  
  # ---- Gráficos ----
  
  
  output$Grafico01 <- renderPlot({
    t01 <- datos() %>% select(input$VarCual)
    colnames(t01) <- "Variable"
    
    t01 <- t01 %>% 
      group_by(Variable) %>% 
      summarise(Registros = n(), .groups = "drop") %>%
      ggplot(aes(x = forcats::fct_reorder(Variable, Registros), y = Registros)) +
      geom_col(width = 0.7, fill = input$colx, color = "grey25", alpha = 0.9) +
      coord_flip() +
      geom_text(aes(label = scales::comma(Registros)),
                hjust = -0.1, size = 3.6) +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold"),
        plot.margin = unit(c(5, 20, 5, 5), "pt")
      )
    
    return(t01)
  })
  
  
  
  output$Grafico02 <- renderPlot({
    t02 <- datos() %>% select(input$VarCuan, input$VarCuan2)
    colnames(t02) <- c("Variable1", "Variable2")
    
    t02 <- t02 %>%
      ggplot(aes(x = Variable1, y = Variable2)) +
      # puntos con transparencia y borde
      geom_point(aes(color = Variable1, size = Variable2),
                 alpha = 0.7, shape = 21, fill = input$colx, color = "grey25") +
      # línea de tendencia suavizada
      geom_smooth(method = "lm", se = TRUE, color = "#132b60", linetype = "dashed", size = 0.9) +
      # degradado de color
      scale_color_gradient(low = "#9ecae1", high = "#08306b") +
      labs(
        x = input$VarCuan,
        y = input$VarCuan2,
        title = "Relación entre variables cuantitativas",
        subtitle = "Tamaño proporcional y color degradado por valor",
        caption = "Fuente: datos cargados en la aplicación"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", color = "#132b60", size = 14),
        plot.subtitle = element_text(color = "#555555"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e5e5e5"),
        legend.position = "right"
      )
    
    return(t02)
  })
  
  output$Grafico03 <- renderPlot({
    # Selecciona la variable cualitativa
    t03 <- datos() %>% select(input$VarCual)
    colnames(t03) <- "Variable"
    
    # Calcula los porcentajes
    t03 <- t03 %>%
      group_by(Variable) %>%
      summarise(Registros = n(), .groups = "drop") %>%
      mutate(PORCENTAJE = Registros / sum(Registros),
             etiqueta = paste0(Variable, " (", scales::percent(PORCENTAJE), ")"))
    
    # Gráfico de pastel
    t03 <- ggplot(t03, aes(x = "", y = PORCENTAJE, fill = Variable)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y", start = 0) +
      geom_text(aes(label = scales::percent(PORCENTAJE, accuracy = 0.1)),
                position = position_stack(vjust = 0.5),
                size = 3.8, color = "white", fontface = "bold") +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = paste("Distribución porcentual de", input$VarCual),
        subtitle = "Gráfico de pastel con porcentajes",
        fill = input$VarCual
      ) +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", color = "#132b60", size = 14, hjust = 0.5),
        plot.subtitle = element_text(color = "#555555", hjust = 0.5),
        legend.title = element_text(face = "bold", color = "#132b60"),
        legend.position = "right"
      )
    
    return(t03)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ---- Tablas ----
  output$TablaVariable <- function(){
    res01 <- datos() %>% select(NUM_ID, SEXO, ESTADO_CIVIL, PROVINCIA_DOMICILIO, ACTIVIDAD_ECONOMICA, MONTO_OTORGADO)
    res01 %>% 
      kable() %>% 
      kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff") %>% 
      scroll_box(width = "700px", height = "300px")
  }
  
  
  output$CreditosXProv <- function(){
    res02 <- datos() %>% 
      mutate(
        EDAD = as.numeric(difftime(Sys.Date(), FECHANACIMIENTO, units = "days")) / 365.25,
        RANGO_EDAD = cut(
          EDAD,
          breaks = c(18, 25, 50, 65, 100),
          right = FALSE, include.lowest = TRUE,
          labels = c("[18-25)", "[25-50)", "[50-65)", "[65-100)")
        )
      ) %>%
      group_by(RANGO_EDAD) %>%
      summarise(
        TIPO_CREDITO   = {x <- na.omit(TIPO_CREDITO_OTORGADO); if (length(x)) names(which.max(table(x))) else NA_character_},
        FUENTE_INGRESOS = {x <- na.omit(FUENTE_INGRESOS);       if (length(x)) names(which.max(table(x))) else NA_character_},
        NIVEL_EDUCACION = {x <- na.omit(NIVEL_EDUCACION);       if (length(x)) names(which.max(table(x))) else NA_character_},
        TOTAL_GASTOS    = sum(TOTALGASTOS, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(RANGO_EDAD)
    
    res02 %>% 
      kable() %>% 
      kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff") %>% 
      scroll_box(width = "1000px", height = "350px")
  }
  
  output$Tabla3 <- function(){
    res02 <- datos() %>%
      group_by(PROVINCIA_DOMICILIO, TIPO_CREDITO_OTORGADO, FUENTE_INGRESOS, NIVEL_EDUCACION) %>%
      summarise(
        CREDITOS = n(),
        TOTAL_GASTOS = sum(TOTALGASTOS, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(PORCENTAJE = percent(CREDITOS / sum(CREDITOS))) %>%
      arrange(desc(CREDITOS))
    
    colnames(res02) <- c("PROVINCIA", "TIPO_CRÉDITO", "FUENTE_INGRESOS", "NIVEL_EDUCACIÓN", "CREDITOS", "TOTAL_GASTOS", "PORCENTAJE")
    
    # Colorear los valores
    res02$CREDITOS <- cell_spec(res02$CREDITOS, color = ifelse(res02$CREDITOS <= 100, "red", "blue"))
    res02$PORCENTAJE <- color_bar("lightgreen")(res02$PORCENTAJE)
    
    # Tabla HTML estilizada
    tab02 <- res02 %>%
      kable("html", escape = FALSE, booktabs = TRUE) %>%
      kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff")
    
    HTML(tab02)
  }
  
  archivo <- reactive({
    req(input$file)
    path <- input$file$datapath
    excel_sheets(path)
  })
  
  output$sheet_ui <- renderUI({
    req(archivo())
    selectInput("hoja", "Seleccionar hoja:", choices = archivo())
  })
  
  datos <- eventReactive(input$analizar, {
    req(input$file, input$hoja)
    read_excel(input$file$datapath, sheet = input$hoja)
  })
  
  
  output$descargar_pdf <- downloadHandler(
    filename = function() {
      paste0("Informe_Estadistico_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      temp_data <- tempfile(fileext = ".rds")
      saveRDS(datos(), temp_data)
      
      # Obtener las variables de entrada que definen los gráficos
      var_cual <- input$VarCual
      var_cuan1 <- input$VarCuan
      var_cuan2 <- input$VarCuan2
      color <- input$colx
      
      qmd_path <- normalizePath("reporte.qmd")
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Convertir QMD a RMD temporal
      file.copy(qmd_path, temp_rmd, , overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        params = list(data_path = temp_data,
                      var_cual_param = var_cual, # Variable cualitativa
                      var_cuan1_param = var_cuan1, # Variable cuantitativa 1
                      var_cuan2_param = var_cuan2, # Variable cuantitativa 2
                      color_param = color # Color
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}
  
  
