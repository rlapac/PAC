#################################################
###### Universidad Continental ##################
###### Maestría en Economía #####################
# =============================================
# *** Alumno: Rafael Marcos Lapa Camargo
# *** Curso : Herramientas Informáticas I
# *** Proyecto: Dashboard Interactivo - Gapminder
# =============================================

# ----------------------------
# 1. Configuración Inicial
# ----------------------------
rm(list = ls())  # Limpiar entorno

# Verificación e instalación de paquetes
paquetes <- c("shiny", "ggplot2", "dplyr", "gapminder", "plotly", "janitor", "skimr")
instalar_y_cargar <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}
invisible(lapply(paquetes, instalar_y_cargar))

# ----------------------------
# 2. Preparación de datos
# ----------------------------

# Cargar y limpiar gapminder
data(gapminder)
datos <- gapminder %>% 
  clean_names() %>%                      # nombres limpios
  rename(pais = country, continente = continent, anio = year, 
         esperanza_vida = life_exp, pib_per_capita = gdp_percap, poblacion = pop)

# Categorizar esperanza de vida
datos <- datos %>%
  mutate(
    categoria_vida = case_when(
      esperanza_vida < 50 ~ "Baja",
      esperanza_vida < 70 ~ "Media",
      TRUE ~ "Alta"
    ),
    log_pib = log(pib_per_capita)
  )

# ----------------------------
# 3. Interfaz de Usuario (UI)
# ----------------------------

ui <- fluidPage(
  titlePanel("🌍 Dashboard Interactivo - Gapminder"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continente", "Selecciona un continente:",
                  choices = unique(datos$continente),
                  selected = "Asia"),
      selectInput("pais", "Selecciona un país:",
                  choices = NULL),
      sliderInput("anio", "Selecciona un año:",
                  min = min(datos$anio),
                  max = max(datos$anio),
                  value = 2007, step = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráficos por país",
                 plotlyOutput("lifeExpPlot"),
                 plotlyOutput("gdpPlot")),
        tabPanel("Comparación en continente",
                 plotlyOutput("continentPlot"))
      )
    )
  )
)

# ----------------------------
# 4. Lógica del Servidor (Server)
# ----------------------------

server <- function(input, output, session) {
  
  # Actualizar lista de países según continente
  observe({
    paises <- datos %>%
      filter(continente == input$continente) %>%
      distinct(pais) %>%
      pull()
    updateSelectInput(session, "pais", choices = paises)
  })
  
  # Datos por país seleccionado
  datos_filtrados <- reactive({
    req(input$pais)
    datos %>% filter(pais == input$pais)
  })
  
  # Expectativa de vida por año
  output$lifeExpPlot <- renderPlotly({
    p <- ggplot(datos_filtrados(), aes(x = anio, y = esperanza_vida)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "black") +
      theme_minimal() +
      labs(title = paste("Expectativa de Vida:", input$pais),
           x = "Año", y = "Años")
    ggplotly(p)
  })
  
  # PIB per cápita por año
  output$gdpPlot <- renderPlotly({
    p <- ggplot(datos_filtrados(), aes(x = anio, y = pib_per_capita)) +
      geom_line(color = "forestgreen", size = 1.2) +
      geom_point(color = "black") +
      theme_minimal() +
      labs(title = paste("PIB per cápita:", input$pais),
           x = "Año", y = "USD")
    ggplotly(p)
  })
  
  # Comparación en el continente para un año
  output$continentPlot <- renderPlotly({
    df <- datos %>%
      filter(continente == input$continente, anio == input$anio)
    
    p <- ggplot(df, aes(x = pib_per_capita, y = esperanza_vida,
                        size = poblacion, color = pais,
                        text = paste("País:", pais,
                                     "<br>PIB:", round(pib_per_capita),
                                     "<br>Vida:", round(esperanza_vida),
                                     "<br>Población:", format(poblacion, big.mark = ",")))) +
      geom_point(alpha = 0.7) +
      scale_x_log10() +
      theme_minimal() +
      labs(title = paste("PIB vs Vida en", input$continente, "-", input$anio),
           x = "PIB per cápita (log)", y = "Esperanza de Vida")
    
    ggplotly(p, tooltip = "text")
  })
}

# ----------------------------
# 5. Ejecutar Aplicación
# ----------------------------
shinyApp(ui = ui, server = server)
