#################################################
###### Universidad Continental ##################
###### Maestría en Economía #####################
# =============================================
# *** Alumno: Rafael Marcos Lapa Camargo
# *** Curso : Herramientas Informáticas I
# *** Prof. Joel Turco Quinto
# *** Proyecto: Visualización y Dashboard - Gapminder
# =============================================

# 1. LIMPIAR ENTORNO Y CARGAR PAQUETES ---------------------------

rm(list = ls())

paquetes <- c("shiny", "ggplot2", "dplyr", "gapminder", "plotly", "broom",
              "janitor", "tidyverse", "ggthemes", "scales", "viridis",
              "gganimate", "patchwork", "ggrepel", "ggridges")

lapply(paquetes, function(p) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
})

# 2. PREPARAR DATOS ----------------------------------------------

data(gapminder)

datos <- gapminder %>%
  clean_names() %>%
  rename(pais = country, continente = continent, anio = year,
         esperanza_vida = life_exp, pib_per_capita = gdp_percap,
         poblacion = pop) %>%
  mutate(
    log_pib = log(pib_per_capita),
    log_pop = log(poblacion),
    categoria_vida = case_when(
      esperanza_vida < 50 ~ "Baja",
      esperanza_vida < 70 ~ "Media",
      TRUE ~ "Alta"
    )
  )

gap2007 <- datos %>% filter(anio == 2007)

# 3. UI -----------------------------------------------------------

ui <- fluidPage(
  titlePanel("🌍 Dashboard Gapminder: Estadística, Econometría y Animaciones"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("continente", "Selecciona continente:", 
                  choices = unique(datos$continente), selected = "Asia"),
      selectInput("pais", "Selecciona país:", choices = NULL),
      sliderInput("anio", "Año:", min = min(datos$anio), max = max(datos$anio),
                  value = 2007, step = 5),
      checkboxGroupInput("continentes", "Filtrar continentes para regresión:",
                         choices = unique(gap2007$continente),
                         selected = unique(gap2007$continente))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("🌎 Video Gapminder", plotlyOutput("animPlot")),
        tabPanel("📊 Gráficos por País", plotlyOutput("lifeExpPlot"), plotlyOutput("gdpPlot")),
        tabPanel("🌍 Comparación Continental", plotlyOutput("continentPlot")),
        tabPanel("📈 LATAM Vida", plotlyOutput("latamPlot")),
        tabPanel("📦 Distribuciones", plotlyOutput("histogramaVida"), plotlyOutput("boxplotPIB")),
        tabPanel("🔍 Econometría Simple", plotlyOutput("regresionSimple")),
        tabPanel("📉 Econometría Múltiple", plotlyOutput("regresionMultiple")),
        tabPanel("🏅 País más Rico", plotlyOutput("scatter_con_etiquetas"))
      )
    )
  )
)

# 4. SERVER -------------------------------------------------------

server <- function(input, output, session) {
  
  # Actualizar países al cambiar continente
  observe({
    paises <- datos %>%
      filter(continente == input$continente) %>%
      distinct(pais) %>% pull()
    updateSelectInput(session, "pais", choices = paises, selected = paises[1])
  })
  
  datos_pais <- reactive({
    req(input$pais)
    datos %>% filter(pais == input$pais)
  })
  
  datos_regresion <- reactive({
    gap2007 %>% filter(continente %in% input$continentes)
  })
  
  output$lifeExpPlot <- renderPlotly({
    ggplotly(
      ggplot(datos_pais(), aes(x = anio, y = esperanza_vida)) +
        geom_line(color = "blue", size = 1) +
        geom_point() +
        labs(title = paste("Esperanza de Vida:", input$pais),
             x = "Año", y = "Años") +
        theme_minimal()
    )
  })
  
  output$gdpPlot <- renderPlotly({
    ggplotly(
      ggplot(datos_pais(), aes(x = anio, y = pib_per_capita)) +
        geom_line(color = "forestgreen", size = 1) +
        geom_point() +
        labs(title = paste("PIB per cápita:", input$pais),
             x = "Año", y = "USD") +
        theme_minimal()
    )
  })
  
  output$continentPlot <- renderPlotly({
    df <- datos %>% filter(continente == input$continente, anio == input$anio)
    ggplotly(
      ggplot(df, aes(x = pib_per_capita, y = esperanza_vida,
                     size = poblacion, color = pais)) +
        geom_point(alpha = 0.7) +
        scale_x_log10() +
        labs(title = paste("PIB vs Vida en", input$continente, input$anio),
             x = "PIB per cápita (log)", y = "Esperanza de Vida") +
        theme_minimal()
    )
  })
  
  output$animPlot <- renderPlotly({
    df <- datos %>% filter(continente == input$continente)
    plot_ly(data = df, x = ~pib_per_capita, y = ~esperanza_vida,
            size = ~poblacion, color = ~pais, frame = ~anio,
            type = "scatter", mode = "markers", ids = ~pais,
            text = ~paste("País:", pais,
                          "<br>PIB:", round(pib_per_capita),
                          "<br>Vida:", round(esperanza_vida)),
            marker = list(sizemode = "diameter", opacity = 0.6)) %>%
      layout(title = paste("Video Gapminder:", input$continente),
             xaxis = list(title = "PIB per cápita", type = "log"),
             yaxis = list(title = "Esperanza de Vida")) %>%
      animation_opts(frame = 1000, redraw = TRUE)
  })
  
  output$latamPlot <- renderPlotly({
    latam <- c("Chile", "Brazil", "Argentina", "Mexico", "Peru")
    df <- datos %>% filter(pais %in% latam)
    ggplotly(
      ggplot(df, aes(x = anio, y = esperanza_vida, color = pais)) +
        geom_line(size = 1.2) +
        facet_wrap(~ pais) +
        theme_minimal() +
        labs(title = "Esperanza de Vida en LATAM", x = "Año", y = "Vida")
    )
  })
  
  output$histogramaVida <- renderPlotly({
    ggplotly(
      ggplot(datos_regresion(), aes(x = esperanza_vida, fill = continente)) +
        geom_histogram(alpha = 0.7, bins = 20, position = "identity") +
        theme_minimal() +
        labs(title = "Distribución Esperanza de Vida (2007)", x = "Vida", y = "Frecuencia")
    )
  })
  
  output$boxplotPIB <- renderPlotly({
    ggplotly(
      ggplot(datos_regresion(), aes(x = continente, y = pib_per_capita, fill = continente)) +
        geom_boxplot(alpha = 0.7) +
        scale_y_log10(labels = dollar) +
        theme_minimal() +
        labs(title = "PIB per cápita por continente", y = "PIB (log)", x = "Continente")
    )
  })
  
  output$regresionSimple <- renderPlotly({
    ggplotly(
      ggplot(datos_regresion(), aes(x = log_pib, y = esperanza_vida)) +
        geom_point(aes(color = continente), alpha = 0.8) +
        geom_smooth(method = "lm", se = TRUE, color = "black") +
        theme_minimal() +
        labs(title = "Regresión: Vida ~ log(PIB)", x = "log(PIB)", y = "Vida")
    )
  })
  
  output$regresionMultiple <- renderPlotly({
    modelo <- lm(esperanza_vida ~ log_pib + log_pop, data = datos_regresion())
    ggplotly(
      ggplot(datos_regresion(), aes(x = log_pib, y = esperanza_vida)) +
        geom_point(aes(color = continente, size = log_pop), alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "black") +
        theme_minimal() +
        labs(title = "Regresión Múltiple: Vida ~ log(PIB) + log(Población)",
             x = "log(PIB)", y = "Esperanza de Vida")
    )
  })
  
  output$scatter_con_etiquetas <- renderPlotly({
    top <- gap2007 %>% group_by(continente) %>% top_n(1, pib_per_capita)
    ggplotly(
      ggplot(gap2007, aes(x = pib_per_capita, y = esperanza_vida, color = continente)) +
        geom_point(aes(size = poblacion), alpha = 0.6) +
        geom_text_repel(data = top, aes(label = pais), color = "black", size = 3.5) +
        scale_x_log10() +
        theme_minimal() +
        labs(title = "País más rico por continente (2007)", x = "PIB (log)", y = "Vida")
    )
  })
}

# 5. INICIAR APP --------------------------------------------------
shinyApp(ui = ui, server = server)
