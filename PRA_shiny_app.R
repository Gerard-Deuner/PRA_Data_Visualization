#############################################
# Shiny App de la Visualització Interactiva #
#############################################

# Carregar llibreries
library(shiny)
library(dplyr)
library(tidyr)
library(scales)
library(ggradar)
library(ggplot2)
library(ggthemes)

# Llegir les dades processades
df <- read.table("data/Mental_Health_Lifestyle_Dataset_Processed.csv",
                 sep = ",", header = TRUE)

# Ordenar els grups d'edat
age_levels_ordered <- c("10-19", "20-39", "40-59", "60-69")
df$AgeGroup <- factor(df$AgeGroup, levels = age_levels_ordered)

# Calculars els rangs (max, min) de les variables numèriques associades al lifestyle
global_ranges <- df %>%
  summarise(
    Sleep.Hours.min = min(Sleep.Hours, na.rm = TRUE),
    Sleep.Hours.max = max(Sleep.Hours, na.rm = TRUE),
    Exercise.Level.min = min(Exercise.Level, na.rm = TRUE),
    Exercise.Level.max = max(Exercise.Level, na.rm = TRUE),
    Screen.Time.min = min(Screen.Time.per.Day..Hours., na.rm = TRUE),
    Screen.Time.max = max(Screen.Time.per.Day..Hours., na.rm = TRUE),
    Social.Interaction.min = min(Social.Interaction.Score, na.rm = TRUE),
    Social.Interaction.max = max(Social.Interaction.Score, na.rm = TRUE),
    Work.Hours.min = min(Work.Hours.per.Week, na.rm = TRUE),
    Work.Hours.max = max(Work.Hours.per.Week, na.rm = TRUE)
  )

# Configurar la user interface
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      h2 { font-family: Times New Roman, Times, serif; }
      body, label, input, select, button, .well, .slider-animate-container, .irs, .irs-bar, .irs-grid-text {
        font-family: Times New Roman, Times New Roman, Times New Roman, Times New Roman;
      }
      .populist-text {
        font-size: 16px;
        font-weight: bold;
        color: #d9480f;
        margin-top: 20px;
        margin-bottom: 15px;
        font-style: italic;
      }
      .nyt-caption {
        font-size: 12px;
        color: #555555;
        font-style: italic;
        margin-top: 10px;
        margin-bottom: 10px;
      }
    "))
  ),
  
  titlePanel("The Lifestyle Sweet Spot for Happiness"),
  
  tags$h4("How sleep, exercise, screen time, social interaction and work shape happiness across ages and countries"),
  tags$br(),
  
  # Afegir filtres per filtrar per país, grup d'edat i el 10% de persones més felices
  fluidRow(
    column(4,
           selectInput("country", "Select Country:", 
                       choices = c("All", unique(df$Country)),
                       selected = "All")
    ),
    column(4,
           checkboxGroupInput("agegroup", "Select Age Group(s):",
                              choices = levels(df$AgeGroup),
                              selected = levels(df$AgeGroup))
    ),
    column(4,
           sliderInput("quantileFilter", "Top % happiest people:", 
                       min = 10, max = 100, value = 10, step = 10,
                       post = "%")
    )
  ),
  
  # Distrribuir el plot per sota els filtres
  fluidRow(
    column(12,
           plotOutput("radarPlot", height = "600px")
    )
  )
)

# Configurar el servidor
server <- function(input, output) {
  
  # Filtrar el dataset segons la selecció de l'usuari
  df_filtered <- reactive({
    df_sub <- df
    
    if (input$country != "All") {
      df_sub <- df_sub %>% filter(Country == input$country)
    }
    
    if (!is.null(input$agegroup)) {
      df_sub <- df_sub %>% filter(AgeGroup %in% input$agegroup)
    }
    
    # Seleccionar el top % de persones més felices
    threshold <- quantile(df_sub$Happiness.Score, probs = 1 - input$quantileFilter / 100, na.rm = TRUE)
    df_sub <- df_sub %>% filter(Happiness.Score >= threshold)
    
    df_sub
  })
  
  # Calcular valors a plotejar de les diferents variables
  df_summary <- reactive({
    df_sub <- df_filtered()
    
    # Calcular la mitjana de le svariables segons grup d'edat i país
    df_sub %>%
      group_by(AgeGroup) %>%
      summarise(
        Sleep.Hours = mean(Sleep.Hours, na.rm = TRUE),
        Exercise.Level = mean(Exercise.Level, na.rm = TRUE),
        Screen.Time = mean(Screen.Time.per.Day..Hours., na.rm = TRUE),
        Social.Interaction = mean(Social.Interaction.Score, na.rm = TRUE),
        Work.Hours = mean(Work.Hours.per.Week, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # COnfigurar radarplot 
  output$radarPlot <- renderPlot({
    
    df_plot <- df_summary()
    
    # Normalitzar els valors mitjans així totes les variables tenen el mateix rang [0,1]
    df_norm <- df_plot %>%
      mutate(
        Sleep.Hours = (Sleep.Hours - global_ranges$Sleep.Hours.min) / 
          (global_ranges$Sleep.Hours.max - global_ranges$Sleep.Hours.min),
        Exercise.Level = (Exercise.Level - global_ranges$Exercise.Level.min) / 
          (global_ranges$Exercise.Level.max - global_ranges$Exercise.Level.min),
        Screen.Time = (Screen.Time - global_ranges$Screen.Time.min) / 
          (global_ranges$Screen.Time.max - global_ranges$Screen.Time.min),
        Social.Interaction = (Social.Interaction - global_ranges$Social.Interaction.min) / 
          (global_ranges$Social.Interaction.max - global_ranges$Social.Interaction.min),
        Work.Hours = (Work.Hours - global_ranges$Work.Hours.min) / 
          (global_ranges$Work.Hours.max - global_ranges$Work.Hours.min)
      ) %>%
      rename(group = AgeGroup)
    
    
    colors <- c("#3b3b3b", "#e3120b", "#08519c", "#6baed6")
    
    # Plotejar el ggradar 
    ggradar(df_norm,
            grid.min = 0, grid.mid = 0.5, grid.max = 1,
            values.radar = c("0", "0.5", "1"),
            group.line.width = 2,
            group.point.size = 4,
            background.circle.colour = NA,
            gridline.mid.colour = "grey70",
            group.colours = colors[1:nrow(df_norm)],
            legend.position = "bottom",
            legend.text.size = 12,
            font.radar = "Times New Roman") +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.5, alpha = 0.15, fill = "#fceae9") +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1, alpha = 0.15, fill = "#e6f4ea") +
      labs(title = "The Lifestyle Sweet Spot for Happiness",
           subtitle = paste("Country:", input$country,
                            "| Age Groups:", paste(input$agegroup, collapse = ", "),
                            "| Top", input$quantileFilter, "% happiest"),
           caption = "Source: Mental Health and Lifestyle Habits (2019-2024) dataset from Kaggle") +
      theme_minimal(base_family = "Times New Roman") +
      theme(
        plot.background = element_rect(fill = "#f7f7f7", color = NA),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10, color = "gray40", face = "italic", hjust = 0)
      ) +
      annotate("text", x = 1, y = -1.3, label = paste0("Showing the top ", input$quantileFilter, "% of the happiest people."), size = 5, color = "gray40", fontface = "italic")
  })
  
}

# Executar la shiny app
shinyApp(ui = ui, server = server)
