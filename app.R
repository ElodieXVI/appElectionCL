library(shiny)
library(readxl)
library(dplyr)

# Définition de l'interface utilisateur
ui <- fluidPage(
  titlePanel("Nettoyage fichier votant·e·s"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Fichier de votes (.xlsx)"),
      fileInput("file2", "Liste des ID (.xlsx)"),
      tags$hr(),
      actionButton("clean_button", "Nettoyer"),
      br(),
      downloadButton("clean_download", "Télécharger le fichier de votes nettoyé"),
      br(),
      textOutput("file1_info"),
      textOutput("file2_info")
    ),
    mainPanel(
      uiOutput("progress_bar"),
      br(),
      tableOutput("votant_table")
    )
  )
)

# Définition du serveur
server <- function(input, output, session) {
  
  # Charger les données du premier fichier Excel
  data1 <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    req(input$file1)
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    data1(read_excel(inFile1$datapath))
  })
  
  # Charger les données du deuxième fichier Excel
  data2 <- reactiveVal(NULL)
  
  observeEvent(input$file2, {
    req(input$file2)
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    data2(read_excel(inFile2$datapath))
  })
  
  # Définir reactiveValues pour stocker les données nettoyées
  cleaned_data <- reactiveVal(NULL)
  
  # Afficher la barre de chargement
  output$progress_bar <- renderUI({
    if (is.null(cleaned_data()) || is.null(data1()) || is.null(data2())) return(NULL)
    tagList(
      if (!input$clean_button) {
        div(class = "progress", style = "height: 20px;",
            div(class = "progress-bar", role = "progressbar", style = "width: 0%;", "0%"))
      }
    )
  })
  
  # Nettoyer les données
  observeEvent(input$clean_button, {
    req(data1(), data2())
    
    data2_clean <- data2() %>% mutate(votant = 1)
    data1_aug <- left_join(data1(), data2_clean, by = "ID")
    data1_aug <- data1_aug %>% mutate(votant = ifelse(is.na(votant), "Non votant", "Votant"))
    data1_clean <- data1_aug %>%
      group_by(ID) %>%
      filter(row_number() == n())
    data_finale <- data1_clean %>% filter(votant == "Votant")
    cleaned_data(data_finale)
  })
  
  # Downloadable csv of selected dataset ----
  output$clean_download <- downloadHandler(
    filename =  function() {
      paste('resultats_vote.csv', sep=',')
    },
    
    content = function(filename) {
      write.csv(as.data.frame(cleaned_data()), filename, row.names=FALSE)
    } 
  )
  
  # Afficher le nombre de lignes pour chaque fichier téléchargé
  output$file1_info <- renderText({
    if (!is.null(data1())) {
      paste("Nombre de lignes dans le fichier 1 :", nrow(data1()))
    }
  })
  
  output$file2_info <- renderText({
    if (!is.null(data2())) {
      paste("Nombre de lignes dans le fichier 2 :", nrow(data2()))
    }
  })
  
  # Afficher le tableau des valeurs de la variable "votant" dans les données nettoyées
  output$votant_table <- renderTable({
    req(cleaned_data())
    table(cleaned_data()$votant)
  })
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
