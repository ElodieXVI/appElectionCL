library(shiny)
library(readxl)
library(dplyr)
library(openxlsx)
library(DT)

generate_unique_codes <- function(n) {
  codes <- character(n)
  letters_part <- function() paste0(sample(LETTERS, 3, replace = TRUE), collapse = "")
  numbers_part <- function() sprintf("%03d", sample(0:999, 1))
  
  for (i in 1:n) {
    repeat {
      code <- paste0(letters_part(), numbers_part())
      if (!code %in% codes) {
        codes[i] <- code
        break
      }
    }
  }
  return(codes)
}

# Définition de l'interface utilisateur
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .shiny-text-output {
      background-color:#fff;
    }
    .info-block {
      background-color: #d0e7ff;
      padding: 20px;
      border-radius: 5px;
      margin-bottom: 20px;
    }
    .title-background {
    background-image: url('fond.jpeg');
      background-color: #1f2985;
      color: white;
      padding: 10px;
      margin-bottom: 20px;
      border-radius: 5px;
      text-align: center;
    }
  "))),
  div(class = "title-background",
      h1("Aide aux votes",
         style = "
        color: #fff; text-align: center;
        background-image: url('fond.png');
        padding: 20px")),
  br(),
  div(class = "info-block",
      fluidRow(
        column(6, offset = 3,
               p("Cet outil a été créé pour le vote du CL parisien. Il aide à réalisationd d'un vote confidentiel avec Google Form. 
             Les fichiers pris en charge en générés sont au format xlsx."),
               p("Voici un processus de vote possible avec cet outil :"),
               p("Avant le vote - Via ce site et à partir d'un fichier de contact : générer des ID aléatoires (3 lettres, 3 chiffres)"),
               p("Avant le vote - Par Gmail : envoyer par mail à l'ensemble des participant·e·s au vote leur ID confidentiel"),
               p("Le vote - Procéder au vote sur Google Form en demandant aux participant·e·s d'entrer uniquement leur ID confidentiel
             "),
               p("Après le vote - Récupérer en format xlsx les résultats du vote et les déposer ici : les doubles votant·e·s seront supprimés 
             et seuls les votants compris dans la liste ID retenu"),
               br(),
               a("Code source", class = "btn btn-primary btn-md", 
                 href = "https://github.com/ElodieXVI/appElectionCL")
        )
      )),
  
  br(),
  
  fluidRow(
    column(6,
           wellPanel(
             h3("Génération ID"),
             fileInput("fileID", "Fichier contacts (.xlsx)"),
             tags$hr(),
             actionButton("cleanID_button", "Générer ID"),
             br(),
             downloadButton("cleanID_download", "Télécharger le fichier de contacts avec ID"),
             br(),
             textOutput("fileID_info"),
           )),
    column(6,
           wellPanel(
             h3("Nettoyage fichier de votes issu de gform"),
             fileInput("file1", "Fichier de votes (.xlsx)"),
             p("Attention : déposer ici seulement la liste des ID et pas le fichier contacts en entier !"),
             fileInput("file2", "Liste des ID (.xlsx)"),
             tags$hr(),
             actionButton("clean_button", "Détecter les non votants"),
             br(),
             downloadButton("clean_download", "Télécharger le fichier de votes nettoyé"),
             br(),
             textOutput("file1_info"),
             textOutput("file2_info"),
             textOutput("error_message")
           ))
  ),

  fluidRow(
    column(6, offset = 3,
           wellPanel(
             h3("Vérification"),
             p("Affichage de 5 lignes aléatoires pour vérifier la détection des votants"),
             dataTableOutput("table"),
             textOutput("error_message")
           ))
  )
)

# Définition du serveur
server <- function(input, output, session) {
  
  # Data ID
  dataID <- reactiveVal(NULL)
  
  observeEvent(input$fileID, {
    req(input$fileID)
    inFileID <- input$fileID
    if (is.null(inFileID))
      return(NULL)
    tryCatch({
      dataID(read_excel(inFileID$datapath))
      output$error_message <- renderText({""}) # Clear any previous error messages
    }, error = function(e) {
      output$error_message <- renderText({"Erreur lors de la lecture du fichier contacts : vérifiez le format et les colonnes."})
    })
  })
  
  # Charger les données du premier fichier Excel
  data1 <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    req(input$file1)
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    tryCatch({
      data1(read_excel(inFile1$datapath))
      output$error_message <- renderText({""}) # Clear any previous error messages
    }, error = function(e) {
      output$error_message <- renderText({"Erreur lors de la lecture du fichier de votes : vérifiez le format et les colonnes."})
    })
  })
  
  # Charger les données du deuxième fichier Excel
  data2 <- reactiveVal(NULL)
  
  observeEvent(input$file2, {
    req(input$file2)
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    tryCatch({
      data2(read_excel(inFile2$datapath))
      output$error_message <- renderText({""}) # Clear any previous error messages
    }, error = function(e) {
      output$error_message <- renderText({"Erreur lors de la lecture du fichier des IDs : vérifiez le format et les colonnes."})
    })
  })
  
  # Définir reactiveValues pour stocker les données nettoyées
  cleaned_data <- reactiveVal(NULL)
  cleanedID_data <- reactiveVal(NULL)
  
  # Nettoyer les ID
  observeEvent(input$cleanID_button, {
    req(dataID())
    dataID_finale <- dataID() %>% mutate(ID = generate_unique_codes(nrow(.)))
    cleanedID_data(dataID_finale)
  })
  
  # Nettoyer les données et afficher l'échantillon aléatoire
  observeEvent(input$clean_button, {
    req(data1(), data2())
    tryCatch({
      # Partie de nettoyage des données data1 et data2
      data1_temp <- data1() %>% rename(ID = 2)
      data2_clean <- data2() %>% mutate(liste = "Liste")
      
      data1_aug <- left_join(data1_temp, data2_clean %>% select(ID, liste), by = "ID")
      
      data1_aug <- data1_aug %>% mutate(liste = ifelse(is.na(liste), "Pas liste", liste))
      
      data1_aug <- data1_aug %>%
        group_by(ID) %>%
        mutate(doublon = ifelse(row_number() == n(), "A conserver", "A supprimer")) %>% 
        ungroup()
      
      data_finale <- data1_aug %>% mutate(
        votant = ifelse(liste == "Liste" & doublon == "A conserver", "Votant", "Non votant")
      ) %>% select(-liste, -doublon)
      
      cleaned_data(data_finale)
      output$error_message <- renderText({""}) # Clear any previous error messages
      # 
      # # Sélectionner 6 lignes aléatoires du DataFrame nettoyé
      # echantillon <- sample_n(as.data.frame(cleaned_data(data_finale)), 6)
      
      # Afficher les lignes sélectionnées dans un tableau
      output$table <- DT::renderDataTable({
        DT::datatable(sample_n(data_finale, 3))
      })
    }, error = function(e) {
      output$error_message <- renderText({"Erreur lors du nettoyage des données. Vérifiez que les fichiers contiennent les colonnes nécessaires."})
    })
  })
  
  # Download Excel ID ----
  output$cleanID_download <- downloadHandler(
    filename = function() {
      paste('contacts_ID.xlsx', sep = '')
    },
    
    content = function(filename) {
      write.xlsx(as.data.frame(cleanedID_data()), filename)
    }
  )
  
  # Download Excel données nettoyées ----
  output$clean_download <- downloadHandler(
    filename = function() {
      paste('resultats_vote.xlsx', sep = '')
    },
    
    content = function(filename) {
      write.xlsx(as.data.frame(cleaned_data()), filename)
    }
  )
  
  # Afficher le nombre de lignes pour chaque fichier téléchargé
  output$fileID_info <- renderText({
    if (!is.null(dataID())) {
      paste("Nombre de lignes dans le fichier contacts :", nrow(dataID()))
    }
  })
  
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
  
  # Afficher les messages d'erreur
  output$error_message <- renderText({ "" })
  

}

# Exécution de l'application
shinyApp(ui = ui, server = server)
