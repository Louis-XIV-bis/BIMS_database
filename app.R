options(timeout = 2000)
if (!require('tibble', quietly = T)) install.packages('tibble');
if (!require('shiny', quietly = T)) install.packages('shiny');
if (!require('shinydashboard', quietly = T)) install.packages('shinydashboard');
if (!require('shinybusy', quietly = T)) install.packages('shinybusy');
if (!require('shinycssloaders', quietly = T)) install.packages('shinycssloaders');
if (!require('shinyjs', quietly = T)) install.packages('shinyjs');
if (!require('shinyalert', quietly = T)) install.packages('shinyalert');
if (!require('stringr', quietly = T)) install.packages('stringr');
if (!require('stringi', quietly = T)) install.packages('stringi');
if (!require('DT', quietly = T)) install.packages('DT');
if (!require('htmlwidgets', quietly = T)) install.packages('htmlwidgets');
if (!require('ggplot2', quietly = T)) install.packages("ggplot2");
if (!require('devtools', quietly = T)) install.packages("devtools");
if (!require('shinyThings', quietly = T)) devtools::install_github("gadenbuie/shinyThings");
if (!require('ggrepel', quietly = T)) install.packages("ggrepel");
if (!require('viridis', quietly = T)) install.packages("viridis");
if (!require('dplyr', quietly = T)) install.packages("dplyr");
if (!require('gsheet', quietly = T)) install.packages("gsheet");

library(tibble)
library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
library(stringr)
library(stringi)
library(DT)
library(htmlwidgets)
library(ggplot2)
library(data.table)
library(devtools)
library(shinyThings)
library(leaflet)
library(tidygeocoder)
library(tibble)
library(dplyr)
library(ggrepel)
library(viridis)
library(dplyr)
library(gsheet)
#####################################################################
box2 <- function(...){
  box(
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    ...
  )
}

conditional <- function(condition, success) {
  if (condition) success else TRUE
}

# Créer le pie plot pour le type de contrat (poursuite)
create_pie_plot_poursuite <- function(data) {
  
  # Calcul des position pour les labels
  data <- data %>%
    mutate(Pourcentage = round((Effectif / sum(Effectif)) * 100, digits = 1))
  
  df2 <- data %>% 
    mutate(csum = rev(cumsum(rev(Effectif))), 
           pos = Effectif/2 + lead(csum, 1),
           pos = if_else(is.na(pos), Effectif/2, pos))
  
  total_n = cumsum(data$Effectif)
  
  ggplot(data = data, aes(x = "", y = Effectif, fill = Contrat)) +
    geom_col(width = 1, color = 1) +
    geom_bar(stat="identity", width=1, color="white") +
    geom_text(aes(label = paste0(Pourcentage,"%")), position = position_stack(vjust = 0.5)) +
    geom_label_repel(data = df2, 
                     aes(y = pos, label = Contrat),
                     size = 4.5, nudge_x = 1, label.padding = 0.5, point.padding = 7, show.legend = FALSE) +
    coord_polar("y", start=0) +
    theme_void() +
    labs(title = paste0("Poursuite de carrière des étudiants du master BIMS (n = ", total_n,")")) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 16, hjust = 0.5))  # Center-align the title
  
}

# Créer le pie plot pour la répartition des domaines
create_pie_plot_domaine <- function(data) {
  
  # Calcul des position pour les labels
  data <- data %>%
    mutate(Pourcentage = round((Effectif / sum(Effectif)) * 100, digits = 1))
  
  total_n = sum(data$Effectif)
  
  ggplot(data = data, aes(x = "", y = Effectif, fill = Domaine)) +
    geom_col(width = 1, color = 1) +
    geom_bar(stat="identity", width=1, color="white") +
    geom_text(aes(label = paste0(Pourcentage,"%")), position = position_stack(vjust = 0.5)) +
    coord_polar("y", start=0) +
    theme_void() +
    labs(title = paste0("Représentation des domaines de la bioinformatique des étudiants du master BIMS (n = ", total_n, ")"),
         subtitle = "Somme des différentes expériences des étudiants (stage(s) et alternance)") +
    theme(plot.title = element_text(size = 16, hjust = 0.5))  # Center-align the title
  
}

# Créer le bar plot pour la répartition des sexes
create_plot_sexe <- function(data) {
  
  total_n = sum(data$Effectif)
  
  ggplot(data = data, aes(x = Parcours, y = Effectif, fill = Sexe)) +
    geom_bar(stat = "identity", color="black") +
    theme_minimal() + 
    labs(title = paste0("Répartition des sexes au sein du master BIMS (n = ", total_n, ")")) +
    theme(plot.title = element_text(size = 16, hjust = 0.5))  # Center-align the title
}

# Créer le bar plot pour la répartition des effectifs par promo
create_plot_effectif_promo <- function(data) {
  ggplot(data = data, aes(x = Promotion, y = Effectif, fill = Parcours)) +
    geom_bar(stat = "identity", color="black") +
    theme_minimal() +
    labs(title = "Effectifs des différentes promotions du master BIMS") +
    theme(plot.title = element_text(size = 16, hjust = 0.5))  # Center-align the title
}

# javascript code to collapse box
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

###########################################################################################################################
                                                      #UI
###########################################################################################################################

# See above for the definitions of ui and server
ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "BiMS'TRO", titleWidth = 200),
                    dashboardSidebar(
                      #width sidebar
                      width = 200,
                      sidebarMenu(
                        id="tabs",
                        # Menu 
                        #https://getbootstrap.com/docs/3.4/components/#glyphicons
                        menuItem("Accueil", tabName = "accueil", icon = icon("home", lib = "glyphicon")),
                        menuItem("Alumni", tabName = "alumni", icon =  icon("book", lib = "glyphicon")),
                        menuItem("Stage", tabName = "stage", icon =  icon("globe", lib = "glyphicon")),
                        menuItem("Stats", tabName = "stats", icon =  icon("signal", lib = "glyphicon"))
                      )
                    ),
                    #extendShinyjs(text = 'shinyjs.scrolltop = function() {window.scrollTo(0, 0)};', functions = c("scrolltop")),
                    # Body content
                    dashboardBody(  
                      add_busy_gif(src = 'cat.gif', position = 'full-page', timeout = 300),
                      # Including Javascript
                      useShinyjs(),
                      tags$style(HTML("
      .box-header {
        padding: 0 10px 0 0;
      }
      .box.box-solid.box-primary>.box-header {
        color:#fff;
        background:#2E2C2C
      }
      .skin-black .main-header .logo{
        background-color:#000000;
        color:#fff
      }
      .skin-black .main-header .navbar {
        background-color: #000000;
      }
      .box-header h3 {
        width: 100%;
        padding: 10px;
      }")),
                      
                      tabItems(
                        tabItem("accueil",
                                h1("Bienvenue à vous sur BIMS'TRO"),
                                br(),
                                h2("Description de l'application"),
                                br(),
                                h4("Cette application interactive sous R Shiny a pour but de rassembler et de mettre à disposition une base de données des étudiants du master BIMS."),
                                h4("Elle a pour but d'être utilisée lors de recherche de contact pour un stage / alternance / autre tout en fournissant quelques statistiques sur le master."),
                                br(),
                                h4("Afin que cette application dure dans le temps et puisse être utile au futur étudiants, nous invitons les promotions actuelles à mettre à jour l'application / la base de données. "),
                                h4("..."),
                                br(),
                                br(),
                                h2("Qui sommes-nous ?"),
                                br(),
                                h4("La création de cette application vient de 3 merveilleux étudiants de la promo 2023 (nous même) :"),
                                h4("Meije MATHE : https://www.linkedin.com/in/meije-mathe/"),
                                h4("Solene PETY : https://www.linkedin.com/in/solenepety/"),
                                h4("Louis OLLIVIER : https://www.linkedin.com/in/louis-ollivier/"),
                                br(),
                                h2("Liste des contributeurs"),
                                br(),
                                h4("Qui va s'agrandir au fil des années..."),
                                
                                imageOutput("logo_master")
                                
                        ),
                        tabItem("alumni",
                                fluidRow(
                                  column(width=4,
                                         box2(title = "Filtres",
                                              #Parcours
                                              selectInput("parcours",
                                                          label = "Parcours",
                                                          choices = c("BIMS"="BIMS",
                                                                      "CCB4"="CCB4"),
                                                          selected = NULL,
                                                          multiple = TRUE
                                              ),
                                              #Entreprise
                                              selectizeInput('entreprise', 
                                                             label = "Entreprise",
                                                             choices = NULL,
                                                             selected = NULL,
                                                             multiple = TRUE
                                              ),
                                              #Annee
                                              selectizeInput('annee', 
                                                             label = "Année du diplôme",
                                                             choices = NULL,
                                                             selected = NULL,
                                                             multiple = TRUE
                                              ),
                                              #Ville
                                              selectizeInput('ville', 
                                                             label = "Ville",
                                                             choices = NULL,
                                                             selected = NULL,
                                                             multiple = TRUE
                                              ),
                                              #Pays
                                              selectizeInput('pays', 
                                                             label = "Pays",
                                                             choices = NULL,
                                                             selected = NULL,
                                                             multiple = TRUE
                                              ),
                                              #Domaine
                                              selectizeInput('domaine', 
                                                             label = "Domaine",
                                                             choices = NULL,
                                                             selected = NULL,
                                                             multiple = TRUE
                                              ),
                                              #Contrat
                                              selectizeInput('contrat', 
                                                             label = "Contrat post-master",
                                                             choices = NULL,
                                                             selected = NULL,
                                                             multiple = TRUE
                                              )
                                         )
                                  ),
                                  column(width=8,
                                         pagination <- shinyThings::paginationUI("pager", width = 8, offset = 0, class = "text-center"),
                                         tags$hr(),
                                         uiOutput("myboxes")
                                  )
                                )
                        ),
                        tabItem("stage",
                                fluidRow(
                                  box2(title = "Filtre",
                                       radioButtons("stage", "Type de stage",
                                                    c("Tout" = "all",
                                                      "Stage M1" = "M1",
                                                      "Alternance" = "alternance",
                                                      "Stage M2.2" = "M2")
                                       ),
                                  )
                                  
                                ),
                                leafletOutput("mymap"),
                                p()
                        ),
                        tabItem("stats", 
                                fluidPage(
                                  titlePanel("Tableau de bord BDD BIMS"),
                                  
                                  fluidRow(
                                    
                                    column(width = 6,
                                           div(style = "border: 2px solid black; height: 500px;",
                                               plotOutput("plot_insertion", height = "100%")),
                                           downloadButton("dl_poursuite", "Téléchargement")
                                    ),
                                    
                                    column(width = 6,
                                           div(style = "border: 2px solid black; height: 500px;",
                                               plotOutput("plot_sexes", height = "100%")),
                                           downloadButton("dl_sexes", "Téléchargement")
                                    )
                                    
                                  ),
                                  
                                  fluidRow(
                                    style = "margin-top: 20px;", 
                                    column(width = 6,
                                           div(style = "border: 2px solid black; height: 500px;",
                                               plotOutput("plot_effectif_promo", height = "100%")),
                                           downloadButton("dl_effectif_promo", "Téléchargement")
                                    ),
                                    
                                    column(width = 6,
                                           div(style = "border: 2px solid black; height: 500px;",
                                               plotOutput("plot_domaines", height = "100%")),
                                           downloadButton("dl_domaines", "Téléchargement")
                                    )
                                  )
                                )
                        )
                      )
                    )
)

###########################################################################################################################
                                                            #server
###########################################################################################################################

server <- function(input, output, session) {
  
  #####################################################################
  # load dataset
  #####################################################################
  
  # read data file
  data <- reactive({
    url <- "https://docs.google.com/spreadsheets/d/1K-3kAdyWFrslbS-6FRazDS9H4cX9jKleLr6a1YRdTp4/edit?usp=sharing"
    df <- gsheet2tbl(url)
    #replace empty cell with NA
    df[df == ''] <- NA
    df <- df[order(df$Nom, df$Prenom), ]
    df
  })
  
  # reformat data for map tab 
  data_stage_clean_points <- reactive({
    # clean up data
    req(data)
    data_stage <- data()
    # remove useless columns
    data_stage <- subset(data_stage, select = -c(Parcours, Annee_sortie, Linkedin,Stage1_site_web, Stage1_domaine,
                                                 Alternance_site_web, Alternance_domaine, Stage2_site_web, Stage2_domaine,
                                                 Poursuite_contrat, Poursuite_site_web, Poursuite_domaine))
    # M1 internship data
    data_stageM1 <- subset(data_stage, select = c(Nom, Prenom, Stage1_entreprise, Stage1_ville, Stage1_pays))
    colnames(data_stageM1)[c(3, 4, 5)] <- c("Entreprise", "Ville", "Pays")
    Stage <- rep("M1", nrow(data_stageM1))
    data_stageM1 <- cbind(data_stageM1, Stage)
    # apprenticeship data
    data_alternance <- subset(data_stage, select = c(Nom, Prenom, Alternance_entreprise, Alternance_ville, Alternance_pays))
    colnames(data_alternance)[c(3, 4, 5)] <- c("Entreprise", "Ville", "Pays")
    Stage <- rep("Alternance", nrow(data_alternance))
    data_alternance <- cbind(data_alternance, Stage)
    # M2 internship data
    data_stageM2.2 <- subset(data_stage, select = c(Nom, Prenom, Stage2_entreprise, Stage2_ville, Stage2_pays))
    colnames(data_stageM2.2)[c(3, 4, 5)] <- c("Entreprise", "Ville", "Pays")
    Stage <- rep("M2", nrow(data_stageM2.2))
    data_stageM2.2 <- cbind(data_stageM2.2, Stage)
    
    # (interactive) map
    
    data_stage_clean <-  rbind(data_stageM1, data_alternance, data_stageM2.2)
    # remove rows with NAs
    data_stage_clean <- na.omit(data_stage_clean)
    # addresses : couples ville - pays
    villes_pays <- data_stage_clean[,c("Ville","Pays")]
    address <- paste(villes_pays$Ville, ",", villes_pays$Pays)
    addresses_combine <- tibble(
      address = address
    )
    # geo coordinates
    cascade_results <- addresses_combine %>%
      geocode_combine(
        queries = list(
          # list(method = 'census'),
          list(method = 'osm')
        ),
        global_params = list(address = 'address')
      )
    points = cbind(cascade_results %>% pull(long),cascade_results %>% pull(lat),gsub("^(.*?),.*", "\\1", cascade_results %>% pull(address)))
    colnames(points)[c(1, 2, 3)] <- c("lat", "long", "Ville")
    points[,"Ville"] <- str_sub(points[,"Ville"], end = -2)
    # remove duplicate geo coord
    points <- unique(points)
    data_stage_clean_points <- merge(data_stage_clean, points, by = "Ville")
    data_stage_clean_points$lat <- as.numeric(data_stage_clean_points$lat)
    data_stage_clean_points$long <- as.numeric(data_stage_clean_points$long)
    
    data_stage_clean_points[data_stage_clean_points == "M1"] <- 1
    data_stage_clean_points[data_stage_clean_points == "Alternance"] <- 2
    data_stage_clean_points[data_stage_clean_points == "M2"] <- 3
    data_stage_clean_points$Stage <- as.numeric(data_stage_clean_points$Stage)
    data_stage_clean_points
  })
  
  #####################################################################
  # Accueil tab
  #####################################################################
  
  output$logo_master <- renderImage({
    list(src = "img/BioInfo_logo_quadri_fdclair.png",
         height = 330)
  }, deleteFile = F)
  
  #####################################################################
  # Alumni tab
  #####################################################################
  
  observeEvent(input$tabs == "alumni", {
    req(data)
    updateSelectizeInput(session,
                         'entreprise', 
                         choices = as.matrix(cbind(data()$Stage1_entreprise,
                                                   data()$Alternance_entreprise,
                                                   data()$Stage2_entreprise)) %>% as.vector() %>% unique() %>% sort(), 
                         server = TRUE)
    updateSelectizeInput(session, 
                         'ville', 
                         choices = as.matrix(cbind(data()$Stage1_ville,
                                                   data()$Alternance_ville,
                                                   data()$Stage2_ville)) %>% as.vector() %>% unique() %>% sort(), 
                         server = TRUE)
    updateSelectizeInput(session, 
                         'pays', 
                         choices = as.matrix(cbind(data()$Stage1_pays,
                                                   data()$Alternance_pays,
                                                   data()$Stage2_pays)) %>% as.vector() %>% unique() %>% sort(), 
                         server = TRUE)
    updateSelectizeInput(session, 
                         'domaine', 
                         choices = as.matrix(cbind(data()$Stage1_domaine,
                                                   data()$Alternance_domaine,
                                                   data()$Stage2_domaine)) %>% as.vector() %>% unique() %>% sort(), 
                         server = TRUE)
    updateSelectizeInput(session, 
                         'contrat', 
                         choices = data()$Poursuite_contrat %>% unique() %>% sort(), 
                         server = TRUE)
    
    updateSelectizeInput(session, 
                         'annee', 
                         choices = data()$Annee_sortie %>% unique() %>% sort(), 
                         server = TRUE)
    
    runjs("
      $('.box').on('click', '.box-header h3', function() {
          $(this).closest('.box')
                 .find('[data-widget=collapse]')
                 .click();
      });")
    
    data_filter <- reactive({
      req(data)
      df <- data()
      df <- df %>% dplyr::filter(
        conditional(!is.null(input$parcours), Parcours %in% input$parcours),
        conditional(!is.null(input$annee), Annee_sortie %in% input$annee),
        conditional(!is.null(input$entreprise), Stage1_entreprise %in% input$entreprise | Stage2_entreprise %in% input$entreprise |Alternance_entreprise %in% input$entreprise),
        conditional(!is.null(input$ville), Stage1_ville %in% input$ville | Stage2_ville %in% input$ville |Alternance_ville %in% input$ville),
        conditional(!is.null(input$pays), Stage1_pays %in% input$pays | Stage2_pays %in% input$pays |Alternance_pays %in% input$pays),
        conditional(!is.null(input$domaine), Stage1_domaine %in% input$domaine | Stage2_domaine %in% input$domaine |Alternance_domaine %in% input$domaine)
      )
      validate(need(nrow(df)!=0, "Oups, pas de résultats pour cette recherche.  \n Essaye à nouveau en modifiant les filtres !"))
      df
    })
    
    req(data_filter)
    v <- reactive({
      req(data_filter)
      v <- list()
      for (i in 1:nrow(data_filter())){
        v[[i]] <- box2(title = h3(paste0(data_filter()$Prenom[i]," ",data_filter()$Nom[i],"  \n  "), 
                                  style = "display:inline; font-weight:bold"),
                       if(!is.na(data_filter()$Parcours[i])){
                         h4(HTML(paste0("<b>Parcours :</b>  ",data_filter()$Parcours[i])))
                       },
                       if(!is.na(data_filter()$Annee_sortie[i])){
                         h4(HTML(paste0("<b>Année diplôme :</b>  ",data_filter()$Annee_sortie[i])))
                       },
                       if(!is.na(data_filter()$Stage1_entreprise[i]) &
                          !is.na(data_filter()$Stage1_site_web[i]) &
                          !is.na(data_filter()$Stage1_ville[i]) &
                          !is.na(data_filter()$Stage1_pays[i]) &
                          !is.na(data_filter()$Stage1_domaine[i])){
                         h4(HTML(paste0("<b>Stage M1 :</b>  ",
                                        a(data_filter()$Stage1_entreprise[i],
                                          href = data_filter()$Stage1_site_web[i],
                                          target = "_blank"),
                                        ", ",
                                        data_filter()$Stage1_ville[i],
                                        ", ",
                                        data_filter()$Stage1_pays[i],
                                        ", #",
                                        data_filter()$Stage1_domaine[i])))
                       },
                       if(!is.na(data_filter()$Alternance_entreprise[i]) &
                          !is.na(data_filter()$Alternance_site_web[i]) &
                          !is.na(data_filter()$Alternance_ville[i]) &
                          !is.na(data_filter()$Alternance_pays[i]) &
                          !is.na(data_filter()$Alternance_domaine[i])){
                         h4(HTML(paste0("<b>Alternance :</b>  ",
                                        a(data_filter()$Alternance_entreprise[i],
                                          href = data_filter()$Alternance_site_web[i],
                                          target = "_blank"),
                                        ", ",
                                        data_filter()$Alternance_ville[i],
                                        ", ",
                                        data_filter()$Alternance_pays[i],
                                        ", #",
                                        data_filter()$Alternance_domaine[i])))
                       },
                       if(!is.na(data_filter()$Stage2_entreprise[i]) &
                          !is.na(data_filter()$Stage2_site_web[i]) &
                          !is.na(data_filter()$Stage2_ville[i]) &
                          !is.na(data_filter()$Stage2_pays[i]) &
                          !is.na(data_filter()$Stage2_domaine[i])){
                         h4(HTML(paste0("<b>Stage M2 :</b>  ",
                                        a(data_filter()$Stage2_entreprise[i],
                                          href = data_filter()$Stage2_site_web[i],
                                          target = "_blank"),
                                        ", ",
                                        data_filter()$Stage2_ville[i],
                                        ", ",
                                        data_filter()$Stage2_pays[i],
                                        ", #",
                                        data_filter()$Stage2_domaine[i])))
                       },
                       if(!is.na(data_filter()$Poursuite_entreprise[i]) &
                          !is.na(data_filter()$Poursuite_site_web[i]) &
                          !is.na(data_filter()$Poursuite_ville[i]) &
                          !is.na(data_filter()$Poursuite_pays[i]) &
                          !is.na(data_filter()$Poursuite_domaine[i])){
                         h4(HTML(paste0("<b>Post-master :</b>  ",
                                        a(data_filter()$Poursuite_entreprise[i],
                                          href = data_filter()$Poursuite_site_web[i],
                                          target = "_blank"),
                                        ", ",
                                        data_filter()$Poursuite_ville[i],
                                        ", ",
                                        data_filter()$Poursuite_pays[i],
                                        ", #",
                                        data_filter()$Poursuite_domaine[i])))
                       },
                       if(!is.na(data_filter()$Linkedin[i])){
                         h4(a("Linkedin",
                              href = data_filter()$Linkedin[i],
                              target = "_blank"))
                         
                       },
                       collapsible = T,
                       collapsed = T
        )
      }
      v
    })
    
    
    page_break <- reactive({6})
    n_items <- reactive(length(v()))
    print(n_items)
    page_indices <- shinyThings::pager("pager", n_items, page_break)
    
    output$myboxes <- renderUI({
      return(v()[page_indices()])
    })
  })
  
  #####################################################################
  # Map tab
  #####################################################################
  
  observeEvent(input$tabs == "stage", {
    
    req(data_stage_clean_points)
    
    # remove duplicate labs ?
    
    #print(data_stage_clean_points)
    
    getColor <- function(data_stage_clean_points) {
      sapply(data_stage_clean_points$Stage, function(Stage) {
        if(Stage == 1) {
          "green"
        } else if(Stage == 2) {
          "orange"
        } else {
          "red"
        } 
      })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(data_stage_clean_points())
      
    )
    
    #print(icons)
    
    output$mymap <- renderLeaflet({
      leaflet(data_stage_clean_points()) %>%
        addTiles() %>%
        addAwesomeMarkers(~ lat, ~ long, icon = icons, popup = ~ Entreprise,
                          clusterOptions = markerClusterOptions())
    })
  })
  
  #####################################################################
  # Plot tab
  #####################################################################
  
  # Plot type de contrat insertion
  pie_plot_insertion <- reactive({
    
    # Recupère les données 
    req(data)
    pie_data <- table(data()$Poursuite_contrat)
    
    # Transforme les données en data.frame pour la suite (ggplot2)
    pie_data <- data.frame(pie_data)
    colnames(pie_data) <- c("Contrat", "Effectif")
    
    # Suppression des NA dans le jeu de données 
    pie_data <- subset(pie_data, Contrat != "")
    
    # Appel de la fonction pour créer le plot
    create_pie_plot_poursuite(pie_data)
    
  })
  
  # Plot domaine
  pie_plot_domaine <- reactive({
    
    # Recupère les données des différents domaines toutes expériences confondues
    req(data)
    
    stage1 <- table(data()$Stage1_domaine)
    alternance <- table(data()$Alternance_domaine)
    stage2 <- table(data()$Stage2_domaine)
    
    # Combiner les données en un seul tableau 
    pie_data <- data.frame(
      Value = names(c(stage1,  alternance, stage2)),
      Total_Frequency = as.vector(c(stage1,  alternance, stage2))
    )
    
    colnames(pie_data) <- c("Domaine", "Effectif")
    
    # Suppression des NA dans le jeu de données + grouper les domaines
    pie_data <- subset(pie_data, Domaine != "") %>%
      group_by(Domaine) %>%
      summarize(Effectif = sum(Effectif) + n() - 1)
    
    # Appel de la fonction pour créer le plot
    create_pie_plot_domaine(pie_data)
    
  })
  
  # Plot repartition des sexes 
  barplot_sexe <- reactive({
    
    # Recupère les données 
    req(data)
    data_sexe <- table(data()$Sexe, data()$Parcours) 
    
    # Supprimer les valeurs manquantes du jeu de données
    row_names <- rownames(data_sexe)
    empty_labels <- row_names[row_names == ""]
    data_sexe <- data_sexe[!rownames(data_sexe) %in% empty_labels, ]
    
    # Transforme les données en data.frame pour la suite (ggplot2)
    data_sexe <- data.frame(data_sexe)
    colnames(data_sexe) <- c("Sexe", "Parcours", "Effectif")
    
    # Appel de la fonction pour créer le plot
    create_plot_sexe(data_sexe)
    
  })
  
  
  # Plot effectif par promo 
  barplot_effectif_promo <- reactive({
    
    # Recupère les données 
    req(data)
    data_promo <- table(data()$Annee_sortie,data()$Parcours)
    
    # Transforme les données en data.frame pour la suite (ggplot2)
    data_promo <- data.frame(data_promo)
    
    colnames(data_promo) <- c("Promotion", "Parcours", "Effectif")
    data_promo <- subset(data_promo, Parcours != "")
    #print(data_promo)
    
    # Appel de la fonction pour créer le plot
    create_plot_effectif_promo(data_promo)
    
  })
  
  
  output$plot_insertion <- renderPlot({
    pie_plot_insertion()
    
  })
  
  output$plot_domaines <- renderPlot({
    pie_plot_domaine() 
    
  })
  
  output$plot_sexes <- renderPlot({
    barplot_sexe() 
    
  })
  
  output$plot_effectif_promo <- renderPlot({
    barplot_effectif_promo() 
    
  })
  
  
  output$dl_poursuite <- downloadHandler(
    filename = function() {
      "poursuite.pdf"
    },
    content = function(file) {
      ggsave(file, plot = pie_plot_insertion(), device = "pdf")
      
    }
  )
  
  output$dl_domaines <- downloadHandler(
    filename = function() {
      "domaines.pdf"
    },
    content = function(file) {
      ggsave(file, plot = pie_plot_domaine(), device = "pdf")
      
    }
  )
  
  output$dl_sexes <- downloadHandler(
    filename = function() {
      "sexes.pdf"
    },
    content = function(file) {
      ggsave(file, plot = barplot_sexe(), device = "pdf")
      
    }
  )
  
  output$dl_effectif_promo <- downloadHandler(
    filename = function() {
      "effectif_promos.pdf"
    },
    content = function(file) {
      ggsave(file, plot = barplot_effectif_promo(), device = "pdf")
      
    }
  )
  
}

shinyApp(ui = ui, server = server)