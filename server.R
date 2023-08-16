
#####################################################################
# Initialisation des packages 

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

#####################################################################
# Useful functions
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

############################################################################################################################

# Debut
function(input, output, session) {
  
  # read data file
  data <- reactive({
    df <- fread("data/data_test.csv")
    #replace empty cell with NA
    df[df == ''] <- NA
    df <- df[order(df$Nom, df$Prenom), ]
    df
  })
  
  # tab Accueil
  output$logo_master <- renderImage({
    list(src = "img/BioInfo_logo_quadri_fdclair.png",
         height = 330)
  }, deleteFile = F)
  
  # tab Alumni
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
         v[[i]] <- box2(title = h3(paste0(data_filter()[i]$Prenom," ",data_filter()[i]$Nom,"  \n  "), 
                                   style = "display:inline; font-weight:bold"),
                        if(!is.na(data_filter()[i]$Parcours)){
                          h4(HTML(paste0("<b>Parcours :</b>  ",data_filter()[i]$Parcours)))
                        },
                        if(!is.na(data_filter()[i]$Annee_sortie)){
                          h4(HTML(paste0("<b>Année diplôme :</b>  ",data_filter()[i]$Annee_sortie)))
                        },
                        if(!is.na(data_filter()[i]$Stage1_entreprise) &
                                  !is.na(data_filter()[i]$Stage1_site_web) &
                                         !is.na(data_filter()[i]$Stage1_ville) &
                                                !is.na(data_filter()[i]$Stage1_pays) &
                                                       !is.na(data_filter()[i]$Stage1_domaine)){
                          h4(HTML(paste0("<b>Stage M1 :</b>  ",
                                         '<a href="',
                                         data_filter()[i]$Stage1_site_web,
                                         '">',
                                         data_filter()[i]$Stage1_entreprise,
                                         '</a>, ',
                                         data_filter()[i]$Stage1_ville,
                                         ", ",
                                         data_filter()[i]$Stage1_pays,
                                         ", #",
                                         data_filter()[i]$Stage1_domaine)))
                        },
                        if(!is.na(data_filter()[i]$Alternance_entreprise) &
                                  !is.na(data_filter()[i]$Alternance_site_web) &
                                         !is.na(data_filter()[i]$Alternance_ville) &
                                                !is.na(data_filter()[i]$Alternance_pays) &
                                                       !is.na(data_filter()[i]$Alternance_domaine)){
                          h4(HTML(paste0("<b>Alternance :</b>  ",
                                         '<a href="',
                                         data_filter()[i]$Alternance_site_web,
                                         '">',
                                         data_filter()[i]$Alternance_entreprise,
                                         '</a>,',
                                         data_filter()[i]$Alternance_ville,
                                         ", ",
                                         data_filter()[i]$Alternance_pays,
                                         ", #",
                                         data_filter()[i]$Alternance_domaine)))
                        },
                        if(!is.na(data_filter()[i]$Stage2_entreprise) &
                                  !is.na(data_filter()[i]$Stage2_site_web) &
                                         !is.na(data_filter()[i]$Stage2_ville) &
                                                !is.na(data_filter()[i]$Stage2_pays) &
                                                       !is.na(data_filter()[i]$Stage2_domaine)){
                          h4(HTML(paste0("<b>Stage M2 :</b>  ",
                                         '<a href="',
                                         data_filter()[i]$Stage2_site_web,
                                         '">',
                                         data_filter()[i]$Stage2_entreprise,
                                         '</a>, ',
                                         data_filter()[i]$Stage2_ville,
                                         ", ",
                                         data_filter()[i]$Stage2_pays,
                                         ", #",
                                         data_filter()[i]$Stage2_domaine)))
                        },
                        if(!is.na(data_filter()[i]$Poursuite_entreprise) &
                                  !is.na(data_filter()[i]$Poursuite_site_web) &
                                         !is.na(data_filter()[i]$Poursuite_ville) &
                                                !is.na(data_filter()[i]$Poursuite_pays) &
                                                       !is.na(data_filter()[i]$Poursuite_domaine)){
                          h4(HTML(paste0("<b>Post-master :</b>  ",
                                         '<a href="',
                                         data_filter()[i]$Poursuite_site_web,
                                         '">',
                                         data_filter()[i]$Poursuite_entreprise,
                                         '</a>, ',
                                         data_filter()[i]$Poursuite_ville,
                                         ", ",
                                         data_filter()[i]$Poursuite_pays,
                                         ", #",
                                         data_filter()[i]$Poursuite_domaine)))
                        },
                        if(!is.na(data_filter()[i]$Linkedin)){
                          h4(HTML(paste0('<a href="',data_filter()[i]$Linkedin,'">Linkedin</a>')))
                        },
                        collapsible = T,
                        collapsed = T
         )
       }
       v
     })
    
      output$myboxes <- renderUI({
        req(v)
        if(length(v())>=6){
          n_items <- reactiveVal(length(v()))
          page_break <- reactive({6})
          page_indices <- shinyThings::pager("pager", n_items, page_break)
          pagination <- shinyThings::paginationUI("pager", width = 8, offset = 0, class = "text-center")
          list_boxes <- v()[page_indices()]
          return(c(pagination[[3]],list_boxes))
        } else {
          return(v())
        }
      })
  })
  
  # tab Stages
  observeEvent(input$tabs == "stage", {
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
    
    # remove duplicate labs ?
    
    print(data_stage_clean_points)

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
      markerColor = getColor(data_stage_clean_points)

    )
    
    print(icons)
    
    output$mymap <- renderLeaflet({
      leaflet(data_stage_clean_points) %>%
        addTiles() %>%
        addAwesomeMarkers(~ lat, ~ long, icon = icons, popup = ~ Entreprise,
                          clusterOptions = markerClusterOptions())
    })
  })
}


