
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
if (!require('data.table', quietly = T)) install.packages("data.table");

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

############################################################################################################################

# Debut
function(input, output, session) {
  
  
  data <- reactive({
    df <- fread("data/data_test.csv")
    df
  })
  
  output$logo_master <- renderImage({
    list(src = "img/BioInfo_logo_quadri_fdclair.png",
         height = 330)
  }, deleteFile = F)
  
  observeEvent(input$tabs == "alumni", {
    req(data)
    updateSelectizeInput(session,
                         'entreprise', 
                         choices = as.matrix(cbind(data()$Stage1_entreprise,
                                                   data()$Alternance_entreprise,
                                                   data()$Stage2_entreprise)) %>% as.vector() %>% unique(), 
                         server = TRUE)
    updateSelectizeInput(session, 
                         'ville', 
                         choices = as.matrix(cbind(data()$Stage1_ville,
                                                   data()$Alternance_ville,
                                                   data()$Stage2_ville)) %>% as.vector() %>% unique(), 
                         server = TRUE)
    updateSelectizeInput(session, 
                         'pays', 
                         choices = as.matrix(cbind(data()$Stage1_pays,
                                                   data()$Alternance_pays,
                                                   data()$Stage2_pays)) %>% as.vector() %>% unique(), 
                         server = TRUE)
    updateSelectizeInput(session, 
                         'domaine', 
                         choices = as.matrix(cbind(data()$Stage1_domaine,
                                                   data()$Alternance_domaine,
                                                   data()$Stage2_domaine)) %>% as.vector() %>% unique(), 
                         server = TRUE)
    updateSelectizeInput(session, 
                         'contrat', 
                         choices = data()$Poursuite_contrat %>% unique(), 
                         server = TRUE)
  
    updateSelectizeInput(session, 
                         'annee', 
                         choices = data()$Annee_sortie %>% unique(), 
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
        if(!is.null(input$parcours)){
          df <- df %>% dplyr::filter(Parcours %in% input$parcours)
        } else if (!is.null(input$annee)){
          df <- df %>% dplyr::filter(Annee_sortie %in% input$annee)
        } else if (!is.null(input$entreprise)){
          df <- df %>% dplyr::filter(Stage1_entreprise %in% input$entreprise | Stage2_entreprise %in% input$entreprise |Alternance_entreprise %in% input$entreprise)
        } else if (!is.null(input$ville)){
          df <- df %>% dplyr::filter(Stage1_ville %in% input$ville | Stage2_ville %in% input$ville |Alternance_ville %in% input$ville)
        } else if (!is.null(input$pays)){
          df <- df %>% dplyr::filter(Stage1_pays %in% input$pays | Stage2_pays %in% input$pays |Alternance_pays %in% input$pays)
        } else if (!is.null(input$domaine)){
          df <- df %>% dplyr::filter(Stage1_domaine %in% input$domaine | Stage2_domaine %in% input$domaine |Alternance_domaine %in% input$domaine)
        } else {
          df <- data()
        }
        df
      })

     v <- reactive({
       req(data_filter)
       v <- list()
       for (i in 1:nrow(data_filter())){
         v[[i]] <- box2(title = h3(paste0(data_filter()[i]$Prenom," ",data_filter()[i]$Nom), 
                                   style = "display:inline; font-weight:bold"),
                        h4(HTML(paste0("<b>Parcours :</b>  ",data_filter()[i]$Parcours))),
                        h4(HTML(paste0("<b>Année diplôme :</b>  ",data_filter()[i]$Annee_sortie))),
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
                                       data_filter()[i]$Stage1_domaine))),
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
                                       data_filter()[i]$Alternance_domaine))),
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
                                       data_filter()[i]$Stage2_domaine))),
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
                                       data_filter()[i]$Poursuite_domaine))),
                        h4(HTML(paste0('<a href="',data_filter()[i]$Linkedin,'">Linkedin</a>'))),
                        collapsible = T,
                        collapsed = T
         )
       }
       v
     })
     
    req(v) 
    output$myboxes <- renderUI(v())
  })
}

