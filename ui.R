
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
box2 <- function(...){
  box(
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    ...
  )
}

# javascript code to collapse box
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

ui = dashboardPage(skin="black",
                   dashboardHeader(title = "BiMS'TRO", titleWidth = 350),
    dashboardSidebar(
      #width sidebar
      width = 350,
      sidebarMenu(
        id="tabs",
        # Menu 
        menuItem("Accueil", tabName = "accueil", icon = icon("database")),
        menuItem("Alumni", tabName = "alumni", icon = icon("sitemap")),
        menuItem("Stage", tabName = "stage", icon = icon("chart-pie")),
        menuItem("Stats", tabName = "stats", icon = icon("th"))
      )
    ),
    #extendShinyjs(text = 'shinyjs.scrolltop = function() {window.scrollTo(0, 0)};', functions = c("scrolltop")),
    # Body content
    dashboardBody(  
      # Including Javascript
      useShinyjs(),
      
      tags$style(HTML("
      .box-header {
        padding: 0 10px 0 0;
      }
      .box-header h3 {
        width: 100%;
        padding: 10px;
      }")),
      
      tabItems(
        tabItem("accueil",
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
                   conditionalPanel("input.contrat.length >= 1|input.domaine.length >= 1|input.pays.length >= 1|input.ville.length >= 1|input.annee.length >= 1|input.entreprise.length >= 1|input.parcours.length >= 1",
                     column(width=8,
                            uiOutput("myboxes")
                     )
                   ),
                   conditionalPanel("input.contrat.length < 1 & input.domaine.length < 1 & input.pays.length < 1 & input.ville.length < 1 & input.annee.length < 1 & input.entreprise.length < 1 & input.parcours.length < 1",
                                    column(width=8,
                                           box2(title = "Résultats",
                                                h2("Trop de résultats, choisissez au moins un filtre s'il-vous-plaît")
                                           )
                                    )
                   )
                 )
        ),
        tabItem("stage"
        ),
        tabItem("stats"
        )
      )
    )
)

        
        
        
        
        
        
