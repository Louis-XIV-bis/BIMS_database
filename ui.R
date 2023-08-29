
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

# javascript code to collapse box
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

ui = dashboardPage(skin="black",
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

        
        
        
        
        
        
