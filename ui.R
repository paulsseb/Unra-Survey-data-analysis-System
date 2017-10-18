library(shinythemes)
library(shiny)


rm(list = ls())
shinyUI(fluidPage(
  theme = shinytheme("superhero"),
  tags$head(
    tags$style(HTML("
      @import url('../ww/css.txt');
    "))
  ),
 
  #cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.
  # Application title

  # Application title
  titlePanel( 
    headerPanel(
      h1("UNRA SURVEY DATA ANALYSIS SYSTEM", 
         style = "font-family: 'Segoui', cursive;
         font-weight: 500; line-height: 1.1; 
         color:white;"))),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("choose_dataset"),
      hr(),
      
      

      uiOutput("choose_question"),
      hr(),
      
      uiOutput("choose_category"),
      hr()
    
      
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
     
      tabsetPanel(
        tabPanel("Regions", plotOutput("plotsou",height=600),plotOutput("plotcen",height=600), plotOutput("ploteas",height=600),plotOutput("plotnor",height=600),plotOutput("plotwes",height=600 )), 
        tabPanel("Map", plotOutput("map"), plotOutput("map1"), plotOutput("map2"), plotOutput("map3"), plotOutput("map4")), 
        tabPanel("Districts",plotOutput("Dvd"), plotOutput("Dd"),plotOutput("Dnsnd"),plotOutput("Dsa"),plotOutput("Dvrys")),
        tabPanel("Road Types", plotOutput("roadt",height=600), plotOutput("roadt1",height=600), plotOutput("roadt2",height=600), plotOutput("roadt3",height=600), plotOutput("roadt4",height=600)),
        tabPanel("Respondent Gender", plotOutput("gender",height=600), plotOutput("gender1",height=600), plotOutput("gender2",height=600), plotOutput("gender3",height=600), plotOutput("gender4",height=600)),
        tabPanel("Road name", plotOutput("rdname",height=1000), plotOutput("rdname1",height=1000), plotOutput("rdname2",height=1000), plotOutput("rdname3",height=1000), plotOutput("rdname4",height=1000)),
        tabPanel("Respondent Type", plotOutput("respt"), plotOutput("respt1"), plotOutput("respt2"), plotOutput("respt3"), plotOutput("respt4")),
        tabPanel("Age Bracket", plotOutput("age"), plotOutput("age1"), plotOutput("age2"), plotOutput("age3"), plotOutput("age4")),
        tabPanel("Amin Login", uiOutput("uiLogin"),
                 textOutput("pass"),uiOutput("upload"))
      )
      
    )
  )
))
