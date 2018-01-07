library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Grafični prikaz"),
  
  tabsetPanel(
    tabPanel("Tabela",
             hr(),
             sidebarLayout(
               
               sidebarPanel(
                 selectInput("sprem1", label="Izberi spremenljivko",
                             choices=colnames(Skupni_podatki[c(-4,-14,-16)]), selected='WS')),
               mainPanel(DT::dataTableOutput("tabele")))
    ),
    
    tabPanel("Graf", 

             sidebarPanel(
               selectInput("sprem2", label="Izberi spremenljivko",
                           choices=colnames(nabori[c(-1,-8)]), selected='WS'),
               checkboxGroupInput('ostalo', label="Izberi države",
                                               selected=c(1))
             ),
             mainPanel(plotOutput("grafi")))
    )
    
    
    
    
))
    
