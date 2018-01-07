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
             mainPanel(plotOutput("grafi"))),
    
  
    tabPanel("Primerjava",
           
           
           sidebarPanel(
             selectInput("spremenljivka1", label="Izberi spremenljivko na x osi",
                         choices=colnames(Skupni_podatki[c(-1,-2,-3,-4,-14,-15,-16)]), selected='WS'),
             selectInput("spremenljivka2", label="Izberi spremenljivko na y osi",
                         choices=colnames(Skupni_podatki[c(-1,-2,-3,-4,-14,-15,-16)]), selected='G'),
             sliderInput('letnica', label='Izberi leto',
                         min=2007, max=2012, step=1, value = 2007),
             selectInput("priblizek2", label="Izberi regresivno funkcijo:",
                         choices=c('Izberi funkcijo', 'y ~ x', 'y ~ x + I(x^2)', 'y ~ x + I(x^2) + I(x^3)'), selected='Izberi funkcijo'),
             sliderInput('skup', label='Izberi število skupin glede na BDPpc',
                         min=1, max=25, step=1, value = 5)

           ),
           mainPanel(plotOutput("primerjava")))
    
)
    
    
))
    
