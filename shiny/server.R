library(shiny)
require(stats)
library(mgcv)
#source("../vizualizacija/vizualizacija.r", encoding = "UTF-8")


shinyServer(function(input, output) {
  output$tabele <- DT::renderDataTable({
    dcast(Skupni_podatki[c('Igralec', 'Pozicija', input$sprem1)], Igralec ~ Pozicija, value.var = input$sprem1)

  })
  
  
  output$grafi <- renderPlot({
    tabela <- nabori[c('Nabor', input$sprem2)]
    colnames(tabela) <- c('Nabor', 'sprem')
    print(tabela)
    lin <- lm(data = tabela, sprem ~ Nabor)
    print(ggplot(tabela) +
            aes(x=Nabor, y=sprem) + geom_line() +
            ggtitle(paste(input$sprem2, 'skozi leta', sep = ' ')) +
            xlab('Nabor') + ylab(input$sprem2))
  })
  
})
