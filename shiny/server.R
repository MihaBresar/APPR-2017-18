



shinyServer(function(input, output) {

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
  
  output$primerjava <- renderPlot({
    tabela <- Skupni_podatki[c('Leto', 'Igralec', input$spremenljivka1[1], input$spremenljivka2[1])]
    colnames(tabela) <- c('Leto', 'Igralec', 'prva', 'druga')
    if (input$priblizek2 == 'Izberi funkcijo'){
      n <- sum(tabela$Leto == input$letnica)
      if (input$skup>=n){
        print(ggplot(tabela %>% filter(Leto==as.integer(input$letnica))) +
                aes(x=prva, y=druga, color=Igralec) + geom_point(size=2) +
                ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za Leto', as.character(input$letnica), sep = ' ')) +
                xlab(input$spremenljivka1) + ylab(input$spremenljivka2))
      } else {
        tabela$skupine <- hclust(dist(scale(Skupni_podatki$WS))) %>% cutree(input$skup)
        print(ggplot(tabela %>% filter(Leto==as.integer(input$letnica))) +
                aes(x=prva, y=druga, color=as.character(skupine)) + geom_point(size=3, show.legend=F) +
                ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za Leto', as.character(input$letnica), sep = ' ')) +
                xlab(input$spremenljivka1) + ylab(input$spremenljivka2))}
    } else {
      print(ggplot(tabela %>% filter(Leto==as.integer(input$letnica))) +
              aes(x=prva, y=druga) + geom_point() +
              geom_smooth(method = 'lm', formula = as.formula(input$priblizek2)) +
              ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za Leto', as.character(input$letnica), sep = ' ')) +
              xlab(input$spremenljivka1) + ylab(input$spremenljivka2))
    }
  })
  
})
