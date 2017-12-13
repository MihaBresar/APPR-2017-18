sl <- locale("sl", decimal_mark = ",", grouping_mark = ".")

uvozi.draft<- function(site) {
  link <- site
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable sortable']") %>%
    .[[1]] %>% html_table(dec = ",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("Krog", "Izbor", "Igralec", "Pozicija", "Državljanstvo",
                        "Ekipa", "Ekipa pred naborom")
  for (col in c("Krog", "Izbor")) {
    tabela[[col]] <- parse_number(tabela[[col]], na = "-", locale = sl)
  }
  for (col in c("Igralec", "Pozicija", "Državljanstvo","Ekipa", "Ekipa pred naborom")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  
  return(tabela)
}




a1 = uvozi.draft("https://en.wikipedia.org/wiki/2007_NBA_draft")
a1["Leto"] <- NA
a1$Leto <- 2007
a2 = uvozi.draft("https://en.wikipedia.org/wiki/2008_NBA_draft")
a2["Leto"] <- NA
a2$Leto <- 2008
a3 = uvozi.draft("https://en.wikipedia.org/wiki/2009_NBA_draft")
a3["Leto"] <- NA
a3$Leto <- 2009
a4 = uvozi.draft("https://en.wikipedia.org/wiki/2010_NBA_draft")
a4["Leto"] <- NA
a4$Leto <- 2010
a5 = uvozi.draft("https://en.wikipedia.org/wiki/2011_NBA_draft")
a5["Leto"] <- NA
a5$Leto <- 2011
a6 = uvozi.draft("https://en.wikipedia.org/wiki/2012_NBA_draft")
a6["Leto"] <- NA
a6$Leto <- 2012
c = rbind(a1,a2,a3,a4,a5,a6)

