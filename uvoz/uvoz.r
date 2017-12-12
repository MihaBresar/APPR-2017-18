sl <- locale("sl", decimal_mark = ",", grouping_mark = ".")


uvozi.draft1 <- function() {
  link <- "https://en.wikipedia.org/wiki/2007_NBA_draft"
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




uvozi.draft2 <- function() {
  link <- "https://en.wikipedia.org/wiki/2008_NBA_draft"
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
a = uvozi.draft1()
a["Leto"] <- NA
a$Leto <- 2007
b = uvozi.draft2()
b["Leto"] <- NA
b$Leto <- 2008
c = rbind(a,b)
