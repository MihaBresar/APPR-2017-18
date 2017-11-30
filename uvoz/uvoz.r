uvozi.draft <- function() {
  link <- "https://en.wikipedia.org/wiki/2007_NBA_draft"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable sortable']") %>%
    .[[1]] %>% html_table(dec = ",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("1", "2", "3", "4", "5",
                        "6", "7")
  for (col in c("1", "2")) {
    tabela[[col]] <- parse_number(tabela[[col]], na = "-", locale = sl)
  }
  for (col in c("3", "4", "5","6","7")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  
  return(tabela)
}
  
