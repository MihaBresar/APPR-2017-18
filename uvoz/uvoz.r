
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
  colnames(tabela) <- c("Rk", "Pk", "Igralec", "Pozicija", "Drzavljanstvo",
                        "Ekipa", "Ekipa pred naborom")
  for (col in c("Rk", "Pk")) {
    tabela[[col]] <- parse_number(tabela[[col]], na = "-", locale = sl)
  }
  for (col in c("Igralec", "Pozicija", "Drzavljanstvo","Ekipa", "Ekipa pred naborom")) {
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
Podatki_wiki= rbind(a1,a2,a3,a4,a5,a6)



uvoz_csv <- function(mapa) {
  data <- read_csv(mapa, 
                   locale=locale(encoding="cp1250"),skip = 1)
  
  return(data)
}



b1 <- uvoz_csv("podatki/2007draft.csv")
b2 <- uvoz_csv("podatki/2008draft.csv")
b3 <- uvoz_csv("podatki/2009draft.csv")
b4 <- uvoz_csv("podatki/2010draft.csv")
b5 <- uvoz_csv("podatki/2011draft.csv")
b6 <- uvoz_csv("podatki/2012draft.csv")

Podatki_csv <- rbind(b1,b2,b3,b4,b5,b6)
Podatki_csv <- separate(Podatki_csv,"Player", c("Igralec", "koda"), sep = "\\\\")
Podatki_csv <- Podatki_csv[-c(5)]
Podatki_csv <- replace_na(Podatki_csv, list(G = 0))
Podatki_wiki["Igralec"] <- Podatki_csv["Igralec"]
df <- Podatki_wiki[,c("Igralec","Pozicija","Drzavljanstvo","Leto")]
Skupni_podatki<- inner_join(Podatki_csv, df, by = NULL, copy = FALSE)




