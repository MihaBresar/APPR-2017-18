

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

Podatki_wiki$Drzavljanstvo <- as.character(Podatki_wiki$Drzavljanstvo)
Podatki_wiki[9,5] <- "France"
Podatki_wiki[83,5] <- "Greece"
Podatki_wiki[84,5] <- "Spain"
Podatki_wiki[144,5] <- "United Kingdom"
Podatki_wiki[165,5] <- "Greece"
Podatki_wiki[170,5] <- "Bosnia and Herzegovina"
Podatki_wiki[177,5] <- "Turkey"
Podatki_wiki[243,5] <- "Turkey"
Podatki_wiki[256,5] <- "Montenegro"
Podatki_wiki[263,5] <- "Spain"
Podatki_wiki[271,5] <- "Croatia"
Podatki_wiki[294,5] <- "Serbia"
Podatki_wiki[352,5] <- "Serbia"
Podatki_wiki$Drzavljanstvo <- as.factor(Podatki_wiki$Drzavljanstvo)




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
Podatki_csv$Tm[Podatki_csv$Tm == "SEA"] <- "OKC"
Podatki_csv$Tm[Podatki_csv$Tm == "NOK"] <- "NOH"

Podatki_csv <- separate(Podatki_csv,"Player", c("Igralec", "koda"), sep = "\\\\")
Podatki_csv <- Podatki_csv[-c(5,6)]
Podatki_wiki["Igralec"] <- Podatki_csv["Igralec"]
Podatki_csv["Rk"] <- Podatki_wiki["Rk"]
df <- Podatki_wiki[,c("Igralec","Pozicija","Drzavljanstvo","Leto")]
Skupni_podatki<- inner_join(Podatki_csv, df, by = NULL, copy = FALSE)
Skupni_podatki[is.na(Skupni_podatki)] = 0
Podatki_csv[is.na(Podatki_csv)] = 0

rm(df,a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6)
Skupni_podatki <- Skupni_podatki[-c(7:10,12:13,19,20)]

Pravi_2006<- uvoz_csv("podatki/2006draft.csv")
Pravi_2006 <- separate(Pravi_2006,"Player", c("Igralec", "koda"), sep = "\\\\")
Pravi_2006 <- Pravi_2006[-c(5,6)]
Pravi_2006 <- Pravi_2006[c(2,4,14:18,21)]

Pravi_2006 <- filter(Pravi_2006, Pravi_2006$Pk < 7)



Pravi_2013<- uvoz_csv("podatki/2013draft.csv")
Pravi_2013 <- separate(Pravi_2013,"Player", c("Igralec", "koda"), sep = "\\\\")
Pravi_2013 <- Pravi_2013[-c(5,6)]
Pravi_2013 <- Pravi_2013[c(2,4,14:18,21)]

Pravi_2013 <- filter(Pravi_2013, Pravi_2013$Pk < 6)


