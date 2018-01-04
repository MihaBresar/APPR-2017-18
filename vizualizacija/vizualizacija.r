
# Tabele


Podatki_csv["WS"] <- abs(Podatki_csv["WS"])
Moc_izborov <- aggregate(Podatki_csv$WS, by=list(Podatki_csv$Pk), FUN=sum)
colnames(Moc_izborov) <- c("Izbor","Moč")

ocene_igralcev <-  Skupni_podatki[c(1,2,3,4,18)]
colnames(ocene_igralcev) <- c("runda","Izbor","Ekipa","Igralec","WS")

ocene_igralcev<- inner_join(ocene_igralcev, Moc_izborov, by = "Izbor", copy = FALSE)

a <- ocene_igralcev["WS"]/abs(ocene_igralcev["Moč"])

ocene_igralcev["uspeh"] <- a



Uspesnost_ekip1 <- aggregate(Skupni_podatki$WS, by=list(Skupni_podatki$Tm), FUN=sum)
Uspesnost_ekip2 <- aggregate(ocene_igralcev$uspeh, by=list(ocene_igralcev$Ekipa), FUN=sum)
Uspesnost_ekip3 <- aggregate(ocene_igralcev$Izbor, by=list(ocene_igralcev$Ekipa), FUN=mean)

colnames(Uspesnost_ekip1) <- c("Ekipa","Uspeh")
colnames(Uspesnost_ekip2) <- c("Ekipa","Relativen_uspeh")
colnames(Uspesnost_ekip3) <- c("Ekipa","Povprecen_izbor")


Uspesnost_ekip <- merge(Uspesnost_ekip1,Uspesnost_ekip2)
Uspesnost_ekip <- merge(Uspesnost_ekip,Uspesnost_ekip3)
rm(Uspesnost_ekip1,Uspesnost_ekip2,Uspesnost_ekip3)

Uspesnost_drzav <- aggregate(Skupni_podatki$WS, by=list(Skupni_podatki$Drzavljanstvo), FUN=sum)
leta_drzava <- aggregate(Skupni_podatki$Yrs, by=list(Skupni_podatki$Drzavljanstvo), FUN=sum)
igralci_drzava <- aggregate(Skupni_podatki$Leto, by=list(Skupni_podatki$Drzavljanstvo), FUN=sum)
igralci_drzava["x"] <- round(igralci_drzava["x"]/2007)

colnames(Uspesnost_drzav) <- c("drzava","Uspeh")
colnames(leta_drzava) <- c("drzava","Let")
colnames(igralci_drzava) <- c("drzava","Igralcev")

Uspesnost_drzav <- merge(Uspesnost_drzav,leta_drzava)
Uspesnost_drzav <- merge(Uspesnost_drzav,igralci_drzava)

Evropske <- Uspesnost_drzav[-c(1, 2, 3, 5, 6, 7, 8, 9, 12, 13, 16, 17, 18, 24, 29, 30, 31, 32, 33, 35, 40,44:53), ] 

Evropske.norm <- Evropske %>% select(-drzava, -Let, -Igralcev) %>% scale()
rownames(Evropske.norm) <- Evropske$drzava

najboljsi_igralci <-   Skupni_podatki[rev(order(Skupni_podatki$WS)),]
najboljsi_igralci <- najboljsi_igralci[c(1:10),]


row.names(Skupni_podatki) <- Skupni_podatki$Igralec
Skupni_podatki$Igralec <- NULL

Evropske1 <- Evropske
row.names(Evropske1) <- Evropske1$drzava
Evropske1$drzava <- NULL
Evropske1 <- Evropske1[c(1)]

Ekipe1 <- Uspesnost_ekip
row.names(Ekipe1) <- Ekipe1$Ekipa
Ekipe1$Ekipa <- NULL

#Število skupin
n <- 3
skupine_drzav <- hclust(dist(scale(Evropske1))) %>% cutree(n)
m <- 5
skupine_ekip <- hclust(dist(scale(Ekipe1))) %>% cutree(m)

skupine_ekip <- inner_join(Uspesnost_ekip, data.frame(Ekipa = names(skupine_ekip),skupina = factor(skupine_ekip)), by = "Ekipa")

skupine_ekip$skupina <- as.character(skupine_ekip$skupina)
skupine_ekip[1,5] <- "Povprečne"
skupine_ekip[6,5] <- "Podpovprečne"
skupine_ekip[2,5] <- "Boston"
skupine_ekip[11,5] <- "Indiana"
skupine_ekip[21,5] <- "Oklahoma"
skupine_ekip$skupina <- as.factor(skupine_ekip$skupina)

skupine_ekip$skupina[skupine_ekip$skupina == "1"] <- "Povprečne"
skupine_ekip$skupina[skupine_ekip$skupina == "3"] <- "Podpovprečne"

skupine_evropskih <- inner_join(Evropske, data.frame(drzava = names(skupine_drzav),skupina = factor(skupine_drzav)), by = "drzava")


skupine_evropskih$skupina <- as.character(skupine_evropskih$skupina)
skupine_evropskih[1,5] <- "Manj uspešne"
skupine_evropskih[5,5] <- "Bolj uspešne"
skupine_evropskih[10,5] <- "Srednje uspešne"
skupine_evropskih$skupina <- as.factor(skupine_evropskih$skupina)
skupine_evropskih$skupina[skupine_evropskih$skupina == "1"] <- "Manj uspešne"
skupine_evropskih$skupina[skupine_evropskih$skupina == "2"] <- "Bolj uspešne"
skupine_evropskih$skupina[skupine_evropskih$skupina == "3"] <- "Srednje uspešne"





#Grafi

graf_Uspeh <- ggplot(data = Uspesnost_ekip) + 
  aes(x = reorder(Ekipa, -Uspeh), y = Uspeh)+ 
  geom_bar(stat="identity",fill ="cornflowerblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Ekipa") + 
  ylab("Uspeh") +
  ggtitle("Uspešnost ekip na naborih")

graf_Relativen_uspeh <- ggplot(data = Uspesnost_ekip) + 
  aes(x = reorder(Ekipa, -Relativen_uspeh), y = Relativen_uspeh)+ 
  geom_bar(stat="identity",fill ="red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Ekipa") + 
  ylab("Relativen_uspeh") +
  ggtitle("Uspešnost ekip na naborih")



Graf_ekip <- ggplot(skupine_ekip, aes(x = Uspeh, y = Relativen_uspeh, color = skupina, size = Povprecen_izbor/30))+ geom_point() +
  ggtitle("Uspešnost ekip na naborih") +
  xlab(expression("Uspeh")) + ylab("Relativen uspeh") +
  guides(color = guide_legend(title = "Skupina"),
         size = guide_legend(title = "Povprecen_izbor"))



  
ex = skupine_evropskih[c(2,5,7,10,16,17,18,20),]

Graf_EU <- ggplot() + 
    geom_point(data=skupine_evropskih, mapping=aes(x=Igralcev, y=Uspeh, fill=skupina), size=3, shape=21, color="black") +
    geom_text(data=ex, mapping=aes(x=Igralcev, y=Uspeh, label=drzava), size=3, vjust=3, hjust=0.5) +
    ggtitle("Graf")




#Zemljevidi:




zemljevid <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", 
                             encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(long > -30, long < 50, lat > 25, lat < 75)


colnames(zemljevid)[11] <- 'drzava'

zemljevid$drzava <- as.character(zemljevid$drzava)
zemljevid$drzava[zemljevid$drzava == "Republic of Serbia"] <- "Serbia"
zemljevid$drzava <- as.factor(zemljevid$drzava)







zemljevid1 <- ggplot(data = zemljevid) + geom_polygon(data = zemljevid %>% left_join(skupine_evropskih, by = c("drzava" = "drzava")),
                                                      aes(x = long, y = lat, group = group, fill = skupina),color = 'black', show.legend=T) +
  geom_text(data = inner_join(zemljevid, ex, by = c("drzava" = "drzava")) %>%
              group_by(drzava) %>%
              summarise(avg_long = mean(long), avg_lat = mean(lat)),
            aes(x = avg_long, y = avg_lat, label = drzava), color = "red") +
  xlab("long") + ylab("lat") +
  facet_wrap(scales = 'free')
  coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))

zemljevid2 <- ggplot() + geom_polygon(data = zemljevid %>% left_join(Evropske),
                                      aes(x = long, y = lat, group = group, fill=Igralcev)) +
  ggtitle('Države glede na število igralcev v ligi NBA')
  coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))














