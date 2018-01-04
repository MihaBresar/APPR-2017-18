
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
m <- 3
skupine2 <- hclust(dist(scale(Ekipe1))) %>% cutree(m)

ex=subset(Ekipe1, Uspeh>=max(Uspeh)|Relativen_uspeh>=max(Relativen_uspeh))




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



Graf_ekip <- ggplot(inner_join(Uspesnost_ekip, data.frame(Ekipa = names(skupine2),skupina = factor(skupine2)), by = "Ekipa")
                         , aes(x = Uspeh, y = Relativen_uspeh, color = skupina, size = Povprecen_izbor/30, label=Uspesnost_ekip$Ekipa))+ geom_point() +
  ggtitle("graf evropskih") +
  xlab(expression("Uspeh")) + ylab("Relativen uspeh") +
  guides(color = guide_legend(title = "Skupina"),
         size = guide_legend(title = "Povprecen_izbor")) +
  geom_text()

Graf_evropskih <- ggplot(inner_join(Evropske, data.frame(drzava = names(skupine_drzav),skupina = factor(skupine_drzav)), by = "drzava")
                         , aes(x = Uspeh, y = Let, color = skupina, size = Igralcev/1, label = Evropske$drzava)) + geom_point() +
  ggtitle("graf evropskih") +
  xlab(expression("Uspeh")) + ylab("Št. let v ligi") +
  guides(color = guide_legend(title = "Skupina"),
         size = guide_legend(title = "Št.Igralcev")) +
  geom_text(check_overlap = TRUE) + 
  












#Zemljevidi:




zemljevid <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", 
                             encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(long > -30, long < 50, lat > 25, lat < 75)


colnames(zemljevid)[11] <- 'drzava'

zemljevid$drzava <- as.character(zemljevid$drzava)
zemljevid$drzava[zemljevid$drzava == "Republic of Serbia"] <- "Serbia"
zemljevid$drzava <- as.factor(zemljevid$drzava)

Skupine <- data.frame(drzava = Evropske$drzava, skupina = factor(skupine_drzav))
rownames(Skupine)<- NULL
Skupine$skupina <- as.character(Skupine$skupina)
Skupine[1,2] <- "Manj uspešne"
Skupine[5,2] <- "Bolj uspešne"
Skupine[10,2] <- "Srednje uspešne"
Skupine$skupina <- as.factor(Skupine$skupina)
Skupine$skupina[Skupine$skupina == "1"] <- "Manj uspešne"
Skupine$skupina[Skupine$skupina == "2"] <- "Bolj uspešne"
Skupine$skupina[Skupine$skupina == "3"] <- "Srednje uspešne"





zemljevid1 <- ggplot() + geom_polygon(data = zemljevid %>% left_join(Skupine, by = c("drzava" = "drzava")),
                                       aes(x = long, y = lat, group = group, fill = skupina), show.legend=T) +
  ggtitle('Države razdeljene v 5 skupin glede na uspešnsost') + xlab("long") + ylab("lat") +
  coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))

zemljevid2 <- ggplot() + geom_polygon(data = zemljevid %>% left_join(Evropske),
                                      aes(x = long, y = lat, group = group, fill=Igralcev)) +
  ggtitle('Države glede na število igralcev v ligi NBA')
  coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))














