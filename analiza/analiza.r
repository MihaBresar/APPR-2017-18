Moc_izborov <- aggregate(Skupni_podatki$WS, by=list(Skupni_podatki$Pk), FUN=sum)
colnames(Moc_izborov) <- c("Izbor","Moč")
Uspešnost_ekip <- aggregate(Skupni_podatki$WS, by=list(Skupni_podatki$Tm), FUN=sum)

ocene_igralcev <-  Skupni_podatki[c(1,2,3,4,18)]
colnames(ocene_igralcev) <- c("runda","Izbor","Ekipa","Igralec","WS")

ocene_igralcev<- inner_join(ocene_igralcev, Moc_izborov, by = NULL, copy = FALSE)

a <- ocene_igralcev["WS"]/abs(ocene_igralcev["Moč"])



