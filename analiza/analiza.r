
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
colnames(Uspesnost_ekip2) <- c("Ekipa","Skupna_koristnost")
colnames(Uspesnost_ekip3) <- c("Ekipa","Povprecen_izbor")


Uspesnost_ekip <- merge(Uspesnost_ekip1,Uspesnost_ekip2)
Uspesnost_ekip <- merge(Uspesnost_ekip,Uspesnost_ekip3)
rm(Uspesnost_ekip1,Uspesnost_ekip2,Uspesnost_ekip3)

