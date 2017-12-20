# 4. faza: Analiza podatkov
Moc_izborov <- aggregate(Skupni_podatki$WS, by=list(Skupni_podatki$Pk), FUN=sum)
colnames(Moc_izborov) <- c("Izbor","Moč")
Moc_naborov <- aggregate(Skupni_podatki$WS/48, by=list(Skupni_podatki$Leto), FUN=sum)
