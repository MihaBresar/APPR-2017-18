Prvih_deset <- filter(Skupni_podatki, Skupni_podatki$Pk < 11)

md1 = lm(MP_1 ~ Pk + Pozicija + Leto, data=Prvih_deset)
md2 = lm(WS ~ Pk + Pozicija + Leto, data=Prvih_deset)
md3 = lm(PTS_1 ~ Pk + Pozicija + Leto, data=Prvih_deset)
md4 = lm(TRB_1 ~ Pk + Pozicija + Leto, data=Prvih_deset)
md5 = lm(AST_1 ~ Pk + Pozicija + Leto, data=Prvih_deset)
md6 = lm(VORP ~ Pk + Pozicija + Leto, data=Prvih_deset)

#Napoved za leto 2013 in 2006

a1 <- c(1,2,3,4,5)
a2 <- c(2013,2013,2010,2013,2013)
a3 <- c('PF','SG','SF','C','C')

a <- data.frame(a1,a2,a3)
colnames(a) <- c('Pk','Leto','Pozicija')

s1 <- round(predict(md1,a),2)
s2 <- round(predict(md2,a),2)
s3 <- round(predict(md3,a),2)
s4 <- round(predict(md4,a),2)
s5 <- round(predict(md5,a),2)
s6 <- round(predict(md6,a),2)
  
Napoved2013 <- data.frame(s1,s3,s4,s5,s2,s6)
colnames(Napoved2013) <- c('MP_1','PTS_1','TRB_1','AST_1','WS','VORP')

a1 <- c(1,2,3,4,5,6)
a2 <- c(2006,2006,2006,2006,2006,2006)
a3 <- c('PF','PF','SF','PF','PF','SG')

a <- data.frame(a1,a2,a3)
colnames(a) <- c('Pk','Leto','Pozicija')

s1 <- round(predict(md1,a),2)
s2 <- round(predict(md2,a),2)
s3 <- round(predict(md3,a),2)
s4 <- round(predict(md4,a),2)
s5 <- round(predict(md5,a),2)
s6 <- round(predict(md6,a),2)

Napoved2006 <- data.frame(s1,s3,s4,s5,s2,s6)
colnames(Napoved2006) <- c('MP_1','PTS_1','TRB_1','AST_1','WS','VORP')


Napoved2006["Igralec"] <- Pravi_2006["Igralec"]

Napoved2013["Igralec"] <- Pravi_2013["Igralec"]

rm(s1,s2,s3,s4,s5,s6,md1,md2,md3,md4,md5,md6)

Napoved2006 <- Napoved2006[c("Igralec", "MP_1", "PTS_1", "TRB_1","AST_1","WS","VORP")]
Napoved2013 <- Napoved2013[c("Igralec", "MP_1", "PTS_1", "TRB_1","AST_1","WS","VORP")]


colnames(Napoved2006) <-c("Igralec", "Minute", "Točke", "Skoki","Podaje","Uspesnost","Dodana vrednost")
colnames(Napoved2013) <-c("Igralec", "Minute", "Točke", "Skoki","Podaje","Uspesnost","Dodana vrednost")

Pravi_2006 <- Pravi_2006[c(-1)]
Pravi_2013 <- Pravi_2013[c(-1)]
colnames(Pravi_2006) <-c("Igralec", "Minute", "Točke", "Skoki","Podaje","Uspesnost","Dodana vrednost")
colnames(Pravi_2013) <-c("Igralec", "Minute", "Točke", "Skoki","Podaje","Uspesnost","Dodana vrednost")

#Napoved za Luko dončiča

md1 = lm(MP_1 ~ Pk + Pozicija, data=Prvih_deset)
md2 = lm(WS ~ Pk + Pozicija, data=Prvih_deset)
md3 = lm(PTS_1 ~ Pk + Pozicija, data=Prvih_deset)
md4 = lm(TRB_1 ~ Pk + Pozicija, data=Prvih_deset)
md5 = lm(AST_1 ~ Pk + Pozicija, data=Prvih_deset)
md6 = lm(VORP ~ Pk + Pozicija, data=Prvih_deset)



a1 <- c(1)
a3 <- c('SG')
a <- data.frame(a1,a3)
colnames(a) <- c('Pk','Pozicija')

s1 <- round(predict(md1,a),1)
s2 <- round(predict(md2,a),1)
s3 <- round(predict(md3,a),1)
s4 <- round(predict(md4,a),1)
s5 <- round(predict(md5,a),1)
s6 <- round(predict(md6,a),1)

NapovedDoncic1 <- data.frame(s1,s3,s4,s5,s2,s6)
colnames(NapovedDoncic1) <- c('MP_1','PTS_1','TRB_1','AST_1','WS','VORP')

NapovedDoncic1["Igralec"] <- c("Luka Dončič")
NapovedDoncic1 <- NapovedDoncic1[c("Igralec", "MP_1", "PTS_1", "TRB_1","AST_1","WS","VORP")]
colnames(NapovedDoncic1) <-c("Igralec", "Minute", "Točke", "Skoki","Podaje","Uspesnost","Dodana vrednost")

a <- filter(najboljsi_igralci,Igralec == 'Goran Dragic')
a <- a[c(4,8:13)]

colnames(a) <-c("Igralec", "Minute", "Točke", "Skoki","Podaje","Uspesnost","Dodana vrednost")

NapovedDoncic <- rbind(NapovedDoncic1,a)


rm(s1,s2,s3,s4,s5,s6,md1,md2,md3,md4,md5,md6)

names(Skupni_podatki)[8:11] <- c("Minute","Tocke","Skoki","Podaje")
names(Skupni_podatki)[5:6] <- c("Leta","Igre")
