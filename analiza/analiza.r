Prvih_deset <- filter(Skupni_podatki, Skupni_podatki$Pk < 11)

md1 = lm(MP_1 ~ Pk + Pozicija + Leto, data=Prvih_deset)
md2 = lm(WS ~ Pk + Pozicija + Leto, data=Prvih_deset)
md3 = lm(PTS_1 ~ Pk + Pozicija + Leto, data=Prvih_deset)
md4 = lm(TRB_1 ~ Pk + Pozicija + Leto, data=Prvih_deset)
md5 = lm(AST_1 ~ Pk + Pozicija + Leto, data=Prvih_deset)
md6 = lm(VORP ~ Pk + Pozicija + Leto, data=Prvih_deset)



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

Napoved <- Napoved2006
