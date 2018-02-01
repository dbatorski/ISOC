# Wykresy
# Obstacles for web sales (isoc_ec_wsobs_n2)

library("eurostat")
library(dplyr)

dat_WSOds <- get_eurostat(id="isoc_ec_wsobs_n2", time_format="num")
dat_WSOds <- dat_WSOds %>%
  filter(unit=="PC_ENT_AWSEU", sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017)

dat_WSOds2 <- label_eurostat(dat_WSOds)

table(dat_WSOds$indic_is)
table(dat_WSOds$unit)

dat_WSOds %>% 
  filter(geo=="PL") %>%
  select(indic_is, values)

# Jakiekolwiek problemy
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DANY") %>%
  select(geo, values)
kolory = rep("grey",29)
kolory[which(dane$geo=='PL')] = "red"
kolory[which(dane$geo=='EU28')] = "blue"
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciAll.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Firmy napotykające trudności przy sprzedaży do innych krajów UE")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=4, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DAPL trudności z przystosowaniem oznakowania produktów zgodnie z wymogami innych krajów UE
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DAPL") %>%
  select(geo, values)
kolory = rep("grey",29)
kolory[which(dane$geo=='PL')] = "red"
kolory[which(dane$geo=='EU28')] = "blue"
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciLabel.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Trudności z przystosowaniem oznakowania produktów zgodnie z wymogami innych krajów UE")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=4, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DBP ograniczenia nałożone przez własnych partnerów biznesowych
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DBP") %>%
  select(geo, values)
kolory = rep("grey",29)
kolory[which(dane$geo=='PL')] = "red"
kolory[which(dane$geo=='EU28')] = "blue"
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciPartn.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Ograniczenia nałożone przez własnych partnerów biznesowych")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=4, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DFL nieznajomość języków obcych na poziomie umożliwiającym komunikację z klientami
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DFL") %>%
  select(geo, values)
kolory = rep("grey",29)
kolory[which(dane$geo=='PL')] = "red"
kolory[which(dane$geo=='EU28')] = "blue"
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciJezyk.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Nieznajomość języków obcych na poziomie umożliwiającym komunikację z klientami z UE")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=4, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DHCD wysokie koszty dostawy lub zwrotu produktów
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DHCD") %>%
  select(geo, values)
kolory = rep("grey",29)
kolory[which(dane$geo=='PL')] = "red"
kolory[which(dane$geo=='EU28')] = "blue"
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciKoszty.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Wysokie koszty dostawy lub zwrotu produktów")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=4, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DRCD trudności związane z rozpatrywaniem reklamacji i sporów
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DRCD") %>%
  select(geo, values)
kolory = rep("grey",29)
kolory[which(dane$geo=='PL')] = "red"
kolory[which(dane$geo=='EU28')] = "blue"
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciReklamacje.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Trudności związane z rozpatrywaniem reklamacji i sporów")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=4, cex=0.75, pos=3)
dev.off()

dPol = dat_WSOds %>% 
  filter(geo=="PL") %>%
  select(indic_is, Polska=values)
dEU = dat_WSOds %>% 
  filter(geo=="EU28") %>%
  select(indic_is, EU28=values)
dane = left_join(dPol,dEU)
dane = dane[-1,]
dane = dane[c(4,6,1,3,2,5),]
labelki = c("wysokie koszty dostawy \n lub zwrotu produktów", 
            "trudności z rozpatrywaniem \n reklamacji i sporów",
            "trudności z przystosowaniem \n oznakowania produktów",
            "nieznajomość języków obcych",
            "ograniczenia nałożone przez \n partnerów biznesowych",
            "brak problemów")
op = par()

png("figures/eSale-TrudnosciPL.png", width=640, height=480)
par(mar=c(5, 13, 4, 2) + 0.1)
barplot(t(as.matrix(dane[,c(2,3)])), beside=T, las=1, horiz=T, xlim=c(0,70),
        names.arg=labelki, xlab="Procent firm sprzedających do innych krajów UE",
        main="Trudności w związku ze sprzedażą online do krajów UE", col=c(2,4), border=NA)
legend("right", legend=c("EU28", "Polska"), fill=c(4,2), border=NA, bty="n" )
dPol=as.vector(unlist(dane[,2]))
dEU=as.vector(unlist(dane[,3]))
text(dPol, 1+c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5), labels=dPol, col=2, cex=0.75,pos=4,offset=0.1)
text(dEU, 2+c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5), labels=dEU, col=4, cex=0.75,pos=4,offset=0.1)
par(op)
dev.off()

  
  
