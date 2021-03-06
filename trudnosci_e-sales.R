# Wykresy
# Obstacles for web sales (isoc_ec_wsobs_n2)

library("eurostat")
library(dplyr)

dat_WSOds1 <- get_eurostat(id="isoc_ec_wsobs_n2", time_format="num")
dat_WSOds <- dat_WSOds1 %>%
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
kolory = rep(kolor0,29)
kolory[which(dane$geo=='PL')] = kolor2
kolory[which(dane$geo=='EU28')] = kolor1
maks = max(dane$values, na.rm=T)
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
dane$geo=as.character(dane$geo)
dane$geo[unia]='UE'
png("figures/eSale-TrudnosciAll.png", width=800, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.75, xlab="Kraj", ylab="% firm sprzedających do UE", 
        main="Firmy napotykające trudności przy sprzedaży do innych krajów UE")
#        xaxt='n')
#axis(1, at=(1:29)*1.2-0.5, tick=F, labels=dane$geo, cex.axis=0.7)
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DAPL trudności z przystosowaniem oznakowania produktów zgodnie z wymogami innych krajów UE
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DAPL") %>%
  select(geo, values)
kolory = rep(kolor0,29)
kolory[which(dane$geo=='PL')] = kolor2
kolory[which(dane$geo=='EU28')] = kolor1
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciLabel.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Trudności z przystosowaniem oznakowania produktów zgodnie z wymogami innych krajów UE")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DBP ograniczenia nałożone przez własnych partnerów biznesowych
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DBP") %>%
  select(geo, values)
kolory = rep(kolor0,29)
kolory[which(dane$geo=='PL')] = kolor2
kolory[which(dane$geo=='EU28')] = kolor1
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciPartn.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Ograniczenia nałożone przez własnych partnerów biznesowych")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DFL nieznajomość języków obcych na poziomie umożliwiającym komunikację z klientami
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DFL") %>%
  select(geo, values)
kolory = rep(kolor0,29)
kolory[which(dane$geo=='PL')] = kolor2
kolory[which(dane$geo=='EU28')] = kolor1
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciJezyk.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Nieznajomość języków obcych na poziomie umożliwiającym komunikację z klientami z UE")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DHCD wysokie koszty dostawy lub zwrotu produktów
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DHCD") %>%
  select(geo, values)
kolory = rep(kolor0,29)
kolory[which(dane$geo=='PL')] = kolor2
kolory[which(dane$geo=='EU28')] = kolor1
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciKoszty.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Wysokie koszty dostawy lub zwrotu produktów")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
dev.off()

# E_AWSEU_DRCD trudności związane z rozpatrywaniem reklamacji i sporów
dane <- filter(dat_WSOds, indic_is=="E_AWSEU_DRCD") %>%
  select(geo, values)
kolory = rep(kolor0,29)
kolory[which(dane$geo=='PL')] = kolor2
kolory[which(dane$geo=='EU28')] = kolor1
maks = max(dane$values, na.rm=T)
png("figures/eSale-TrudnosciReklamacje.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, ylim=c(0,100), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm sprzedających do UE", 
        main="Trudności związane z rozpatrywaniem reklamacji i sporów")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
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

png("figures/eSale-TrudnosciPL.png", width=800, height=400)
par(mar=c(5, 13, 4, 2) + 0.1)
barplot(t(as.matrix(dane[,c(2,3)])), beside=T, las=1, horiz=T, xlim=c(0,70),
        names.arg=labelki, xlab="% firm sprzedających do innych krajów UE",
        main="Trudności w związku ze sprzedażą online do krajów UE", col=c(kolor2,kolor1), border=NA)
legend("right", legend=c("EU28", "Polska"), fill=c(kolor1,kolor2), border=NA, bty="n" )
dPol=as.vector(unlist(dane[,2]))
dEU=as.vector(unlist(dane[,3]))
text(dPol, 1+c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5), labels=dPol, col=kolor2, cex=0.75,pos=4,offset=0.1)
text(dEU, 2+c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5), labels=dEU, col=kolor1, cex=0.75,pos=4,offset=0.1)
par(op)
dev.off()


trud5 = c('E_AWSEU_DAPL','E_AWSEU_DBP','E_AWSEU_DFL','E_AWSEU_DHCD','E_AWSEU_DRCD')  
dat_WSOds1 %>% 
  filter(unit=="PC_ENT_AWSEU", sizen_r2 %in% branza$kod,  geo %in% c("PL", "EU28"), 
         time==2017, indic_is=="E_AWSEU_DANY") %>%
  select(sizen_r2, geo, values) %>%
  tidyr::spread(key=geo, value=values)

t2 = dat_WSOds1 %>% 
  filter(unit=="PC_ENT_AWSEU", sizen_r2 %in% branza$kod,  geo == "PL", 
         time==2017, indic_is %in% trud5) %>%
  select(sizen_r2, indic_is, values) %>%
  tidyr::spread(key=indic_is, value=values)
t2

op=par()
par(mar=c(4,14,2,1))

barplot(t2$E_AWSEU_DAPL, names.arg=branze12, main="etykiety",
        horiz=T, las=1, xlim=c(0,50), border=NA, col=kolor2, 
        xlab="Procent firm z danej branży")
barplot(t2$E_AWSEU_DBP, names.arg=branze12, main="ograniczenia partnerów",
        horiz=T, las=1, xlim=c(0,50), border=NA, col=kolor2, 
        xlab="Procent firm z danej branży")
barplot(t2$E_AWSEU_DFL, names.arg=branze12, main="języki",
        horiz=T, las=1, xlim=c(0,100), border=NA, col=kolor2, 
        xlab="Procent firm z danej branży")
barplot(t2$E_AWSEU_DHCD, names.arg=branze12, main="dostawy i zwroty",
        horiz=T, las=1, xlim=c(0,100), border=NA, col=kolor2, 
        xlab="Procent firm z danej branży")
barplot(t2$E_AWSEU_DRCD, names.arg=branze12, main="reklamacje",
        horiz=T, las=1, xlim=c(0,100), border=NA, col=kolor2, 
        xlab="Procent firm z danej branży")

par(op)

