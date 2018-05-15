# Wykresy
#  Type of connections to the internet [isoc_ci_it_en2]

dat_Con <- get_eurostat(id="isoc_ci_it_en2", time_format="num")
table(dat_Con$indic_is)
table(dat_Con$unit)

dat_Con2 <- dat_Con %>%
  filter(unit=="PC_ENT", sizen_r2=="10_C10_S951_XK", geo %in% countries)
table(dat_Con2$indic_is)

##################################################################################
# E_BROAD2 	Enterprises with broadband access (fixed or mobile)
# E_BROAD 	Enterprises internet connection: fixed broadband access
# E_FIXBB 	Enterprises use DSL or other fixed broadband connection (as of 2014)
# E_DSL 	Enterprises internet connection type: DSL
# E_BBOTH 	Enterprises internet connection type: another fixed connection (e.g. cable)
# E_ISPDF_LT2 	The maximum contracted download speed of the fastest fixed internet connection is less than 2 Mb/s
# E_ISPDF_2_10 	The maximum contracted download speed of the fastest fixed internet connection is at least 2 Mb/s but less than 10 Mb/s
# E_ISPDF_10_30 	The maximum contracted download speed of the fastest fixed internet connection is at least 10 Mb/s but less than 30 Mb/s
# E_ISPDF_30_100 	The maximum contracted download speed of the fastest fixed internet connection is at least 30 Mb/s but less than 100 Mb/s
# E_ISPDF_GE100 	The maximum contracted download speed of the fastest fixed internet connection is at least 100 Mb/s
# E_ISPD_LT2 	The contracted download speed of the enterprise's fastest internet connection is less than 2 Mb/s
# E_ISPD_2_10 	The contracted download speed of the enterprise's fastest internet connection is at least 2 Mb/s but less than 10 Mb/s
# E_ISPD_10_30 	The contracted download speed of the enterprise's fastest internet connection is at least 10 Mb/s but less than 30 Mb/s
# E_ISPD_30_100 	The contracted download speed of the enterprise's fastest internet connection is at least 30 Mb/s but less than 100 Mb/s
# E_ISPD_GE100 	The contracted download speed of the enterprise's fastest internet connection is at least 100 Mb/s
# E_ISPDFOK 	The speed of the fixed internet connection is sufficient for the actual needs of the enterprise
# E_ISPDFOK_GE30 	The speed of the fixed internet connection (at least 30 Mb/s) is sufficient for the actual needs of the enterprise
# E_ISPDFOKX_2_10 	The speed of the fixed internet connection ([2-10[ Mb/s) is not sufficient for the actual needs of the enterprise
# E_ISPDFOKX_10_30 	The speed of the fixed internet connection ([10-30[ Mb/s) is not sufficient for the actual needs of the enterprise
# E_ISPDFOKX_GE30 	The speed of the fixed internet connection (at least 30 Mb/s) is not sufficient for the actual needs of the enterprise
# E_MOBBB 	Enterprises connecting to the internet via a mobile broadband connection (3G modem or 3G handset)
# E_MOB 	Enterprises connecting to the internet via a mobile connection (e.g. mobile phone, GPRS, UMTS, etc.)
# E_MOB2 	Enterprises connecting to the internet via mobile connection (broadband or other mobile connection) 

png("figures/eCon100+.png", width=800, height=400)
plot_eurostat_bars(dat_Con2, "E_ISPDF_GE100", rok=2017, plottitle="Firmy posiadające łącze o przepustowości przynajmniej 100Mb/s")
dev.off()
png("figures/eCon30-100.png", width=640, height=400)
plot_eurostat_bars(dat_Con2, "E_ISPDF_30_100", rok=2017, plottitle="Firmy posiadające łącze o przepustowości 30-100Mb/s")
dev.off()

plot_eurostat_bars(dat_Con2, "E_ISPDF_10_30", rok=2017, plottitle="Firmy posiadające łącze o przepustowości 10-30Mb/s")
plot_eurostat_bars(dat_Con2, "E_ISPDF_2_10", rok=2017, plottitle="Firmy posiadające łącze o przepustowości przynajmniej 100Mb/s")
plot_eurostat_bars(dat_Con2, "E_ISPDF_LT2", rok=2017, plottitle="Firmy posiadające łącze o przepustowości przynajmniej 100Mb/s")

d1 = dat_Con2 %>%
  filter(indic_is=="E_ISPDF_GE100", time==2017) %>%
  select(geo, c100=values)
d2 = dat_Con2 %>%
  filter(indic_is=="E_ISPDF_30_100", time==2017) %>%
  select(geo, c30100=values)
dane = left_join(d1,d2) %>%
  mutate(c30=c100+c30100) %>%
  arrange(desc(c30))

png("figures/eCon30+.png", width=640, height=400)
kolory = rep(kolor0,29)
kolory[which(dane$geo=='PL')] = kolor2
kolory[which(dane$geo=='EU28')] = kolor1
maks = max(dane$c30, na.rm=T)
barplot(dane$c30, names.arg=dane$geo, ylim=c(0,maks), border=NA, col=kolory, 
        las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm", main="Firmy posiadające łącze o przepustowości przynajmniej 30Mb/s")
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$c30[pol], labels=dane$c30[pol], col=kolor2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$c30[unia], labels=dane$c30[unia], col=kolor1, cex=0.75, pos=3)
dev.off()

d30 = dane %>%
  select(geo, val1=c30)
d100 = dane %>%
  select(geo, val1=c100)

png("figures/eCon30+eSell.png", width=800, height=400)
plot_eurostat_cross(d30, dat_eSales17, "Sprzedaż online a liczba kupujących przez internet", 
                    "% firm posiadających łącze o przepustowości minimum 30Mb/s", "% firm, które sprzedają w sieci",
                    regresja = T)
dev.off()

png("figures/eCon100+eSell.png", width=800, height=400)
plot_eurostat_cross(d100, dat_eSales17, "Sprzedaż online a liczba kupujących przez internet", 
                    "Procent firm posiadających łącze o przepustowości minimum 100Mb/s", "Procent firm, które sprzedają w sieci")
dev.off()


dat_Con100mb <- dat_Con %>%
  filter(unit=="PC_ENT", indic_is=="E_ISPDF_GE100", time==2017, 
         sizen_r2 %in% branza$kod, geo %in% countries) %>%
  select(sizen_r2, geo, values) 
#%>%  tidyr::spread(key=sizen_r2, value=values)

# Analizy na poziomie branż
c1 = filter(dat_Con100mb, geo=="PL")
c2 = filter(dat_Con100mb, geo=="EU28")
barplot(c1$values, names.arg =branze12, horiz=T, las=1) 

png("figures/eCon100pleu.png", width=800, height=400)
op=par()
par(mar=c(4,14,2,1))
barplot(rbind(c1$values, c2$values), names.arg=branze12, 
        horiz=T, beside=T, las=1, xlim=c(0,50), border=NA, col=c(kolor2,kolor1), 
        xlab="% firm")
text(c1$values, (1:12)*3-1.5, labels=c1$values, cex=0.9,
     col=kolor2, pos=4, offset=0.3)
text(c2$values, (1:12)*3-0.5, labels=c2$values, cex=0.9,
     col=kolor1, pos=4, offset=0.3)
par(op)
dev.off()

branze12
c1$sizen_r2


####################################################

dt1 = dat_Con2 %>%
  filter(indic_is=="E_ISPDF_GE100", geo %in% c('PL', 'EU28')) %>%
  select(geo, time, c100=values)
dt2 = dat_Con2 %>%
  filter(indic_is=="E_ISPDF_30_100", geo %in% c('PL', 'EU28')) %>%
  select(geo, time, c30100=values)

dt3 = left_join(dt1,dt2) %>%
  mutate(c30=c100+c30100) %>%
  arrange(c30)

dt3

png("figures/Con-firmy-pleu.png", width=800, height=400)
plot(c(2014,2018), c(0,50), type="n", bty='l', las=1, xlab="rok", ylab="% firm",
     main="Przepustowość łącz internetowych w firmach")
lines(2014:2017, dt3$c30[dt3$geo=='PL'], lty=2, lwd=2, col=kolor2)
lines(2014:2017, dt3$c30[dt3$geo=='EU28'], lty=2, lwd=2, col=kolor1)
lines(2014:2017, dt3$c100[dt3$geo=='PL'], lty=1, lwd=2, col=kolor2)
lines(2014:2017, dt3$c100[dt3$geo=='EU28'], lty=1, lwd=2, col=kolor1)
text(2017, dt3$c30[dt3$geo=='PL' & dt3$time==2017], pos=4, offset=0.2, col=kolor2, 
     cex=0.9, paste0(dt3$c30[dt3$geo=='PL' & dt3$time==2017],'%'))
text(2017, dt3$c30[dt3$geo=='EU28' & dt3$time==2017], pos=4, offset=0.2, col=kolor1, 
     cex=0.9, paste0(dt3$c30[dt3$geo=='EU28' & dt3$time==2017],'%'))
text(2017, dt3$c100[dt3$geo=='PL' & dt3$time==2017], pos=4, offset=0.2, col=kolor2, 
     cex=0.9, paste0(dt3$c100[dt3$geo=='PL' & dt3$time==2017],'%'))
text(2017, dt3$c100[dt3$geo=='EU28' & dt3$time==2017], pos=4, offset=0.2, col=kolor1, 
     cex=0.9, paste0(dt3$c100[dt3$geo=='EU28' & dt3$time==2017],'%'))
legend("topleft", legend=c("minimum 30mb/s", "minimum 100mb/s", 'Polska', 'kraje UE'),
       lty=c(2,1,1,1), lwd=2, col=c(1,1,kolor2,kolor1), bty='n', cex=1)
dev.off()
