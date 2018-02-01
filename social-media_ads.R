# Wykresy
# Social media use by type, internet advertising [isoc_cismt]

dat_SocMed <- get_eurostat(id="isoc_cismt", time_format="num")
table(dat_SocMed2$indic_is)
table(dat_SocMed$unit)

dat_SocMed2 <- dat_SocMed %>%
  filter(unit=="PC_ENT", sizen_r2=="10_C10_S951_XK", geo %in% countries)

##################################################################################
# Płatne reklamy

dane = dat_SocMed2 %>%
  filter(indic_is=="E_ADS" & time==2016) %>%
  arrange(desc(values))

kolory = rep("grey",29)
kolory[which(dane$geo=='PL')] = "red"
kolory[which(dane$geo=='EU28')] = "blue"

png("figures/eAds.png", width=640, height=400)
barplot(dane$values, names.arg=dane$geo, main = "Firmy płacące za reklamę w sieci (2016)", 
        border=NA, las=1, xlab="Kraj", ylab="Procent firm", col=kolory, cex.names=0.8)
pol=which(dane$geo=='PL')
unia=which(dane$geo=='EU28')
text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=2, cex=0.75, pos=3)
text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=4, cex=0.75, pos=3)
dev.off()


# E_ADS_KW Pay to advertise on the internet, based on the webpages' content or keywords searched by users 1
# E_ADS_LOC Pay to advertise on the internet, based on the geolocation of internet users 1
# E_ADS_OTH Pay to advertise on the internet, based on any other method of targeted advertising 1
# E_ADS_SM1_ANY Pay to advertise on the Internet and use any social media 1 1 1
# E_ADS_TRK Pay to advertise on the internet, based on the tracking of internet users' past activities or profile 1
# E_ADS_WEB Have a website and pay to advertise on the internet 1 1 1
# E_ADS3 Pay to advertise on the internet, based on the webpages' content, keywords, users' past activities or profile or the geolocation

##################################################################################
# Media społecznościowe

png("figures/eSMblog.png", width=640, height=400)
plot_eurostat_sm(dat_SocMed2, "E_SM1_BLOG", "Blogi lub mikroblogi")
dev.off()

png("figures/eSMcntshr.png", width=640, height=400)
plot_eurostat_sm(dat_SocMed2, "E_SM1_CNTSHR", "Strony umożliwiające udostępnianie multimediów")
dev.off()

png("figures/eSMsm.png", width=640, height=400)
plot_eurostat_sm(dat_SocMed2, "E_SM1_SNET", "Serwisy społecznościowe")
dev.off()

png("figures/eSMwiki.png", width=640, height=400)
plot_eurostat_sm(dat_SocMed2, "E_SM1_WIKI", "Narzędzia wymiany informacji Wiki")
dev.off()

png("figures/eSM.png", width=840, height=600)
split.screen(c(2,2))
screen(1)
plot_eurostat_sm(dat_SocMed2, "E_SM1_SNET", "Serwisy społecznościowe")
screen(2)
plot_eurostat_sm(dat_SocMed2, "E_SM1_BLOG", "Blogi lub mikroblogi")
screen(3)
plot_eurostat_sm(dat_SocMed2, "E_SM1_CNTSHR", "Strony umożliwiające udostępnianie multimediów")
screen(4)
plot_eurostat_sm(dat_SocMed2, "E_SM1_WIKI", "Narzędzia wymiany informacji Wiki")
close.screen(all = TRUE)
dev.off()
