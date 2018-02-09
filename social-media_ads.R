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

png("figures/eAds.png", width=640, height=400)
plot_eurostat_sm(dat_SocMed2, "E_ADS", "Firmy płacące za reklamę w sieci (2016)", rok=2016)
dev.off()

png("figures/eAds4.png", width=840, height=600)
split.screen(c(2,2))
screen(1)
plot_eurostat_sm(dat_SocMed2, "E_ADS_KW", "bazujące na zawartości stron lub słowach kluczowych", rok=2016)
screen(2)
plot_eurostat_sm(dat_SocMed2, "E_ADS_LOC", "bazujące na geolokalizacji użytkowników", rok=2016)
screen(3)
plot_eurostat_sm(dat_SocMed2, "E_ADS_TRK", "bazujące na śledzeniu aktywności lub profilu  użytkownika", rok=2016)
screen(4)
plot_eurostat_sm(dat_SocMed2, "E_ADS_OTH", "inne metody reklamy celowanej", rok=2016)
dev.off()

plot_eurostat_sm(dat_SocMed2, "E_ADS_WEB", "Have a website and pay to advertise on the internet", rok=2016)
plot_eurostat_sm(dat_SocMed2, "E_ADS_SM1_ANY", "Pay to advertise on the Internet and use any social media", rok=2016)

png("figures/eAdsProfil.png", width=640, height=400)
plot_eurostat_sm(dat_SocMed2, "E_ADS3", "Firmy korzystające z reklamy profilowanej w internecie", rok=2016)
dev.off()

## E_ADS_KW Pay to advertise on the internet, based on the webpages' content or keywords searched by users 1
## E_ADS_LOC Pay to advertise on the internet, based on the geolocation of internet users 1
## E_ADS_OTH Pay to advertise on the internet, based on any other method of targeted advertising 1
# E_ADS_SM1_ANY Pay to advertise on the Internet and use any social media 1 1 1
## E_ADS_TRK Pay to advertise on the internet, based on the tracking of internet users' past activities or profile 1
# E_ADS_WEB Have a website and pay to advertise on the internet 1 1 1
# E_ADS3 Pay to advertise on the internet, based on the webpages' content, keywords, users' past activities or profile or the geolocation

##################################################################################
# Zależności z reklamami

dat_SocMed16 = dat_SocMed2 %>%
  filter(indic_is=="E_ADS" & time==2016) %>%
  select(geo, val1=values)
dat_eSales17 <- dat_eSales %>% 
  filter(indic_is=="E_ESELL", unit=='PC_ENT', sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017) %>%
  select(geo, val2=values)

png("figures/eAdsESell.png", width=640, height=400)
plot_eurostat_cross(dat_SocMed16, dat_eSales17, "Reklama w sieci a sprzedaż online", 
                    "Procent firm reklamujących się w sieci", "Procent firm, które sprzedają w sieci")
dev.off()

dat_eSalesALL17 <- dat_eSalesALL %>% 
  filter(indic_is=="E_AESELL", unit=='PC_ENT', sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017) %>%
  select(geo, val2=values)

png("figures/eAdsAESell.png", width=640, height=400)
plot_eurostat_cross(dat_SocMed16, dat_eSalesALL17, "Reklama w sieci a otrzymywanie zamówień przez sieć", 
                    "Procent firm reklamujących się w sieci", "Procent firm otrzymujących zamówienia przez sieć")
abline(a=0,b=1,col="grey",lty=3)
dev.off()


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
