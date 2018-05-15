# Wykresy
# Websites and functionalities [isoc_ciweb]

dat_Web <- get_eurostat(id="isoc_ciweb", time_format="num")
table(dat_Web$indic_is)
table(dat_Web$unit)

dat_Web2 <- dat_Web %>%
  filter(unit=="PC_ENT", sizen_r2=="10_C10_S951_XK", geo %in% countries)
table(dat_Web2$indic_is)

##################################################################################
# E_WEB 	Enterprises having a website
# E_WEBACC 	Enterprises with website providing product catalogues or price lists
# E_WEBPRV 	Enterprises where the website provided a private policy statement, a privacy seal or certification related to website safety
# E_WEBVAC 	Enterprises where the website provided advertisement of open job positions or online job application
# E_WEBORD 	Enterprises where the website provided online ordering or reservation or booking, e.g. shopping cart
# E_WEBOT 	Enterprises where the website provided order tracking available online
# E_WEBCMP 	Enterprises where the website has electronic submission of complaints
# E_WEBF2 	Website has at least one of the following: webacc, webctm, webot or webper
# E_WEBF3 	Website has online ordering, reservation or booking and at least one of: webacc, webctm, webot or webper 

dat_Web2

png("figures/eWeb.png", width=800, height=400)
plot_eurostat_bars(dat_Web2, "E_WEB", rok=2017, plottitle="Firmy posiadające stronę internetową")
dev.off()

png("figures/eWeb2.png", width=400, height=400)
op=par()
par(mar=c(5,4,2,0.1))
plot_eurostat_bars(dat_Web2, "E_WEB", rok=2017, plottitle="Firmy posiadające stronę internetową")
par(op)
dev.off()

png("figures/eWebT.png", width=800, height=400)
plot_eurostat_lines(dat_Web2, indicator="E_WEB", plottitle="Firmy posiadające stronę internetową 2010-2017")
dev.off()

# PC_ENT_WEB
#dat_Web3 <- dat_Web %>%
#  filter(unit=="PC_ENT_WEB", sizen_r2=="10_C10_S951_XK", geo %in% countries)

png("figures/eWebPrez.png", width=400, height=400)
plot_eurostat_bars(dat_Web2, "E_WEBACC", rok=2017, plottitle="Strona WWW: prezentacja towarów, wyrobów lub usług oraz cenników")
dev.off()

png("figures/eWebSel.png", width=400, height=400)
plot_eurostat_bars(dat_Web2, "E_WEBORD", rok=2017, plottitle="Strona WWW: zamawianie lub rezerwacja on-line")
dev.off()

#plot_eurostat_bars(dat_Web3, "E_WEBOT", rok=2017, plottitle="Strona WWW: sprawdzanie lub śledzenie stanu realizacji zamówienia on-line")
#plot_eurostat_bars(dat_Web2, "E_WEBCTM", rok=2016, plottitle="Firmy ")

dat_Web4 = dat_Web2 %>%
  filter(time==2017, indic_is %in% c("E_WEBACC","E_WEBORD")) %>%
  select(indic_is, geo, values) %>%
  tidyr::spread(key=indic_is, value=values)
dat_Web4

plot(c(0,100), c(0,40), type='n', bty='l', las=1)
points(dat_Web4$E_WEBACC,dat_Web4$E_WEBORD, pch=19, cex=0.5, col=kolor0)
text(dat_Web4$E_WEBACC,dat_Web4$E_WEBORD, labels = dat_Web4$geo, cex=0.8, col=kolor3)
