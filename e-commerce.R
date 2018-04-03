#  title: "Polskie firmy na JRC"

library("eurostat")
library(dplyr)
source("functions/plot_eurostat_lines.R")
source("functions/plot_eurostat_bars.R")
source("functions/plot_eurostat_cross.R")
# Dane
dat_eSales <- get_eurostat(id="isoc_ec_eseln2", time_format="num")
dat_IntAccess <- get_eurostat(id="isoc_ci_in_en2", time_format="num")
dat_IntType <- get_eurostat(id="isoc_ci_it_en2", time_format="num")
dat_IntBuy <- get_eurostat(id="isoc_ec_ibuy", time_format="num")
# Select data for all enterprises and prct.
dat_eSalesALL <- dat_eSales %>% 
  filter(unit=='PC_ENT')
dat_IntAccess <- dat_IntAccess %>% 
  filter(unit=='PC_ENT')
# Kraje
countries=c("BG","CY","CZ","EE","EL","ES","HR","HU","IT","LU","LV","NL","PL","PT","RO","SE",
            "SK","AT","BE","DE","DK","EA","EU28","FI","FR","IE","LT","SI","UK")

# E_AESELL 	Enterprises having received orders via computer mediated networks
# E_ESELL 	Enterprises selling online (at least 1% of turnover)
# E_AXSELL 	Enterprises having received orders placed via EDI-type messages
# E_AWSELL 	Enterprises having received orders via a website or apps (web sales)
# E_AWS_B2BG 	Enterprises which sold via a website or apps - B2B and B2G
# E_AWS_B2C 	Enterprises which sold via a website or apps - B2C
# E_AWS_B2C_WEBCMP 	Enterprises which sold via a website or apps - B2C and website has electronic submission of complaints
# E_AWSVAL_B2C_GE10WS 	Enterprises where B2C web sales are 10% or more of the web sales
# E_AWS_B2C_GT1WS 	Enterprises where B2C web sales are more than 1% of the web sales
# E_AWS_GT1_B2C_GT10WS 	Enterprises where web sales are more than 1% of total turnover and B2C web sales more than 10% of the web sales
# E_AWSHM 	Enterprises with web sales to the own country
# E_AWSEU 	Enterprises with web sales to other EU countries
# E_AWSWW 	Enterprises with web sales to the rest of the world
# E_AESHM 	Enterprises having done electronic sales to the own country
# E_AESEU 	Enterprises having done electronic sales to other EU countries
# E_AESWW 	Enterprises having done electronic sales to the rest of the world
# E_AESEUWW 	Enterprises having done electronic sales to other EU countries and the rest of the world
# E_AESBHM 	Enterprises having done electronic sales or purchases to the own country
# E_AESBEU 	Enterprises having done electronic sales or purchases to other EU countries
# E_AESBWW 	Enterprises having done electronic sales or purchases in the rest of the world
# E_AESPAYON 	Enterprises accepting online payment for sales via website
# E_AESPAYOFF 	Enterprises accepting offline payment for sales via website
# E_AWS_B2C_CMP 	Enterprises which sold via a website or apps - B2C and via an e-commerce marketplace
# E_AWSVAL_B2C_GE10WS_CMP 	Enterprises where B2C web sales are 10% or more of the total web sales and which sold via an e-commerce marketplace
# E_AWSFOR 	Received orders placed via a website or apps from customers in foreign countries (EU or rest of the world)
# E_AWS_COWN 	Enterprises which sold via a website or apps - via their own website or apps
# E_AWS_CMP 	Enterprises which sold via a website or apps - via an e-commerce marketplace
# E_AWS_CMP_GE20 	Enterprises which sold via a website or apps - via an e-commerce marketplace for at least 20% of the web sales 


## Zamówienia przez sieć

png("figures/e-Zamowienia.png", width=640, height=400, pointsize=11)
plot_eurostat_bars(dat_eSalesALL, "E_AESELL", "10_C10_S951_XK", 2017, "Firmy otrzymujące zamówienia przez sieć")
dev.off()

png("figures/e-ZamowieniaT.png", width=640, height=400)
plot_eurostat_lines(dat_eSalesALL, "E_AESELL", "10_C10_S951_XK", "Firmy otrzymujące zamówienia przez sieć")
dev.off()


png("figures/e-Zamowienia3.png", width=640, height=400)
mat <- matrix(c(1,2,3), 1)
layout(mat)
plot_eurostat_lines(dat_eSalesALL, "E_AESELL", "L_C10_S951_XK", "Duże")
plot_eurostat_lines(dat_eSalesALL, "E_AESELL", "M_C10_S951_XK", "Średnie")
plot_eurostat_lines(dat_eSalesALL, "E_AESELL", "S_C10_S951_XK", "Małe")
layout(1)
dev.off()


## Sprzedaż w internecie

png("figures/e-Sale.png", width=640, height=360)
plot_eurostat_bars(dat_eSalesALL, "E_ESELL", "10_C10_S951_XK", 2017, "Firmy sprzedające przez sieć (min 1% przychodów)")
dev.off()

png("figures/e-SaleT.png", width=640, height=400)
plot_eurostat_lines(dat_eSalesALL, "E_ESELL", "10_C10_S951_XK", "Firmy sprzedające przez sieć (min 1% przychodów)")
dev.off()

png("figures/e-Sale3.png", width=640, height=400)
mat <- matrix(c(1,2,3), 1)
layout(mat)
plot_eurostat_lines(dat_eSalesALL, "E_ESELL", "L_C10_S951_XK", "Duże")
plot_eurostat_lines(dat_eSalesALL, "E_ESELL", "M_C10_S951_XK", "Średnie")
plot_eurostat_lines(dat_eSalesALL, "E_ESELL", "S_C10_S951_XK", "Małe")
layout(1)
dev.off()

dBranze_eSalesW = dat_eSalesALL %>%
  filter(sizen_r2 %in% wielkosc3, geo %in% c("PL","EU28"), time==2017, indic_is=="E_ESELL") %>%
  select(sizen_r2, geo, values) %>%
  tidyr::spread(key=geo, value=values)
dBranze_eSalesW


## Sprzedaż w sieci w podziale na branże
names(dat_eSalesALL)
table(dat_eSalesALL$sizen_r2)
dBranze_eSales = dat_eSalesALL %>%
  filter(sizen_r2 %in% branza$kod, geo=="PL", time==2017, indic_is=="E_ESELL") %>%
  select(sizen_r2, values)
dBranze_eSales = dat_eSalesALL %>%
  filter(sizen_r2 %in% branza$kod, geo %in% c("PL","EU28"), time==2017, indic_is=="E_ESELL") %>%
  select(sizen_r2, geo, values) %>%
  tidyr::spread(key=geo, value=values)
dBranze_eSales

png("figures/e-Sale-Branże.png", width=640, height=360)
op=par()
par(mar=c(4,14,2,1))
barplot(rbind(dBranze_eSales$PL, dBranze_eSales$EU28), names.arg=branze12, 
        horiz=T, beside=T, las=1, xlim=c(0,70), border=NA, col=c(kolor2,kolor1), 
        xlab="Procent firm z danej branży")
text(dBranze_eSales$PL, (1:12)*3-1.5, labels=dBranze_eSales$PL, cex=0.9,
     col=kolor2, pos=4, offset=0.3)
text(dBranze_eSales$EU28, (1:12)*3-0.5, labels=dBranze_eSales$EU28, cex=0.9,
     col=kolor1, pos=4, offset=0.3)
par(op)
dev.off()

# Zamówienia w podziale na branże

dBranze_eSales2 = dat_eSalesALL %>%
  filter(sizen_r2 %in% branza$kod, geo %in% c("PL","EU28"), time==2017, indic_is=="E_AESELL") %>%
  select(sizen_r2, geo, values) %>%
  tidyr::spread(key=geo, value=values)
dBranze_eSales2

png("figures/e-Zamowienia-Branże.png", width=640, height=360)
op=par()
par(mar=c(4,14,2,1))
barplot(rbind(dBranze_eSales2$PL, dBranze_eSales2$EU28), names.arg=branze12, 
        horiz=T, beside=T, las=1, xlim=c(0,70), border=NA, col=c(kolor2,kolor1), 
        xlab="Procent firm z danej branży")
text(dBranze_eSales2$PL, (1:12)*3-1.5, labels=dBranze_eSales2$PL, cex=0.9,
     col=kolor2, pos=4, offset=0.3)
text(dBranze_eSales2$EU28, (1:12)*3-0.5, labels=dBranze_eSales2$EU28, cex=0.9,
     col=kolor1, pos=4, offset=0.3)
par(op)
dev.off()

dBranze_eSales/dBranze_eSales2


## Podgrupy sprzedających w sieci

# E_AXSELL 	Enterprises having received orders placed via EDI-type messages
# E_AWSELL 	Enterprises having received orders via a website or apps (web sales)
png("figures/e-Sale-Web.png", width=640, height=400)
plot_eurostat_bars(dat_eSalesALL, "E_AXSELL", "10_C10_S951_XK", 2017, "Firmy otrzymujące zamówienia przez strony lub aplikacje")
dev.off()
png("figures/e-Sale-EDI.png", width=640, height=400)
plot_eurostat_bars(dat_eSalesALL, "E_AWSELL", "10_C10_S951_XK", 2017, "Firmy otrzymujące zamówienia w systemie EDI")
dev.off()

# E_AWS_B2BG 	Enterprises which sold via a website or apps - B2B and B2G
# E_AWS_B2C 	Enterprises which sold via a website or apps - B2C
png("figures/e-Sale-B2BG.png", width=640, height=400)
plot_eurostat_bars(dat_eSalesALL, "E_AWS_B2BG", "10_C10_S951_XK", 2017, "Firmy sprzedające przez strony i aplikacje B2B")
dev.off()
png("figures/e-Sale-B2C.png", width=640, height=400)
plot_eurostat_bars(dat_eSalesALL, "E_AWS_B2C", "10_C10_S951_XK", 2017, "Firmy sprzedające przez strony i aplikacje B2C")
dev.off()


## Sprzedaż do krajów UE oraz reszty świata

png("figures/e-Sale-EU.png", width=640, height=360)
plot_eurostat_bars(dat_eSalesALL, "E_AESEU", "10_C10_S951_XK", 2017, "Firmy sprzedające przez sieć do innych krajów UE")
dev.off()
png("figures/e-Sale-Oth.png", width=640, height=360)
plot_eurostat_bars(dat_eSalesALL, "E_AESEUWW", "10_C10_S951_XK", 2017, "Firmy sprzedające przez sieć do innych krajów")
dev.off()

png("figures/e-Sale-TEU.png", width=400, height=360)
plot_eurostat_lines(dat_eSalesALL, "E_AESEU", "10_C10_S951_XK", "Firm sprzedające przez sieć do innych krajów UE")
dev.off()
png("figures/e-Sale-TOth.png", width=640, height=400)
plot_eurostat_lines(dat_eSalesALL, "E_AESEUWW", "10_C10_S951_XK", "Firm sprzedające przez sieć do innych krajów")
dev.off()

png("figures/e-Sale-TEU3.png", width=640, height=400)
mat <- matrix(c(1,2,3), 1)
layout(mat)
plot_eurostat_lines(dat_eSalesALL, "E_AESEU", "L_C10_S951_XK", "Duże")
plot_eurostat_lines(dat_eSalesALL, "E_AESEU", "M_C10_S951_XK", "Średnie")
plot_eurostat_lines(dat_eSalesALL, "E_AESEU", "S_C10_S951_XK", "Małe")
layout(1)
dev.off()

plot_eurostat_lines(dat_eSalesALL, "E_AESEUWW", "L_C10_S951_XK", "Firm sprzedające przez sieć do innych krajów")


## Sprzedaż do krajów UE w podziale na branże
dBranze_eSalesEU = dat_eSalesALL %>%
  filter(sizen_r2 %in% branza$kod, geo %in% c("PL","EU28"), time==2017, indic_is=="E_AESEU") %>%
  select(sizen_r2, geo, values) %>%
  tidyr::spread(key=geo, value=values)
dBranze_eSalesEU

png("figures/e-Sale-BranżeEU.png", width=400, height=360)
op=par()
par(mar=c(4,14,2,1))
barplot(rbind(dBranze_eSalesEU$PL, dBranze_eSalesEU$EU28), names.arg=branze12, 
        horiz=T, beside=T, las=1, xlim=c(0,70), border=NA, col=c(kolor2,kolor1), 
        xlab="Procent firm z danej branży")
text(dBranze_eSalesEU$PL, (1:12)*3-1.5, labels=dBranze_eSalesEU$PL, cex=0.9,
     col=kolor2, pos=4, offset=0.3)
text(dBranze_eSalesEU$EU28, (1:12)*3-0.5, labels=dBranze_eSalesEU$EU28, cex=0.9,
     col=kolor1, pos=4, offset=0.3)
par(op)
dev.off()


## Uwarunkowania sprzedaży przez sieć

dat_IntBuy17 <- dat_IntBuy %>% 
  filter(indic_is=="I_BUY3", unit=="PC_IND", ind_type=="IND_TOTAL" , 
         geo %in% countries, time==2017) %>%
  select(geo, val1=values)
dat_eSales17 <- dat_eSales %>% 
  filter(indic_is=="E_ESELL", unit=='PC_ENT', sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017) %>%
  select(geo, val2=values)

png("figures/war_eSale_buy.png", width=800, height=400)
plot_eurostat_cross(dat_IntBuy17, dat_eSales17, "Sprzedaż online a liczba kupujących przez internet", "Procent osób kupujących przez sieć w ostatnich 3 miesiącach", "Procent firm, które sprzedają w sieci")
dev.off()

plot_eurostat_bars(dat_IntAccess, "E_IACC", plottitle="Dostęp do internetu w firmach")

dat_IntAccess17 <- dat_IntAccess %>% 
  filter(indic_is=="E_IACC", unit=='PC_ENT', sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017) %>%
  select(geo, val1=values)
plot_eurostat_cross(dat_IntAccess17, dat_eSales17, "Sprzedaż online a dostęp do sieci w firmach")

dat_IntType17 = dat_IntType %>%
  filter(indic_is=="E_BROAD2", unit=='PC_ENT', sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017) %>%
  select(geo, val2=values)
plot_eurostat_cross(dat_IntAccess17, dat_IntType17, "Dostęp do sieci w firmach", regresja = F)
