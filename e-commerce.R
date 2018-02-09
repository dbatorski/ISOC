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


## Zamówienia przez sieć

png("figures/e-Zamowienia.png", width=640, height=400)
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

png("figures/e-Sale.png", width=640, height=400)
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


## Sprzedaż do krajów UE oraz reszty świata

png("figures/e-Sale-EU.png", width=640, height=400)
plot_eurostat_bars(dat_eSalesALL, "E_AESEU", "10_C10_S951_XK", 2017, "Firmy sprzedające przez sieć do innych krajów UE")
dev.off()
png("figures/e-Sale-Oth.png", width=640, height=400)
plot_eurostat_bars(dat_eSalesALL, "E_AESEUWW", "10_C10_S951_XK", 2017, "Firmy sprzedające przez sieć do innych krajów")
dev.off()

png("figures/e-Sale-TEU.png", width=640, height=400)
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


## Uwarunkowania sprzedaży przez sieć

dat_IntBuy17 <- dat_IntBuy %>% 
  filter(indic_is=="I_BUY3", unit=="PC_IND", ind_type=="IND_TOTAL" , 
         geo %in% countries, time==2017) %>%
  select(geo, val1=values)
dat_eSales17 <- dat_eSales %>% 
  filter(indic_is=="E_ESELL", unit=='PC_ENT', sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017) %>%
  select(geo, val2=values)

png("figures/war_eSale_buy.png", width=640, height=400)
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
