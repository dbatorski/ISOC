#######################################################################
# Read the data

# Packages
#install.packages("eurostat")
library("eurostat")
library(dplyr)


kolor0 = rgb(216, 216, 216, max=255)
kolor1 = rgb(0, 36, 87, max=255)
kolor2 = rgb(248, 152, 29, max=255)
kolor3 = rgb(0, 178, 136, max=255)
kolor4 = rgb(153, 51, 0, max=255)
kolor5 = rgb(0, 174, 216, max=255)
kolor6 = rgb(83, 83, 83, max=255)

# Search for data tables
toc <- get_eurostat_toc()
toc
query <- search_eurostat("E-commerce sales")
query

#library(knitr)
#kable(query)


# DANE sprzedaż przez internet
dat_eSales <- get_eurostat(id="isoc_ec_eseln2", time_format="num")

# DANE dostęp do internetu w firmach
dat_IntAccess <- get_eurostat(id="isoc_ci_in_en2", time_format="num")

# DANE korzystanie z internetu
dat_IntUse <- get_eurostat(id="isoc_ci_ifp_iu", time_format="num")
table(dat_IntUse$indic_is)
dat_IntUse <- dat_IntUse %>% 
  filter(indic_is=="I_IU3", unit=="PC_IND", ind_type=="IND_TOTAL", geo, time)
#dat_IntUse <- label_eurostat(dat_IntUse)

# DANE kupowanie w internecie
dat_IntBuy <- get_eurostat(id="isoc_ec_ibuy", time_format="num")
dat_IntBuy <- dat_IntBuy %>% 
  filter(indic_is=="I_BUY3", unit=="PC_IND", ind_type=="IND_TOTAL" , geo %in% countries) #, time)
dat_IntBuy$indic_is


head(dat_eSales)

# Select data for all enterprises and prct.
dat_eSalesALL <- dat_eSales %>% 
  filter(sizen_r2=='10_C10_S951_XK' & unit=='PC_ENT')
dat_eSalesALL <- dat_eSales %>% 
  filter(unit=='PC_ENT')
head(dat_eSalesALL)
table(dat_eSalesALL$indic_is)
table(dat_eSalesALL$geo)

dat_eSalesALL %>%
  filter(indic_is=="E_ESELL" & geo=='HR') %>%
  select(time, values) %>%
  plot(ylim=c(0,40), las=1, bty='l', type='l', lwd=2)

countries=c("BG","CY","CZ","EE","EL","ES","HR","HU","IT","LU","LV","NL","PL","PT","RO","SE","SK",
           "AT","BE","DE","DK","EA","EU28","FI","FR","IE","LT","SI","UK")

#######################################################################
# Wykresy 

plot_eurostat_lines(dat_eSalesALL, "E_AESELL", "10_C10_S951_XK", "Firm otrzymujące zamówienia przez sieć")
plot_eurostat_lines(dat_eSalesALL, "E_ESELL", "10_C10_S951_XK", "Firm sprzedające przez sieć (min 1% przychodów)")
plot_eurostat_lines(dat_eSalesALL, "E_AESEU", "10_C10_S951_XK", "Firm sprzedające przez sieć do innych krajów UE")
plot_eurostat_lines(dat_eSalesALL, "E_AESEUWW", "10_C10_S951_XK", "Firm sprzedające przez sieć do innych krajów")

plot_eurostat_lines(dat_eSalesALL, "E_AESELL", "L_C10_S951_XK", "Firm otrzymujące zamówienia przez sieć")
plot_eurostat_lines(dat_eSalesALL, "E_ESELL", "L_C10_S951_XK", "Firm sprzedające przez sieć (min 1% przychodów)")
plot_eurostat_lines(dat_eSalesALL, "E_AESEU", "L_C10_S951_XK", "Firm sprzedające przez sieć do innych krajów UE")
plot_eurostat_lines(dat_eSalesALL, "E_AESEUWW", "L_C10_S951_XK", "Firm sprzedające przez sieć do innych krajów")

plot_eurostat_bars(dat_eSalesALL, "E_AESELL", "10_C10_S951_XK", 2017, "Firm otrzymujące zamówienia przez sieć")
plot_eurostat_bars(dat_eSalesALL, "E_ESELL", "10_C10_S951_XK", 2017, "Firm sprzedające przez sieć (min 1% przychodów)")
plot_eurostat_bars(dat_eSalesALL, "E_AESEU", "10_C10_S951_XK", 2017, "Firm sprzedające przez sieć do innych krajów UE")
plot_eurostat_bars(dat_eSalesALL, "E_AESEUWW", "10_C10_S951_XK", 2017, "Firm sprzedające przez sieć do innych krajów")

plot_eurostat_cross(dat_eSalesALL, "E_AESELL","E_ESELL", "10_C10_S951_XK", 
                   2017, "Firm otrzymujące zamówienia przez sieć")
plot_eurostat_cross(dat_eSalesALL, "E_ESELL", "E_IACC", "10_C10_S951_XK", 
                    2017, "Firm otrzymujące zamówienia przez sieć", dat_IntAccess)

plot_eurostat_cross(dat_eSalesALL, "E_ESELL", "E_IACC", "10_C10_S951_XK", 
                    2017, "Firm otrzymujące zamówienia przez sieć", dat_IntUse)

dat_IntBuy17 <- dat_IntBuy %>% 
  filter(indic_is=="I_BUY3", unit=="PC_IND", ind_type=="IND_TOTAL" , 
         geo %in% countries, time==2017) %>%
  select(geo, val1=values)
dat_eSales17 <- dat_eSales %>% 
  filter(indic_is=="E_ESELL", unit=='PC_ENT', sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017) %>%
  select(geo, val2=values)
plot_eurostat_cross(dat_IntBuy17, dat_eSales17, "Firm otrzymujące zamówienia przez sieć")


#######################################################################
# Testowy kod do robienia wykresów
dane = dat_eSalesALL %>%
  filter(indic_is=="E_ESELL", geo %in% countries)
maks = max(dane$values, na.rm=T)
names(table(dane$geo))
plot(c(2009,2018),c(0,maks), type='n', las=1, bty='l', xlab="Rok", ylab="Procent firm",
     main="Procent firm otrzymujących zamówienia przez sieć")
for(i in coutries){
  seria <- dane %>% 
    filter(geo==i) %>%
    select(time, values)
  lines(seria, col='grey')
  text(2017, seria[seria$time==2017,], labels=i, pos=4, offset=0.5, cex=0.6, col='grey')
  text(2010, seria[seria$time==2010,], labels=i, pos=2, offset=0.5, cex=0.6, col='grey')
}
s.pol <- dane[dane$geo=="PL",c("time", "values")]
s.ue <- dane[dane$geo=="EU28",c("time", "values")]
lines(s.pol, lwd=2, col=2)
lines(s.ue, lwd=2, col=4)
text(2010, s.pol[s.pol$time==2010,], labels="PL", pos=2, offset=0.5, cex=0.6, col=2)
text(2017, s.pol[s.pol$time==2017,], labels="PL", pos=4, offset=0.5, cex=0.6, col=2)
text(2010, s.ue[s.ue$time==2010,], labels="EU28", pos=2, offset=0.5, cex=0.6, col=4)
text(2017, s.ue[s.ue$time==2017,], labels="EU28", pos=4, offset=0.5, cex=0.6, col=4)
