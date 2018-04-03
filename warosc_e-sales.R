# Wykresy
# Value of e-commerce sales [isoc_ec_evaln2]

# E_ETURN 	Enterprises' total turnover from e-commerce
# E_AWSVAL 	Enterprises' turnover from web sales
# E_AXSVAL 	Enterprises' turnover from EDI-type sales
# E_AWSVAL_B2C 	Enterprises' turnover from web sales - B2C
# E_AWSVAL_B2BG 	Enterprises' turnover from web sales - B2B and B2G

dat_sValue <- get_eurostat(id="isoc_ec_evaln2", time_format="num")
table(dat_sValue$indic_is)
table(dat_sValue$unit)

dat_sValue2 <- dat_sValue %>%
  filter(unit=="PC_TURN", sizen_r2=="10_C10_S951_XK", geo %in% countries)

png("figures/eSale-WartoscE.png", width=640, height=400)
plot_eurostat_lines(dat_sValue2, "E_ETURN", "10_C10_S951_XK", "Wartość przychodów z e-commerce",
                    ylab="Procent przychodów firm")
dev.off()
png("figures/eSale-WartoscS.png", width=640, height=400)
plot_eurostat_lines(dat_sValue2, "E_AWSVAL", "10_C10_S951_XK", "Wartość przychodów ze sprzedaży przez internet",
                    ylab="Procent przychodów firm")
dev.off()

png("figures/eSale-WartoscEDI.png", width=640, height=400)
plot_eurostat_lines(dat_sValue2, "E_AXSVAL", "10_C10_S951_XK", "Wartość przychodów ze sprzedaży w systemie EDI",
                    ylab="Procent przychodów firm")
dev.off()

#plot_eurostat_bars(dat_sValue2, "E_AWSVAL_B2C", "10_C10_S951_XK", 2017, "tyt")
#plot_eurostat_bars(dat_sValue2, "E_AWSVAL_B2BG", "10_C10_S951_XK", 2017, "tyt")

library(tidyr)
dat_sValue3 <- dat_sValue %>%
  filter(indic_is %in% c("E_AWSVAL_B2C", "E_AWSVAL_B2BG"), 
         unit=="PC_TURN", sizen_r2=="10_C10_S951_XK", geo %in% countries, time==2017) %>%
  select(indic_is, geo, values) %>%
  spread(key='indic_is', value='values')

png("figures/eSale-WartoscB2.png", width=640, height=480)
barplot(t(as.matrix(dat_sValue3[,c(2,3)])), beside=F, horiz=T, names.arg=dat_sValue3$geo, 
        las=1, border=NA, col=c("wheat", "sienna3"), xlab="Procent przychodów",
        main="Źródła przychodów ze sprzedaży przez internet")
legend("topright", legend=c("B2B i B2G", "B2C"), border=NA, bty='n',fill=c("wheat", "sienna3"))
dev.off()