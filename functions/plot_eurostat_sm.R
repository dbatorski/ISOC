# Barplot for social media

plot_eurostat_sm <- function(dane=dat_SocMed2, zmienna, plottitle){
  dane = dat_SocMed2 %>%
    filter(indic_is==zmienna & time==2017) %>%
    arrange(desc(values))
  kolory = rep("grey",29)
  kolory[which(dane$geo=='PL')] = "red"
  kolory[which(dane$geo=='EU28')] = "blue"
  barplot(dane$values, names.arg=dane$geo, main =plottitle, 
          border=NA, las=1, xlab="Kraj", ylab="Procent firm", col=kolory, cex.names=0.8)
  pol=which(dane$geo=='PL')
  unia=which(dane$geo=='EU28')
  text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=2, cex=0.75, pos=3)
  text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=4, cex=0.75, pos=3)
}
