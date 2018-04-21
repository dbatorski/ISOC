# Barplot for social media

plot_eurostat_sm <- function(dane=dat_SocMed2, zmienna, plottitle, rok=2017){
  dane = dat_SocMed2 %>%
    filter(indic_is==zmienna & time==rok) %>%
    arrange(desc(values))
  kolory = rep(kolor0,dim(dane)[1])
  kolory[which(dane$geo=='PL')] = kolor2
  kolory[which(dane$geo=='EU28')] = kolor1
  pol=which(dane$geo=='PL')
  unia=which(dane$geo=='EU28')
  dane$geo=as.character(dane$geo)
  dane$geo[unia]="UE"
  barplot(dane$values, names.arg=dane$geo, main =plottitle, 
          border=NA, las=1, xlab="Kraj", ylab="% firm", col=kolory, 
          cex.names=0.8, xaxt='n')
  axis(1, at=(1:dim(dane)[1])*1.2-0.5, labels=dane$geo, tick=F, cex.axis=0.75)
  text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
  text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
}
