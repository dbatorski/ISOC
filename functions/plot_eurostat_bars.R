# Function for ploting braplots with eurostat data

plot_eurostat_bars = function(dataset, indicator, breakdown='10_C10_S951_XK', rok=2017,
                              plottitle){
  countries=c("BG","CY","CZ","EE","EL","ES","HR","HU","IT","LU","LV","NL","PL","PT","RO","SE","SK",
              "AT","BE","DE","DK","EA","EU28","FI","FR","IE","LT","SI","UK")
  dane = dataset %>%
    filter(indic_is==indicator, sizen_r2==breakdown, time==rok, geo %in% countries) %>%
    arrange(desc(values))
  kolory = rep(kolor0,dim(dane)[1])
  kolory[which(dane$geo=='PL')] = kolor2
  kolory[which(dane$geo=='EU28')] = kolor1
  maks = max(dane$values, na.rm=T)
  barplot(dane$values, names.arg=dane$geo, ylim=c(0,maks), border=NA, col=kolory, 
          las=1, cex.names=0.7, xlab="Kraj", ylab="Procent firm", main=plottitle,
          xaxt='n')
  axis(1, at=(1:dim(dane)[1])*1.2-0.5, labels=dane$geo, tick=F, cex.axis=0.7)
  pol=which(dane$geo=='PL')
  unia=which(dane$geo=='EU28')
  text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
  text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
}
  