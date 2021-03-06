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
  pol=which(dane$geo=='PL')
  unia=which(dane$geo=='EU28')
  nazwykrajow = as.character(dane$geo)
  nazwykrajow[unia]="UE" # zmiana nazwy na UE
  barplot(dane$values, names.arg=nazwykrajow, ylim=c(0,maks), border=NA, col=kolory, 
          las=1, cex.names=0.62, xlab="", ylab="% firm", main=plottitle, xaxt='n')
#  axis(1, at=(1:dim(dane)[1])*1.2-0.5, labels=dane$geo, tick=F, cex.axis=0.62)
  x1 = 1:dim(dane)[1]
  nparz = which(x1%%2==1)
  aparz = which(x1%%2==0)
  axis(1, at=((1:dim(dane)[1])*1.2-0.5)[nparz], labels=nazwykrajow[nparz], 
       tick=F, cex.axis=0.62, line=-1)
  axis(1, at=((1:dim(dane)[1])*1.2-0.5)[aparz], labels=nazwykrajow[aparz], 
       tick=F, cex.axis=0.62)
  text(1.2*pol-0.6, dane$values[pol], labels=dane$values[pol], col=kolor2, cex=0.75, pos=3)
  text(1.2*unia-0.6, dane$values[unia], labels=dane$values[unia], col=kolor1, cex=0.75, pos=3)
}
  