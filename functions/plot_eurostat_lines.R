# Function for ploting lines with eurostat data

plot_eurostat_lines = function(dataset, indicator, breakdown='10_C10_S951_XK', plottitle,
                               ylab="Procent firm"){
  countries=c("BG","CY","CZ","EE","EL","ES","HR","HU","IT","LU","LV","NL","PL","PT","RO","SE","SK",
              "AT","BE","DE","DK","EA","EU28","FI","FR","IE","LT","SI","UK")
  dane = dataset %>%
    filter(indic_is==indicator, sizen_r2==breakdown, geo %in% countries)
  maks = max(dane$values, na.rm=T)
  rmin = min(dane$time, na.rm=T)
  plot(c(rmin-1,2018),c(0,maks), type='n', las=1, bty='l', xlab="Rok", ylab=ylab,
       main=plottitle)
  for(i in countries){
    seria <- dane %>% 
      filter(geo==i) %>%
      select(time, values)
    lines(seria, col='grey')
    text(2017, seria[seria$time==2017,], labels=i, pos=4, offset=0.5, cex=0.6, col=kolor0)
    text(rmin, seria[seria$time==rmin,], labels=i, pos=2, offset=0.5, cex=0.6, col=kolor0)
  }
  s.pol <- dane[dane$geo=="PL",c("time", "values")]
  s.ue <- dane[dane$geo=="EU28",c("time", "values")]
  lines(s.pol, lwd=2, col=kolor2)
  lines(s.ue, lwd=2, col=kolor1)
  text(rmin, s.pol[s.pol$time==rmin,], labels="PL", pos=2, offset=0.5, cex=0.6, col=kolor2)
  text(2017, s.pol[s.pol$time==2017,], labels="PL", pos=4, offset=0.5, cex=0.6, col=kolor2)
  text(rmin, s.ue[s.ue$time==rmin,], labels="EU28", pos=2, offset=0.5, cex=0.6, col=kolor1)
  text(2017, s.ue[s.ue$time==2017,], labels="EU28", pos=4, offset=0.5, cex=0.6, col=kolor1)
  
}
