# Function for ploting lines with eurostat data

plot_eurostat_lines = function(dataset, indicator, plottitle){
  countries=c("BG","CY","CZ","EE","EL","ES","HR","HU","IT","LU","LV","NL","PL","PT","RO","SE","SK",
              "AT","BE","DE","DK","EA","EU28","FI","FR","IE","LT","SI","UK")
  dane = dataset %>%
    filter(indic_is==indicator, geo %in% countries)
  maks = max(dane$values, na.rm=T)
  names(table(dane$geo))
  plot(c(2009,2018),c(0,maks), type='n', las=1, bty='l', xlab="Rok", ylab="Procent firm",
       main=plottitle)
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
  
}
