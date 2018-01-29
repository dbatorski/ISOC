# Function for ploting braplots with eurostat data

plot_eurostat_bars = function(dataset, indicator, breakdown='10_C10_S951_XK', rok=2017,
                              plottitle){
  countries=c("BG","CY","CZ","EE","EL","ES","HR","HU","IT","LU","LV","NL","PL","PT","RO","SE","SK",
              "AT","BE","DE","DK","EA","EU28","FI","FR","IE","LT","SI","UK")
  dane = dataset %>%
    filter(indic_is==indicator, sizen_r2==breakdown, time==rok, geo %in% countries) %>%
    arrange(desc(values))
  kolory = rep("grey",29)
  kolory[which(dane$geo=='PL')] = "red"
  kolory[which(dane$geo=='EU28')] = "blue"
  maks = max(dane$values, na.rm=T)
  barplot(dane$values, names.arg=dane$geo, ylim=c(0,maks), border=NA, col=kolory, 
          las=1, cex.names=0.75, xlab="Kraj", ylab="Procent firm", main=plottitle)
  #text(1.2)
}
  