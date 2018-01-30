# Function for ploting braplots with eurostat data

plot_eurostat_cross = function(dataset, indicator1, indicator2, breakdown='10_C10_S951_XK', 
                              rok=2017, plottitle, dataset2=dataset){
  klucz=read.table(file="key.txt", header=F, sep=";")
  countries=c("BG","CY","CZ","EE","EL","ES","HR","HU","IT","LU","LV","NL","PL","PT","RO","SE","SK",
              "AT","BE","DE","DK","EA","EU28","FI","FR","IE","LT","SI","UK")
  dane1 = dataset %>%
    filter(indic_is==indicator1, sizen_r2==breakdown, time==rok, geo %in% countries) %>%
    select(geo, val1=values)
  dane2 = dataset2 %>%
    filter(indic_is==indicator2, sizen_r2==breakdown, time==rok, geo %in% countries) %>%
    select(geo, val2=values)
  dane = left_join(dane1, dane2)
  kolory = rep("grey",29)
  kolory[which(dane$geo=='PL')] = "red"
  kolory[which(dane$geo=='EU28')] = "blue"
  maks1 = max(dane$val1, na.rm=T)
  maks2 = max(dane$val2, na.rm=T)
  plot(c(0,maks1), c(0,maks2), type='n', las=1, bty='l', main=plottitle,
       xlab=klucz[klucz[,1]==indicator1, 2], ylab=klucz[klucz[,1]==indicator2, 2])
  points(dane$val1, dane$val2, pch=19, col=1, cex=0.5)
  text(dane$val1, dane$val2, labels=dane$geo, cex=0.75, col='grey', offset=0)
  text(dane$val1[dane$geo=='PL'], dane$val2[dane$geo=='PL'], labels="PL", cex=0.75, col=2, offset=0)
  text(dane$val1[dane$geo=='EU28'], dane$val2[dane$geo=='EU28'], labels="EU28", cex=0.75, col=4, offset=0)
}
  