# Function for ploting braplots with eurostat data

plot_eurostat_cross = function(dataset, dataset2, plottitle, labx="X", laby="Y", regresja=T){
#plot_eurostat_cross = function(dataset, indicator1, breakdown1='10_C10_S951_XK', 
#                              dataset2=dataset, indicator2=indicator1, breakdown2=breakdown1,
#                              rok=2017, plottitle){
  klucz=read.table(file="key.txt", header=F, sep=";")
  countries=c("BG","CY","CZ","EE","EL","ES","HR","HU","IT","LU","LV","NL","PL","PT","RO","SE",
              "SK","AT","BE","DE","DK","EA","EU28","FI","FR","IE","LT","SI","UK")
#  dane1 = dataset %>%
#    filter(indic_is==indicator1, sizen_r2==breakdown1, time==rok, geo %in% countries) %>%
#    select(geo, val1=values)
#  dane2 = dataset2 %>%
#    filter(indic_is==indicator2, sizen_r2==breakdown2, time==rok, geo %in% countries) %>%
#    select(geo, val2=values)
#  dane = left_join(dane1, dane2)
  dane = left_join(dataset, dataset2)
  kolory = rep(kolor0,29)
  kolory[which(dane$geo=='PL')] = kolor2
  kolory[which(dane$geo=='EU28')] = kolor1
  dane$geo=as.character(dane$geo)
  unia = which(dane$geo=="EU28")
  dane$geo[unia]="UE"
#  dane$geo[dane$geo=="EA"]=""
  maks1 = max(dane$val1, na.rm=T)
  maks2 = max(dane$val2, na.rm=T)
  plot(c(0,maks1), c(0,maks2), type='n', las=1, bty='l', main=plottitle, xlab=labx, ylab=laby)
  points(dane$val1, dane$val2, pch=19, col=kolor0, cex=0.5)
  text(dane$val1, dane$val2, labels=dane$geo, cex=0.75, col=kolor3, offset=0)
  text(dane$val1[dane$geo=='PL'], dane$val2[dane$geo=='PL'], labels="PL", cex=0.75, col=kolor2, offset=0)
  text(dane$val1[unia], dane$val2[unia], labels="UE", cex=0.75, col=kolor1, offset=0)
  if(regresja==T){
    model1 <- lm(dane$val2 ~ dane$val1)
    abline(model1, col=1)
  }
}
  