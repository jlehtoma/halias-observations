# install.packages("devtools")
# devtools::install_github("hadley/colformat")
# devtools::install_github("ropenscilabs/skimr")

library(skimr)
library(tidyverse)

# !!!NOTE!!! Original data CSV's encoding is not set, run fix script first
source("R/fix_data.R")

# Haliasaineisto
# 
# Hae csv-tiedosto / choose the csv-file
Halias <- readr::read_csv("data/Kokodata_20161025_20170724_fixed.csv") 
# aineiston rakenne / structure of the data
skim(Halias)

# Tuotetaan csv-tiedosto, jossa asemalla havaittujen lajien ja lajiryhmien lista / Produces a csv-file...
# including a list of all the observed species and species groups 
Halias_sp <- sort(unique(Halias$Species_Abb))
readr::write_csv(data.frame(spp = Halias_sp), "data/Halias_sp.csv") 

# Valitse laji ja kirjoita se lainausmerkkien sisään (tässä tukkasotka AYTFUL).
# Choose a species and write it between the quotation marks (here tufted duck AYTFUL).
sp <- c("AYTFUL") # TÄHÄN KANNATTAA VALITA MAHDOLLISIMMAN RAFLAAVA LAJI

# Seuraava käskyjoukko tuottaa kolmiosaisen kuvaajan, jossa ylin kuvaaja kertoo paikallismäärien
# vaihtelun vuoden aikana ja keskimmäinen muuttajien vastaavan vaihtelun. Kuvaajissa x-akselilla on
# juokseva päivämäärä (1.1. = 1 jne). Kolmas kuvaaja kertoo paikallisten ja muuttajien yhdistetyn 
# runsauden ennen vuotta 2000 (sininen viiva) ja vuoden 2009 jälkeen (punainen viiva). Näiden avulla 
# käyrien avulla voi tarkastelajin havaintomäärissä tapahtuneita pitkäaikaisia muutoksia.

phen=list()
for(i in 1:366){
  apup=Halias[which(Halias$Species_Abb==sp & Halias$Day.of.Year==i & Halias$Observed=="FALSE"),10]
  apum=Halias[which(Halias$Species_Abb==sp & Halias$Day.of.Year==i),11]
  apupbegin=Halias[which(Halias$Species_Abb==sp & Halias$Year<2000 & Halias$Day.of.Year==i),10]
  apupend=Halias[which(Halias$Species_Abb==sp & Halias$Year>2009 & Halias$Day.of.Year==i),10]
  apumbegin=Halias[which(Halias$Species_Abb==sp & Halias$Year<2000 & Halias$Day.of.Year==i),11]
  apumend=Halias[which(Halias$Species_Abb==sp & Halias$Year>2009 & Halias$Day.of.Year==i),11]
  
  apuhp=Halias[which(Halias$Day.of.Year==i),]
  apuhp2=Halias[which(Halias$Species_Abb==sp & Halias$Day.of.Year==i & Halias$Observed=="TRUE"),10]
  apuhpbegin=Halias[which(Halias$Day.of.Year==i & Halias$Year<2000),]
  apuhp2begin=Halias[which(Halias$Species_Abb==sp & Halias$Year<2000 & Halias$Day.of.Year==i & Halias$Observed=="TRUE"),10]
  apuhpend=Halias[which(Halias$Day.of.Year==i & Halias$Year>2009),]
  apuhp2end=Halias[which(Halias$Species_Abb==sp & Halias$Year>2009 & Halias$Day.of.Year==i & Halias$Observed=="TRUE"),10]
  
  apuHP=length(unique(apuhp$Year))-length(apuhp2)
  apuHM=length(unique(apuhp$Year))
  apuHPbegin=length(unique(apuhpbegin$Year))-length(apuhp2begin)
  apuHMbegin=length(unique(apuhpbegin$Year))
  apuHPend=length(unique(apuhpend$Year))-length(apuhp2end)
  apuHMend=length(unique(apuhpend$Year))
  
  phen$day[i]=i
  # Locals whole period
  if(length(apuHP)>0){
    phen$paik[i]=sum(apup)/(apuHP)
    }else {
      phen$paik[i]=NA
    }
  # Migrants whole period
  if(length(apuHM)>0){
    phen$muutto[i]=sum(apum)/apuHM
  }else {
    phen$muutto[i]=0
  
  # Locals beginning of the period  
  }
  if(length(apuHPbegin)>0){
    phen$paikbegin[i]=sum(apupbegin)/(apuHPbegin)
  }else {
    phen$paikbegin[i]=NA
  }
  
  # Migrants beginning of the period
  if(length(apuHMbegin)>0){
    phen$muuttobegin[i]=sum(apumbegin)/apuHMbegin
  }else {
    phen$muuttobegin[i]=0
  }
  
  # Locals end of the period
  if(length(apuHPend)>0){
    phen$paikend[i]=sum(apupend)/(apuHPend)
  }else {
    phen$paikend[i]=NA
  }
  
  # Migrants end of the period
  if(length(apuHMend)>0){
    phen$muuttoend[i]=sum(apumend)/apuHMend
  }else {
    phen$muuttoend[i]=0
  }
}


phen=as.data.frame(phen)

phen$begin=phen$paikbegin+phen$muuttobegin
phen$end=phen$paikend+phen$muuttoend

if(max(phen[which(phen$begin>-1),8])>max(phen[which(phen$begin>-1),9])){
  attach(phen)
  par(mfrow=c(3,1))
  plot(day,paik, type="l",main="Paikalliset / Lokal / Locals")
  plot(day,muutto, type="l",main="Muuttavat / Flyttande / Migrants")
  plot(day,phen$begin,col="blue", type="l", main="Muutos / Förändring / Change 1979-1999 => 2010-2016")
  lines(day,end,col="red")
}else {
  attach(phen)
  par(mfrow=c(3,1))
  plot(day,paik, type="l",main="Paikalliset / Lokal / Locals")
  plot(day,muutto, type="l",main="Muuttavat / Flyttande / Migrants")
  plot(day,end,col="red", type="l", main="Muutos / Förändring / Change 1979-1999 => 2010-2016")
  lines(day,phen$begin,col="blue")
}

