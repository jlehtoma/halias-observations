# Haliasaineisto

# Hae csv-tiedosto / choose the csv-file
Halias <- read.csv(file.choose(), fileEncoding="macintosh")
str(Halias) # aineiston rakenne / structure of the data

# Tuotetaan csv-tiedosto, jossa asemalla havaittujen lajien ja lajiryhmien lista / Produces a csv-file...
# including a list of all the observed species and species groups
Halias_sp <- unique(Halias$Species_Abb)
write.csv(Halias_sp, "Halias_sp.csv")

# Valitse laji ja kirjoita se lainausmerkkien sisään (tässä tukkasotka AYTFUL).
# Choose a species and write it between the quotation marks (here tufted duck AYTFUL).
sp <- c("GAVSTE") # TÄHÄN KANNATTAA VALITA MAHDOLLISIMMAN RAFLAAVA LAJI

# Seuraava käskyjoukko tuottaa kolmiosaisen kuvaajan, jossa ylin kuvaaja kertoo paikallismäärien
# vaihtelun vuoden aikana ja keskimmäinen muuttajien vastaavan vaihtelun. Kuvaajissa x-akselilla on
# juokseva päivämäärä (1.1. = 1 jne). Kolmas kuvaaja kertoo paikallisten ja muuttajien yhdistetyn
# runsauden ennen vuotta 2000 (sininen viiva) ja vuoden 2009 jälkeen (punainen viiva). Näiden avulla
# käyrien avulla voi tarkastelajin havaintomäärissä tapahtuneita pitkäaikaisia muutoksia.

phen <- data.frame(day = 1:366, paik = NA_real_, muutto = 0,
                   paikbegin = NA_real_, muuttobegin = 0,
                   paikend = NA_real_, muuttoend = 0)
apusp <- Halias$Species_Abb == sp
apulow <- Halias$Year < 2000
apuhigh <- Halias$Year > 2008
apusplow <- apusp & apulow
apusphigh <- apusp & apuhigh
for(i in 1:366) {
  apudoy <- Halias$Day.of.Year == i
  apuspdoy <- apusp & apudoy
  apuspdoyobs <- apuspdoy & Halias$Observed

  apup <- Halias$Local[which(apuspdoy & !Halias$Observed)]
  apum <- Halias$Migr[which(apuspdoy)]
  idxbegin <- which(apusplow & apudoy)
  idxend <- which(apusphigh & apudoy)
  apupbegin <- Halias$Local[idxbegin]
  apupend <- Halias$Local[idxend]
  apumbegin <- Halias$Migr[idxbegin]
  apumend <- Halias$Migr[idxend]

  apuhp <- Halias$Year[which(apudoy)]
  apuhp2 <- which(apuspdoyobs)
  apuhpbegin <- Halias$Year[which(apudoy & apulow)]
  apuhp2begin <- Halias$Local[which(apuspdoyobs & apulow)]
  apuhpend <- Halias$Year[which(apudoy & apuhigh)]
  apuhp2end <- Halias$Local[which(apuspdoyobs & apuhigh)]

  apuHM <- length(unique(apuhp))
  apuHP <- apuHM - length(apuhp2)
  apuHMbegin <- length(unique(apuhpbegin))
  apuHPbegin <- apuHMbegin - length(apuhp2begin)
  apuHMend <- length(unique(apuhpend))
  apuHPend <- apuHMend - length(apuhp2end)

  # Locals whole period
  if(length(apuHP) > 0) {
    phen$paik[i] <- sum(apup) / apuHP
  }

  # Migrants whole period
  if(length(apuHM) > 0) {
    phen$muutto[i] <- sum(apum) / apuHM
  }

  # Locals beginning of the period
  if(length(apuHPbegin) > 0) {
    phen$paikbegin[i] <- sum(apupbegin) / apuHPbegin
  }

  # Migrants beginning of the period
  if(length(apuHMbegin) > 0) {
    phen$muuttobegin[i] <- sum(apumbegin) / apuHMbegin
  }

  # Locals end of the period
  if(length(apuHPend) > 0) {
    phen$paikend[i] <- sum(apupend) / apuHPend
  }

  # Migrants end of the period
  if(length(apuHMend) > 0) {
    phen$muuttoend[i] <- sum(apumend) / apuHMend
  }
}

phen$begin <- phen$paikbegin + phen$muuttobegin
phen$end <- phen$paikend + phen$muuttoend


spSCI=unique(Halias[which(Halias$Species_Abb==sp),2])

root=paste("/Users/aleksilehikoinen/R/Halias/", sp, ".pdf", sep="")



#paste(sp, “, Paikalliset / Lokal / Locals”, sep=””)
#pdf(file = root )

par(mfrow=c(3, 1))

plot(phen$day, phen$paik, type="l", main=paste(spSCI, ", Paikalliset / Lokal / Locals", sep=""),
          xlab="Day of Year", ylab="Yks./pvm - Ind./day")
plot(phen$day, phen$muutto, type="l", main="Muuttavat / Flyttande / Migrants",
     xlab="Day of Year", ylab="Yks./pvm - Ind./day")

apuphen <- which(phen$begin > -1)
maintxt <- "Muutos / Förändring / Change"
if(max(phen$begin[apuphen]) > max(phen$end[apuphen])) {
  plot(phen$day, phen$begin, col="blue", type="l", main=maintxt,
       xlab="Day of Year", ylab="Yks./pvm - Ind./day")
  lines(phen$day, phen$end, col="red")
  text(30,max(phen$begin[apuphen])*0.95,"1979-1999",col="blue")
  text(21,max(phen$begin[apuphen])*0.83,"2009-",col="red")
} else {
  plot(phen$day, phen$end, col="red", type="l", main=maintxt,
       xlab="Day of Year", ylab="Yks./pvm - Ind./day")
  lines(phen$day, phen$begin, col="blue")
  text(30,max(phen$end[apuphen])*0.95,"1979-1999",col="blue")
  text(22,max(phen$end[apuphen])*0.83,"2009-",col="red")
  
}

#dev.off()

