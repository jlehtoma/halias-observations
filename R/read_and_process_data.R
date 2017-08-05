
# Helper functions --------------------------------------------------------

process_sp <- function(sp) {
  
  message("Processing species: ", sp, "...")
  
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
  for (i in 1:366) {
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
    if (length(apuHP) > 0) {
      phen$paik[i] <- sum(apup) / apuHP
    }
    
    # Migrants whole period
    if (length(apuHM) > 0) {
      phen$muutto[i] <- sum(apum) / apuHM
    }
    
    # Locals beginning of the period
    if (length(apuHPbegin) > 0) {
      phen$paikbegin[i] <- sum(apupbegin) / apuHPbegin
    }
    
    # Migrants beginning of the period
    if (length(apuHMbegin) > 0) {
      phen$muuttobegin[i] <- sum(apumbegin) / apuHMbegin
    }
    
    # Locals end of the period
    if (length(apuHPend) > 0) {
      phen$paikend[i] <- sum(apupend) / apuHPend
    }
    
    # Migrants end of the period
    if (length(apuHMend) > 0) {
      phen$muuttoend[i] <- sum(apumend) / apuHMend
    }
  }
  
  phen$begin <- phen$paikbegin + phen$muuttobegin
  phen$end <- phen$paikend + phen$muuttoend
  # Convert to Date. We need to provide some origin year (here 2000) although
  # this doesn't really matter
  phen$day <- as.Date(phen$day - 1, origin = "2000-01-01")
  
  # Get species name in Finnish
  spSCI <- unique(Halias[which(Halias$Species_Abb == sp), ]$FIN_name)
  phen$sp <- spSCI
  
  return(phen)
}

# Haliasaineisto

# Hae csv-tiedosto / choose the csv-file
Halias <- read.csv("data/Kokodata_20161025_20170726.csv", fileEncoding = "macintosh", 
                   stringsAsFactors = FALSE)
str(Halias) # aineiston rakenne / structure of the data

# Tuotetaan csv-tiedosto, jossa asemalla havaittujen lajien ja lajiryhmien lista / Produces a csv-file...
# including a list of all the observed species and species groups
Halias_sp <- unique(Halias$Species_Abb)
write.csv(Halias_sp, "data/Halias_sp.csv")

# Valitse laji ja kirjoita se lainausmerkkien sisään (tässä tukkasotka AYTFUL).
# Choose a species and write it between the quotation marks (here tufted duck AYTFUL).
# sp <- c("GAVSTE") # TÄHÄN KANNATTAA VALITA MAHDOLLISIMMAN RAFLAAVA LAJI

dat <- Halias_sp %>% 
  purrr::map(process_sp) %>% 
  dplyr::bind_rows()

save(dat, file = "data/sp_yearly.RData")
