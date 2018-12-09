library(progress)
library(skimr)
library(tidyverse)
# Haliasaineisto

# Haetaan varsinainen aineiston csv-tiedosto / choose the csv-file
Halias <- read.csv("data/1.1/Haliasdata_v1.1.csv",
  fileEncoding = "macintosh"
) # Haliasdata_v1.1.csv
skimr::skim(Halias) # aineiston rakenne / structure of the data

# Haetaan lajeista ja niiden lisätiedoista koostuva tiedosto
Halias_sp <- read.csv("data/1.1/Halias_sp_v1.2.csv") # Halias_sp_v1.2.csv
# sp_species=unique(as.character(Halias_sp[which(Halias_sp$Sp==0),5]))

# sp_species=as.data.frame(sp_species)
# for(i in 1:nrow(sp_species)){
#  if(nrow(Halias[which(Halias$Species_Abb==as.character(sp_species$sp_species[i])),10:11])>0){
#  sp_species$sum[i]=sum(Halias[which(Halias$Species_Abb==as.character(sp_species$sp_species[i])),10:11])
#  }
# }

# Haetaan tiedosto, jossa kerrottu mihin lajeihin lajilleen määrittämättömät taksonit voidaan yhdistää
sp_species <- read.csv("data/1.1/Sp_species_1.1.csv") # Sp_species_1.1.csv

Halias$spLocal <- Halias$Local
Halias$spMigr <- Halias$Migr
Halias$spStand <- Halias$Stand

# Halias$Migr=Halias$spMigr
# Halias$Local=Halias$spLocal

taxon <- unique(as.character(sp_species$Taxon))

# taxN=data.frame(taxon)
# for(i in 1:length(taxon)){
# taxN$Nl[i]=nrow(Halias[which(Halias$Species_Abb==taxon[i] & Halias$Local>0),])
# taxN$Nm[i]=nrow(Halias[which(Halias$Species_Abb==taxon[i] & Halias$Migr>0),])
# }

# Käy läpi kaikki määrittämättömät taksonit ja lisätään nämä määritettyihin näiden määritysosuuksien
# mukaan, tämä looppi kestää useita tunteja

# Set up the progress bar
pb <- progress::progress_bar$new(
  format = "  processing :taxon [:bar] :percent in :elapsed",
  total = length(taxon), clear = FALSE, width = 60)

for (i in 1:length(taxon)) {

  pb$tick(tokens = list(taxon = paste0(taxon[i], "  ")))
  
  apu <- sp_species[which(sp_species$Taxon == taxon[i]), ]

  sp_loc <- Halias[which(Halias$Species_Abb == taxon[i] & Halias$Local > 0), ]
  sp_mig <- Halias[which(Halias$Species_Abb == taxon[i] & Halias$Migr > 0), ]

  # Jos kyseisen taksonin havaintojen jakaminen riippuu vuodenajasta ja jaettavia lajeja on useita
  if (length(unique(apu$Date1)) > 1 & length(unique(apu$Species)) > 1) {
    if (nrow(sp_loc) > 0) {
      # käydään kaikki paikallishavikset läpi
      for (h in 1:nrow(sp_loc)) {
        # for(h in 1:1){

        apup <- sp_species[which(sp_species$Taxon == taxon[i] & sp_species$Date1 <= sp_loc$Day.of.Year[h] & sp_species$Date2 >= sp_loc$Day.of.Year[h]), ]

        # jos jaettavia lajeja on vain yksi tässä vuodenaikaisjaksossa (esim. V -> clahye loppusyksystä)
        if (length(unique(apup$Species)) == 1) {

          # jos saavalla lajilla on jo aikaisemmin havaintoja ko havaintopäivältä lisätään tähän havaintoriviin määrät
          if (sum(which(Halias$Species_Abb == as.character(apup$Species[1]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h])) > 0) {
            Halias[which(Halias$Species_Abb == as.character(apup$Species[1]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16] <-
              Halias[which(Halias$Species_Abb == as.character(apup$Species[1]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16] + sp_loc$Local[h]
            # jos saavalra lajilta ei ole aikaisemmin havaintoja ko havaintopäivältä tehdään uusi havaintorivi
          } else if (sum(which(Halias$Species_Abb == as.character(apup$Species[1]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h])) == 0) {
            rivi <- sp_loc[h, ]
            riviapu <- Halias[which(Halias$Species_Abb == as.character(apup$Species[1])), ]
            rivi[1, 1:6] <- riviapu[1, 1:6]
            rivi[1, 10:12] <- 0
            rivi[1, 13] <- NA
            rivi[1, 16] <- sp_loc$Local[h]
            rivi[1, 17:18] <- 0
            Halias <- rbind(Halias, rivi)
          }
        } else if (length(unique(apup$Species)) > 1) {

          # lasketaan kunkin jaettavan lajin runsaus lähipäivinä (painotettu keskiarvo)
          for (l in 1:nrow(apup)) {
            # for(l in 1:1){
            apup$sum[l] <- sum(sum(Halias[which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] - 2 & Halias$Year == sp_loc$Year[h]), 16]) +
              sum(Halias[which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] - 1 & Halias$Year == sp_loc$Year[h]), 16]) * 2 +
              sum(Halias[which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16]) * 3 +
              sum(Halias[which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] + 1 & Halias$Year == sp_loc$Year[h]), 16]) * 2 +
              sum(Halias[which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] + 2 & Halias$Year == sp_loc$Year[h]), 16])) / 9
          }

          # jos edes jostakin jaettavasta lajista on havainto lähipäiviltä
          if (sum(apup$sum > 0)) {

            # jaetaan havainnot lajeille (lajien läpikäynti)
            for (l in 1:nrow(apup)) {
              apup$ratio[l] <- round(apup$sum[l] / sum(apup$sum) * sp_loc$Local[h])

              # jos saavalla lajilla on jo aikaisemmin havaintoja ko havaintopäivältä lisätään tähän havaintoriviin määrät
              if (sum(which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h])) > 0) {
                Halias[which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16] <-
                  Halias[which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16] + apup$ratio[l]

                # jos saavalta lajilta ei ole aikaisemmin havaintoja ko havaintopäivältä tehdään uusi havaintorivi
              } else if (sum(which(Halias$Species_Abb == as.character(apup$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h])) == 0 & apup$ratio[l] > 0) {
                rivi <- sp_loc[h, ]
                riviapu <- Halias[which(Halias$Species_Abb == as.character(apup$Species[l])), ]
                rivi[1, 1:6] <- riviapu[1, 1:6]
                rivi[1, 10:12] <- 0
                rivi[1, 13] <- NA
                rivi[1, 16] <- apup$ratio[l]
                rivi[1, 17:18] <- 0
                Halias <- rbind(Halias, rivi)
              }
            }
          }
        }
      }
    }
    # käydään kaikki muuttohavikset läpi
    for (h in 1:nrow(sp_mig)) {
      # for(h in 1:1){

      apum <- sp_species[which(sp_species$Taxon == taxon[i] & sp_species$Date1 <= sp_mig$Day.of.Year[h] & sp_species$Date2 >= sp_mig$Day.of.Year[h]), ]

      # jos jaettavia lajeja on vain yksi tässä vuodenaikaisjaksossa (esim. V -> clahye loppusyksystä)
      if (length(unique(apum$Species)) == 1) {

        # jos saavalla lajilla on jo aikaisemmin havaintoja ko havaintopäivältä lisätään tähän havaintoriviin määrät
        if (sum(which(Halias$Species_Abb == as.character(apum$Species[1]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h])) > 0) {
          # 9
          Halias[which(Halias$Species_Abb == as.character(apum$Species[1]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17] <-
            Halias[which(Halias$Species_Abb == as.character(apum$Species[1]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17] + sp_mig$Migr[h]

          Halias[which(Halias$Species_Abb == as.character(apum$Species[1]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 18] <-
            Halias[which(Halias$Species_Abb == as.character(apum$Species[1]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 18] + sp_mig$Stand[h]

          # jos saavalta lajilta ei ole aikaisemmin havaintoja ko havaintopäivältä tehdään uusi havaintorivi
        } else if (sum(which(Halias$Species_Abb == as.character(apum$Species[1]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h])) == 0 & sp_mig$Migr[h] > 0) {
          rivi <- sp_mig[h, ]
          riviapu <- Halias[which(Halias$Species_Abb == as.character(apum$Species[1])), ]
          rivi[1, 1:6] <- riviapu[1, 1:6]
          rivi[1, 10:12] <- 0
          rivi[1, 13] <- NA
          rivi[1, 16] <- 0
          rivi[1, 17] <- sp_mig$Migr[h]
          rivi[1, 18] <- sp_mig$Stand[h]
          Halias <- rbind(Halias, rivi)
        }
      } else if (length(unique(apum$Species)) > 1) {


        # lasketaan kunkin jaettavan lajin runsaus lähipäivinä (painotettu keskiarvo)
        for (l in 1:nrow(apum)) {
          apum$sum[l] <- sum(sum(Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] - 2 & Halias$Year == sp_mig$Year[h]), 17]) +
            sum(Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] - 1 & Halias$Year == sp_mig$Year[h]), 17]) * 2 +
            sum(Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17]) * 3 +
            sum(Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] + 1 & Halias$Year == sp_mig$Year[h]), 17]) * 2 +
            sum(Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] + 2 & Halias$Year == sp_mig$Year[h]), 17])) / 9
        }

        # jos edes jostakin jaettavasta lajista on havainto lähipäiviltä
        if (sum(apum$sum > 0)) {

          # jaetaan havainnot lajeille (lajien läpikäynti)
          for (l in 1:nrow(apum)) {
            apum$ratio[l] <- round(apum$sum[l] / sum(apum$sum) * sp_mig$Migr[h])
            apum$ratiost[l] <- round(apum$sum[l] / sum(apum$sum) * sp_mig$Stand[h])

            # jos saavalla lajilla on jo aikaisemmin havaintoja ko havaintopäivältä lisätään tähän havaintoriviin määrät
            if (sum(which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h])) > 0) {
              Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17] <-
                Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17] + apum$ratio[l]

              Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 18] <-
                Halias[which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 18] + apum$ratiost[l]

              # jos saavalta lajilta ei ole aikaisemmin havaintoja ko havaintopäivältä tehdään uusi havaintorivi
            } else if (sum(which(Halias$Species_Abb == as.character(apum$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h])) == 0 & apum$ratio[l] > 0) {
              rivi <- sp_mig[h, ]
              riviapu <- Halias[which(Halias$Species_Abb == as.character(apum$Species[l])), ]
              rivi[1, 1:6] <- riviapu[1, 1:6]
              rivi[1, 10:12] <- 0
              rivi[1, 13] <- NA
              rivi[1, 16] <- 0
              rivi[1, 17] <- apum$ratio[l]
              rivi[1, 18] <- apum$ratiost[l]
              Halias <- rbind(Halias, rivi)
            }
          }
        }
      }
    }


    # Jos jaettavat lajit ovat samat koko vuoden ajan
  } else if (length(unique(apu$Date1)) == 1) {

    # jos jaettavia lajeja on vain yksi (esim. strepto -> strtur )
    if (length(unique(apu$Species)) == 1) {
      if (nrow(sp_loc) > 0) {
        # käydään kaikki paikallishavikset läpi
        for (h in 1:nrow(sp_loc)) {
          # for(h in 1:1){

          # jos saavalla lajilla on jo aikaisemmin havaintoja ko havaintopäivältä lisätään tähän havaintoriviin määrät
          if (sum(which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h])) > 0) {
            Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16] <-
              Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16] + sp_loc$Local[h]

            # jos saavalta lajilta ei ole aikaisemmin havaintoja ko havaintopäivältä tehdään uusi havaintorivi
          } else if (sum(which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h])) == 0) {
            rivi <- sp_loc[h, ]
            riviapu <- Halias[which(Halias$Species_Abb == as.character(apu$Species[1])), ]
            rivi[1, 1:6] <- riviapu[1, 1:6]
            rivi[1, 10:12] <- 0
            rivi[1, 13] <- NA
            rivi[1, 16] <- sp_loc$Local[h]
            rivi[1, 17:18] <- 0
            Halias <- rbind(Halias, rivi)
          }
        }
      }
      # käydään kaikki muuttohavikset läpi
      for (h in 1:nrow(sp_mig)) {
        # for(h in 1:1){

        # jos saavalla lajilla on jo aikaisemmin havaintoja ko havaintopäivältä lisätään tähän havaintoriviin määrät
        if (sum(which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h])) > 0) {
          Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17] <-
            Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17] + sp_mig$Migr[h]

          Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 18] <-
            Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 18] + sp_mig$Stand[h]

          # jos saavalta lajilta ei ole aikaisemmin havaintoja ko havaintopäivältä tehdään uusi havaintorivi
        } else if (sum(which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h])) == 0) {
          rivi <- sp_mig[h, ]
          riviapu <- Halias[which(Halias$Species_Abb == as.character(apu$Species[1])), ]
          rivi[1, 1:6] <- riviapu[1, 1:6]
          rivi[1, 10:12] <- 0
          rivi[1, 13] <- NA
          rivi[1, 16] <- 0
          rivi[1, 17] <- sp_mig$Migr[h]
          rivi[1, 18] <- sp_mig$Stand[h]
          Halias <- rbind(Halias, rivi)
        }
      }
      # jos jaettavia lajeja on useampi
    } else if (length(unique(apu$Species)) > 1) {
      if (nrow(sp_loc) > 0) {
        # käydään kaikki paikallishavikset läpi
        for (h in 1:nrow(sp_loc)) {
          # for(h in 147:147){

          # lasketaan kunkin jaettavan lajin runsaus lähipäivinä (painotettu keskiarvo)
          for (l in 1:nrow(apu)) {
            # for(l in 1:1){
            apu$sum[l] <- sum(sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] - 2 & Halias$Year == sp_loc$Year[h]), 16]) +
              sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] - 1 & Halias$Year == sp_loc$Year[h]), 16]) * 2 +
              sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16]) * 3 +
              sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] + 1 & Halias$Year == sp_loc$Year[h]), 16]) * 2 +
              sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] + 2 & Halias$Year == sp_loc$Year[h]), 16])) / 9
          }
          # jos edes jostakin jaettavasta lajista on havainto lähipäiviltä
          if (sum(apu$sum > 0)) {

            # jaetaan havainnot lajeille (lajien läpikäynti)
            for (l in 1:nrow(apu)) {
              apu$ratio[l] <- round(apu$sum[l] / sum(apu$sum) * sp_loc$Local[h])

              # jos saavalla lajilla on jo aikaisemmin havaintoja ko havaintopäivältä lisätään tähän havaintoriviin määrät
              if (sum(which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h])) > 0) {
                Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16] <-
                  Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h]), 16] + apu$ratio[l]

                # jos saavalta lajilta ei ole aikaisemmin havaintoja ko havaintopäivältä tehdään uusi havaintorivi
              } else if (sum(which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_loc$Day.of.Year[h] & Halias$Year == sp_loc$Year[h])) == 0 & apu$ratio[l] > 0) {
                rivi <- sp_loc[h, ]
                riviapu <- Halias[which(Halias$Species_Abb == as.character(apu$Species[l])), ]
                rivi[1, 1:6] <- riviapu[1, 1:6]
                rivi[1, 10:12] <- 0
                rivi[1, 13] <- NA
                rivi[1, 16] <- apu$ratio[l]
                rivi[1, 17:18] <- 0
                Halias <- rbind(Halias, rivi)
              }
            }
          }
        }
      }
      # }

      # käydään kaikki muuttohavikset läpi
      for (h in 1:nrow(sp_mig)) {
        # for(h in 1:1){

        # lasketaan kunkin jaettavan lajin runsaus lähipäivinä (painotettu keskiarvo)
        for (l in 1:nrow(apu)) {
          apu$sum[l] <- sum(sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] - 2 & Halias$Year == sp_mig$Year[h]), 17]) +
            sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] - 1 & Halias$Year == sp_mig$Year[h]), 17]) * 2 +
            sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17]) * 3 +
            sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] + 1 & Halias$Year == sp_mig$Year[h]), 17]) * 2 +
            sum(Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] + 2 & Halias$Year == sp_mig$Year[h]), 17])) / 9
        }
        # jos edes jostakin jaettavasta lajista on havainto lähipäiviltä
        if (sum(apu$sum > 0)) {

          # jaetaan havainnot lajeille (lajien läpikäynti)
          for (l in 1:nrow(apu)) {
            apu$ratio[l] <- round(apu$sum[l] / sum(apu$sum) * sp_mig$Migr[h])
            apu$ratiost[l] <- round(apu$sum[l] / sum(apu$sum) * sp_mig$Stand[h])

            # jos saavalla lajilla on jo aikaisemmin havaintoja ko havaintopäivältä lisätään tähän havaintoriviin määrät
            if (sum(which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h])) > 0) {
              Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17] <-
                Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 17] + apu$ratio[l]

              Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 18] <-
                Halias[which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h]), 18] + apu$ratiost[l]

              # jos saavalta lajilta ei ole aikaisemmin havaintoja ko havaintopäivältä tehdään uusi havaintorivi
            } else if (sum(which(Halias$Species_Abb == as.character(apu$Species[l]) & Halias$Day.of.Year == sp_mig$Day.of.Year[h] & Halias$Year == sp_mig$Year[h])) == 0 & apu$ratio[l] > 0) {
              rivi <- sp_mig[h, ]
              riviapu <- Halias[which(Halias$Species_Abb == as.character(apu$Species[l])), ]
              rivi[1, 1:6] <- riviapu[1, 1:6]
              rivi[1, 10:12] <- 0
              rivi[1, 13] <- NA
              rivi[1, 16] <- 0
              rivi[1, 17] <- apu$ratio[l]
              rivi[1, 18] <- apu$ratiost[l]
              Halias <- rbind(Halias, rivi)
            }
          }
        }
      }
    }
  }
}

save(Halias, file = "data/processed_Halias.RData")

# Valitse laji ja kirjoita sen lyhenne lainausmerkkien sisään (tässä telkkä BUCCLA).
# Choose a species and write it's abbreviation between the quotation marks (here common goldeneye BUCCLA).
sp <- c("SOMMOL")

# Seuraava käskyjoukko tuottaa kolmiosaisen kuvaajan, jossa ylin kuvaaja kertoo paikallismäärien
# vaihtelun vuoden aikana ja keskimmäinen muuttajien vastaavan vaihtelun. Kuvaajissa x-akselilla on päivämäärä 1.1.-31.12.
# Kolmas kuvaaja kertoo paikallisten ja muuttajien yhdistetyn runsauden ennen vuotta 2000 (sininen viiva)
# ja vuoden 2009 jälkeen (punainen viiva). Näiden avulla käyrien avulla voi tarkastelajin havaintomäärissä
# tapahtuneita pitkäaikaisia muutoksia.

# Following code will draw a figure with three panels of the selected species.
# The upper panel shows the mean counts of local birds per each calendar day over the years.
# The middle panel shows the mean migration counts per each calendar day over the years.
# The lowest panel shows combined mean daily abundances (locals and migrants) during the beginning of the study
# period 1979-1999 (blue line) and since 2009 (red line).
# The panel visualise potential long-term changes in abundance of the selected species.

Halias_SP <- Halias_sp[which(Halias_sp$Sp == 1), ]

Halias$Additional <- as.numeric(Halias$Additional)

# Haliasvali <- Halias[which(Halias$Additional==0),]
Haliasvali <- Halias[which(Halias$Additional > 0 & Halias$Species_code > 469), ]
Haliasvali$lisaP <- Haliasvali$spLocal + Haliasvali$Additional
Haliasvali2 <- Halias[which(is.na(Halias$Additional == TRUE)), ]
Haliasvali2$lisaP <- Haliasvali2$Local
Halias <- rbind(Haliasvali, Haliasvali2)

# Hae lajilistaus / download list of species

Haliasvali <- Halias[which(Halias$Night_migr > -1), ]
Haliasvali$lisaM <- Haliasvali$spMigr + Haliasvali$Night_migr
Haliasvali2 <- Halias[which(is.na(Halias$Night_migr == TRUE)), ]
Haliasvali2$lisaM <- NA
Halias <- rbind(Haliasvali, Haliasvali2)


trend <- list()
spring <- matrix(NA, nrow = 0, ncol = 2)
colnames(spring) <- c("sp", "date")

autumn <- matrix(NA, nrow = 0, ncol = 2)
colnames(autumn) <- c("sp", "date")

setwd("/users/aleksilehikoinen/R/Halias")


for (l in 1:nrow(Halias_SP)) {
  # for(l in 1:1){

  # Valitse laji ja kirjoita se lainausmerkkien sisään (tässä tukkasotka AYTFUL).
  # Choose a species and write it between the quotation marks (here tufted duck AYTFUL).
  sp <- as.character(Halias_SP$Species_Abb[l])

  # sp <- c("AEGCAU") # TÄHÄN KANNATTAA VALITA MAHDOLLISIMMAN RAFLAAVA LAJI

  # Jos lajin lisäpaikalliset soveltuu paikalliskuvaajaan niin käytetään myös lisäaluetta (rarit)
  if ((Halias_SP$Add[l]) > 0) {
    Halias$P <- Halias$lisaP
  } else {
    Halias$P <- Halias$spLocal
  }

  # Jos lajin yömuuttavat soveltuu muuttokuvaajaan niin käytetään myös yöm (Botste)

  if ((Halias_SP$Night[l]) > 0) {
    Halias$M <- Halias$lisaM
  } else {
    Halias$M <- Halias$spMigr
  }

  # Seuraava käskyjoukko tuottaa kolmiosaisen kuvaajan, jossa ylin kuvaaja kertoo paikallismäärien
  # vaihtelun vuoden aikana ja keskimmäinen muuttajien vastaavan vaihtelun. Kuvaajissa x-akselilla on
  # juokseva päivämäärä (1.1. = 1 jne). Kolmas kuvaaja kertoo paikallisten ja muuttajien yhdistetyn
  # runsauden ennen vuotta 2000 (sininen viiva) ja vuoden 2009 jälkeen (punainen viiva). Näiden avulla
  # käyrien avulla voi tarkastelajin havaintomäärissä tapahtuneita pitkäaikaisia muutoksia.

  phen <- data.frame(
    day = 1:366, paik = NA_real_, muutto = 0,
    paikbegin = NA_real_, muuttobegin = 0,
    paikend = NA_real_, muuttoend = 0
  )
  apusp <- Halias$Species_Abb == sp
  apulow <- Halias$Year < 2000
  apumed <- Halias$Year > 1999 & Halias$Year < 2011
  apuhigh <- Halias$Year > 2010
  apusplow <- apusp & apulow
  apuspmed <- apusp & apumed
  apusphigh <- apusp & apuhigh
  for (i in 1:366) {
    apudoy <- Halias$Day.of.Year == i
    apuspdoy <- apusp & apudoy
    apuspdoyobs <- apuspdoy & Halias$Observed

    apup <- Halias$P[which(apuspdoy & !Halias$Observed)]
    apum <- Halias$M[which(apuspdoy)]
    idxbegin <- which(apusplow & apudoy)
    idxmed <- which(apuspmed & apudoy)
    idxend <- which(apusphigh & apudoy)
    apupbegin <- Halias$P[idxbegin]
    apupmed <- Halias$P[idxmed]
    apupend <- Halias$P[idxend]
    apumbegin <- Halias$M[idxbegin]
    apummed <- Halias$M[idxmed]
    apumend <- Halias$M[idxend]

    apuhp <- Halias$Year[which(apudoy)]
    apuhp2 <- which(apuspdoyobs)
    apuhpbegin <- Halias$Year[which(apudoy & apulow)]
    apuhp2begin <- Halias$P[which(apuspdoyobs & apulow)]
    apuhpmed <- Halias$Year[which(apudoy & apumed)]
    apuhp2med <- Halias$P[which(apuspdoyobs & apumed)]
    apuhpend <- Halias$Year[which(apudoy & apuhigh)]
    apuhp2end <- Halias$P[which(apuspdoyobs & apuhigh)]

    apuHM <- length(unique(apuhp))
    apuHP <- apuHM - length(apuhp2)
    apuHMbegin <- length(unique(apuhpbegin))
    apuHPbegin <- apuHMbegin - length(apuhp2begin)
    apuHMmed <- length(unique(apuhpmed))
    apuHPmed <- apuHMmed - length(apuhp2med)
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
    # Locals med of the period
    if (length(apuHPmed) > 0) {
      phen$paikmed[i] <- sum(apupmed) / apuHPmed
    }

    # Migrants med of the period
    if (length(apuHMmed) > 0) {
      phen$muuttomed[i] <- sum(apummed) / apuHMmed
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

  if (Halias_SP$Trend[l] == 1) {
    phen$begin <- phen$muuttobegin
    phen$med <- phen$muuttomed
    phen$end <- phen$muuttoend
  } else if (Halias_SP$Trend[l] == 2) {
    phen$begin <- phen$paikbegin + phen$muuttobegin
    phen$med <- phen$paikmed + phen$muuttomed
    phen$end <- phen$paikend + phen$muuttoend
  }
  phen$day <- as.Date(phen$day - 1, origin = "2000-01-01")

  N <- list()
  N$begin <- sum(phen[which(phen$begin > 0), 10])
  N$med <- sum(phen[which(phen$med > 0), 11])
  N$end <- sum(phen[which(phen$end > 0), 12])


  # Fenologia muutokset
  Phen <- list()

  if (Halias_SP$P[l] == 1 & Halias_SP$M[l] == 0) {
    Phen$begin <- phen$paikbegin
    Phen$med <- phen$paikmed
    Phen$end <- phen$paikend
  } else if (Halias_SP$P[l] == 0 & Halias_SP$M[l] == 1) {
    Phen$begin <- phen$muuttobegin
    Phen$med <- phen$muuttomed
    Phen$end <- phen$muuttoend
  } else if (Halias_SP$P[l] == 1 & Halias_SP$M[l] == 1) {
    Phen$begin <- phen$paikbegin + phen$muuttobegin
    Phen$med <- phen$paikmed + phen$muuttomed
    Phen$end <- phen$paikend + phen$muuttoend
  }

  PHENs <- data.frame()
  for (dt in Halias_SP$SpB[l]:Halias_SP$SpE[l]) {
    PH <- c(
      dt,
      sum(Phen$begin[Halias_SP$SpB[l]:dt], na.rm = TRUE) / sum(Phen$begin[Halias_SP$SpB[l]:Halias_SP$SpE[l]], na.rm = TRUE),
      sum(Phen$med[Halias_SP$SpB[l]:dt]) / sum(Phen$med[Halias_SP$SpB[l]:Halias_SP$SpE[l]]),
      sum(Phen$end[Halias_SP$SpB[l]:dt]) / sum(Phen$end[Halias_SP$SpB[l]:Halias_SP$SpE[l]])
    )

    PHENs <- rbind(PHENs, PH)
  }
  colnames(PHENs) <- c("dt", "begin", "med", "end")

  PHENa <- data.frame()

  if (Halias_SP$AuE[l] > 70) {
    for (dt in Halias_SP$AuB[l]:Halias_SP$AuE[l]) {
      PH <- c(
        dt,
        sum(Phen$begin[Halias_SP$AuB[l]:dt], na.rm = TRUE) / sum(Phen$begin[Halias_SP$AuB[l]:Halias_SP$AuE[l]], na.rm = TRUE),
        sum(Phen$med[Halias_SP$AuB[l]:dt]) / sum(Phen$med[Halias_SP$AuB[l]:Halias_SP$AuE[l]]),
        sum(Phen$end[Halias_SP$AuB[l]:dt]) / sum(Phen$end[Halias_SP$AuB[l]:Halias_SP$AuE[l]])
      )

      PHENa <- rbind(PHENa, PH)
    }
  } else if (Halias_SP$AuE[l] < 70) {
    for (dt in Halias_SP$AuB[l]:366) {
      PH <- c(
        dt,
        sum(Phen$begin[Halias_SP$AuB[l]:dt], na.rm = TRUE) /
          (sum(Phen$begin[1:Halias_SP$AuE[l]], na.rm = TRUE) + sum(Phen$begin[Halias_SP$AuB[l]:366], na.rm = TRUE)),
        sum(Phen$med[Halias_SP$AuB[l]:dt]) /
          (sum(Phen$med[1:Halias_SP$AuE[l]], na.rm = TRUE) + sum(Phen$med[Halias_SP$AuB[l]:366], na.rm = TRUE)),
        sum(Phen$end[Halias_SP$AuB[l]:dt]) /
          (sum(Phen$end[1:Halias_SP$AuE[l]], na.rm = TRUE) + sum(Phen$end[Halias_SP$AuB[l]:366], na.rm = TRUE))
      )

      PHENa <- rbind(PHENa, PH)
    }
    for (dt in 1:Halias_SP$AuE[l]) {
      PH <- c(
        366 + dt,
        (sum(Phen$begin[1:dt], na.rm = TRUE) + sum(Phen$begin[Halias_SP$AuB[l]:366], na.rm = TRUE)) /
          (sum(Phen$begin[1:Halias_SP$AuE[l]], na.rm = TRUE) + sum(Phen$begin[Halias_SP$AuB[l]:366], na.rm = TRUE)),

        (sum(Phen$med[1:dt], na.rm = TRUE) + sum(Phen$med[Halias_SP$AuB[l]:366], na.rm = TRUE)) /
          (sum(Phen$med[1:Halias_SP$AuE[l]], na.rm = TRUE) + sum(Phen$med[Halias_SP$AuB[l]:366], na.rm = TRUE)),

        (sum(Phen$end[1:dt], na.rm = TRUE) + sum(Phen$end[Halias_SP$AuB[l]:366], na.rm = TRUE)) /
          (sum(Phen$end[1:Halias_SP$AuE[l]], na.rm = TRUE) + sum(Phen$end[Halias_SP$AuB[l]:366], na.rm = TRUE))
      )

      PHENa <- rbind(PHENa, PH)
    }
  }

  colnames(PHENa) <- c("dt", "begin", "med", "end")

  SPHEN <- list()
  sphen <- matrix(NA, nrow = 0, ncol = 4)
  colnames(sphen) <- c("sp", "begin", "med", "end")
  sphen <- as.list(sphen)

  if (sum(Phen$begin[Halias_SP$SpB[l]:Halias_SP$SpE[l]], na.rm = TRUE) > 1 &
    sum(Phen$med[Halias_SP$SpB[l]:Halias_SP$SpE[l]]) > 1 &
    sum(Phen$end[Halias_SP$SpB[l]:Halias_SP$SpE[l]]) > 1) {
    SPHEN$begin <- min(PHENs[which(PHENs$begin > 0.5), 1])
    SPHEN$med <- min(PHENs[which(PHENs$med > 0.5), 1])
    SPHEN$end <- min(PHENs[which(PHENs$end > 0.5), 1])

    sphen$sp[1] <- as.character(Halias_SP$Species_Abb[l])
    sphen$begin[1] <- SPHEN$begin
    sphen$med[1] <- SPHEN$med
    sphen$end[1] <- SPHEN$end
    sphen <- as.data.frame(sphen)

    spring <- rbind(spring, sphen)

    alku <- main <- paste("1979-1999", format(strptime(SPHEN$begin, format = "%j"), format = "%m-%d"))
    keski <- main <- paste("2000-2010", format(strptime(SPHEN$med, format = "%j"), format = "%m-%d"))
    loppu <- main <- paste("2011-", format(strptime(SPHEN$end, format = "%j"), format = "%m-%d"))
  } else {
    alku <- main <- paste("1979-1999")
    keski <- main <- paste("2000-2010")
    loppu <- main <- paste("2011-")
  }

  APHEN <- list()
  aphen <- matrix(NA, nrow = 0, ncol = 4)
  colnames(aphen) <- c("sp", "begin", "med", "end")
  aphen <- as.list(aphen)

  if (sum(Phen$begin[Halias_SP$AuB[l]:366], na.rm = TRUE) > 1 &
    sum(Phen$med[Halias_SP$AuB[l]:366], na.rm = TRUE) > 1 &
    sum(Phen$end[Halias_SP$AuB[l]:366], na.rm = TRUE) > 1) {
    APHEN$begin <- min(PHENa[which(PHENa$begin > 0.5), 1])
    APHEN$med <- min(PHENa[which(PHENa$med > 0.5), 1])
    APHEN$end <- min(PHENa[which(PHENa$end > 0.5), 1])

    aphen$sp[1] <- as.character(Halias_SP$Species_Abb[l])
    aphen$begin[1] <- APHEN$begin
    aphen$med[1] <- APHEN$med
    aphen$end[1] <- APHEN$end
    aphen <- as.data.frame(aphen)

    spring <- rbind(spring, sphen)

    autumn <- rbind(autumn, aphen)
  }



  spSCI <- unique(Halias[which(Halias$Species_Abb == sp), 4])

  root <- paste("/Users/aleksilehikoinen/R/Halias/", sp, ".pdf", sep = "")

  pdf(file = root)

  par(mfrow = c(3, 1))

  plot(phen$day, phen$paik,
    type = "l", main = paste(spSCI, ", Paikalliset / Stationära / Locals", sep = ""),
    xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
  )
  plot(phen$day, phen$muutto,
    type = "l", main = "Muuttavat / Flyttande / Migrants",
    xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
  )

  apuphen <- which(phen$begin > -1)

  if (max(phen$begin[apuphen], na.rm = TRUE) > max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) > max(phen$med[apuphen], na.rm = TRUE) & sum(phen[which(phen$med > 0), 11]) > 0) {
    plot(phen$day, phen$begin,
      col = "blue", type = "l",
      main = paste("Runsauden muutos / Förändring i antal / Change in abundance long ",
        round((N$end / N$begin - 1) * 100), "%, short ", round((N$end / N$med - 1) * 100), "%",
        sep = ""
      ),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$end, col = "red")
    lines(phen$day, phen$med, col = "cyan")
    text(as.Date(30, origin = "2000-01-01"), max(phen$begin[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$begin[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$begin[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$begin[apuphen], na.rm = TRUE) > max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) > max(phen$med[apuphen], na.rm = TRUE) & sum(phen[which(phen$med > 0), 11]) == 0) {
    plot(phen$day, phen$begin,
      col = "blue", type = "l",
      main = paste("Runsauden muutos / Förändring i antal / Change in abundance long ",
        round((N$end / N$begin - 1) * 100), "%",
        sep = ""
      ),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$end, col = "red")
    lines(phen$day, phen$med, col = "cyan")
    text(as.Date(30, origin = "2000-01-01"), max(phen$begin[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$begin[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$begin[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$begin[apuphen], na.rm = TRUE) > max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) < max(phen$med[apuphen], na.rm = TRUE) & sum(phen[which(phen$begin > 0), 10]) > 0) {
    plot(phen$day, phen$med,
      col = "cyan", type = "l",
      main = paste("Runsauden muutos / Förändring i antal / Change in abundance long ",
        round((N$end / N$begin - 1) * 100), "%, short ", round((N$end / N$med - 1) * 100), "%",
        sep = ""
      ),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$end, col = "red")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$end[apuphen], na.rm = TRUE) < max(phen$med[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) < max(phen$med[apuphen], na.rm = TRUE) & sum(phen[which(phen$begin > 0), 10]) > 0) {
    plot(phen$day, phen$med,
      col = "cyan", type = "l",
      main = paste("Runsauden muutos / Förändring i antal / Change in abundance long ",
        round((N$end / N$begin - 1) * 100), "%, short ", round((N$end / N$med - 1) * 100), "%",
        sep = ""
      ),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$end, col = "red")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$begin[apuphen], na.rm = TRUE) < max(phen$med[apuphen], na.rm = TRUE) & max(phen$end[apuphen], na.rm = TRUE) < max(phen$begin[apuphen], na.rm = TRUE)) {
    plot(phen$day, phen$med,
      col = "cyan", type = "l",
      main = paste("Runsauden muutos / Förändring i antal / Change in abundance long ",
        round((N$end / N$begin - 1) * 100), "%, short ", round((N$end / N$med - 1) * 100), "%",
        sep = ""
      ),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$end, col = "red")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$med[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & sum(phen[which(phen$begin > 0), 10]) > 0 & sum(phen[which(phen$med > 0), 11]) > 0) {
    plot(phen$day, phen$end,
      col = "red", type = "l",
      main = paste("Runsauden muutos / Förändring i antal / Change in abundance long ", round((N$end / N$begin - 1) * 100), "%, short ", round((N$end / N$med - 1) * 100), "%", sep = ""),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$med, col = "cyan")
    text(as.Date(30, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$med[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & sum(phen[which(phen$begin > 0), 10]) > 0 & sum(phen[which(phen$med > 0), 11]) == 0) {
    plot(phen$day, phen$end,
      col = "red", type = "l",
      main = paste("Runsauden muutos / Förändring i antal / Change in abundance long ", round((N$end / N$begin - 1) * 100), "%", sep = ""),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$med, col = "cyan")
    text(as.Date(30, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.71, loppu, col = "red")
  }

  else if (max(phen$med[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & sum(phen[which(phen$begin > 0), 10]) == 0 & sum(phen[which(phen$med > 0), 11]) == 0) {
    plot(phen$day, phen$end,
      col = "red", type = "l", main = "Runsauden muutos / Förändring i antal / Change in abundance",
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$med, col = "cyan")
    text(as.Date(30, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$med[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & sum(phen[which(phen$begin > 0), 10]) == 0 & sum(phen[which(phen$med > 0), 11]) > 0) {
    plot(phen$day, phen$end,
      col = "red", type = "l", main = paste("Runsauden muutos / Förändring i antal / Change in abundance short ", round((N$end / N$med - 1) * 100), "%", sep = ""),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$med, col = "cyan")
    text(as.Date(30, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$end[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$med[apuphen], na.rm = TRUE) > max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) < max(phen$end[apuphen], na.rm = TRUE) & sum(phen[which(phen$begin > 0), 10]) == 0) {
    plot(phen$day, phen$med,
      col = "cyan", type = "l", main = paste("Runsauden muutos / Förändring i antal / Change in abundance short ", round((N$end / N$med - 1) * 100), "%", sep = ""),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$end, col = "red")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.71, loppu, col = "red")
  } else if (max(phen$med[apuphen], na.rm = TRUE) > max(phen$end[apuphen], na.rm = TRUE) & max(phen$begin[apuphen], na.rm = TRUE) == max(phen$end[apuphen], na.rm = TRUE) & sum(phen[which(phen$begin > 0), 10]) == 0) {
    plot(phen$day, phen$med,
      col = "cyan", type = "l", main = paste("Runsauden muutos / Förändring i antal / Change in abundance short ", round((N$end / N$med - 1) * 100), "%", sep = ""),
      xlab = "Day of Year", ylab = "Yks./pvm - Ind./day"
    )
    lines(phen$day, phen$begin, col = "blue")
    lines(phen$day, phen$end, col = "red")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.95, alku, col = "blue")
    text(as.Date(30, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.83, keski, col = "cyan")
    text(as.Date(23.5, origin = "2000-01-01"), max(phen$med[apuphen]) * 0.71, loppu, col = "red")
  }

  dev.off()

  # Lajikohtaiset kannanmuutosprosentit
  if (N$begin > 0) {
    trend$sp[l] <- as.character(sp)
    trend$slope[l] <- round((N$end / N$begin - 1) * 100)
    trend$Nbegin[l] <- N$begin
    trend$Nmed[l] <- N$med
    trend$Nend[l] <- N$end
    trend$NbeginS[l] <- sum(phen$begin[Halias_SP$SpB[l]:Halias_SP$SpE[l]], na.rm = TRUE)
    trend$NmedS[l] <- sum(phen$med[Halias_SP$SpB[l]:Halias_SP$SpE[l]])
    trend$NendS[l] <- sum(phen$end[Halias_SP$SpB[l]:Halias_SP$SpE[l]])
    trend$NbeginA[l] <- sum(phen$begin[Halias_SP$AuB[l]:335], na.rm = TRUE)
    trend$NmedA[l] <- sum(phen$med[Halias_SP$AuB[l]:335])
    trend$NendA[l] <- sum(phen$end[Halias_SP$AuB[l]:335])
  }
}

# trend=as.data.frame(trend)
# write.csv(trend,"Halias_trend20181205.csv")

# plot(log(trend$NendA[which(trend$NbeginS>1 & trend$NbeginA>1)]/trend$NmedA[which(trend$NbeginS>1 & trend$NbeginA>1)]),
#     log(trend$Nend[which(trend$NbeginS>1 & trend$NbeginA>1)]/trend$Nmed[which(trend$NbeginS>1 & trend$NbeginA>1)]))

############# Kuvaajien piirtäminen loppuu ##############


## Annual sums
# nsum=c(1979:2017)
# nsum=as.data.frame(nsum)
# nsum$sum=NA
# for(i in 1979:2017){
#  nsum$sum[i-1978]=sum(Halias$Migr[which(Halias$Year==i & Halias$Species_Abb=="PICCAN" & Halias$Day.of.Year>180)])
#                  +sum(Halias$Paik[which(Halias$Year==i & Halias$Species_Abb=="PICCAN" & Halias$Day.of.Year>180)])
# }


### Percentiles of phenology


# psp="SYLBOR"
# T1=1979
# T2=2017
# phe=c(T1:T2)
# phe=as.data.frame(phe)
# phe$phes=NA
# for(i in T1:T2){
#  apus=Halias[which(Halias$Year==i & Halias$Species_Abb==as.character(psp) & Halias$Day.of.Year<168),]
# apua=Halias[which(Halias$Year==i & Halias$Species_Abb==as.character(psp) & Halias$Day.of.Year>183),]
#  if(nrow(apus)>4 & sum(as.numeric(sum(apus$Local)+sum(apus$Migr)))>20){
#    apus$phe=NA
#    apus$N=NA
#  for(j in 1:nrow(apus)){
#
#    apus$phe[j]=(sum(apus$Local[1:j])+sum(apus$Migr[1:j]))/(sum(apus$Local)+sum(apus$Migr))
#
#    }
#  phe$phes[i-T1+1]=apus$Day.of.Year[min(which(apus$phe>=0.5))]
#
#  }
#  phe$N[i-T1+1]=sum(apus$Local)+sum(apus$Migr)
# }
