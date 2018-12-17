library(tidyverse)


# Load data ---------------------------------------------------------------

load("data/processed_Halias.RData")

# Preprocessing -----------------------------------------------------------

halias_sp <- Halias_sp %>% 
  dplyr::filter(Sp == 1)

halias <- Halias

halias$Additional <- as.numeric(halias$Additional)

pp2 <- function() {
  halias <<- halias %>% 
    dplyr::mutate(lisaP = ifelse(!is.na(Additional) & Species_code > 469, spLocal + Additional, Local)) %>% 
    dplyr::mutate(lisaM = ifelse(!is.na(Night_migr) & Night_migr > -1, spMigr + Night_migr, NA))  
}

t2 <- system.time(res2 <- pp2())
