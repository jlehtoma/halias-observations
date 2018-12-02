library(ggplot2)
library(gridExtra)
library(scales)
library(tidyverse)

# Read in data ------------------------------------------------------------

# Load data. The relevant object is called "dat"
load("shiny/sp_yearly.RData")

# Select only one spcecies
spSCI <- "kaakkuri"
sp_dat <- dat %>% 
  dplyr::filter(sp == spSCI)

# Make a subselectiong of the data containing two different epochs:
# 1979-1999 and 2009-
epochs <- sp_dat %>% 
  dplyr::select(day, begin, end) %>% 
  tidyr::gather(variable, value, -day)

# Plot data ---------------------------------------------------------------

p1 <- ggplot(sp_dat, aes(x = day, y = paik)) +
  geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
  ggtitle(paste0(spSCI, ", Paikalliset / Lokal / Locals")) + 
  scale_x_date(labels = date_format('%e %b')) + theme_bw()

p2 <- ggplot(sp_dat, aes(x = day, y = muutto)) +
  geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
  ggtitle(paste0(spSCI, ", Muuttavat / Flyttande / Migrants")) + 
  scale_x_date(labels = date_format('%e %b')) + theme_bw()

p3 <- ggplot(epochs, aes(x = day, y = value, color = variable)) +
  geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
  ggtitle("Muutos / Förändring / Change") + 
  scale_color_manual(values = c("red", "blue"),
                     labels = c("1979-1999", "2009-")) + 
  scale_x_date(labels = date_format('%e %b')) +
  theme_bw() + theme(legend.title = element_blank(), legend.position = c(0.9, 0.75))

p4 <- grid.arrange(p1, p2, p3, nrow = 3, ncol = 1)
