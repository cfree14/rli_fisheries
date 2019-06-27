
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(taxize)
library(tidyverse)

# Directories
datadir <- "data/ramldb"

# Read RAM Legacy Database
load("/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Model Fit Data/DBdata (model fits included).RData")

# Load function to get G0
source("/Users/cfree/Dropbox/Chris/Rutgers/projects/mscom/data/priors/fishlife2/fishlife2.R")

# Build data
################################################################################

# Build key
taxa_key <- taxonomy %>% 
  # Select columns
  select(kingdom, phylum, classname, ordername, family, genus, scientificname, commonname1) %>% 
  # Rename columns
  rename(class=classname, order=ordername, species=scientificname, comm_name=commonname1) %>% 
  # Update scientific names
  mutate(species=gsub("spp.", "spp", species),
         species=plyr::revalue(species, c("Chrysophrys auratus"="Pagrus auratus",
                                          "Clupea pallasii"="Clupea pallasii pallasii",
                                          "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                                          "Epinephelus niveatus"="Hyporthodus niveatus",
                                          "Etrumeus teres"="Etrumeus sadina",
                                          "Loligo bleekeri"="Heterololigo bleekeri",
                                          "Loligo pealeii"="Doryteuthis pealeii",
                                          "Loligo reynaudii"="Loligo vulgaris reynaudii",
                                          "Merluccius gayi"="Merluccius gayi gayi",
                                          "Mullus barbatus"="Mullus barbatus barbatus",
                                          "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                                          "Psetta maxima"="Scophthalmus maximus",
                                          "Strangomera bentincki"="Clupea bentincki",
                                          "Tetrapturus albidus"="Kajikia albida",
                                          "Sardinops melanostictus"="Sardinops sagax")))

# Look up life history data (generation time)
lh_data <- fishlife2(taxa_key$species)

# Merge taxa key and life history data (generation time)
spp_key <- taxa_key %>% 
  left_join(lh_data, by="species") %>% 
  select(kingdom:comm_name, g_yr)

# Export
write.csv(spp_key, file=file.path(datadir, "ram4.41_generation_times_finfish_only.csv"), row.names=F)






