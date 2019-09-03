
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/ramldb"

# Read RAM Legacy Database
load("/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Model Fit Data/DBdata (model fits included).RData")


# Build stock key
################################################################################

# Format assessment info
assess_info <- assessment %>% 
  # Reduce columns
  select(assessid, stockid, assessmethod, assessyear, mostrecent) %>% 
  # Identify most recent, properly
  mutate(year2=sapply(assessyear, function(x) as.numeric(unlist(strsplit(x, "-"))[2])),
         mostrecent=as.numeric(mostrecent)) %>% 
  group_by(stockid) %>% 
  filter(year2==max(year2))

# Check for and fix duplicates
dup_ids <- assess_info[duplicated(assess_info$stockid),]$stockid
dup_data <- filter(assess_info, stockid %in% dup_ids)
assess_info <- assess_info %>% 
  filter(!(stockid=="ANCHMEDGSA16" & mostrecent==0))

# Build stock key
stock_key <- stock %>% 
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Add assessid
  left_join(select(assess_info, stockid, assessid, assessmethod), by="stockid") %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname, method=assessmethod) %>% 
  # Add taxonomy
  left_join(select(taxonomy, phylum, classname, ordername, family, scientificname), by=c("species"="scientificname")) %>% 
  rename(class=classname, order=ordername) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species),
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
                                    "Sardinops melanostictus"="Sardinops sagax"))) %>% 
  # Rearrange columns
  select(stockid, stocklong, assessid, method, country, region, area, 
         species, comm_name, phylum, class, order, family) 

# Fill missing taxonomic info
stock_key[stock_key$species=="Lepidorhombus spp", c("phylum", "class", "order", "family")] <- c("Chordata",	"Actinopterygii",	"Pleuronectiformes", "Scophthalmidae")

# Check names
# freeR::check_names(stock_key$species)
# freeR::complete(stock_key)

# Build data
################################################################################

# Shark/ray key
stocks <- stock_key %>% 
  filter(order %in% c("Squaliformes", "Lamniformes", "Carcharhiniformes"))

# Time series data
data <- timeseries_values_views %>% 
  # Filter to sharks
  filter(stockid %in% stocks$stockid) %>% 
  # Select and rename columns
  select(stockid, year, TB, SSB, TN, R, TC, TL, F, ER,
         TBdivTBmsy, SSBdivSSBmsy, FdivFmsy, ERdivERmsy) %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(fmort=f, bbmsy_tb=tbdivtbmsy, bbmsy_ssb=ssbdivssbmsy, ffmsy=fdivfmsy, uumsy=erdivermsy) %>% 
  # Add units
  left_join(timeseries_units_views %>% 
              select(stockid, TB, SSB, TN, R, TC, TL) %>% 
              rename(tb_units=TB, ssb_units=SSB, tn_units=TN, r_units=R, tc_units=TC, tl_units=TL), by="stockid") %>% 
  # Rearrange columns
  select(stockid, year, tb, tb_units, ssb, ssb_units, tn, tn_units, r, r_units,
         tc, tc_units, tl, tl_units, fmort, er, everything())
  
# Export data
save(stocks, data, file=file.path(datadir, "ram_shark_data.Rdata"))


