
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
  select(stockid, stocklong, assessid, method, country, region, area, species, comm_name) 

# Check names
# freeR::check_names(stock_key$species)
# freeR::complete(stock_key)


# Build data
################################################################################

# Build data
data <- timeseries_values_views %>% 
  # Get values
  select(stockid, stocklong, year, TB, SSB, TN, FdivFmsy, ERdivERmsy) %>% 
  rename(tb=TB, ssb=SSB, tn=TN, ffmsy=FdivFmsy, uumsy=ERdivERmsy) %>% 
  # Add units
  left_join(select(timeseries_units_views, stockid, TB, SSB, TN), by="stockid") %>% 
  rename(tb_units=TB, ssb_units=SSB, tn_units=TN) %>% 
  # Add sources
  left_join(select(timeseries_sources_views, stockid, TB, SSB, TN, FdivFmsy, ERdivERmsy), by="stockid") %>% 
  rename(tb_source=TB, ssb_source=SSB, tn_source=TN, ffmsy_source=FdivFmsy, uumsy_source=ERdivERmsy) %>% 
  # Rearrange columns
  select(stockid, stocklong, year, 
         ssb, ssb_units, ssb_source,
         tb, tb_units, tb_source,
         tn, tn_units, tn_source,
         ffmsy, ffmsy_source,
         uumsy, uumsy_source) %>% 
  # Remove dataless years
  filter(!is.na(ssb) | !is.na(tb) | !is.na(tn) | !is.na(ffmsy) | !is.na(uumsy))
  

# Learn about data
##################################################################

# All biomass estimates come from assessments  
table(data$ssb_source)
table(data$tb_source)
table(data$tn_source)

# Units
table(data$ssb_units)
table(data$tb_units)
table(data$tn_units)

# Some MSY reference points come from production model fits
table(data$ffmsy_source)
table(data$uumsy_source)


# Time series stats
##################################################################

# Time series stats
ts_stats <- data %>% 
  group_by(stockid) %>% 
  summarize(ssb_nyr=sum(!is.na(ssb)),
            ssb_units=unique(ssb_units),
            tb_nyr=sum(!is.na(tb)),
            tb_units=unique(tb_units),
            tn_nyr=sum(!is.na(tn)),
            tn_units=unique(tn_units),
            ffmsy_nyr=sum(!is.na(ffmsy)),
            ffmsy_source=unique(ffmsy_source),
            uumsy_nyr=sum(!is.na(uumsy)),
            uumsy_source=unique(uumsy_source))

plot(ffmsy_nyr ~ uumsy_nyr, ts_stats)


# Data selection process:
# 1. No salmon
# 1. SSB > TB > TN (but TB used if it offers 5% more data)
# 2. F/FMSY > U/UMSY (because F/FMSY always from assessment)

g <- ggplot(ts_stats, aes(x=uumsy_nyr, y=ffmsy_nyr, color=uumsy_source)) +
  geom_point() + 
  geom_abline(slope=0.95, intercept=0) +
  labs(x="Number of years with U/UMSY", y="Number of years with F/FMSY") + theme_bw()
g


# Data requirements
nyr_b_req <- 25

# Build use key
ts_use <- ts_stats %>% 
  # 1. Remove salmon stocks
  left_join(select(stock_key, stockid, region), by="stockid") %>% 
  filter(!grepl("Salmon", region)) %>% 
  select(-region) %>% 
  # 2. Remove stocks with insufficient biomass info
  filter(ssb_nyr >= nyr_b_req | tb_nyr >= nyr_b_req | tn_nyr >= nyr_b_req) %>% 
  # 3. Identify which biomass time series to use
  mutate(b_use=ifelse(ssb_nyr>=0.95*tb_nyr, "SSB", 
                      ifelse(tb_nyr>=0.95*tn_nyr, "TB", "TN"))) %>% 
  # 4. Identify which F/FMSY time series to use
  mutate(ffmsy_use=ifelse(ffmsy_nyr==0 & uumsy_nyr==0, "none",
                          ifelse(ffmsy_nyr>=0.95*uumsy_nyr, "F/FMSY", "U/UMSY")))

table(ts_use$b_use)



  