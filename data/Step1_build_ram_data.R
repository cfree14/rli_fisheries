#### NP VERSION ####

# Clear workspace
rm(list = ls())
# Setup
################################################################################

# Packages
library(tidyverse)
library(freeR)
library(readxl)

# Directories
# datadir <- "data/ramldb"

# Read RAM Legacy Database
# load("/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Model Fit Data/DBdata (model fits included).RData")
load("./Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Model Fit Data/DBdata (model fits included).RData")

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
  
  # Add MSY reference points
  left_join(select(bioparams_values_views, stockid, TBmsy, SSBmsy), by="stockid") %>%
  rename(bmsy_tb=TBmsy, bmsy_ssb=SSBmsy) %>% 
  left_join(select(bioparams_units_views, stockid, TBmsy, SSBmsy), by="stockid") %>%
  rename(bmsy_tb_units=TBmsy, bmsy_ssb_units=SSBmsy) %>% 
  left_join(select(bioparams_sources_views, stockid, TBmsy, SSBmsy), by="stockid") %>%
  rename(bmsy_tb_source=TBmsy, bmsy_ssb_source=SSBmsy) %>% 
  
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
         species, comm_name, phylum, class, order, family, bmsy_tb, bmsy_tb_units, bmsy_tb_source, bmsy_ssb, bmsy_ssb_units, bmsy_ssb_source) 
  # Fill missing taxonomic info
  stock_key[stock_key$species=="Lepidorhombus spp", c("phylum", "class", "order", "family")] <- c("Chordata",	"Actinopterygii",	"Pleuronectiformes", "Scophthalmidae")

# Check names
# freeR::check_names(stock_key$species)
# freeR::complete(stock_key)


# Build data
################################################################################

# Build data
ts_data <- timeseries_values_views %>% 
  # Get values
  select(stockid, stocklong, year, TC, TL, TB, SSB, TN, FdivFmsy, ERdivERmsy, TBdivTBmsy, SSBdivSSBmsy) %>% 
  rename(tc=TC, tl=TL, tb=TB, ssb=SSB, tn=TN, ffmsy=FdivFmsy, uumsy=ERdivERmsy, bbmsy_tb=TBdivTBmsy, bbmsy_ssb=SSBdivSSBmsy) %>% 
  # Add units
  left_join(select(timeseries_units_views, stockid, TC, TL, TB, SSB, TN), by="stockid") %>% 
  rename(tc_units=TC, tl_units=TL, tb_units=TB, ssb_units=SSB, tn_units=TN) %>% 
  # Add sources
  left_join(select(timeseries_sources_views, stockid, TB, SSB, TN, FdivFmsy, ERdivERmsy, TBdivTBmsy, SSBdivSSBmsy), by="stockid") %>% 
  rename(tb_source=TB, ssb_source=SSB, tn_source=TN, ffmsy_source=FdivFmsy, uumsy_source=ERdivERmsy, bbmsy_tb_source=TBdivTBmsy, bbmsy_ssb_source=SSBdivSSBmsy) %>% 
  # Rearrange columns
  select(stockid, stocklong, year, 
         ssb, ssb_units, ssb_source,
         tc, tc_units,
         tl, tl_units,
         tb, tb_units, tb_source,
         tn, tn_units, tn_source,
         ffmsy, ffmsy_source,
         uumsy, uumsy_source, 
         bbmsy_ssb, bbmsy_ssb_source,
         bbmsy_tb, bbmsy_tb_source) %>% 
  # Remove dataless years
  filter(!is.na(ssb) | !is.na(tb) | !is.na(tn) | !is.na(ffmsy) | !is.na(uumsy))
  

# Learn about data
##################################################################

# All biomass estimates come from assessments  
table(ts_data$ssb_source)
table(ts_data$tb_source)
table(ts_data$tn_source)

# Units
table(ts_data$ssb_units)
table(ts_data$tb_units)
table(ts_data$tn_units)

# Some MSY reference points come from production model fits
table(ts_data$ffmsy_source)
table(ts_data$uumsy_source)


# Time series stats
##################################################################

# Time series stats
ts_stats <- ts_data %>% 
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

# Data selection process:
# 1. No salmon
# 2. No invertebrates
# 3. SSB > TB > TN (but TB used if it offers 5% more data)
    ##NP Only SSB > TB > TN
# 4. F/FMSY > U/UMSY (because F/FMSY always from assessment)
# 5. Select B/BMSY that matches biomass selection

# g <- ggplot(ts_stats, aes(x=uumsy_nyr, y=ffmsy_nyr, color=uumsy_source)) +
#   geom_point() +
#   geom_abline(slope=0.95, intercept=0) +
#   labs(x="Number of years with U/UMSY", y="Number of years with F/FMSY") + theme_bw()
# g


#Add species and common name ##NP
ts_stats$genus_species <- stock_key[match(ts_stats$stockid, stock_key$stockid),'species'] ##NP
ts_stats$genus <- sub('\\s.*','',stock_key[match(ts_stats$stockid, stock_key$stockid),'species']) ##NP
ts_stats$species <- sub('.*\\s','',stock_key[match(ts_stats$stockid, stock_key$stockid),'species']) ##NP
ts_stats$comm_name <- stock_key[match(ts_stats$stockid, stock_key$stockid),'comm_name'] ##NP

# xxx <- ts_stats[ts_stats$ssb_nyr < ts_stats$tb_nyr | ts_stats$ssb_nyr < ts_stats$tn_nyr,]
# xxx <- xxx[xxx$ssb_nyr != 0,]
# xxx$maxy <- ifelse((xxx$tb_nyr>xxx$tn_nyr),xxx$tb_nyr,xxx$tn_nyr)
# plot(xxx$ssb_nyr,col='red',pch=19, ylim=c(0,max(c(xxx$ssb_nyr,xxx$tb_nyr , xxx$tn_nyr))))
# points(xxx$tb_nyr,col='green')
# points(xxx$tn_nyr,col='blue')
# plot(sort(xxx$maxy-xxx$ssb_nyr),col='red',pch=19)

# see which fish we don't have the GL : ALL finfish have GL!
# ts_stats$phylum <- sub('\\s.*','',stock_key[match(ts_stats$stockid, stock_key$stockid),'phylum']) ##NP # select only chordata
# GL_df <- read.csv("C:/Postdoc_analyses/rli_fisheries/data/ramldb/ram4.41_generation_times_finfish_only.csv")
# GL_df <- GL_df %>%
#   filter(phylum=="Chordata") %>%
#   select(-phylum)
# GL_df_noNA <- GL_df[!is.na(GL_df$g_yr), ]
# file_list <- unique(ts_stats$genus_species[ts_stats$phylum=='Chordata'])
# file_list[!(file_list %in% GL_df_noNA$species)]

# Data requirements
nyr_b_req <- 10

# Build use key
ts_use <- ts_stats %>% 
  # 1. Remove salmon stocks
  left_join(select(stock_key, stockid, region, phylum), by="stockid") %>% 
  filter(!grepl("Salmon", region)) %>% 
  select(-region) %>% 
  # 2. Remove invertebrate stocks
  filter(phylum=="Chordata") %>% 
  select(-phylum) %>% 
  # 3. Remove stocks with insufficient biomass info
  filter(ssb_nyr >= nyr_b_req | tb_nyr >= nyr_b_req | tn_nyr >= nyr_b_req) %>% 
  # 4. Identify which biomass time series to use
  # mutate(b_use=ifelse(ssb_nyr>=0.95*tb_nyr & ssb_nyr>=0.95*tn_nyr, "SSB", 
  #                     ifelse(tb_nyr>=0.95*tn_nyr, "TB", "TN")),
  mutate(b_use=ifelse(ssb_nyr != 0, "SSB", ##NP
                      ifelse(tb_nyr>=tn_nyr, "TB", "TN")), ##NP
         b_units=ifelse(b_use=="SSB", ssb_units,
                        ifelse(b_use=="TB", tb_units, tn_units))) %>% 
  # 5. Identify which F/FMSY time series to use
  mutate(ffmsy_use=ifelse(ffmsy_nyr==0 & uumsy_nyr==0, "none",
                          ifelse(ffmsy_nyr>=0.95*uumsy_nyr, "F/FMSY", "U/UMSY")))


table(ts_use$b_use)

# Build final dataset
################################################################################

# Build final dataset
data <- ts_data %>% 
  # Only stocks in dataset 
  ungroup() %>% 
  filter(stockid %in% ts_use$stockid) %>% 
  # Figure out which BIOMASS and F/FMSY to use
  left_join(select(ts_use, stockid, b_use, ffmsy_use), by="stockid") %>% 
  mutate(biomass=ifelse(b_use=="SSB", ssb, 
                        ifelse(b_use=="TB", tb, tn)), 
         biomass_units=ifelse(b_use=="SSB", ssb_units, 
                              ifelse(b_use=="TB", tb_units, tn_units)),
         f=ifelse(ffmsy_use=="none", NA,
                  ifelse(ffmsy_use=="F/FMSY", ffmsy, uumsy)),
         f_source=ifelse(ffmsy_use=="none", "none",
                         ifelse(ffmsy_use=="F/FMSY", ffmsy_source, uumsy_source)),
         bbmsy=ifelse(b_use=="SSB", bbmsy_ssb, bbmsy_tb),
         bbmsy_source=ifelse(b_use=="SSB", bbmsy_ssb_source, bbmsy_tb_source),
         bbmsy_units=ifelse(b_use=="SSB", "SSB/SSBMSY", "TB/TBMSY")) %>% 
  # Reduce and rename columns
  select(stockid, year, biomass, b_use, biomass_units, bbmsy, bbmsy_units, bbmsy_source, f, ffmsy_use, f_source, tc, tc_units, tl, tl_units) %>% 
  rename(biomass_type=b_use, ffmsy=f, ffmsy_type=ffmsy_use, ffmsy_source=f_source) %>% 
  # Remove empty biomasses
  filter(!is.na(biomass))

# Recalculate summary statistics to add to stock key
stats <- data %>%
  group_by(stockid, biomass_type, biomass_units, ffmsy_type, ffmsy_source) %>% 
  summarize(biomass_nyr=sum(!is.na(biomass)),
            ffmsy_nyr=sum(!is.na(ffmsy)))
 
# Stocks
stocks <- stock_key %>% 
  filter(stockid %in% ts_use$stockid) %>% 
  left_join(stats, by="stockid")

# Add species and common name + GL ##NP
stocks$genus_species <- stock_key[match(stocks$stockid, stock_key$stockid),'species'] ##NP
stocks$species <- NULL
stocks$genus <- sub('\\s.*','',stock_key[match(stocks$stockid, stock_key$stockid),'species']) ##NP
stocks$species <- sub('.*\\s','',stock_key[match(stocks$stockid, stock_key$stockid),'species']) ##NP
stocks$comm_name <- stock_key[match(stocks$stockid, stock_key$stockid),'comm_name'] ##NP
GL_df <- read.csv("C:/Postdoc_analyses/rli_fisheries/data/ramldb/ram4.41_generation_times_finfish_only.csv") ## NP
stocks$GL <- GL_df[match(stocks$genus_species, GL_df$species),'g_yr'] ## NP
stocks$GL3 <- round(3*stocks$GL,0) ## NP



# Add FAO areas + Marine Ecoregions to stocks and sp_stocks ## NP
ID_areas <- read_excel("data/ramldb_v3.8_stock_boundary_centroids_areas_fixed.xlsx") ## NP
unmatchID_areas <- as_tibble(read.csv("C:/Postdoc_analyses/rli_fisheries/data/fao_areas_for_unmatched_assessids.csv")) ## NP
old_ID_areas <- read.csv("C:/Postdoc_analyses/rli_fisheries/data/ramldb_v3.8_spsst_pella_cobe_lme_LINKolddatabaseAssessidToStockid.csv") ## NP

# add stockid to ID_areas from the two source ## NP
ID_areas$stockid <- NA ## NP
ID_areas$stockid <- stocks$stockid[match(ID_areas$assessid, stocks$assessid)] ## NP
ID_areas$stockid <- old_ID_areas$stockid[match(ID_areas$assessid, old_ID_areas$assessid)] ## NP

# link fao areas ## NP
stocks <- bind_cols(stocks,ID_areas[match(stocks$assessid, ID_areas$assessid),'fao_area']) ## NP
stocks$fao_area[which(is.na(stocks$fao_area))] <- pull(ID_areas[match(stocks$stockid, ID_areas$stockid),'fao_area'])[which(is.na(stocks$fao_area))] ## NP
stocks$fao_area[which(is.na(stocks$fao_area))] <- pull(unmatchID_areas[match(stocks$assessid, unmatchID_areas$assessid),'fao_area'])[which(is.na(stocks$fao_area))] ## NP
# link LME ## NP
stocks <- bind_cols(stocks,ID_areas[match(stocks$assessid, ID_areas$assessid),'lme_name']) ## NP
stocks$lme_name[which(is.na(stocks$lme_name))] <- pull(ID_areas[match(stocks$stockid, ID_areas$stockid),'lme_name'])[which(is.na(stocks$lme_name))] ## NP


# Create list of stocks by species ##NP
temp_stocks <- stocks
temp_stocks$start_y <- apply(temp_stocks,1,function(x) min(data$year[data$stockid == x[1]])) # x[1] = temp_stocks$stockid
temp_stocks$end_y <- apply(temp_stocks,1,function(x) max(data$year[data$stockid == x[1]])) # x[1] = temp_stocks$stockid
sp_stock <- split(temp_stocks, temp_stocks$genus_species)
sp_stock <- lapply(sp_stock,as.list)

GL3_sp_stock <- unlist(lapply(sp_stock,function(x) (max(x$end_y)-min(x$start_y)+1-unique(x$GL3)>0)))
sp_stock <- sp_stock[GL3_sp_stock]
##NP


# Add ISSCAAP code (functional group) to stocks and sp_stocks ## NP
ISSCAAP_code <- as.data.frame(read_excel("ASFIS_sp_2019_NP.xlsx")) ## NP
stocks$ISSCAAP_code <- as.numeric(ISSCAAP_code[match(stocks$genus_species, ISSCAAP_code$Scientific_name),'ISSCAAP']) ##NP






# Export data 
# save(stocks, data, file=file.path(datadir, "ram4.41_for_analysis.Rdata"))
    # + select only species with span of aggregated time-series > GL3
    stocks <- stocks[stocks$genus_species %in% names(sp_stock),]
    data <- data[data$stockid %in% stocks$stockid,]
    # + select only species with span of aggregated time-series > GL3
save(stocks, data, sp_stock, file=file.path("ram4.41_for_analysis_NP.Rdata"))
sum(stocks$ffmsy_source!="none")
table(stocks$ffmsy_source)
table(stocks$biomass_units)


# What propotion of total catch?
################################################################################

c_stats <- data %>% 
  select(stockid, year, tc, tl, tc_units, tl_units) %>% 
  mutate(catch_mt=ifelse(!is.na(tc) & tc_units=="MT", tc, 
                         ifelse(!is.na(tl) & tl_units=="MT", tl, NA)),
         catch_type=ifelse(!is.na(tc) & tc_units=="MT", "TC", 
                         ifelse(!is.na(tl) & tl_units=="MT", "TL", "none"))) %>% 
  group_by(year) %>% 
  summarize(catch_mmt=sum(catch_mt, na.rm=T)/1e6,
            nstocks=sum(catch_type!="none"),
            pstocs=nstocks/nrow(stocks))

  # mutate(catch_mt=ifelse((tc>=tl | (!is.na(tc) & is.na(tl))) & tc_units=="MT", tc, 
  #                        ifelse((tl>=tc | (!is.na(tl) & is.na(tc))) & tl_units=="MT", tl, NA)),
  #        catch_type=ifelse((tc>=tl | (!is.na(tc) & is.na(tl))) & tc_units=="MT", "TC", 
  #                          ifelse((tl>=tc | (!is.na(tl) & is.na(tc))) & tl_units=="MT", "TL", "none")))



  