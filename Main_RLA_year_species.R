rm(list=ls())
cat("\014") # clear console# set working directory


# required packages
library("crayon") ##NP
library(ggplot2)
source("C:/Postdoc_analyses/RLA_NWAtlantic/autojags_NP.R")
library(coda)

A1 <- TRUE #Criterion for population A1 = count, A2 = relative abundance


round_realsum <- function(x, digits = 0) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

val_cont <- c(  seq(-100,ifelse(A1,-90,-80), length = ifelse(A1,101,201))
           , seq(ifelse(A1,-90,-80), ifelse(A1,-70,-50), length = ifelse(A1,200,200))[-1]
           , seq(ifelse(A1,-70,-50), ifelse(A1,-50,-30), length = ifelse(A1,200,200))[-1]
           , seq(ifelse(A1,-50,-30), ifelse(A1,-40,-20), length = ifelse(A1,100,100))[-1]
           # , 0
)

unscaled_val_cont <- c(  rev(seq(3,4, length = ifelse(A1,101,201)))
                    , rev(seq(2,3, length = ifelse(A1,200,200))[-1])
                    , rev(seq(1,2, length = ifelse(A1,200,200))[-1])
                    , rev(seq(0,1, length = ifelse(A1,100,100))[-1])
                    # , 0
)

unscaled_val_cont_WEIGHTED <- c(  rev(seq(3*0.05,4*0.5, length = ifelse(A1,101,201)))
                                , rev(seq(2*0.005,3*0.05, length = ifelse(A1,200,200))[-1])
                                , rev(seq(1*0.0005,2*0.005, length = ifelse(A1,200,200))[-1])
                                , rev(seq(0,1*0.0005, length = ifelse(A1,100,100))[-1])
                                # , 0
)

### rescale one value
# rescale_RLI <- function(value=value,weighted = weighted){
#   if(value>ifelse(A1,-40,-20)){0} else {
#   xxx <- if(weighted){unscaled_val_cont_WEIGHTED}else{unscaled_val_cont}
#   xxx[which.min(abs(val_cont - value))]
#   }
# }

### rescale vector
rescale_RLI <- function(value=value, weighted = weighted){
  xxx <- if(weighted){unscaled_val_cont_WEIGHTED}else{unscaled_val_cont}
  sapply(value,function(element) if(element>ifelse(A1,-40,-20)){0}else{xxx[which.min(abs(val_cont - element))]})
}

       

load("ram4.41_for_analysis_NP.Rdata")
GL_df <- read.csv("C:/Postdoc_analyses/rli_fisheries/data/ramldb/ram4.41_generation_times_finfish_only.csv")


file_list <- sort(unique(stocks$genus_species)) # add column fish
i<- 1

# df_change_allspecies <- data.frame('year'= year_dat[(nber_of_possible_RLA-1):(nber_of_possible_RLA+GL3)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
# which(dat$Year %in% data$year[data$stockid == colnames(dat)[stock]])

for(i in 1:length(file_list)){
  cat(yellow(paste0('\nSpecies : ', file_list[i])))
  load(paste0("./Outputs/", file_list[i], '/', file_list[i], "_Ntot.rdata"))
  year_dat <- sort(unique(data$year[which(data$stockid %in% stocks$stockid[stocks$genus_species == file_list[i]])]))
  # year_dat <- c(year_dat,(tail(year_dat,1)+1))
  GL <- stocks$GL[stocks$genus_species == file_list[i]][1]
  GL3 <- round(3*GL,0)
  nber_of_possible_RLA <- (length(year_dat)-GL3)
  df_change <- data.frame('year'= year_dat[(GL3+1):length(year_dat)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
  
  for(RLA_nber in 1:nber_of_possible_RLA){
    year_for_RLA <- year_dat[(RLA_nber+1):(RLA_nber+GL3)]
    mp.assess <- c((RLA_nber+1),(RLA_nber+GL3))
    change <- (apply(out1[,(mp.assess[2]-1):(mp.assess[2]+1)],1,median)/apply(out1[,(mp.assess[1]-1):(mp.assess[1]+1)],1,median)-1)*100
    
    CR = sum(ifelse(change<= ifelse(A1,-90,-80),1,0))/length(change)*100
    EN = sum(ifelse(change> ifelse(A1,-90,-80) & change<= ifelse(A1,-70,-50),1,0))/length(change)*100
    VU = sum(ifelse(change> ifelse(A1,-70,-50) & change<= ifelse(A1,-50,-30),1,0))/length(change)*100
    NT = sum(ifelse(change> ifelse(A1,-50,-30) & change<= ifelse(A1,-40,-20),1,0))/length(change)*100
    LC = sum(ifelse(change> ifelse(A1,-40,-20),1,0))/length(change)*100
    old_status <- c(CR,EN,VU,NT,LC)
    
    if(sum(round(old_status,0))!=100){
      CR = round_realsum(old_status,0)[1]
      EN = round_realsum(old_status,0)[2]
      VU = round_realsum(old_status,0)[3]
      NT = round_realsum(old_status,0)[4]
      LC = round_realsum(old_status,0)[5]
    } else{
      CR=round(CR,0)
      EN=round(EN,0)
      VU=round(VU,0)
      NT=round(NT,0)
      LC=round(LC,0)
    }
    df_change[RLA_nber,'CR'] <- CR
    df_change[RLA_nber,'EN'] <- EN
    df_change[RLA_nber,'VU'] <- VU
    df_change[RLA_nber,'NT'] <- NT
    df_change[RLA_nber,'LC'] <- LC
  }
  
  long_df_change <- data.frame('Year'= rep(df_change$year,each=5)
                               # , 'Status'=rep(c('CR','EN','VU','NT','LC'),5)
                               , 'Status'= factor(rep(c('CR','EN','VU','NT','LC'),length(df_change$year)), levels = c('CR','EN','VU','NT','LC'))
                               , 'Percent' = c(t(df_change[,-1]))
                               )
  
  ####
  ###### NEED TO COMBINE WITH PLOT REAL DATA
  ####
  
  ggplot(long_df_change, aes(x = Year, y = Percent, fill=Status)) + 
    # geom_bar(stat = 'identity') +
    geom_area() +
    scale_fill_manual('Status', values = c('CR' = '#D81E05','EN' = '#FC7F3F','VU' = '#F9E814','NT' = '#CCE226','LC' = '#60C659'))
  ggsave(paste0("./Outputs/",file_list[i],"/",file_list[i], "_RLA_plot.png"))
  
  write.table(df_change, paste0("./Outputs/",file_list[i],"/df_change",file_list[i], ".txt"), row.names = FALSE)
  
}


### calculate RLI ###
# df_change <- data.frame('year'= year_dat[(GL3+1):(nber_of_possible_RLA+GL3)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
RL_status <- array(as.numeric(rep(NA,length(year_dat[(GL3+1):(nber_of_possible_RLA+GL3)])*30000)), dim= c(length(year_dat[(GL3+1):(nber_of_possible_RLA+GL3)]),30000))

for(RLA_nber in 1:nber_of_possible_RLA){
  year_for_RLA <- year_dat[(RLA_nber+1):(RLA_nber+GL3)]
  mp.assess <- c((RLA_nber+1),(RLA_nber+GL3))
  change <- (apply(out1[,(mp.assess[2]-1):(mp.assess[2]+1)],1,median)/apply(out1[,(mp.assess[1]-1):(mp.assess[1]+1)],1,median)-1)*100
  RL_status[RLA_nber,] <- 1-(rescale_RLI(change, weighted = F)/5) # weighted = T --> weight of categories following Butchart et al. 2004       
}

# hist(rescale_RLI(change, weighted = F))
# RLI <- 1 - (apply(RL_status,1,sum))/(30000*5)

RLI_df <- data.frame(year=year_dat[(GL3+1):(nber_of_possible_RLA+GL3)]
                            , RLI_mean = apply(RL_status,1,mean)
                            , RLI_HPDlow = apply(RL_status,1,function(x) HPDinterval(as.mcmc(x), prob = 0.95)[1])
                            , RLI_HPDhigh = apply(RL_status,1,function(x) HPDinterval(as.mcmc(x), prob = 0.95)[2])
)

plot(RLI_mean~year,data=RLI_df, las=1, type = 'l', lty = 'dashed', ylim = c(0,1.1), cex.axis = 1.3, cex.lab = 1.8, xlim = c(1970,2020), ylab = 'Red List Index', xlab = 'Year', yaxs="i", xaxs="i", bty="n")
polygon(c(rev(RLI_df$year), RLI_df$year), c(rev(RLI_df$RLI_HPDlow), RLI_df$RLI_HPDhigh)
        , col = 'grey', border = NA)
lines(RLI_mean~year,data=RLI_df, type = 'l', lty = 'dashed')




# RLI <- array(as.numeric(rep(NA,year_dat*30000)), dim= c(year_dat,30000))
# 
# for(t in 1:nrow(RLI)){
#   xxx <- NA
#   xxx <- dt_subocean_OCEAN[t,1,]
#   OCEAN_indices_for_calc <- which(!is.na(xxx))
#   if(length(xxx[!is.na(xxx)])==0) {
#     dt_global_temp[t,] <- NA
#     # LPI_subocean_global[t,] <- NA
#   } else {
#     if(length(xxx[!is.na(xxx)])==1){
#       dt_global_temp[t,] <- dt_subocean_OCEAN[t,, OCEAN_indices_for_calc]
#     } else{
#       dt_global_temp[t,] <- apply(dt_subocean_OCEAN[t,, OCEAN_indices_for_calc],1,sum,na.rm=TRUE)/length(xxx[!is.na(xxx)])
#     }
#     if(is.na(dt_global_temp[t,1]) == FALSE & is.na(LPI_subocean_global[t-1,1]) == FALSE) {
#       LPI_subocean_global[t,] <- LPI_subocean_global[t-1,] * 10^(dt_global_temp[t,])
#     }
#     if(is.na(dt_global_temp[t,1]) == FALSE & is.na(LPI_subocean_global[t-1,1]) == TRUE) {
#       LPI_subocean_global[t,] <- tail(na.omit(LPI_subocean_global[c(1:t),]), n=1) * 10^(dt_global_temp[t,])
#     }
#   } 
# }  
