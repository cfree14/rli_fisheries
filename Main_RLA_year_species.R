rm(list=ls())
cat("\014") # clear console# set working directory

round_realsum <- function(x, digits = 0) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

# required packages
library("crayon") ##NP
source("C:/Postdoc_analyses/RLA_NWAtlantic/autojags_NP.R")

load("./data/ramldb/ram4.41_for_analysis.Rdata")
GL_df <- read.csv("C:/Postdoc_analyses/rli_fisheries/data/ramldb/ram4.41_generation_times_finfish_only.csv")
A1 <- FALSE #Criterion for population A1 = count, A2 = relative abundance

file_list <- unique(stocks$genus_species)
i<- 1

# df_change_allspecies <- data.frame('year'= year_dat[(nber_of_possible_RLA-1):(nber_of_possible_RLA+GL3)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
# which(dat$Year %in% data$year[data$stockid == colnames(dat)[stock]])

for(i in 1:length(file_list)){
  cat(yellow(paste0('\nSpecies : ', file_list[i])))
  load(paste0("./Outputs/", file_list[i], '/', file_list[i], "_Ntot.rdata"))
  year_dat <- sort(unique(data$year[which(data$stockid %in% stocks$stockid[stocks$genus_species == file_list[i]])]))
  # year_dat <- c(year_dat,(tail(year_dat,1)+1))
  GL <- if(file_list[i] %in% GL_df$species & !is.na(GL_df$g_yr[GL_df$species == file_list[i]])){GL_df$g_yr[GL_df$species == file_list[i]]}else{NA}
  GL3 <- round(3*GL,0)
  nber_of_possible_RLA <- (length(year_dat)-GL3)
  df_change <- data.frame('year'= year_dat[(nber_of_possible_RLA-1):(nber_of_possible_RLA+GL3)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
  
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
  ggplot(long_df_change, aes(x = Year, y = Percent, fill=Status)) + 
    geom_bar(stat = 'identity') +
    scale_fill_manual('Status', values = c('CR' = '#D81E05','EN' = '#FC7F3F','VU' = '#F9E814','NT' = '#CCE226','LC' = '#60C659'))
  ggsave(paste0("./Outputs/",file_list[i],"/",file_list[i], "_RLA_plot.png"))
  
  write.table(df_change, paste0("./Outputs/",file_list[i],"/df_change",file_list[i], ".txt"), row.names = FALSE)
  
}





### calculate RLI ###
df_change <- data.frame('year'= year_dat[(nber_of_possible_RLA-1):(nber_of_possible_RLA+GL3)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
RL_status <- array(as.numeric(rep(NA,length(year_dat[(nber_of_possible_RLA-1):(nber_of_possible_RLA+GL3)])*30000)), dim= c(length(year_dat[(nber_of_possible_RLA-1):(nber_of_possible_RLA+GL3)]),30000))

for(RLA_nber in 1:nber_of_possible_RLA){
  year_for_RLA <- year_dat[(RLA_nber+1):(RLA_nber+GL3)]
  mp.assess <- c((RLA_nber+1),(RLA_nber+GL3))
  change <- (apply(out1[,(mp.assess[2]-1):(mp.assess[2]+1)],1,median)/apply(out1[,(mp.assess[1]-1):(mp.assess[1]+1)],1,median)-1)*100
  RL_status[RLA_nber,] <- sapply(change,function(x)
    if(x <= ifelse(A1,-90,-80)){4}else{
      if(x > ifelse(A1,-90,-80) & x <= ifelse(A1,-70,-50)){3}else{
        if(x > ifelse(A1,-70,-50) & x <= ifelse(A1,-50,-30)){2}else{
          if(x > ifelse(A1,-50,-30) & x <= ifelse(A1,-40,-20)){1}else{
            if(x > ifelse(A1,-40,-20)){0}
          }
        }
      }
    }
  )               
}


                                                 
                                                 
RLI <- 1 - (apply(RL_status,1,sum))/(30000*5)

RLI_df <- data.frame(year=year_dat[(nber_of_possible_RLA-1):(nber_of_possible_RLA+GL3)]
                            , RLI_mean = apply(RLI,1,mean)
                            , RLI_HPDlow = apply(RLI,1,function(x) HPDinterval(as.mcmc(x), prob = 0.95)[1])
                            , RLI_HPDhigh = apply(RLI,1,function(x) HPDinterval(as.mcmc(x), prob = 0.95)[2])
)

plot(RLI_mean~year,data=LPI_mean_plot, las=1, type = 'l', lty = 'dashed', ylim = c(0,1.4), cex.axis = 1.3, cex.lab = 1.8, xlim = c(1970,2020), ylab = 'Living Planet Index', xlab = 'Year', yaxs="i", xaxs="i", bty="n")
polygon(c(rev(LPI_mean_plot$year), LPI_mean_plot$year), c(rev(CIlow_real), CIhigh_real)
        , col = adjustcolor(col_real,alpha.f=alpha_polygon_real), border = NA)





RLI <- array(as.numeric(rep(NA,year_dat*30000)), dim= c(year_dat,30000))

for(t in 1:nrow(RLI)){
  xxx <- NA
  xxx <- dt_subocean_OCEAN[t,1,]
  OCEAN_indices_for_calc <- which(!is.na(xxx))
  if(length(xxx[!is.na(xxx)])==0) {
    dt_global_temp[t,] <- NA
    # LPI_subocean_global[t,] <- NA
  } else {
    if(length(xxx[!is.na(xxx)])==1){
      dt_global_temp[t,] <- dt_subocean_OCEAN[t,, OCEAN_indices_for_calc]
    } else{
      dt_global_temp[t,] <- apply(dt_subocean_OCEAN[t,, OCEAN_indices_for_calc],1,sum,na.rm=TRUE)/length(xxx[!is.na(xxx)])
    }
    if(is.na(dt_global_temp[t,1]) == FALSE & is.na(LPI_subocean_global[t-1,1]) == FALSE) {
      LPI_subocean_global[t,] <- LPI_subocean_global[t-1,] * 10^(dt_global_temp[t,])
    }
    if(is.na(dt_global_temp[t,1]) == FALSE & is.na(LPI_subocean_global[t-1,1]) == TRUE) {
      LPI_subocean_global[t,] <- tail(na.omit(LPI_subocean_global[c(1:t),]), n=1) * 10^(dt_global_temp[t,])
    }
  } 
}  
