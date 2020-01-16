rm(list=ls())
cat("\014") # clear console# set working directory


library("crayon")
library(coda)

######
# A1 <- TRUE #Criterion for population A1 = count, A2 = relative abundance
# 
# val_cont <- c(  seq(-100,ifelse(A1,-90,-80), length = ifelse(A1,101,201))
#               , seq(ifelse(A1,-90,-80), ifelse(A1,-70,-50), length = ifelse(A1,200,200))[-1]
#               , seq(ifelse(A1,-70,-50), ifelse(A1,-50,-30), length = ifelse(A1,200,200))[-1]
#               , seq(ifelse(A1,-50,-30), ifelse(A1,-40,-20), length = ifelse(A1,100,100))[-1]
#               # , 0
# )
# 
# unscaled_val_cont <- c(  rev(seq(3,4, length = ifelse(A1,101,201)))
#                        , rev(seq(2,3, length = ifelse(A1,200,200))[-1])
#                        , rev(seq(1,2, length = ifelse(A1,200,200))[-1])
#                        , rev(seq(0,1, length = ifelse(A1,100,100))[-1])
#                        # , 0
# )
# 
# unscaled_val_cont_WEIGHTED <- c(  rev(seq(3*0.05,4*0.5, length = ifelse(A1,101,201)))
#                                 , rev(seq(2*0.005,3*0.05, length = ifelse(A1,200,200))[-1])
#                                 , rev(seq(1*0.0005,2*0.005, length = ifelse(A1,200,200))[-1])
#                                 , rev(seq(0,1*0.0005, length = ifelse(A1,100,100))[-1])
#                                 # , 0
# )
# 
# ### rescale one value
# # rescale_RLI <- function(value=value,weighted = weighted){
# #   if(value>ifelse(A1,-40,-20)){0} else {
# #   xxx <- if(weighted){unscaled_val_cont_WEIGHTED}else{unscaled_val_cont}
# #   xxx[which.min(abs(val_cont - value))]
# #   }
# # }
# 
# ### rescale vector
# rescale_RLI <- function(value=value, weighted = weighted){
#   xxx <- if(weighted){unscaled_val_cont_WEIGHTED}else{unscaled_val_cont}
#   sapply(value,function(element) if(element>ifelse(A1,-40,-20)){0}else{xxx[which.min(abs(val_cont - element))]})
# }
######

rescale_RLI <- function(value=value, A1=A1, weighted = weighted){
  xxx<-value
  if(A1){lim <- c(-100,-90,-70,-50,-40)}else{lim <- c(-100,-80,-50,-30,-20)}
  if(weighted){newlim=c(4*.5,3*.05,2*.005,1*.0005,0)}else{newlim=c(4,3,2,1,0)}
  value[xxx <= lim[2]] <- (((value[xxx <= lim[2]] - lim[2]) * (newlim[1] - newlim[2])) / (lim[1] - lim[2])) + newlim[2]
  # (((OldValue                            - OldMin) * (NewMax    - NewMin))    / (OldMax - OldMin)) + NewMin
  value[xxx > lim[2] & xxx <= lim[3]] <- (((value[xxx > lim[2] & xxx <= lim[3]] - lim[3]) * (newlim[2] - newlim[3])) / (lim[2] - lim[3])) + newlim[3]
  value[xxx > lim[3] & xxx <= lim[4]] <- (((value[xxx > lim[3] & xxx <= lim[4]] - lim[4]) * (newlim[3] - newlim[4])) / (lim[3] - lim[4])) + newlim[4]
  value[xxx > lim[4] & xxx <= lim[5]] <- (((value[xxx > lim[4] & xxx <= lim[5]] - lim[5]) * (newlim[4] - newlim[5])) / (lim[4] - lim[5])) + newlim[5]
  value[xxx > lim[5]] <- 0
  value
  # ttt=data.frame(value)
} 

load("ram4.41_for_analysis_NP.Rdata")
GL_df <- read.csv("C:/Postdoc_analyses/rli_fisheries/data/ramldb/ram4.41_generation_times_finfish_only.csv")

start_RLI <- 1970
end_RLI <- 2012 # 2012 cut year?
file_list <- sort(unique(stocks$genus_species)) # add column fish
# i<- 20

Launch_RLI <- function(sp_start_i=sp_start_i, sp_end_i=sp_end_i, year_start=year_start, year_end=year_end, RLI_forecast=RLI_forecast, RLI_backcast=RLI_backcast, plotting=plotting, weighted=weighted, A1=A1){
  RL_status <- NULL
  # for(i in 1:length(file_list)){
  for(i in sp_start_i:sp_end_i){
    cat(yellow(paste0('\nSpecies ', i, '/', length(file_list), ' : ', file_list[i])))
    
    RL_status_tmp <- NULL
    RLI_df <- NULL
    
    load(paste0("./Outputs/",file_list[i],"/",file_list[i], "_df_change.RData"))
    year_dat <- sort(unique(data$year[which(data$stockid %in% stocks$stockid[stocks$genus_species == file_list[i]])]))
    year_RLA <- as.numeric(row.names(df_change))
    if(any((year_RLA[1]>year_end | tail(year_RLA,1) < year_start))){next}
      else{year_RLA <- year_RLA[year_RLA>=year_start & year_RLA<=year_end]}
    
    ### calculate RLI ###
    RL_status_tmp <- array(as.numeric(rep(NA,length(year_start:year_end)*30000)), dim= c(length(year_start:year_end),30000))
    rownames(RL_status_tmp) <- year_start:year_end
    RL_status_tmp[as.character(year_RLA),] <- t(apply(df_change[as.character(year_RLA),,drop=F],1, function(x) 1-(rescale_RLI(value=x, weighted = weighted, A1=A1)/5))) # weighted = T --> weight of categories following Butchart et al. 2004       
    # if(year_RLA[1]>year_start){RL_status_tmp[as.character(year_start:(year_RLA[1]-1)),] <- matrix(rep(RL_status_tmp[as.character(year_RLA[1]),],length(year_start:(year_RLA[1]-1))),nrow=length(year_start:(year_RLA[1]-1)),byrow=T)}
    if(RLI_forecast == 'OK' & tail(year_RLA,1)<year_end){RL_status_tmp[as.character((tail(year_RLA,1)+1):year_end),] <- RL_status_tmp[as.character(tail(year_RLA,1)),]}
    if(RLI_backcast == 'OK' & year_RLA[1]>year_start){RL_status_tmp[as.character(year_start:(year_RLA[1]-1)),] <- matrix(rep(RL_status_tmp[as.character(year_RLA[1]),],length(year_start:(year_RLA[1]-1))),nrow=length(year_start:(year_RLA[1]-1)),byrow=T)}
    
    if(plotting == 'OK'){
      RLI_df_tmp <- data.frame('year' = year_start:year_end
                               # , 'colors' = c(rep('black',year_RLA[1]-year_start),rep('red',length(year_RLA)),rep('black',year_end-tail(year_RLA,1)))
                               , 'RLI_mean' = apply(RL_status_tmp,1,mean,na.rm=TRUE)
                               , 'RLI_HPDlow' = apply(RL_status_tmp,1,function(x) if(!is.na(x[1])){HPDinterval(as.mcmc(x), prob = 0.95)[1]}else{NA})
                               , 'RLI_HPDhigh' = apply(RL_status_tmp,1,function(x) if(!is.na(x[1])){HPDinterval(as.mcmc(x), prob = 0.95)[2]}else{NA})
      )
      
      png(file = paste0("./Outputs/",file_list[i],"/RLI_", file_list[i], ".png"), width = 5, height = 4,
          res = 200, units = "in")
      plot(NULL, las=1, type = 'l', lty = 'dashed', ylim = c(0,1.1), cex.axis = 1.2, cex.lab = 1.5, xlim = c(year_start,year_end), ylab = 'Red List Index', xlab = 'Year', yaxs="i", xaxs="i", bty="n")
      polygon(c(rev(RLI_df_tmp$year[!is.na(RLI_df_tmp$RLI_HPDlow)]), RLI_df_tmp$year[!is.na(RLI_df_tmp$RLI_HPDlow)]), c(rev(RLI_df_tmp$RLI_HPDlow[!is.na(RLI_df_tmp$RLI_HPDlow)]), RLI_df_tmp$RLI_HPDhigh[!is.na(RLI_df_tmp$RLI_HPDlow)])
              , col = 'grey', border = NA)
      lines(RLI_mean~year,data=RLI_df_tmp, type = 'l', lwd=1.2)
      lines(RLI_mean~year,data=RLI_df_tmp[RLI_df_tmp$year %in% year_RLA, ], type = 'o', pch=20, col='red', lwd=1.2)
      dev.off()
    }
    RL_status <- cbind(RL_status, RL_status_tmp)
  }
  assign("RL_status", RL_status, envir = .GlobalEnv)
}

Launch_RLI(sp_start_i=1,
           sp_end_i=30,
           year_start=start_RLI,
           year_end=end_RLI,
           RLI_forecast='OK', #OK or something else
           RLI_backcast='OK', #OK or something else
           plotting='no', #OK or something else
           weighted=F,
           A1=T)
 
  
  

##### GLOBAL RED LIST INDEX
# See with mean or median
RLI_df <- data.frame('year' = start_RLI:end_RLI
                     # , 'colors' = c(rep('black',year_RLA[1]-1950),rep('red',length(year_RLA)),rep('black',2020-tail(year_RLA,1)))
                     , 'RLI_mean' = apply(RL_status,1,mean, na.rm=T)
                     , 'RLI_median' = apply(RL_status,1,median, na.rm=T)
                     , 'RLI_HPDlow' = apply(RL_status,1,function(x) if(!all(is.na(x))){HPDinterval(as.mcmc(na.omit(x)), prob = 0.95)[1]}else{NA})
                     , 'RLI_HPDhigh' = apply(RL_status,1,function(x) if(!all(is.na(x))){HPDinterval(as.mcmc(na.omit(x)), prob = 0.95)[2]}else{NA})
)
row.names(RLI_df)<-NULL

png(file = paste0("./RLI_",162, "species_"
                  , start_RLI, "_", end_RLI
                  # , "_backforecast"
                  , ".png"), width = 5, height = 4, res = 200, units = "in")
plot(NULL, las=1, type = 'l', lty = 'dashed', ylim = c(0,1.1), cex.axis = 1.2, cex.lab = 1.5, xlim = c(start_RLI,end_RLI), ylab = 'Red List Index', xlab = 'Year', yaxs="i", xaxs="i", bty="n")
polygon(c(rev(RLI_df$year), RLI_df$year), c(rev(RLI_df$RLI_HPDlow), RLI_df$RLI_HPDhigh)
        , col = 'grey', border = NA)
lines(RLI_mean~year,data=RLI_df, type = 'l', lwd=1.2)
# lines(RLI_mean~year,data=RLI_df[RLI_df$year %in% year_RLA, ], type = 'l', col='red', lwd=1.2)
dev.off()


##### Disaggregated RED LIST INDEX
# See with mean or median
RLI_FAO <- data.frame('year' = start_RLI:end_RLI
                     # , 'colors' = c(rep('black',year_RLA[1]-1950),rep('red',length(year_RLA)),rep('black',2020-tail(year_RLA,1)))
                     , 'RLI_mean' = apply(RL_status,1,mean, na.rm=T)
                     , 'RLI_median' = apply(RL_status,1,median, na.rm=T)
                     , 'RLI_HPDlow' = apply(RL_status,1,function(x) if(!all(is.na(x))){HPDinterval(as.mcmc(na.omit(x)), prob = 0.95)[1]}else{NA})
                     , 'RLI_HPDhigh' = apply(RL_status,1,function(x) if(!all(is.na(x))){HPDinterval(as.mcmc(na.omit(x)), prob = 0.95)[2]}else{NA})
)
row.names(RLI_df)<-NULL

png(file = paste0("./RLI_",162, "species_"
                  , start_RLI, "_", end_RLI
                  # , "_backforecast"
                  , ".png"), width = 5, height = 4, res = 200, units = "in")
plot(NULL, las=1, type = 'l', lty = 'dashed', ylim = c(0,1.1), cex.axis = 1.2, cex.lab = 1.5, xlim = c(start_RLI,end_RLI), ylab = 'Red List Index', xlab = 'Year', yaxs="i", xaxs="i", bty="n")
polygon(c(rev(RLI_df$year), RLI_df$year), c(rev(RLI_df$RLI_HPDlow), RLI_df$RLI_HPDhigh)
        , col = 'grey', border = NA)
lines(RLI_mean~year,data=RLI_df, type = 'l', lwd=1.2)
# lines(RLI_mean~year,data=RLI_df[RLI_df$year %in% year_RLA, ], type = 'l', col='red', lwd=1.2)
dev.off()





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
