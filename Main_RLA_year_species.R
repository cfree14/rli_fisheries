rm(list=ls())
cat("\014") # clear console# set working directory


# required packages
library("crayon") ##NP
library(ggplot2)
source("C:/Postdoc_analyses/RLA_NWAtlantic/autojags_NP.R")
library(coda)
library(scales)

A1 <- TRUE # Criterion for population A1 = count, A2 = relative abundance


round_realsum <- function(x, digits = 0) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

load("ram4.41_for_analysis_NP.Rdata")
GL_df <- read.csv("C:/Postdoc_analyses/rli_fisheries/data/ramldb/ram4.41_generation_times_finfish_only.csv")


file_list <- sort(unique(stocks$genus_species)) # add column fish
# i<- 1

# df_change_allspecies <- data.frame('year'= year_dat[(nber_of_possible_RLA-1):(nber_of_possible_RLA+GL3)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
# which(dat$Year %in% data$year[data$stockid == colnames(dat)[stock]])

# for(i in 1:length(file_list)){
for(i in 1:50){
  cat(yellow(paste0('\nSpecies : ', file_list[i])))
  load(paste0("./Outputs/", file_list[i], '/', file_list[i], "_Ntot.rdata"))
  year_dat <- sort(unique(data$year[which(data$stockid %in% stocks$stockid[stocks$genus_species == file_list[i]])]))
  # year_dat <- c(year_dat,(tail(year_dat,1)+1))
  GL <- stocks$GL[stocks$genus_species == file_list[i]][1]
  GL3 <- round(3*GL,0)
  nber_of_possible_RLA <- (length(year_dat)-GL3)
  df_status <- data.frame('year'= year_dat[(GL3+1):length(year_dat)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
  df_change <- matrix(NA, ncol=30000,nrow=nber_of_possible_RLA)
  rownames(df_change) <- year_dat[(GL3+1):length(year_dat)]
  
  for(RLA_nber in 1:nber_of_possible_RLA){
    year_for_RLA <- year_dat[(RLA_nber+1):(RLA_nber+GL3)]
    mp.assess <- c((RLA_nber+1),(RLA_nber+GL3))
    df_change[RLA_nber,] <- change <- round((apply(out1[,(mp.assess[2]-1):(mp.assess[2]+1)],1,median)/apply(out1[,(mp.assess[1]-1):(mp.assess[1]+1)],1,median)-1)*100,3)
    
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
    df_status[RLA_nber,'CR'] <- CR
    df_status[RLA_nber,'EN'] <- EN
    df_status[RLA_nber,'VU'] <- VU
    df_status[RLA_nber,'NT'] <- NT
    df_status[RLA_nber,'LC'] <- LC
  }
  
  long_df_change <- data.frame('Year'= rep(df_status$year,each=5)
                               # , 'Status'=rep(c('CR','EN','VU','NT','LC'),5)
                               , 'Status'= factor(rep(c('CR','EN','VU','NT','LC'),length(df_status$year)), levels = c('CR','EN','VU','NT','LC'))
                               , 'Percent' = c(t(df_status[,-1]))
                               )
  rescaled_out1 <- rescale(as.matrix(out1), to=c(20,80))
  df_Ntot <- data.frame('Ntot'=apply(rescaled_out1,2,mean)
                        , 'HPDlow' = apply(rescaled_out1,2,function(x) HPDinterval(as.mcmc(x), prob = 0.95)[1])
                        , 'HPDhigh' = apply(rescaled_out1,2,function(x) HPDinterval(as.mcmc(x), prob = 0.95)[2])
                        , 'year'= c(year_dat,tail(year_dat,1)+1))
  df_Ntot <- df_Ntot[-nrow(df_Ntot), ]
  
  ggplot(long_df_change, mapping = aes(x = Year, y = Percent, fill=Status), xlim(year_dat[1],tail(year_dat,1)+1)) +
    theme_bw() + 
    theme(text = element_text(size=15)
          , panel.grid = element_blank()
          # , axis.ticks=element_line(size=1)
          , legend.box.background = element_rect()
          , axis.line = element_line(color='black')
          , panel.border = element_blank()
          ) +
    geom_bar(stat = 'identity') +
    # geom_area() +
    scale_fill_manual('Status', values = c('CR' = '#D81E05','EN' = '#FC7F3F','VU' = '#F9E814','NT' = '#CCE226','LC' = '#60C659')) +
    geom_ribbon(data=df_Ntot, aes(ymin= HPDlow, ymax= HPDhigh, x= year), alpha=.2, inherit.aes = FALSE) +
    geom_line(data=df_Ntot, aes(y= Ntot, x= year), size = 1.1, inherit.aes = FALSE) +
    scale_x_continuous(breaks = seq(1950, 2020, by = 5), expand=c(0.01,0)) +
    scale_y_continuous(expand=c(0,0))

  # ggplot(long_df_change, aes(x = Year, y = Percent, fill=Status)) + 
  #   geom_bar(stat = 'identity') +
  #   # geom_area() +
  #   scale_fill_manual('Status', values = c('CR' = '#D81E05','EN' = '#FC7F3F','VU' = '#F9E814','NT' = '#CCE226','LC' = '#60C659')) +
  # # curve Nposterior
  # geom_line()
  # plot(rescale(apply(out1,2,mean), to=c(20,80)), type='l', col= 'lightgrey')
  ggsave(paste0("./Outputs/",file_list[i],"/",file_list[i], "_RLA_plot.png"), width = 20, height = 10, units = c("cm"))

  write.table(df_status, paste0("./Outputs/",file_list[i],"/df_status_",file_list[i], ".txt"), row.names = FALSE)
  save(df_change, file = paste0("./Outputs/",file_list[i],"/df_change_",file_list[i], ".RData"))
}
