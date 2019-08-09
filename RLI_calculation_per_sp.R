rm(list=ls())
cat("\014") # clear console# set working directory

library("crayon")

A1 <- TRUE #Criterion for population A1 = count, A2 = relative abundance

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

# for(i in 1:length(file_list)){
for(i in 1:10){
cat(yellow(paste0('\nSpecies : ', file_list[i])))
load(paste0("./Outputs/",file_list[i],"/df_change_",file_list[i], ".RData"))
year_dat <- sort(unique(data$year[which(data$stockid %in% stocks$stockid[stocks$genus_species == file_list[i]])]))
year_RLA <-as.numeric(row.names(df_change))


### calculate RLI ###
# df_change <- data.frame('year'= year_dat[(GL3+1):(nber_of_possible_RLA+GL3)], 'CR'=NA,'EN'=NA,'VU'=NA,'NT'=NA,'LC'=NA)
RL_status <- array(as.numeric(rep(NA,length(1950:2020)*30000)), dim= c(length(1950:2020),30000))
rownames(RL_status) <- 1950:2020
RL_status[as.character(year_RLA),] <- t(apply(df_change,1, function(x) 1-(rescale_RLI(x, weighted = F)/5))) # weighted = T --> weight of categories following Butchart et al. 2004       
RL_status[as.character(1950:(year_RLA[1]-1)),] <- RL_status[as.character(year_RLA[1]),]
RL_status[as.character((tail(year_RLA,1)+1):2020),] <- RL_status[as.character(tail(year_RLA,1)),]


RLI_df <- data.frame(year= 1950:2020
                     , RLI_mean = apply(RL_status,1,mean)
                     , RLI_HPDlow = apply(RL_status,1,function(x) HPDinterval(as.mcmc(x), prob = 0.95)[1])
                     , RLI_HPDhigh = apply(RL_status,1,function(x) HPDinterval(as.mcmc(x), prob = 0.95)[2])
)

png(file = paste0("./Outputs/",file_list[i],"/RLI_",file_list[i], ".png"), width = 5, height = 4, 
    res = 200, units = "in")
plot(NULL, las=1, type = 'l', lty = 'dashed', ylim = c(0,1.1), cex.axis = 1.2, cex.lab = 1.5, xlim = c(1950,2020), ylab = 'Red List Index', xlab = 'Year', yaxs="i", xaxs="i", bty="n")
polygon(c(rev(RLI_df$year), RLI_df$year), c(rev(RLI_df$RLI_HPDlow), RLI_df$RLI_HPDhigh)
        , col = 'grey', border = NA)
lines(RLI_mean~year,data=RLI_df, type = 'l', lty = 'dashed', lwd=1.2)

dev.off()
}