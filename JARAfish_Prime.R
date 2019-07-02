rm(list=ls())
cat("\014") # clear console# set working directory



# required packages
library(gplots)
library(coda)
library(rjags)
library(R2jags)
library("fitdistrplus")
library("crayon") ##NP
library(logspline) ##NP
library(svDialogs) ##NP
source("C:/Postdoc_analyses/RLA_NWAtlantic/autojags_NP.R")
library(beepr)

load("./data/ramldb/ram4.41_for_analysis.Rdata")
GL_df <- read.csv("C:/Postdoc_analyses/rli_fisheries/data/ramldb/ram4.41_generation_times_finfish_only.csv")


file_list_fish <- unique(stocks$genus_species[stocks$]) # add column fish
i<- 1

# see which fish we don't have the GL
GL_df_noNA <- GL_df[!is.na(GL_df$g_yr), ]
file_list[!(file_list %in% GL_df_noNA$species)]



for(i in 1:length(file_list)){
  cat(yellow(paste0('\nSpecies : ', file_list[i])))
  
  # Set Working directory file, where assessments are stored 
  File = paste0("./Outputs/", file_list[i])
  dir.create(File,showWarnings = FALSE)
  
  # Set working directory for JABBA R source code
  JARA.file <- "C:/Postdoc_analyses/rli_fisheries"
  
  # JABBA version
  version <- "v1.1Fish_NP"

  # Create assessment species information file
  # nber_row <- length(stocks$stockid[stocks$genus_species == file_list[i]])
  sp.assess <- data.frame('assessment' = file_list[i]
                          , 'run' = 1
                          , 'abundance' = 'relative'
                          , 'generation.length' = if(file_list[i] %in% GL_df$species & !is.na(GL_df$g_yr[GL_df$species == file_list[i]])){GL_df$g_yr[GL_df$species == file_list[i]]}else{NA}
                          , 'start.year' = NA
                          , 'end.year' = NA
                          , 'index.se' = FALSE
                          , 'sigma.obs.est ' = TRUE
                          , 'sigma.obs.add' = 0.25
                          , 'project.r' = 'years'
                          , 'sigma.proc.fixed' = FALSE
                          , 'stochastic.projection' = FALSE
                          , 'Klim' = FALSE
                          , 'K.manual' = FALSE
                          , 'A1' = FALSE
                          )
  
  # Select assessment species
  # for(sp in 1:dim(sp.assess)[1]){
  # for(sp in c(5:6)){
    spsel <- sp.assess[1,]
    cat(cyan(paste0('\nTime-series : ', spsel[[1]]))) ##NP
    
    #-----------------------------------------------------------------
    # Set up JARA based on csv input file 
    #-----------------------------------------------------------------
    assessment <- spsel$assessment # assessment name
    run <- spsel$run 
    abundance <- spsel$abundance # c("census","relative")
    GL <- spsel$generation.length # numeric generation length (years)
    Klim <- spsel$Klim # c(TRUE,FALSE)
    K.manual <- spsel$K.manual # c(TRUE,FALSE), if TRUE provide vector of K for each pop
    SE.I <- spsel$index.se #  c(TRUE,FALSE), if TRUE provide assessment_se.csv file
    sigma.est <- spsel$sigma.obs.est # c(TRUE,FALSE)
    fixed.obsE <- spsel$sigma.obs.add # numeric, fixed component of observation error 
    sigma.proc.fixed <- spsel$sigma.proc.fixed # numeric or TRUE otherwise
    prjr.type <- (spsel$project.r) # c("all","GL1", years) # "all" is default
    proj.stoch <- spsel$stochastic.projection # c(FALSE, TRUE), FALSE is default
    start.year <- spsel$start.year # numeric, NA takes all years
    end.year <- spsel$end.year # numeric, NA takes all years
    A1 <- spsel$A1
    plot.width <- 5 # default is 5 inches
    
    # create dataset for all time-series for one species
    year_dat <- sort(unique(data$year[which(data$stockid %in% stocks$stockid[stocks$genus_species == file_list[i]])]))
    dat <- data.frame(matrix(c(year_dat
                               , rep(NA,length(year_dat)*length(stocks$stockid[stocks$genus_species == file_list[i]])))
                               , ncol = (length(stocks$stockid[stocks$genus_species == file_list[i]])+1), byrow = FALSE))
    colnames(dat) <- c("Year", stocks$stockid[stocks$genus_species == file_list[i]])
    
    for(stock in 2:dim(dat)[2]){
      dat[which(dat$Year %in% data$year[data$stockid == colnames(dat)[stock]]),stock] <- data$biomass[data$stockid == colnames(dat)[stock]]
    }    
    # if(SE.I == TRUE){
    #   se <- read.csv(paste0(File,"/",assessment,"/",assessment,"_se.csv"))
    # }
    
    
    #><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
    # Execute model and produce output
    #><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
    if(abundance == "census"){
      # MCMC settings
      ni <- 70000 # Number of iterations
      nt <- 6 # Steps saved
      nb <- 10000 # Burn-in
    } else {
      ni <- 70000 # Number of iterations
      nt <- 6 # Steps saved
      nb <- 10000 # Burn-in
    }
    nc <- 3 # number of chains
    nsaved <- (ni-nb)/nt*nc
    
    source(paste0(JARA.file,"/JARA",version,".R"))
}
 
beep(1)

