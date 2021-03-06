# this copy of the error script assigns error for each tree

# yanai et al. 2010 says NO
require(plyr)
require(dplyr)
require(tidyverse)

df3 <- read.csv("neon_dbh_test.csv")
##### pull in data file
unique(df3$jenkins_model)


df3 %>%
  select(plotid, jenkins_model, individualid, dbh16, dbh17, siteid) -> bob

#pull out empties
bob <- na.omit(bob, cols = c("dbh16"))


bob <- data.frame(bob)
bob <- na.omit(bob, cols = "jenkins_model")
bob <- na.omit(bob, cols = "plotid")

bob <-droplevels(bob)

table(count(bob$plotid), bob$siteid)

bob %>%
  group_by(siteid) %>%
  summarize(n_unique = n_distinct(plotid)) -
  
  CVI$ID[ELEV=="low" & VEG=="canopy" & PLOT == 3] <-"LC3"
attach(bob)
bob$plot.count[bob$siteid == "BART"] <- 20
bob$plot.count[bob$siteid == "DELA"] <- 19
bob$plot.count[bob$siteid == "GRSM"] <- 19
bob$plot.count[bob$siteid == "HARV"] <- 20
bob$plot.count[bob$siteid == "LENO"] <- 10
bob$plot.count[bob$siteid == "ORNL"] <- 20
bob$plot.count[bob$siteid == "OSBS"] <- 20
bob$plot.count[bob$siteid == "SCBI"] <- 20
bob$plot.count[bob$siteid == "SERC"] <- 20
bob$plot.count[bob$siteid == "TALL"] <- 20
bob$plot.count[bob$siteid == "TREE"] <- 20
bob$plot.count[bob$siteid == "UNDE"] <- 17
# bob$lndbh16 <- log(bob$dbh16)
# bob$lndbh17 <- log(bob$dbh17)
# ###########################################
# ###########################################
# ###########################################
# ###########################################
# ###########################################

bob %>%
  filter(siteid == "HARV") -> jeff



mc.biomass <- function(df){
  
  
  # df <- jeff
  
  plot.name <- unique(df$plotid)
  # for ease  
  hw.check <- any(df$jenkins_model == "aspen/alder/cottonwood/willow")
  
  ##### HARDWOOD ONE aspen/alder/cottonwood/willow
  if(hw.check == FALSE){hw.growth <- matrix(0, 1, 1000)}
  
  if(hw.check == TRUE){
    # parameters from jenkins
    B0 = -2.2094
    B1 = 2.3867
    rse = 0.507441
    cf <- rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "aspen/alder/cottonwood/willow") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    
    hw.growth <- exp(biomass.2017) - exp(biomass.2016)
    hw.growth[hw.growth < 0] <- 0
  }
  
  ##### soft maple check
  mb.check <- any(df$jenkins_model == "soft maple/birch")
  
  ##### Soft maple/birch model
  #############################
  if(mb.check == FALSE){mb.growth <- matrix(0, 1, 1000)}
  
  if(mb.check == TRUE){
    # parameters from jenkins  –1.9123 2.3651 316 66 0.491685 0.958
    B0 = -1.9123
    B1 = 2.3651
    rse = 0.491685
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "soft maple/birch") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    
    #
    mb.growth <- exp(biomass.2017) - exp(biomass.2016)
    mb.growth[mb.growth < 0] <- 0
  }
  
  
  #########################Mixed hardwood 
  
  ##### mixed hardwood model
  mh.check <- any(df$jenkins_model == "mixed hardwood")
  
  ##### Soft maple/birch model
  #############################
  if(mh.check == FALSE){mh.growth <- matrix(0, 1, 1000)}
  
  if(mh.check == TRUE){
    # parameters from jenkins  –2.4800 2.4835 289 56 0.360458 0.980
    B0 = -2.4800
    B1 = 2.4835
    rse = 0.360458
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "mixed hardwood") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    
    #
    mh.growth <- exp(biomass.2017) - exp(biomass.2016)
    mh.growth[mh.growth < 0] <- 0
  }
  
  
  
  
  ####### hard maple/oak/hickory/beech
  
  hm.check <- any(df$jenkins_model == "hard maple/oak/hickory/beech")
  
  ##### hard maple/oak/hickory/beech
  #############################
  if(hm.check == FALSE){hm.growth <- matrix(0, 1, 1000)}
  
  if(hm.check == TRUE){
    # parameters from jenkins  –2.0127 2.4342 485 73 0.236483 0.988
    B0 = -2.0127
    B1 = 2.4342
    rse = 0.236483
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "hard maple/oak/hickory/beech") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    #
    hm.growth <- exp(biomass.2017) - exp(biomass.2016)
    hm.growth[hm.growth < 0] <- 0
  }
  
  
  
  #Softwood Cedar/larch –2.0336 2.2592 196 250 0.294574 0.981
  
  ####### softwood cedar/larch
  
  cl.check <- any(df$jenkins_model == "softwood cedar/larch")
  
  ##### softwood cedar/larch model
  #############################
  if(cl.check == FALSE){cl.growth <- matrix(0, 1, 1000)}
  
  if(cl.check == TRUE){
    # parameters from jenkins  –2.0336 2.2592 196 250 0.294574 0.981
    B0 = -2.0336
    B1 = 2.2592
    rse = 0.294574
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "softwood cedar/larch") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    #
    cl.growth <- exp(biomass.2017) - exp(biomass.2016)
    cl.growth[df.growth < 0] <- 0
  }
  
  #Douglas-fir –2.2304 2.4435 165 210 0.218712 0.992
  
  
  ####### douglas-fir
  
  df.check <- any(df$jenkins_model == "douglas-fir")
  
  ##### douglas-fir
  #############################
  if(df.check == FALSE){df.growth <- matrix(0, 1, 1000)}
  
  if(df.check == TRUE){
    # parameters from jenkins  –2.2304 2.4435 165 210 0.218712 0.992
    B0 = -2.2304
    B1 = 2.4435
    rse = 0.218712
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "douglas-fir") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    #
    df.growth <- exp(biomass.2017) - exp(biomass.2016)
    df.growth[df.growth < 0] <- 0
  }
  
  ##### fir/hemlock check
  fh.check <- any(df$jenkins_model == "fir/hemlock")
  
  #####FIR HEMLOCK MODEL
  ##############################################
  if(fh.check == FALSE){fh.growth <- matrix(0, 1, 1000)}
  
  if(fh.check == TRUE){
    # parameters from jenkins  –2.5384 2.4814 395 230 0.182329
    B0 = -2.5384
    B1 = 2.4814
    rse = 0.182329
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "fir/hemlock") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    
    #
    fh.growth <- exp(biomass.2017) - exp(biomass.2016)
    fh.growth[fh.growth < 0] <- 0
  }
  
  
  
  
  #Pine –2.5356 2.4349 331 180 0.253781 0.987
  ##### pine check
  pine.check <- any(df$jenkins_model == "pine")
  
  ##### pine model
  ##############################################
  if(pine.check == FALSE){pine.growth <- matrix(0, 1, 1000)}
  
  if(pine.check == TRUE){
    # parameters from jenkins  –2.5356 2.4349 331 180 0.253781 0.987
    B0 = -2.5356
    B1 = 2.4349
    rse = 0.253781
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "pine") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    #
    pine.growth <- exp(biomass.2017) - exp(biomass.2016)
    pine.growth[pine.growth < 0] <- 0
  }
  
  #Spruce –2.0773 2.3323 212 250 0.250424 0.988
  ##### spruce
  spruce.check <- any(df$jenkins_model == "spruce")
  
  ##### SPRUCE MODEL
  ##############################################
  if(spruce.check == FALSE){ spruce.growth <- matrix(0, 1, 1000)}
  
  if(spruce.check == TRUE){
    # parameters from jenkins  –2.0773 2.3323 212 250 0.250424 0.988
    B0 = -2.0773
    B1 = 2.3323
    rse = 0.250424
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "spruce") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    
    #
    spruce.growth <- exp(biomass.2017) - exp(biomass.2016)
    spruce.growth[spruce.growth < 0] <- 0
  }
  
  #Woodland|| Juniper/oak/mesquite –0.7152 1.7029 61 78 0.384331 0.938
  ##### juniper/oak/mesquite check
  w.check <- any(df$jenkins_model == "juniper/oak/mesquite")
  
  ##### juniper/oak/mesquite model
  ##############################################
  if(w.check == FALSE){ w.growth <- matrix(0, 1, 1000)}
  
  if(w.check == TRUE){
    # parameters from jenkins  –0.7152 1.7029 61 78 0.384331 0.938
    B0 = -0.7152
    B1 = 1.7029
    rse = 0.384331
    cf = rse^2/2
    
    # filter 
    df %>%
      filter(jenkins_model == "juniper/oak/mesquite") -> x
    
    ## define DBH matrix ##
    DBH.2016 = matrix(0, length(x$dbh16), 1000)
    DBH.2017 = matrix(0, length(x$dbh17), 1000)
    
    ## make 1000 estimates of DBH [cm] for each tree ##
    for(i in 1:length(x$dbh16)){
      DBH.2016[i,] = (rnorm(1000, 0, 0.05))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
      DBH.2017[i,] = (rnorm(1000, 0, 0.05))  + x[i,5]
    }
    ## matrices for biomass variables ##
    biomass.2016 = matrix(0,length(x[,1]),1000)
    biomass.2017 = matrix(0,length(x[,1]),1000)
    
    # biomass
    for(i in 1:1000){
      biomass.2016[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2016[,i])) 
      biomass.2017[,i] = (rnorm(1, 0, rse) ) + (B0 + B1 * log(DBH.2017[,i])) 
    }
    
    #
    w.growth <- exp(biomass.2017) - exp(biomass.2016)
    w.growth[w.growth < 0] <- 0
  }
  
  
  
  
  #######
  
  npp.mat <- rbind(hw.growth, mb.growth, mh.growth, hm.growth, cl.growth, df.growth, fh.growth, pine.growth, spruce.growth, w.growth)
  
  npp.sums <- colSums(npp.mat)
  
  #### plot corrction
  
  # plot.no.cf <- unique(df$plot.count)
  # 
  # npp.sums <- npp.sums/plot.no.cf
  ## change to per hectare
  npp.sums <- npp.sums * 12.5
  
  # change to Mg per hectare
  npp.sums <- npp.sums * 0.001 
  
  
  #### make output
  npp.output <- data.frame(matrix(ncol = 2, nrow = length(npp.sums)))
  
  jj <- c("plotid", "npp_mc")
  colnames(npp.output) <- jj
  
  npp.output$plotid <- plot.name
  npp.output$npp_mc <- npp.sums
  
  #output procedure for variables
  
  outputname <- paste(plot.name, "mc", sep = "_")
  filename <- paste(outputname, ".csv", sep="")
  output_directory <- "./summary/mc_simulations"
  utils::write.csv(npp.output, file.path(output_directory, filename))
  
}