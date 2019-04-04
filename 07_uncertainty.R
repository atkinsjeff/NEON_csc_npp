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
  filter(plotid == "BART_032") -> jeff


mc.biomass <- function(df){
 
  
 # df <- jeff
  
plot.name <- unique(df$plotid)
  # for ease  
hw.check <- any(df$jenkins_model == "aspen/alder/cottonwood/willow")

##### HARDWOOD ONE aspen/alder/cottonwood/willow
if(hw.check == FALSE){
  hw.growth <- matrix(0, 1, 1000)
  hw.dbh <- matrix(0, 1, 1000)}

if(hw.check == TRUE){
# parameters from jenkins
B0 = -2.2094
B1 = 2.3867
rse = 0.507441


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
  error <- rnorm(1, 0, rse) 
  #error <- rnorm(1, 0, 0)
  biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
  biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
}

hw.dbh <- list(DBH.2016, DBH.2017)
hw.growth <- exp(biomass.2017) - exp(biomass.2016)
hw.growth[hw.growth < 0] <- 0
}

##### soft maple check
mb.check <- any(df$jenkins_model == "soft maple/birch")

##### Soft maple/birch model
#############################
if(mb.check == FALSE){
  mb.growth <- matrix(0, 1, 1000)
  mb.dbh <- matrix(0, 1, 1000)}

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
    DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
  }
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  # biomass
  for(i in 1:1000){
    error <- rnorm(1, 0, rse) 
    #error <- rnorm(1, 0, 0) 
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  
  #
  mb.dbh <- list(DBH.2016, DBH.2017)
  
  mb.growth <- exp(biomass.2017) - exp(biomass.2016)
  mb.growth[mb.growth < 0] <- 0
}


#########################Mixed hardwood 

##### mixed hardwood model
mh.check <- any(df$jenkins_model == "mixed hardwood")

##### Soft maple/birch model
#############################
if(mh.check == FALSE){
  mh.growth <- matrix(0, 1, 1000)
  mh.dbh <- matrix(0, 1, 1000)}


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
    DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
  }
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  # biomass
  for(i in 1:1000){
    
    error <- rnorm(1, 0, rse) 
    #error <- rnorm(1, 0, 0)
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  
  
  mh.dbh <- list(DBH.2016, DBH.2017)
  
  mh.growth <- exp(biomass.2017) - exp(biomass.2016)
  mh.growth[mh.growth < 0] <- 0
}




####### hard maple/oak/hickory/beech

hm.check <- any(df$jenkins_model == "hard maple/oak/hickory/beech")

##### hard maple/oak/hickory/beech
#############################
if(hm.check == FALSE){
  hm.growth <- matrix(0, 1, 1000)
  hm.dbh <- matrix(0, 1, 1000)}

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
    DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
  }
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  # biomass
  for(i in 1:1000){
    error <- rnorm(1, 0, rse) 
    #error <- rnorm(1, 0, 0)
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  
  hm.dbh <- list(DBH.2016, DBH.2017)
  hm.growth <- exp(biomass.2017) - exp(biomass.2016)
  hm.growth[hm.growth < 0] <- 0
}



#Softwood Cedar/larch –2.0336 2.2592 196 250 0.294574 0.981

####### softwood cedar/larch

cl.check <- any(df$jenkins_model == "softwood cedar/larch")

##### softwood cedar/larch model
#############################
if(cl.check == FALSE){
  cl.growth <- matrix(0, 1, 1000)
  cl.dbh <- matrix(0, 1, 1000)}

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
    DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
  }
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  # biomass
  for(i in 1:1000){
    error <- rnorm(1, 0, rse) 
    #error <- rnorm(1, 0, 0)
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  #
  cl.dbh <- list(DBH.2016, DBH.2017)
  cl.growth <- exp(biomass.2017) - exp(biomass.2016)
  cl.growth[df.growth < 0] <- 0
}

#Douglas-fir –2.2304 2.4435 165 210 0.218712 0.992


####### douglas-fir

df.check <- any(df$jenkins_model == "douglas-fir")

##### douglas-fir
#############################
if(df.check == FALSE){
  df.growth <- matrix(0, 1, 1000)
  df.dbh <- matrix(0, 1, 1000)}


if(df.check == TRUE){
  # parameters from jenkins  –2.2304 2.4435 165 210 0.218712 0.992
  B0 = -2.2304
  B1 = 2.4435
  rse = 0.218712
  
  
  # filter 
  df %>%
    filter(jenkins_model == "douglas-fir") -> x
  
  ## define DBH matrix ##
  DBH.2016 = matrix(0, length(x$dbh16), 1000)
  DBH.2017 = matrix(0, length(x$dbh17), 1000)
  
  
  ## make 1000 estimates of DBH [cm] for each tree ##
  for(i in 1:length(x$dbh16)){
    DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
  }
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  # biomass
  for(i in 1:1000){
    error <- rnorm(1, 0, rse) 
    #error <- rnorm(1, 0, 0)
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  #
  df.dbh <- list(DBH.2016, DBH.2017)
  df.growth <- exp(biomass.2017) - exp(biomass.2016)
  df.growth[df.growth < 0] <- 0
}

##### fir/hemlock check
fh.check <- any(df$jenkins_model == "fir/hemlock")

#####FIR HEMLOCK MODEL
##############################################
if(fh.check == FALSE){
  fh.growth <- matrix(0, 1, 1000)
  fh.dbh <- matrix(0, 1, 1000)}

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
  DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
  DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
}
## matrices for biomass variables ##
biomass.2016 = matrix(0,length(x[,1]),1000)
biomass.2017 = matrix(0,length(x[,1]),1000)
# biomass
for(i in 1:1000){
  error <- rnorm(1, 0, rse) 
  #error <- rnorm(1, 0, 0)
  biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
  biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
}

#
fh.dbh <- list(DBH.2016, DBH.2017)
fh.growth <- exp(biomass.2017) - exp(biomass.2016)
fh.growth[fh.growth < 0] <- 0
}




#Pine –2.5356 2.4349 331 180 0.253781 0.987
##### pine check
pine.check <- any(df$jenkins_model == "pine")

##### pine model
##############################################
if(pine.check == FALSE){
  pine.growth <- matrix(0, 1, 1000)
  pine.dbh <- matrix(0, 1, 1000)}

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
    DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
  }
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  # biomass
  for(i in 1:1000){
    error <- rnorm(1, 0, rse) 
    #error <- rnorm(1, 0, 0)
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  #
  pine.dbh <- list(DBH.2016, DBH.2017)
  pine.growth <- exp(biomass.2017) - exp(biomass.2016)
  pine.growth[pine.growth < 0] <- 0
}

#Spruce –2.0773 2.3323 212 250 0.250424 0.988
##### spruce
spruce.check <- any(df$jenkins_model == "spruce")

##### SPRUCE MODEL
##############################################
if(spruce.check == FALSE){
  spruce.growth <- matrix(0, 1, 1000)
  spruce.dbh <- matrix(0, 1, 1000)}

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
    DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
  }
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  # biomass
  for(i in 1:1000){
    error <- rnorm(1, 0, rse) 
    #error <- rnorm(1, 0, 0)
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  
  #
  spruce.dbh <- list(DBH.2016, DBH.2017)
  spruce.growth <- exp(biomass.2017) - exp(biomass.2016)
  spruce.growth[spruce.growth < 0] <- 0
}

#Woodland|| Juniper/oak/mesquite –0.7152 1.7029 61 78 0.384331 0.938
##### juniper/oak/mesquite check
w.check <- any(df$jenkins_model == "juniper/oak/mesquite")

##### juniper/oak/mesquite model
##############################################
if(w.check == FALSE){
  w.growth <- matrix(0, 1, 1000)
  w.dbh <- matrix(0, 1, 1000)}


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
    DBH.2016[i,] = (rnorm(1000, 0, 0))  + x[i,4]  # 0.05 is standard msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 0))  + x[i,5]
  }
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  # biomass
  for(i in 1:1000){
    error <- rnorm(1, 0, rse) 
    #error <- rnorm(1, 0, 0)
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  
  #
  w.dbh <- list(DBH.2016, DBH.2017)
  w.growth <- exp(biomass.2017) - exp(biomass.2016)
  w.growth[w.growth < 0] <- 0
}




#######
# writing dbhs to .csv

# all.the.dbh <- list(hw.dbh, mb.dbh, mh.dbh, hm.dbh, cl.dbh, df.dbh, fh.dbh, pine.dbh, spruce.dbh, w.dbh)
# # we give each matrix in the list a unique name
# matrix_names <- as.character(1:length(all.the.dbh))
# 
# # converting to dataframe with the list index preserved
# fffdf <- lapply(1:length(all.the.dbh), function(i)
#   cbind(matrix_names = matrix_names[i], as.data.frame(all.the.dbh[[i]])))
# fffdf <- do.call(rbind, fffdf)
# 
# # writing it to a file
# t <- tempfile(fileext = ".csv")
# write.csv(fffdf, t, row.names = FALSE)
# #######



######

######
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
output_directory <- "./summary/mc_simulations_just_allometry"
utils::write.csv(npp.output, file.path(output_directory, filename))

}

mc.biomass(jeff)

##### from chris



###### apply function across data frames
# # split data frame
df.list <- split(bob, bob$plotid)

for (k in 1:length(df.list)){
  karen <- df.list[[k]]
  #karen <- data.frame(karen)
  mc.biomass(karen)
}

######################################

#set data directory
data_dir <- "./summary/mc_simulations_just_dbh"

# create output data frame
#### make output
mc.output <- data.frame(matrix(ncol = 6, nrow = length(file.names)))

jj <- c("plotid", "mean.npp", "sd.npp", "cv.npp", "n.npp", "est.npp" )
colnames(mc.output) <- jj

#get list of filenames
file.names <- dir(data_dir, pattern =".csv")


for(i in 1:length(file.names)){
  f <- file.names[i]

  g <- file.path(data_dir, f)

#g <- "./summary/mc_simulations_just_allometry/BART_032_mc.csv"
filename <- sub(".*/", "", f)

df2 <- utils::read.csv(g)

mc.output$plotid[i] <- as.character(unique(df2$plotid))


# if( all(df2$npp_mc == 0)){
#   print("butts")}else{
#     print("no")
#   }

if(all(df2$npp_mc == 0)){
  mc.output$sd.npp[i] <- 0
  message("plot sd")
  #print(sd.npp)
  
  mc.output$mean.npp[i] <- 0
  message("plot mean")
  #print(mean.npp)
  
  mc.output$cv.npp[i] <- 0
  message("plot cv")
  #print(cv.npp)
  
  mc.output$n.npp[i] <- 0
  # message("no  of plots")
  # print(n.npp)
}else{

# get sums
npp2.sums <- df2$npp_mc[which(df2$npp_mc > 0)] 


mc.output$sd.npp[i] <- sd(npp2.sums)
message("plot sd")
#print(sd.npp)

mc.output$mean.npp[i]<- mean(npp2.sums)
message("plot mean")
#print(mean.npp)

mc.output$cv.npp[i] <- mc.output$sd.npp[i]/mc.output$mean.npp[i]
message("plot cv")
#print(cv.npp)

mc.output$n.npp[i] <- length(npp2.sums)
}



# get plot npp from other df
match.plot <- unique(df2$plotid)

# fill dataframe
#mc.output$plotid[i] <- as.character(unique(df2$plotid))
# mc.output$plotid[i] <- df2$plotid[1]
# mc.output$mean.npp[i] <- mean.npp
# mc.output$sd.npp[i] <- sd.npp
# mc.output$cv.npp[i] <- cv.npp
# mc.output$n.npp[i] <- n.npp
mc.output$est.npp[i] <- plot.npp$npp[plot.npp$plotid == as.character(match.plot)]

#remove any missing numbers
#mc.output <- na.omit(mc.output)


#plot.npp$npp[plot.npp$plotid == as.character(match.plot)]

# 1. Open jpeg file
#plotname <- paste(as.character(match.plot), "_just_dbh_allometry.jpg", sep="")

# jpeg(plotname, width = 700, height = 700)
# hist(npp2.sums, breaks = 100, main = filename,
#      xlab = "NPP Mg per Ha")
# abline(v = plot.npp$npp[plot.npp$plotid == as.character(match.plot)], col="red")
# dev.off()
}

######
x11()
plot(mc.output$est.npp,mc.output$mean.npp,
     xlab = ("Reported NPP in Paper"),
     ylab = ("MC Mean NPP")
     )


mc.output$siteid <- as.factor(substr(mc.output$plotid, 0,4))

#drop zeros
#pull out empties
mc.output.no.zero <- mc.output[apply(mc.output[c(2:6)], 1, function(x) !any(x == 0)),]

mc.output.no.zero %>%
  group_by(siteid) %>%
  summarise(mean.npp.mc = mean(mean.npp), sd.npp.mc = sd(mean.npp), mean.npp.est = mean(est.npp), sd.npp.est = sd(est.npp)) -> site.table.mc

site.table.mc <- data.frame(site.table.mc)

site.table.mc$mc.cv <-  site.table.mc$sd.npp.mc / site.table.mc$mean.npp.mc
site.table.mc$npp.cv <-  site.table.mc$sd.npp.est / site.table.mc$mean.npp.est
site.table.mc$mc.se <- site.table.mc$sd.npp.mc / sqrt(1000)
site.table.mc$npp.se <- site.table.mc$sd.npp.est / sqrt(1000)


#write.csv(site.table.mc, "site_table_mc_just_dbh_error.csv")

x11()
plot(x = site.table.mc$mean.npp.est, y = site.table.mc$mean.npp.mc,
     xlab = ("Reported NPP in Paper"),
     ylab = ("MC Mean NPP"),
     main = "NPP with DBH error only")
     abline(lm(site.table.mc$mean.npp.mc ~ site.table.mc$mean.npp.est))

write.csv(mc.output, "./summary/plot_level_mc_simulation_allometry_error_only.csv")


####
uncertainty_analysis_one <- function(data, B0, B1, rse) {
  # If data is empty, return blank matrix early
  if (!NROW(data) > 0) return(matrix(0, 1, 1000))
  
  # Otherwise, do stuff...
  #cf <- (rse ^ 2) / 2
  DBH.2016 = matrix(0, length(data$dbh16), 1000)
  DBH.2017 = matrix(0, length(data$dbh17), 1000)
  
  ## make 1000 estimates of DBH [cm] for each tree ##
  for(i in 1:length(x$dbh16)){
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + data[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + data[i,4]
  }
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  # biomass
  for(i in 1:1000){
    #error <- rnorm(1, 0, rse) 
    biomass.2016[,i] = (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = (B0 + B1 * log(DBH.2017[,i])) 
  }
  #
  growth <- exp(biomass.2017) - exp(biomass.2016)
  growth[growth < 0] <- 0
  
  jill <- list(DBH.2016, DBH.2017, biomass.2016, biomass.2017, growth)
}


####
uncertainty_analysis_two <- function(data, B0, B1, rse) {
  # If data is empty, return blank matrix early
  if (!NROW(data) > 0) return(matrix(0, 1, 1000))
  
  # Otherwise, do stuff...
  #cf <- (rse ^ 2) / 2
  DBH.2016 = matrix(0, length(data$dbh16), 1000)
  DBH.2017 = matrix(0, length(data$dbh17), 1000)
  
  ## make 1000 estimates of DBH [cm] for each tree ##
  for(i in 1:length(x$dbh16)){
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + data[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + data[i,4]
  }
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  # biomass
  for(i in 1:1000){
    error <- rnorm(1, 0, rse) 
    biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i])) 
    biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i])) 
  }
  #
  growth <- exp(biomass.2017) - exp(biomass.2016)
  growth[growth < 0] <- 0
  growth
}


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
  error <- rnorm(1, 0, rse)
  biomass.2016[,i] = error + (B0 + B1 * log(DBH.2016[,i]))
  biomass.2017[,i] = error + (B0 + B1 * log(DBH.2017[,i]))
}

hw.growth <- exp(biomass.2017) - exp(biomass.2016)
hw.growth[hw.growth < 0] <- 0
}

mc.biomass <- function(df) {
  plot.name <- unique(df$plotid)

  hw.growth <- df %>%
    filter(jenkins_model == "aspen/alder/cottonwood/willow") %>%
    uncertainty_analysis_one(-2.2094, 2.3867, 0.507331) 

  mb.growth <- df %>%
    filter(jenkins_model == "soft maple/birch") %>%
    uncertainty_analysis_one(-2.2094, 2.3867, 0.507331)

  x <- list(hw.growth, mb.growth)
 
}

mc.biomass(bob)

