

##### pull in data file
unique(df3$jenkins_model)


df3 %>%
  select(plotid, jenkins_model, individualid, dbh16, dbh17) -> bob

bob <- na.omit(bob, cols = c("dbh16"))

bob <- data.frame(bob)

bob$lndbh16 <- log(bob$dbh16)
bob$lndbh17 <- log(bob$dbh17)
###########################################
###########################################
###########################################
###########################################
###########################################



df <- bob[bob$plotid == "HARV_051", ]


hw.check <- any(df$jenkins_model == "aspen/alder/cottonwood/willow")

##### HARDWOOD ONE aspen/alder/cottonwood/willow
if(hw.check == FALSE){hw.growth <- matrix(0, 1, 1000)}

if(hw.check == TRUE){
# parameters from jenkins
B0 = -2.2094
B1 = 2.3867
rse = 0.507441
cf <- rserse^2/2

# filter 
df %>%
  filter(jenkins_model == "aspen/alder/cottonwood/willow") -> x

## define DBH matrix ##
DBH.2016 = matrix(0, length(x$dbh16), 1000)
DBH.2017 = matrix(0, length(x$dbh17), 1000)

## make 1000 estimates of DBH [cm] for each tree ##
for(i in 1:length(x$dbh16)){
  DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,2]  # 0.05 is standartd msmt error in cm for dbh repeats
}

# biomass
for(i in 1:1000){
  biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
  biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }
  
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  
  for(i in 1:1000){
    biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
    biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
if(mb.check == FALSE){mb.growth <- matrix(0, 1, 1000)}

if(mb.check == TRUE){
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
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }
  
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  
  for(i in 1:1000){
    biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
    biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
    filter(jenkins_model == "mixed hardwood") -> x
  
  ## define DBH matrix ##
  DBH.2016 = matrix(0, length(x$dbh16), 1000)
  DBH.2017 = matrix(0, length(x$dbh17), 1000)
  
  
  ## make 1000 estimates of DBH [cm] for each tree ##
  for(i in 1:length(x$dbh16)){
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }
  
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  
  for(i in 1:1000){
    biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
    biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }
  
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  
  for(i in 1:1000){
    biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
    biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }
  
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  
  for(i in 1:1000){
    biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
    biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
  DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
  DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }


## matrices for biomass variables ##
biomass.2016 = matrix(0,length(x[,1]),1000)
biomass.2017 = matrix(0,length(x[,1]),1000)


for(i in 1:1000){
  biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
  biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }
  
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  
  for(i in 1:1000){
    biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
    biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }
  
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  
  for(i in 1:1000){
    biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
    biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
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
    DBH.2016[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]  # 0.05 is standartd msmt error in cm for dbh repeats
    DBH.2017[i,] = (rnorm(1000, 0, 1)) * 0.05 + x[i,4]
  }
  
  
  ## matrices for biomass variables ##
  biomass.2016 = matrix(0,length(x[,1]),1000)
  biomass.2017 = matrix(0,length(x[,1]),1000)
  
  
  for(i in 1:1000){
    biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i])) + cf
    biomass.2017[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2017[,i])) + cf
  }
  
  #
  w.growth <- exp(biomass.2017) - exp(biomass.2016)
  w.growth[w.growth < 0] <- 0
}


#######

npp.mat <- rbind(hw.growth, mb.growth, mh.growth, hm.growth, cl.growth, df.growth, fh.growth, pine.growth, spruce.growth, w.growth)

npp.sums <- colSums(npp.mat)

## change to per hectare
npp.sums <- npp.sums * 12.5

# change to Mg per hectare
npp.sums <- npp.sums * 0.001 

npp.sums <- npp.sums[which(npp.sums > 0)] 
sd.npp <- sd(npp.sums)
mean.npp <- mean(npp.sums)

cv.npp <- sd.npp/mean.npp



# remove negatives

df3$growth[df3$growth < 0] <- 0
df3.mlbs$growth[df3.mlbs$growth < 0] <- 0

# get plot sum
df3 %>% 
  group_by(plotid) %>%
  dplyr::summarise(npp = sum(growth, na.rm = TRUE)) -> plot.npp

####MLBS
df3.mlbs %>%
  group_by(plotid) %>%
  dplyr::summarise(npp = sum(growth,na.rm = TRUE)) -> plot.npp.mlbs


###### now bring them all back together
plot.npp <- rbind(plot.npp, plot.npp.mlbs)

plot.npp <- data.frame(plot.npp)

head(plot.npp)

# change to per hectare
plot.npp$npp <- plot.npp$npp * 12.5

# change to Mg per hectare
plot.npp$npp <- plot.npp$npp * 0.001
## matrices for biomass variables ##
biomass.2016 = matrix(0,length(hw1[,1]),1000)





##define area matrix##
plot.area.2002 = matrix(0,length(data2.2002[,1]),1)


##fill area matrix. areas are in m^2##
for(i in 1:length(data2.2002[,1])){
  if(data2.2002[i,7]>=10){plot.area.2002[i]=130000} else 
    if(data2.2002[i,7]<10){plot.area.2002[i]=15702.48}
}

#fill in biomass
# this should just be the biomass equation fro mjenkins 
B0 = -2.2094
B1 = 2.3867
rse = 0.507441

for(i in 1:1000){
  biomass.2016[,i] = (rnorm(1, 0, 1) * rse) + (B0 + B1 * log(DBH.2016[,i]))
}

# finding error terms
# confidence intervals
quantile(biomass.2016, c(0.025, 0.975))

sd.hw1 <- sd(biomass.2016)
n.hw1 <- length(biomass.2016)
mean.hw1 <- mean(biomass.2016)
se.hw1 <- sd.hw1 / n.hw1
cv <- exp(sd.hw1) / exp(mean.hw1)
##### finding column sums

biomass.2016.sums <- colSums(biomass.2016)


### convert to kgHA
# change to per hectare
biomass.2016.sums <- biomass.2016.sums * 12.5

# change to Mg per hectare
biomass.2016.sums <- biomass.2016.sums * 0.001

# confidence intervals
quantile(biomass.2016, c(0.025, 0.975))

sd.hw1 <- sd(biomass.2016)
n.hw1 <- length(biomass.2016)
mean.hw1 <- mean(biomass.2016)
se.hw1 <- sd.hw1 / n.hw1
#### biomass check
x11()
par(mfrow = c(2,1))
hist(biomass.2016,
     main = "monte carlo biomass",
     xlim = c(0,1000),
     breaks = 100)
hist(df3$b2016,
     main = "Reported biomass",
     xlim = c(0,1000),
     breaks = 300)
