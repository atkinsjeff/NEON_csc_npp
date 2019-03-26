library(MonteCarlo)

#########################################
##      Example: t-test

# Define function that generates data and applies the method of interest

ttest<-function(n,loc,scale){
  
  # generate sample:
  sample<-rnorm(n, loc, scale)
  
  # calculate test statistic:
  stat<-sqrt(n)*mean(sample)/sd(sample)
  
  # get test decision:
  decision<-abs(stat)>1.96
  
  # return result:
  return(list("decision"=decision))
}

# As discussed above, ttest() is formulated in a way as if we only want to generate a single test decision. The arguments of 
# the function are the parameters we are interested in.

#Our ttest() function carries out 4 steps:
  
#   Draw a sample of n observations from a normal distribution with mean μ=loc and standard deviation σ=scale.
# Calculate the t-statistic.
# Determine the test decision.
# Return the desired result in form of a list.
# We then define the combinations of parameters that we are interested in and collect them in a list. T
# he elements of this list must have the same names as the parameters for which we want to supply grids.

# define parameter grid:

n_grid<-c(50,100,250,500)
loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

# collect parameter grids in list:
param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
# To run the simulation, the function ttest() and the parameter grid (param_list) are passed to MonteCarlo(), 
# together with the desired number of Monte Carlo repetitions (here nrep=1000).

# run simulation:

MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)
#There is no further coding required. All the mechanics of the Monte Carlo experiment are handled by the MonteCarlo() function.

#Calling summary produces a short information on the simulation.

summary(MC_result)


MakeTable(output=MC_result, rows="n", cols=c("loc","scale"), digits=2, include_meta=FALSE)


norm.simulated <- rnorm( n = 100, mean = 5, sd = 2)
par(mfrow = c(3,1))
plot(norm.simulated)
hist(norm.simulated)
hist(norm.simulated, freq = FALSE)
curve(dnorm(x, mean = 5, sd = 2), from = 0, to = 10, add = TRUE, col = "red")

mean(norm.simulated)
sd(norm.simulated)

norm.simulated1 <- rnorm( n = 100, mean = 5, sd = 2)
norm.simulated2 <- rnorm( n = 100, mean = 5, sd = 2)
norm.simulated3 <- rnorm( n = 100, mean = 5, sd = 2)
norm.simulated4 <- rnorm( n = 100, mean = 5, sd = 2)

par(mfrow = c(2,2))
hist(norm.simulated1)
hist(norm.simulated2)
hist(norm.simulated3)
hist(norm.simulated4)



norm.sim.all.2 <- replicate(n = 4,  rnorm( n = 100, mean = 5, sd = 2))

#2000 times
norm.sim.all.3 <- replicate(n = 2000,  rnorm( n = 100, mean = 5, sd = 2))

sd( apply(X = norm.sim.all.3, MARGIN = 2, FUN = mean))
2/sqrt(100)

#### line and regression

a = 5
b = 0.7
x  <- seq(2, 20)
y_fixed <- a + b*x
x11()
plot(y_fixed~x)

y.sim.1 <- rnorm(length(x), mean = y_fixed, sd = 2.5)
x11()
plot(y.sim.1 ~ x)
abline(a = 5, b = 0.7, lwd = 2)

####
y.sim.1.lm <- lm(y.sim.1 ~ x)
summary(y.sim.1.lm)
confint(y.sim.1.lm)
abline(reg = y.sim.1.lm, lty = 2, col = "red", lwd = 2)


####
#general form
jenk <- exp(b0 + b1 * log(x))

#hardwood aspen/alder/cottonwood/willow
b0 = -2.2094
b1 = 2.3867
rmse = 0.507441
n = 230
x <- seq(1,100)
# test
hw1 <- rnorm(length(x), mean = jenk, sd = rmse)

x11()
plot(hw1 ~ x)

hw1 <- replicate(n = 2000,  rnorm(length(x), mean = jenk, sd = rmse))

sd( apply(X = hw1, MARGIN = 2, FUN = mean))
2/sqrt(100)


#####

#####
head(faithful)

hist(faithful$eruptions, breaks = 20, col = "skyblue")

# median
median(faithful$eruptions)

# going to be the same rol as size in the MC simulation
B = 10000

med_BT = rep(NA, B)
n = nrow(faithful)

for(i in 1:B){
  w = sample(n, n, replace = TRUE)
             # this generates the indices we are selcting during the sample with replacement
             X_BT = faithful$eruptions[w]
             med_BT[i] = median(X_BT)
}

x11()
hist(med_BT, col = "orchid")
abline(v = median(faithful$eruptions), col = "blue", lwd = 4)

var(med_BT)

median(faithful$eruptions) + qnorm(0.95) * sd(med_BT)
median(faithful$eruptions) - qnorm(0.95) * sd(med_BT)

###
cor(faithful)
## eruptions waiting
## eruptions 1.0000000 0.9008112
## waiting 0.9008112 1.0000000

# this returns a sample correlation matrix
r0 = cor(faithful)[1,2]
# this is the correlation between the two variables
n = nrow(faithful)
B = 10000
r0_BT = rep(NA, B)
for(i in 1:B){
  w = sample(n,n,replace=T)
  faithful_BT = faithful[w,]
  r0_BT[i] = cor(faithful_BT)[1,2]
}
hist(r0_BT, col="pink")
abline(v=r0, col="red",lwd=4)

####
# The bootstrap can also be applied to estimate the uncertainty of a density estimator. Here we will illustrate
# this by the KDE of variable eruptions in the faithful dataset.
dat = faithful$eruptions
h0 = 0.2
dat_kde = density(dat, bw=h0, from=1, to=6)
plot(dat_kde, lwd=3, col="limegreen", main="KDE with h=0.2")
kde_value = dat_kde$y
# this is the density value at the grid point
n = nrow(faithful)
B = 10000
kde_value_BT = matrix(NA, nrow=B, ncol=length(kde_value))
for(i in 1:B){
  w = sample(n,n,replace=T)
  dat_BT = dat[w]
  kde_value_BT[i,] = density(dat_BT, bw=h0, from=1, to=6)$y
} # get the KDE of the bootstrap sample at each grid point
kde_value_sd = rep(NA, length(kde_value))
for(i in 1:length(kde_value)){
  kde_value_sd[i] = sd(kde_value_BT[,i])
} # compute the SD at each grid point
## making the plot
plot(dat_kde, lwd=3, col="limegreen", main="KDE with h=0.2")
lines(x=dat_kde$x, y=dat_kde$y+qnorm(0.95)*kde_value_sd, lwd=2, col="limegreen", lty=2)
lines(x=dat_kde$x, y=dat_kde$y-qnorm(0.95)*kde_value_sd, lwd=2, col="limegreen", lty=2)



###### using propogate
require(propagate)

## In these examples, 'nsim = 100000' to save
## Rcmd check time (CRAN). It is advocated
## to use at least 'nsim = 1000000' though...
## Example without given degrees-of-freedom.
EXPR1 <- expression(x/y)
x <- c(5, 0.01)
y <- c(1, 0.01)
DF1 <- cbind(x, y)
RES1 <- propagate(expr = EXPR1, data = DF1, type = "stat",
                  do.sim = TRUE, verbose = TRUE,
                  nsim = 100000)
RES1

#####?'
# https://www.youtube.com/watch?v=o-RZaxtPBl8
x <- -10:10 # covariate of known values
b0 <- 0
b1 <- 1.5

# generate data
y <- rnorm(length(x), b0 + b1*x, 10)

x11()
plot(y~x)

lm.1 <- lm(y ~ x)
coef(lm.1)
confint(lm.1)

vcov(lm.1) # this is the covariance b/w parameter estimates 
summary(lm.1)

# plot the fitted line
abline(lm.1, lwd = 3)

x.frame <- data.frame(x = x)

conf.int <- predict(lm.1, interval = "conf", newdata = x.frame)

matlines(x = x.frame$x, y = conf.int, lty = c(1, 2, 2), lwd = 2, col = "red")

SimReg1 <- function(mod.input = lm.1){
  d = coef(mod.input)[1] # intercept from model
  b = coef(mod.input)[2] # slope from model
  rse = summary(mod.input)$sigma # residual standard "error" from model
  y.sim <- rnorm(n = length(x), mean = a + b*x, sd = rse) # generate simulated responses
  lm.sim <- lm(y.sim ~ x) # fit model of simulated responses onto predictor
  coef(lm.sim) # extract coefficients (intercept and slope)
}

# now we run it  amillion times
N <- 1000

# using replicate to repate simulation N times
simulated.coef <- replicate(N, SimReg1())

# for convenience, we transpose the out of simulations to have 2 columnes of intercept and slope
simulated.coef <- t(simulated.coef)

sd(simulated.coef[,1]) # this suld appoximate the standard error for the intercept ()
sd(simulated.coef[,2]) # this should appoximate the standard error of the slope


# compare to 
summary(lm.1)$coef[,1:2]

# approximate monte carlo CIs for the slope
quantile(simulated.coef[,2], c(0.025, 0.975))

# compare to asymptotic CIs for the slope
confint(lm.1)[2,]

# as with any parameter estimates we should consider if those estimated in this simulation are correlated
cor(simulated.coef[,1], simulated.coef[,2])
x11()
plot(simulated.coef[,1], simulated.coef[,2])

# no real covariation b/w intercept and slop in this case'
# I did this on purpose, think about how i did this and why it is unlikely for real data!
x11()
plot(y ~ x)

# plotting the lines using the simulated intercepts and slopes.
for(i in 1:95){
  curve(simulated.coef[i,1] + simulated.coef[i,2] * x, add = T, col = "grey", lwd = 0.8)
}

# add asymptotit normal CIs
matlines(x = x.frame$x, y = conf.int, lty = c(1, 2, 2), lwd = 2, col = "red")

#### correcting our simulation to allow for uncertainty in RSE
# as alluded to, the monte carol CIs generated above may be too narrow, as we have not accounted for 
# uncertainty in the RSE

# correcting our simulation to correctly propogate error (sample from RSE
# and then sample values conditional upon that)

# Remember that RSE we get is just an estimate! It represents a RANDOM sample from a disturbiont
# as well

# rse = residual standard error, df = residual degrees of freedom for model
# we only need slightly modify our original funciton (df.sim, rse.sim)
# see Gelman and Hill for description of how are incoporating uncertainty in the RSE

SimReg2 <- function(mod.input = lm.1){
  d = coef(mod.input)[1] # intercept from model
  b = coef(mod.input)[2] # slope from model
  df.sim <- mod.input$df # residual degrees of freeomd
  rse = summary(mod.input)$sigma # residual standard "error" from model
  rse.sim <- rse * sqrt(df.sim/ rchisq(1, df = df.sim)) # incoroporates UNCERTAINTY in RSE
  y.sim <- rnorm(n = length(x), mean = a + b*x, sd = rse.sim) # generate simulated responses
  lm.sim <- lm(y.sim ~ x) # fit model of simulated responses onto predictor
  coef(lm.sim) # extract coefficients (intercept and slope)
}

# run the simulation as before
regression_simulation_adjusted <- replicate(N, SimReg2())

regression_simulation_adjusted <- t(regression_simulation_adjusted)

# plotting the lines using the simulated intercepts and slopes.
for(i in 1:95){
  curve(regression_simulation_adjusted[i,1] + regression_simulation_adjusted[i,2] * x, 
        add = TRUE, col = "blue", lwd = 0.8)
}

# compares CIs
quantile(regression_simulation_adjusted[,2], c(0.025, 0.975))
quantile(simulated.coef[,2], c(0.025, 0.975))
confint(lm.1)[,2]


# monte carlo

monte_carlo_estimates <- matrix(NA, ncol = 2, nrow = N) # intialiate matrix to store simulated valutes

for(i in 1:N) {
  monte_carlo_estimates[i, ] <- SimReg2()}

hist(monte_carlo_estimates[,2])


##### part two
# https://www.youtube.com/watch?v=QAnhUS3zjiA

# setting a few options for clarity
options(digits = 3, sig.digits = 3, show.signif.stars = TRUE)

# read data into R
dll.data <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.8377/dll.csv",
                     header = TRUE)

dll.data$temp <- factor(dll.data$temp)
dll.data$replicate <- factor(dll.data$replicate)
dll.data$genotype <- relevel(dll.data$genotype, "wt") # set the wild-type (wt) as reference

# remove all empty lines
dll.data <- na.omit(dll.data)

# center our covariate (for interpreting the intercept later on)
dll.data$tarsus.cent <- dll.data$tarsus - mean(dll.data$tarsus)

# plot the data (with some jitter for SCT to improve clarity)
x11()
plot(jitter(dll.data$SCT) ~ dll.data$tarsus.cent, xlab = "tarsus", ylab = "SCT")

dll.model <- lm(SCT ~ 1 + tarsus.cent, data = dll.data)
summary(dll.model)$coef[,1:2]
abline(dll.model, lwd = 3) # this will plot the parameter estimates as a line on the plot
# plot the asmptotic normal CI

tarsus.frame <- data.frame(tarsus.cent = seq(min(dll.data$tarsus.cent), max(dll.data$tarsus.cent),
                           length.out = 100))

# creating a df with "values" of our predictors
# we generate a sequence of 100 numbers (length.out) from min to max value of our covariate.

conf.int <- predict(dll.model, interval = "conf", newdata = tarsus.frame) # generating confidence intervals fo rthe 100 values of tarsus

# here we plot sex comb teeth as a function of tarsus length
par(mfrow = c(1,1))
x11()
plot(dll.data$SCT ~ dll.data$tarsus.cent,
     xlab = "tarsus length", ylab = "no. of sex comb teeth",
     main = "Calculated asymptotit normal CI of observed data")

# here we add the confidence bands to the previous plot
matlines(x = tarsus.frame$tarsus.cent, conf.int, lty = c(1, 2, 2), lwd = 2, col = "red")

# approximate CI for model via monte carlo simulation. Approach simulating the response variable.
# in this version we are not simulating the covariates at all, but using PBSERVED values.
# I think this is generally the easiest apporoach as you do not need to worry about explicitily
# modelling co-variation among the co-variates (and thus parameter estimates) as it is alreadin the real data

SimulationUnderModel <- function(model = dll.model, covariate = dll.data$tarsus.cent) {
  # extrace what we need from the model form which we wish to simulate
  a = coef(model)[1]
  b = coef(model)[2]
  rse = summary(model)$sigma
  df = model$df
  
  rse.sim <- rse * sqrt(df/rchisq(1, df = df)) # incorporate uncertainty in RSE
  # simulate data (response) conditional on the simulated RSE
  y.sim <- rnorm(n = length(covariate), mean = a + b*covariate, sd = rse.sim)
  lm.sim <- lm(y.sim ~ 1 + covariate) # fit model with simulated response
  coef(lm.sim)
}

N = 1000

simulated.coefficients <- replicate(N, SimulationUnderModel())
simulated.coefficients <- t(simulated.coefficients)

# CI both simulated and asymptotic
quantile(simulated.coefficients[,2], c(0.025, 0.975))
confint(dll.model)[2,]



# plotting the lines using the simulated intercepts and slopes.
for(i in 1:95){
  curve(simulated.coefficients[i,1] + simulated.coefficients[i,2] * x, 
        add = T, col = "grey", lwd = 0.8)
}


### lets take a look at the distribtuion of the simulated values vs estimates
x11()
par(mfrow = c(1,2))
hist(simulated.coefficients[,2]) # histogram of simulated slopes
abline(v = coef(dll.model)[2], col = "red", lwd = 2, lty = 2) # estimate slope
hist(simulated.coefficients[,1]) # histogram of simulated intercepts
abline(v = coef(dll.model)[1], col = "red", lwd = 2, lty = 2) # estimate intercepts


# examining t5he covariance b/w parameter estimates
x11()
par(mfrow = c(2,1))
plot(simulated.coefficients[,2] ~ simulated.coefficients[,1],
     main = "covariateion b/w simulated parameter estimates",
     ylab = "simulated slopes",
     xlab = "simulated intercepts")
# scatter plot demonstartein covariation b/w simulated parater values
cov(simulated.coefficients) # covariance b/w simulated parameter values
vcov(dll.model) # estimated covariance b/w parameters
# compare to simulated values also check out var(slope and var(intercept))
