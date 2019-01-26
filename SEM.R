
##Structural Equation Modelling
## Brady S. Hardiman
## 10/15/2018
# require(sem)

###########################################################
##
##Using only rugosity and LUE/fPAR variables
##
###########################################################
in.file<-"/Users/bhardima/Dropbox/1 Manuscripts/In Progress/NPP-CSC (Gough)/neon_site_means_20181010_ALL_LUE-fPAR.csv"
data.file<-as.data.frame(read.csv(in.file))

z.scores<-as.data.frame(scale(data.file[,2:ncol(data.file)])) ## Use standardized z scores so that resulting beta coefficients are comparable

npp.vs.all<-lm(z.scores$NPP ~  z.scores$fPAR + z.scores$LUE + z.scores$rugosity +z.scores$mean.vai_mean + z.scores$simpsons.taxon_mean) ##Multiple regression on NPP to ID explanatory variables (all variables entered). I played around with this to eva;uate different diversity indices (doesn't make sense to include multiple in the model) and varioaus transformations of fPAR (doesn't make sense to include multiple in the model). The resulting model included LUE only, but I forced fPAR into the model to reflect our knowledge of how the system works.

npp.vs.all<-lm(z.scores$NPP ~  z.scores$fPAR + z.scores$LUE )
summary(npp.vs.all)
foo<-summary(npp.vs.all)
sqrt(1-foo$r.squared) ##Unexplained variation in NPP (U)

summary(lm(z.scores$NPP ~ z.scores$LUE)) #LUE emerges as the only significant explanatory variable for NPP (see above)

lue.vs.notNPP<-lm(z.scores$LUE ~ z.scores$rugosity + z.scores$mean.vai_mean+ z.scores$shannon.taxon_mean)  #Multiple regression on LUE to ID explanatory variables (all remaining variables entered)
summary(lue.vs.notNPP) ##rugosity is only significant predictor of LUE

npp.vs.fpar<-lm(z.scores$NPP ~ z.scores$fPAR)
summary(npp.vs.fpar) ## fPAR is significant against NPP

summary(lm(z.scores$LUE ~ z.scores$mean.vai_mean)) ## NS
summary(lm(z.scores$LUE ~ z.scores$shannon.taxon_mean)) ## NS (I tried both diversity indices)
summary(lm(z.scores$LUE ~ z.scores$fPAR)) ##Significant but weak correlation between LUE and fPAR


summary(lm(z.scores$LUE ~ z.scores$rugosity)) #Rugosity emerges as the only significant explanatory variable for LUE

rug.vs.vaidiv<-lm(z.scores$rugosity ~z.scores$mean.vai_mean )
summary(rug.vs.vaidiv) ## VAI predictes rugosity

fpar.vs.notNPP<-lm(z.scores$log.fpar ~ z.scores$rugosity + z.scores$mean.vai_mean+ z.scores$shannon.taxon_mean + z.scores$LUE)
summary(fpar.vs.notNPP) ##Nothing predicts fPAR in a multiple regression approach...hmm, let's try this with a pairwise approach

summary(lm(z.scores$fPAR ~ z.scores$rugosity)) ##Rugosity is significant against fPAR
summary(lm(z.scores$fPAR ~ z.scores$shannon.taxon_mean)) ##Diversity is significant against fPAR
summary(lm(z.scores$fPAR ~ z.scores$mean.vai_mean)) ##VAI is significant against fPAR


summary(lm(z.scores$rugosity ~ z.scores$mean.vai_mean))  ##VAI is significant against Rugosity
summary(lm(z.scores$rugosity ~ z.scores$shannon.taxon_mean))  ##Diversity is significant against Rugosity
summary(lm(z.scores$mean.vai_mean ~ z.scores$shannon.taxon_mean))  ##Diversity is significant against VAI

