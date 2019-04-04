# NEON NPP
# Jeff Atkins @atkinsjeff jwatkins6@vcu.edu
# This script accompanies Gough et al. In Review "..."

require(plyr)
require(dplyr)
require(tidyverse)
require(corrplot)
require(ggplot2)

# Importing in the NEON tree dbh data
x16 <- read.csv("./data/vst_apparentIndividuals2016_goughSites.csv")

x16 %>%
  filter(measurementheight == 130) %>%
  select(domainid, siteid, plotid, tagid, individualid, plantstatus, stemdiameter) -> n.16

# add year
n.16$year <- 2016

# n.16 %>%
#   filter(plotid == "BART_032") -> jeff.16
# 
# n.17 %>%
#   filter(plotid == "BART_032") -> jeff.17



n.17pt1 <- read.csv("./data/apparentindividual_fulcrum_2017_1st7sites.csv")
n.17pt2 <- read.csv("./data/apparentindividual_fulcrum_2017_2nd7sites.csv")


n.17pt1 %>%
  filter(measurementheight == 130) %>%
  select(domainid, siteid, plotid, tagid, individualid, plantstatus, stemdiameter) -> x17.1

n.17pt2 %>%
  filter(measurementheight == 130) %>%
  select(domainid, siteid, plotid, tagid, individualid, plantstatus, stemdiameter) -> x17.2


n.17 <- rbind(x17.1, x17.2)

# add a year
n.17$year <- 2017

# this binds the two together
df <- rbind(n.16, n.17)

#######################
# #mlbs specific (2015)
mlbs.15 <- read.csv("./data/apparentindividual_L0_2015_MLBS.CSV")
# 
mlbs.15 %>% filter(measurementHeight == 130) %>%
  select(domainID, siteID, plotID,  individualID, plantStatus, stemDiameter) -> mlbs.15

mlbs.15$year <- 2015

mlbs.15$plotid <- substr(mlbs.15$plotID,0, 8)

colnames(mlbs.15)[colnames(mlbs.15) == "siteID"] <- "siteid"
colnames(mlbs.15)[colnames(mlbs.15) == "plantStatus"] <- "plantstatus"
colnames(mlbs.15)[colnames(mlbs.15) == "stemDiameter"] <- "dbh.15"
colnames(mlbs.15)[colnames(mlbs.15) == "domainID"] <- "domainid"
colnames(mlbs.15)[colnames(mlbs.15) == "individualID"] <- "individualid"

mlbs.15 %>%
  select(-"plotID") -> mlbs.15

mlbs.15 %>%
  select(-"year") -> mlbs.15

# # see what is up with mlbs

#### this brings in mapping and tagging information from NEON including taxonid
tree.id <- read.csv("./data/vst_mappingTagging2016_goughSites.csv")
tree.id.mlbs <- read.csv("./data/mapping_fulcrum_MLBS.csv")

# check the sites
unique(tree.id$siteid)
unique(df$siteid)

# adds in the taxon id
# df.mlbs$taxonid <- tree.id.mlbs$taxonID[match(df.mlbs$individualid, tree.id.mlbs$individualID)]
df$taxonid <- tree.id$taxonid[match(df$individualid, tree.id$individualid)]
mlbs.15$taxonid <- tree.id.mlbs$taxonid[match(mlbs.15$individualid, tree.id.mlbs$individualid)]

#check for dups
df <- df[!duplicated(df[, c(5,8)]), ]
#df.mlbs <- df.mlbs[!duplicated(df.mlbs[, c(4)]), ] 

# create the wide data frame for NPP
npp <- spread(df, year, stemdiameter)


##### MORE MLBS
npp %>%
  filter(siteid == "MLBS") -> npp.mlbs

npp.mlbs$taxonid <- tree.id.mlbs$taxonid[match(npp.mlbs$individualid, tree.id.mlbs$individualid)]
npp.mlbs <- merge(npp.mlbs, mlbs.15[,c("dbh.15", "individualid")], by = "individualid")


#bring in jenkins model from jenkins et al. 2003

jenkins_model <- c("aspen/alder/cottonwood/willow", "soft maple/birch", "mixed hardwood", "hard maple/oak/hickory/beech", "cedar/larch", "doug fir", "fir/hemlock", "pine", "spruce", "juniper/oak/mesquite")

model_name <- c("hw1", "hw2", "hw3", "hw4", "sw1", "sw2", "sw3", "sw4", "sw5", "wl")

beta_one <- c(-2.20294, -1.9123, -2.4800, -2.0127, -2.0336, -2.2304, -2.5384, -2.5356, -2.0773, -0.7152)

beta_two <- c(2.3867, 2.3651, 2.4835, 2.4342, 2.2592, 2.4435, 2.4814, 2.4349, 2.3323, 1.7029)

jenkins <- data.frame(jenkins_model, model_name, beta_one, beta_two)

tax.jenk <- read.csv("./data/neon_taxon_jenkins.csv")

jenkins_plus <- merge(tax.jenk, jenkins)

df2 <- merge(npp, jenkins_plus, by = "taxonid", all = TRUE)
df2.mlbs <- merge(npp.mlbs, jenkins_plus, by = "taxonid", all = TRUE)

# filters out plant status
df2 %>%
  filter(plantstatus == 1 | plantstatus == 4 | plantstatus == 5 | plantstatus == 7 | plantstatus == 9 ) -> df3
# rename columns
colnames(df3)[colnames(df3) == "2016"] <-  "dbh16"
colnames(df3)[colnames(df3) == "2017"] <-  "dbh17"

df2.mlbs %>%
  filter(plantstatus == 1 | plantstatus == 4 | plantstatus == 5 | plantstatus == 7 | plantstatus == 9 ) -> df3.mlbs
# rename columns

colnames(df3.mlbs)[9] <- "dbh17"
###############
#### cleaning scripts
# BART_037 BART NEON.PLA.D01.BART.05279' list as 0
df3$dbh17[df3$plotid == "BART_037" & df3$individualid == "NEON.PLA.D01.BART.05279"] <- 0
# BART_050 NEON.PLA.D01.BART.03310 dbh.16 = 34.0, dbh.17 = 43.1 (dbh.17 = 34.1)
df3$dbh17[df3$plotid == "BART_050" & df3$individualid == "NEON.PLA.D01.BART.03310"] <- 34.1
# HARV_049 NEON.PLA.D01.HARV.08042 list both as 0
df3$dbh17[df3$plotid == "HARV_049" & df3$individualid == "NEON.PLA.D01.HARV.08042"] <- 0
# HARV_43 NEON.PLA.D01.HARV.04958 dbh.16 = 51.2, dbh.17 = 59.8 (change dbh.17 to 51.8)
df3$dbh17[df3$plotid == "HARV_043" & df3$individualid == "NEON.PLA.D01.HARV.04958"] <- 51.8
# OSBS_028 NEON.PLA.D03.OSBS.02510 dbh 17 should be 28.4 (dbh.17 = 28.4)
df3$dbh17[df3$plotid == "OSBS_028" & df3$individualid == "NEON.PLA.D03.OSBS.02510"] <- 28.4
# TREE_039 NEON.PLA.D05.TREE.01468 dbh 17 should be 18.3 (dbh.17 = 18.3)
df3$dbh17[df3$plotid == "TREE_039" & df3$individualid == "NEON.PLA.D05.TREE.01468"] <- 18.3
# MLBS_075 NEON.PLA.D07.MLBS.00444 dbh.17 = 29.5, dbh.15 = 9.0 (dbh.17 = 9.5)
df3.mlbs$dbh17[df3.mlbs$plotid == "MLBS_075" & df3.mlbs$individualid == "NEON.PLA.D07.MLBS.00444"] <- 9.5
# GRSM_52 NEON.PLA.D07.GRSM.00696  dbh16 = 11.3, dbh.17 = 60.3 (dbh.17 = 0)
df3$dbh17[df3$plotid == "GRSM_052" & df3$individualid == "NEON.PLA.D07.GRSM.00696"] <- 0
# SCBI_50 NEON.PLA.D02.SCBI.05072 dbh.16 = 27.8, dbh.17 = 48.6 (dbh.17 = 28.6)
df3$dbh17[df3$plotid == "SCBI_50" & df3$individualid == "NEON.PLA.D02.SCBI.05072"] <- 28.6
# TALL_047 NEON.PLA.D08.TALL.00217 dbh.16 = 14.0, dbh.17 = 40.4 (dbh.17 = 14.4)
df3$dbh17[df3$plotid == "TALL_047" & df3$individualid == "NEON.PLA.D08.TALL.00217"] <- 14.4
# TALL_047 NEON.PLA.D08.TALL.00236 dbh.16 = 25.7, dbh.17 = 36.1 (dbh.17 = 26.1)
df3$dbh17[df3$plotid == "TALL_047" & df3$individualid == "NEON.PLA.D08.TALL.00236"] <- 26.1
# TREE_046 NEON.PLA.D05.TREE.00617 dbh.16 = 34.5, dbh.17 = 54.9 (dbh.17 = 34.9)
df3$dbh17[df3$plotid == "TREE_046" & df3$individualid == "NEON.PLA.D05.TREE.00617"] <- 34.9
# UNDE_055 NEON.PLA.D05.UNDE.01743  dbh.16 = 36.5, dbh.17 = 46.3 
df3$dbh17[df3$plotid == "UNDE_055" & df3$individualid == "NEON.PLA.D05.UNDE.01743"] <- 34.9

###########
#biomass

df3$b2016 <- exp(df3$beta_one + (df3$beta_two * log(df3$dbh16)))
df3$b2017 <- exp(df3$beta_one + (df3$beta_two * log(df3$dbh17)))
df3$growth <- df3$b2017 - df3$b2016

#### MLBS
df3.mlbs$b2015 <- exp(df3.mlbs$beta_one + (df3.mlbs$beta_two * log(df3.mlbs$dbh.15)))
df3.mlbs$b2017 <- exp(df3.mlbs$beta_one + (df3.mlbs$beta_two * log(df3.mlbs$dbh17)))
df3.mlbs$growth <- df3.mlbs$b2017 - df3.mlbs$b2015

# gotta half it
df3.mlbs$growth <- df3.mlbs$growth / 2


#######################################
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

#write.csv(plot.npp, "neon_plot_npp.csv")

### bring in canopy structural complexity data
neon.csc <- read.csv("./data/laserquest_pcl_transects_for_neon_2018.csv")
neon.csc %>% 
  group_by(plotid, siteid) %>%
  summarize_all(funs(mean)) -> neon.csc2

neon.csc2 <-data.frame(neon.csc2)
plot.csc <- neon.csc2

require(stringi)

#fix zeros
stri_sub(plot.csc$plotid, 6, 5) <- 0

#combine
big.boi <- merge(plot.npp, plot.csc)

#filter out zeros
big.boi %>%
  filter(npp > 0) -> big.boi

# npp and rugosity plot

##### graphing
npp.label <- expression(paste("NPP"[Wood]~"(Mg Ha"^-1~")"))


x11()
ggplot(big.boi, aes(x = rugosity, y = npp))+
  geom_point(aes(color=siteid), size = 4)+
  geom_point(shape = 1, size = 4, color = "black")+
  ylab(npp.label)+
  xlab("Canopy Rugosity (m)")+
  theme_classic()+
  stat_smooth(method = "lm", se = FALSE, color = "black")+
  theme(legend.title=element_blank())+
  theme(legend.justification=c(1,0), legend.position=c(1,0))

lm.rc <- lm(npp ~ rugosity, data = big.boi)
lm.rump <- lm(npp ~ rumple, data = big.boi)
lm.mst <- lm(npp ~ mean.std_mean, data = big.boi)

#### correlation matrix
x <- big.boi[c(2, 6:33)]

m <- cor(x)
corrplot(m)
#######


#### making site means
big.boi %>%
  group_by(siteid) %>%
  summarise_all(funs(mean, sd)) -> site.means

big.boi %>%
  group_by(siteid) %>%
  count(siteid) -> site.count

site.means <- data.frame(site.means)

site.means$n <-site.count$n

head(site.means)

write.csv(site.means, "./summary/neon_csc_npp_means.csv")
write.csv(plot.npp, "./summary/neon_plot_npp.csv")


# get a SE for rugosity
site.means$rugosity_se <- site.means$rugosity_sd / sqrt(site.means$n)
site.means$npp_se <- site.means$npp_sd / sqrt(site.means$n)
site.means$vai_se <- site.means$mean.vai_sd / sqrt(site.means$n)

# colors
# The palette with black:
#cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)

ggplot(site.means, aes(x = rugosity_mean, y = npp_mean))+
  geom_errorbarh(aes(xmin = rugosity_mean - rugosity_se, xmax = rugosity_mean + rugosity_se))+
  geom_errorbar(aes(ymin = npp_mean - npp_se, ymax = npp_mean + npp_se))+
  geom_point(aes(color=siteid), size = 5)+
  geom_point(shape = 1, size = 5, color = "black")+
  #scale_colour_manual(values=cbPalette)+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  theme(legend.title=element_blank())+
  ylab(npp.label)+
  xlab("Canopy Rugosity (m)")+
  stat_smooth(method = "lm", se = FALSE)


ggplot(site.means, aes(x = mean.vai_mean, y = npp_mean))+
  geom_errorbarh(aes(xmin = mean.vai_mean - vai_se, xmax = mean.vai_mean + vai_se))+
  geom_errorbar(aes(ymin = npp_mean - npp_se, ymax = npp_mean + npp_se))+
  geom_point(aes(color=siteid), size = 5)+
  geom_point(shape = 1, size = 5, color = "black")+
  #scale_colour_manual(values=cbPalette)+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  theme(legend.title=element_blank())+
  ylab(npp.label)+
  xlab("VAI")

ggsave("npp_rugosity.png", units="in", width=5, height= 5, dpi=300)
png('npp_rugosity.png', units="in", width=5, height=5, res=300)
#insert ggplot code
dev.off()

lm.rc.site <- lm(npp_mean ~ rugosity_mean, data = site.means)
lm.vai.site <- lm(npp_mean ~ mean.vai_mean, data = site.means)

summary(lm.rc.site)
m <- 
  nls.rc.site <- nls(npp_mean ~ rugosity_mean, data = site.means)


####
div.lite <- site.diversity[,c(1, 3:12)]

div.lite %>%
  filter(!siteid == "MLBS") -> div.lite2
site.all <- cbind(site.means, div.lite2)

###
ggplot(site.all, aes(x = shannon.genus_mean, y = npp_mean))+
  # geom_errorbarh(aes(xmin = rugosity_mean - rugosity_se, xmax = rugosity_mean + rugosity_se))+
  # geom_errorbar(aes(ymin = npp_mean - npp_se, ymax = npp_mean + npp_se))+
  geom_point(aes(color=siteid), size = 5)+
  geom_point(shape = 1, size = 5, color = "black")+
  scale_colour_manual(values=cbPalette)+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  theme(legend.title=element_blank())+
  ylab(npp.label)+
  xlab("Shannon Diversity Index")+
  stat_smooth(method = "lm", se = FALSE)

lm.shannon <- lm(npp_mean ~ shannon.genus_mean, data = site.all)
summary(lm.shannon)

ggplot(site.all, aes(y = rugosity_mean, x = shannon.genus_mean))+
  # geom_errorbarh(aes(xmin = rugosity_mean - rugosity_se, xmax = rugosity_mean + rugosity_se))+
  # geom_errorbar(aes(ymin = npp_mean - npp_se, ymax = npp_mean + npp_se))+
  geom_point(aes(color=siteid), size = 5)+
  geom_point(shape = 1, size = 5, color = "black")+
  scale_colour_manual(values=cbPalette)+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  theme(legend.title=element_blank())+
  xlab("Shannon Diversity Index")+
  ylab("Canopy Rugosity (m)")+
  stat_smooth(method = "lm", se = FALSE)

lm.shan.rc <- lm(rugosity_mean ~ shannon.genus_mean, data = site.all)
summary(lm.shan.rc)

write.csv(site.all, "neon_csc_npp_means.csv")

