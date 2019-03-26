# NEON Diversity script
# Jeff Atkins @atkinsjeff jwatkins6@vcu.edu
# This script accompanies Gough et al. In Review "..."

require(plyr)
require(dplyr)
require(tidyr)
require(vegan)
require(ggplot2)
require(corrplot)

# import the 2017 stem data
trees.1 <- read.csv("./data/apparentindividual_fulcrum_2017_1st7sites.csv")
trees.2 <- read.csv("./data/apparentindividual_fulcrum_2017_2nd7sites.csv")

# look at the tops
head(trees.1)
head(trees.2)

dim(trees.1)
dim(trees.2)

# combined
trees <- rbind(trees.1, trees.2)


df <- read.csv("./data/vst_mappingTagging2016_goughSites.csv")
mlbs <- read.csv("./data/mapping_fulcrum_MLBS.csv")

head(df)

##### Adding taxon data
tax.jenk <- read.csv("./data/neon_taxon_jenkins.csv")

df <- merge(df, tax.jenk[, c("taxonid", "Genus", "Family")], by="taxonid", all.x = TRUE)

# merge in with mlbs data too
mlbs.combo <- merge(mlbs, tax.jenk[, c("taxonid", "Genus", "Family")], by="taxonid", all.x = TRUE)

mlbs.combo %>%
  select(siteid, plotid, individualid, taxonid, Genus, Family) -> mlbs.species
# sort to just a df of the plotID and taxonid

df %>%
  select(siteid, plotid, individualid, taxonid, Genus, Family) -> df.species

df.species <- rbind(df.species, mlbs.species)

trees %>% select(siteid, plotid, individualid, stemdiameter) -> tree.short

df.combo <- merge(df.species, tree.short[, c("individualid", "stemdiameter")], by = "individualid", all.x = TRUE)

# holy shit it worked!
df.combo$ba <- pi * (df.combo$stemdiameter / 2) 




# check the dim  should be ~29993
dim(df.combo)

# BA by species by plot
df.combo %>%
  group_by(plotid, taxonid) %>%
  summarize(ba.sum = sum(ba, na.rm = TRUE)) -> df.ba.taxon

df.combo %>%
  group_by(plotid, Genus) %>%
  summarize(ba.sum = sum(ba, na.rm = TRUE)) -> df.ba.genus

df.combo %>%
  group_by(plotid, Family) %>%
  summarize(ba.sum = sum(ba, na.rm = TRUE)) -> df.ba.family

# species by plot
df.combo %>%
  group_by(plotid, taxonid) %>%
  count(taxonid) -> df.count

df.ba.taxon <- data.frame(df.ba.taxon)
df.ba.genus <- data.frame(df.ba.genus)
df.ba.family <- data.frame(df.ba.family)


# Genus by plot
df.combo %>%
  group_by(plotid, siteid) %>%
  count(genus.rich = n_distinct(Genus)) -> df.genus.richness

# Genus by plot
df.combo %>%
  group_by(plotid, siteid) %>%
  count(family.rich = n_distinct(Family)) -> df.family.richness

# richness
df.combo %>%
  group_by(plotid, siteid) %>%
  summarize(richness = n_distinct(taxonid)) -> plot.diversity

#################
# OK now we need make wide for vegan
df.ba.taxon %>%
  spread(taxonid, ba.sum) -> df.ba.wide
df.ba.wide <- data.frame(df.ba.wide)

#check the dimensions
dim(df.ba.wide)

df.ba.wide[is.na(df.ba.wide)] <- 0

df.ba <- df.ba.wide[-1,]

#vegan time
plot.diversity$simpsons.taxon <- diversity(df.ba.wide[,3:247], index = "simpson")
plot.diversity$shannon.taxon <- diversity(df.ba.wide[,3:247], index = "shannon")

#################
# OK now we need make wide for vegan
df.ba.genus %>%
  spread(Genus, ba.sum) -> df.ba.genus.wide
df.ba.genus.wide <- data.frame(df.ba.genus.wide)

#check the dimensions
dim(df.ba.genus.wide)

df.ba.genus.wide[is.na(df.ba.genus.wide)] <- 0

#vegan time
plot.diversity$simpsons.genus <- diversity(df.ba.genus.wide[,2:64], index = "simpson")
plot.diversity$shannon.genus <- diversity(df.ba.genus.wide[,2:64], index = "shannon")

#################
# OK now we need make wide for vegan
df.ba.family %>%
  spread(Family, ba.sum) -> df.ba.fam.wide
df.ba.fam.wide <- data.frame(df.ba.fam.wide)

#check the dimensions
dim(df.ba.fam.wide)

df.ba.fam.wide[is.na(df.ba.fam.wide)] <- 0

#vegan time
plot.diversity$simpsons.fam <- diversity(df.ba.fam.wide[,2:37], index = "simpson")
plot.diversity$shannon.fam <- diversity(df.ba.fam.wide[,2:37], index = "shannon")


# recombine
plot.diversity <- data.frame(plot.diversity)
plot.diversity <- merge(plot.diversity, df.genus.richness)
plot.diversity <- merge(plot.diversity, df.family.richness)

#write.csv(plot.diversity, "./summary/neon_plot_diversity.csv")

### bring in CSC
neon.csc <- read.csv("./data/laserquest_pcl_transects_for_neon_2018.csv")
neon.csc2 <- neon.csc
colnames(neon.csc2)[colnames(neon.csc2) == "siteID"] <- "siteid"
colnames(neon.csc2)[colnames(neon.csc2) == "plotID"] <- "plotid"
#isolate rugosity
neon.csc2 %>%
  group_by(plotid, siteid) %>%
  summarize(rugosity = mean(rugosity), moch = mean(mean.max.ht)) -> plot.rc2

neon.csc2 %>%
  group_by(plotid, siteid) %>%
  summarize_all(mean)-> plot.csc

plot.csc <- data.frame(plot.csc)


#adds that 0
require(stringi)

#fix zeros
stri_sub(plot.csc$plotid, 6, 5) <- 0
stri_sub(plot.csc2$plotid, 6, 5) <- 0
#remove the bad columns
plot.csc %>%
  select(-c( transect.length, plot_1) ) -> plot.csc.clean

#checking mean rugosity values
plot.csc.clean %>%
  group_by(siteid) %>%
  summarize(mean(rugosity))

plot.diversity <- merge(plot.diversity, plot.csc.clean)
write.csv(plot.diversity, "./summary/neon_plot_diversity_csc.csv")

# run a correlation plot here.
require(corrplot)
m <- cor(plot.diversity[,4:39])
corrplot(m, method = "circle")

# normalized CSC

plot.diversity$normal <- plot.diversity$rugosity / plot.diversity$moch

ggplot(plot.diversity, aes(y = rugosity, x = simpsons.taxon))+
  geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank())+
  theme(axis.title = element_text(face="bold", colour="black", size=20),
        axis.text  = element_text(color = "black", size = 16),
        axis.line.x=element_line(color = "black"),
        axis.line.y=element_line(color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="black", size=10, 
                                   face="bold"),
        legend.title = element_blank(),
        legend.position = "right")+
  xlab("Simpson's Index (Taxon)")+
  ylab("Canopy Rugosity")

ggplot(plot.diversity, aes(y = rugosity, x = simpsons.genus))+
  geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank())+
  theme(axis.title = element_text(face="bold", colour="black", size=20),
        axis.text  = element_text(color = "black", size = 16),
        axis.line.x=element_line(color = "black"),
        axis.line.y=element_line(color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="black", size=10, 
                                   face="bold"),
        legend.title = element_blank(),
        legend.position = "right")+
  xlab("Simpson's Index (Genus)")+
  ylab("Canopy Rugosity")

ggplot(plot.diversity, aes(y = rugosity, x = simpsons.fam))+
  geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank())+
  theme(axis.title = element_text(face="bold", colour="black", size=20),
        axis.text  = element_text(color = "black", size = 16),
        axis.line.x=element_line(color = "black"),
        axis.line.y=element_line(color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="black", size=10, 
                                   face="bold"),
        legend.title = element_blank(),
        legend.position = "right")+
  xlab("Simpson's Index (Family)")+
  ylab("Canopy Rugosity")

######

### do site level
plot.diversity %>%
  group_by(siteid) %>%
  summarize_each(funs(mean, sd)) -> site.diversity


site.diversity <- data.frame(site.diversity)
site.diversity$normal <- site.diversity$rugosity_mean / site.diversity$max.ht_mean

ggplot(site.diversity, aes(x = genus.rich_mean, y = rugosity_mean, color = siteid))+
  geom_point(size = 4)+
  # geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
  #                    ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
  # geom_errorbarh( aes(xmin = simpsons.taxon_mean - (simpsons.taxon_sd / (sqrt(7))), 
  #                     xmax = simpsons.taxon_mean + (simpsons.taxon_sd / (sqrt(7)))))+
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank())+
  theme(axis.title = element_text(face="bold", colour="black", size=20),
        axis.text  = element_text(color = "black", size = 16),
        axis.line.x=element_line(color = "black"),
        axis.line.y=element_line(color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="black", size=10, 
                                   face="bold"),
        legend.title = element_blank(),
        legend.position = "right")+
  xlab("Genus richness")+
  ylab("Rugosity")


ggplot(site.diversity, aes(x = simpsons.genus_mean, y = rugosity_mean, color = siteid))+
  geom_point(size = 4)+
  geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                     ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
  geom_errorbarh( aes(xmin = simpsons.genus_mean - (simpsons.genus_sd / (sqrt(7))), 
                      xmax = simpsons.genus_mean + (simpsons.genus_sd / (sqrt(7)))))+
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank())+
  theme(axis.title = element_text(face="bold", colour="black", size=20),
        axis.text  = element_text(color = "black", size = 16),
        axis.line.x=element_line(color = "black"),
        axis.line.y=element_line(color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="black", size=10, 
                                   face="bold"),
        legend.title = element_blank(),
        legend.position = "right")+
  xlab("Simpson's Index (Genus)")+
  ylab("Rugosity")

ggplot(site.diversity, aes(x = simpsons.fam_mean, y = rugosity_mean, color = siteid))+
  geom_point(size = 4)+
  geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                     ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
  geom_errorbarh( aes(xmin = simpsons.fam_mean - (simpsons.fam_sd / (sqrt(7))), 
                      xmax = simpsons.fam_mean + (simpsons.fam_sd / (sqrt(7)))))+
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank())+
  theme(axis.title = element_text(face="bold", colour="black", size=20),
        axis.text  = element_text(color = "black", size = 16),
        axis.line.x=element_line(color = "black"),
        axis.line.y=element_line(color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="black", size=10, 
                                   face="bold"),
        legend.title = element_blank(),
        legend.position = "right")+
  xlab("Simpson's Index (Family)")+
  ylab("Rugosity")


ggplot(site.diversity, aes(x = richness_mean, y = rugosity_mean, color = siteid))+
  geom_point(size = 4)+
  geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                     ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
  geom_errorbarh( aes(xmin = richness_mean - (richness_sd / (sqrt(7))), 
                      xmax = richness_mean + (richness_sd / (sqrt(7)))))+
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank())+
  theme(axis.title = element_text(face="bold", colour="black", size=20),
        axis.text  = element_text(color = "black", size = 16),
        axis.line.x=element_line(color = "black"),
        axis.line.y=element_line(color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="black", size=10, 
                                   face="bold"),
        legend.title = element_blank(),
        legend.position = "right")+
  xlab("Species Richness")+
  ylab("Rugosity")


ggplot(site.diversity, aes(x = n_mean, y = rugosity_mean, color = siteid))+
  geom_point(size = 4)+
  geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                     ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
  geom_errorbarh( aes(xmin = n_mean - (n_sd / (sqrt(7))), 
                      xmax = n_mean + (n_sd / (sqrt(7)))))+
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank())+
  theme(axis.title = element_text(face="bold", colour="black", size=20),
        axis.text  = element_text(color = "black", size = 16),
        axis.line.x=element_line(color = "black"),
        axis.line.y=element_line(color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="black", size=10, 
                                   face="bold"),
        legend.title = element_blank(),
        legend.position = "right")+
  xlab("No. of Individuals")+
  ylab("Rugosity")
+
  geom_smooth(method = "lm", se = FALSE)

ggplot(site.diversity, aes(x = genus.rich, y = rugosity, color = siteid))+
  geom_point(size = 4)+
  ylab("Genus Richness")+
  xlab("Rugosity")+
  stat_smooth(method = "lm", se = FALSE)


#####
write.csv(site.diversity, "./summary/neon_site_diversity.csv")


#################### bayesian change point detection
require(bcp)
x <- plot.diversity$simpsons.taxon
y <- plot.diversity$rugosity

bcp.rc <- bcp(y, x)
residuals(bcp.rc)
summary(bcp.rc)
plot(bcp.rc)

#####
summary(lm(rugosity_mean ~ genus.rich_mean, data = site.diversity))
summary(lm(rugosity_mean ~ simpsons.genus_mean, data = site.diversity))
summary(lm(rugosity_mean ~ shannon_mean, data = site.diversity))
summary(lm(rugosity_mean ~ richness_mean, data = site.diversity))
summary(lm(rugosity_mean ~ n_mean, data = site.diversity))


