#####heat map of correlations of CSC to diversity indices

require(corrplot)
require(plyr)
require(dplyr)
require(tidyverse)
require(lessR)

df <- read.csv("./summary/neon_site_diversity.csv")

df %>%
  select(simpsons.taxon_mean, shannon.taxon_mean, richness_mean, mean.vai_mean, mean.max.ht_mean, 
         clumping.index_mean, porosity_mean, sky.fraction_mean, cover.fraction_mean, rugosity_mean, top.rugosity_mean, rumple_mean) -> m

#
m.cor <- cor(m) 
m.cov <- cov(m)

# matrix of the p-value of the correlation
p.mat <- cor.mtest(m)

#reorganizing to match figure 2 in manuscript
mycor <- corReorder(m.cor, vars=c(mean.vai_mean, mean.max.ht_mean, clumping.index_mean, porosity_mean, sky.fraction_mean, cover.fraction_mean,
                           rugosity_mean, top.rugosity_mean, rumple_mean, richness_mean, simpsons.taxon_mean, shannon.taxon_mean ))

#corrplot
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
x11()
corrplot(mycor, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45)
