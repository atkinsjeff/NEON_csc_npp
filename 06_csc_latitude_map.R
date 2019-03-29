df <- plot.csc

require(ggplot2)
require(ggridges)

x11()
ggplot(df, aes(x = rugosity, y = siteid, fill = siteid))+ 
  geom_density_ridges(aes(alpha = .2))+
    theme_classic()

CVI$ID[ELEV=="low" & VEG=="canopy" & PLOT == 3] <-"LC3"

attach(df)
df$lat[siteid == "ARNO"] <- 42.26
df$lat[siteid == "BART"] <- 44.05
df$lat[siteid == "FERN"] <- 39.05
df$lat[siteid == "GRSM"] <- 35.6
df$lat[siteid == "HARV"] <- 42.53
df$lat[siteid == "MLBS"] <- 37.37
df$lat[siteid == "OSBS"] <- 29.68
df$lat[siteid == "RICE"] <- 37.32
df$lat[siteid == "SCBI"] <- 38.89
df$lat[siteid == "SERC"] <- 38.88
df$lat[siteid == "TALL"] <- 32.95
df$lat[siteid == "TREE"] <- 45.49
df$lat[siteid == "UNDE"] <- 46.23
df$lat[siteid == "UVAX"] <- 38.03


# sort by mpg
df <- df[order(-lat),] 
df$siteid2 = factor(df$siteid, levels = unique(df$siteid[order(df$lat, df$siteid)]), ordered=TRUE)

x11()
ggplot(df, aes(x = rugosity, y = siteid2, fill = siteid2))+ 
  geom_density_ridges(aes(alpha = .2), scale = 2)+
  theme_classic()+
  ylab("")+
  xlab("Canopy Rugosity")

plot(df$rugosity, df$lat)

df %>%
  filter(siteid == "FERN") -> FERN

FERN$W <- as.factor(substr(FERN$plotid, 6,7))

x11()
ggplot(FERN, aes(x = rugosity, y = W, fill = W))+ 
  geom_density_ridges(aes(alpha = .2), scale = 2)+
  theme_classic()+
  ylab("")+
  xlab("Canopy Rugosity")

