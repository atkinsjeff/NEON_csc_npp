#Rice maps
require(maps)
require(mapdata)
require(maptools)
require(scales)
require(ggplot2)
require(ggmap)
require(ggrepel)
df <- read.csv("./data/site_table_gis.csv")

df <- subset(df, df$nature == "y")



states <- map_data("state")

eastcoast <- subset(states, region %in% c("maine","new hampshire", "massachusetts","vermont", "new york", "connecticut", "rhode island", "pennsylvania", "delaware", "new jersey","maryland", "virginia", "ohio", "north carolina", "south carolina", "georgia", "florida", "alabama", "indiana", "louisiana", "illinois", "wisconsin", "michigan", "tennessee", "kentucky", "west virginia", "missouri", "arkansas", "mississippi", "iowa", "minnesota", "lousiana"))


p.map <- ggplot() + geom_polygon(data = eastcoast, aes(x=long, y = lat, group = group), color = "white", fill = "light grey") + 
     coord_fixed(1.3)+
     scale_fill_manual(values=cbPalette)+
     theme_bw()+
     theme(legend.position="none")+
     geom_point(data = df, aes(x = long, y = lat, fill = nature), color = "black", pch = 21, size = 3)+
     geom_text_repel(data = df, aes(label = paste(" ", as.character(site_code), sep = ""), x = long, y = lat), angle = 0, hjust = 0)+
     xlab("Longitude")+
     ylab("Latitude")

ggsave("./summary/site_map.png", p.map, width = 8, height = 6, units = "in", dpi = 600, bg = "transparent")
