#04_light

light <- read.csv("./data/laserquest_plot_means_with_fPAR.csv")


# making plots line up
stri_sub(light$plotid, 5, 4) <- "_0"

light$plotid <- as.factor(light$plotid)

df <- merge(big.boi, light, all.x = TRUE)
df <- merge(big.boi, light[, c("plotid", "aPAR_mean", "bPAR_mean")], by="plotid")

write.csv(df, "plot_npp_with_fpar.csv")