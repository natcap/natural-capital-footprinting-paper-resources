### Create plots of Greenbushes, Australia lithium mine impacts through time
# Load libraries
library(ggplot2)
library(ggpubr)
library(cowplot)
library(reporter)
library(magrittr)

# Import Greenbushes mine data
Greenbushes_trends <- read.csv("./Greenbushes_Impact_trends_PUBLIC.csv")

# Plot change in absolute impact
dAbsolute <- ggplot(Greenbushes_trends, aes(x=footprint_Q1_of_year)) +
  geom_line(aes(y = changeProd*100), linewidth = 1, linetype = 2, color = "#000000") +
  geom_line(aes(y = changeArea*100), linewidth = 1, color = "#D55E00") +
  geom_line(aes(y = changeN*100), linewidth = 1, color = "#56B4E9") +
  geom_line(aes(y = changeSed*100), linewidth = 1, color = "#009E73") +
  geom_line(aes(y = changeNature*100), linewidth = 1, color = "#000000") +
  geom_line(aes(y = changeEndemic*100), linewidth = 1, color = "#0072B2") +
  geom_line(aes(y = changeRedlist*100), linewidth = 1, color = "#E69F00") +
  geom_line(aes(y = changeRich*100), linewidth = 1, color = "#CC79A7") +
  ggtitle("Absolute impact") +  
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        axis.text.x = element_text(size = 26, color = "black"),
        axis.text.y = element_text(size = 26, color = "black")) +
  xlim(2017, 2023) +
  xlab("") +
  ylim(-1, 145) +
  ylab("")

dAbsolute

# Plot change in impact per tonne (production)
d_PER_tonne <- ggplot(Greenbushes_trends, aes(x=footprint_Q1_of_year)) +
  geom_line(aes(y = changeArea_PER_ton*100), linewidth = 1, color = "#D55E00") +
  geom_line(aes(y = changeN_PER_ton*100), linewidth = 1, color = "#56B4E9") +
  geom_line(aes(y = changeSed_PER_ton*100), linewidth = 1, color = "#009E73") +
  geom_line(aes(y = changeNature_PER_ton*100), linewidth = 1, color = "#000000") +
  geom_line(aes(y = changeEndemic_PER_ton*100), linewidth = 1, color = "#0072B2") +
  geom_line(aes(y = changeRedlist_PER_ton*100), linewidth = 1, color = "#E69F00") +
  geom_line(aes(y = changeRich_PER_ton*100), linewidth = 1, color = "#CC79A7") +
  ggtitle("Impact per tonne") +  
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        axis.text.x = element_text(size = 26, color = "black"),
        axis.text.y = element_text(size = 26, color = "black")) +
  xlim(2017, 2023) +
  xlab("") +
  ylim(-30, 50) +
  ylab("")

d_PER_tonne

# Plot change in impact per area (km^2)
d_PER_km2 <- ggplot(Greenbushes_trends, aes(x=footprint_Q1_of_year)) +
  geom_line(aes(y = percChangeN_PER_km2), linewidth = 1, color = "#56B4E9") +
  geom_line(aes(y = percChangeSed_PER_km2), linewidth = 1, color = "#009E73") +
  geom_line(aes(y = percChangeNature_PER_km2), linewidth = 1, color = "#000000") +
  geom_line(aes(y = percChangeEndemic_PER_km2), linewidth = 1, color = "#0072B2") +
  geom_line(aes(y = percChangeRedlist_PER_km2), linewidth = 1, color = "#E69F00") +
  geom_line(aes(y = percChangeRich_PER_km2), linewidth = 1, color = "#CC79A7") +
  ggtitle(expression("Impact per km"^"2")) +  
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        axis.text.x = element_text(size = 26, color = "black"),
        axis.text.y = element_text(size = 26, color = "black")) +
  xlim(2017, 2023) +
  xlab("") +
  ylim(-6, 8) +
  ylab("")

d_PER_km2

# Create legend
names <- c('Production (tonnes)', 'Area', 'Nitrogen retention', 'Sediment retention', 'Nature access', 'Endemic species', 'Red List species', 'Species richness')
clrs <- c('#000000', '#D55E00', '#56B4E9', '#009E73', '#000000', '#0072B2', '#E69F00', '#CC79A7')
ltype2 <- c(2,1)
ltype <- c(1, 1)
plot(NULL ,xaxt = 'n', yaxt='n', bty='n', ylab='', xlab='', xlim = 0:1, ylim = 0:1)
lgnd <- legend("topleft", title = "", legend = names, lty=c(ltype2, ltype, ltype, ltype, ltype, ltype, ltype, ltype), lwd = 2, cex = 1,
               bty='n', col = clrs)
