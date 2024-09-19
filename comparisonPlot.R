### Figure creation for manuscript
# Load libraries
library(ggplot2)
library(ggpubr)
library(scales)

# Import data
compare <- read.csv("./lithiumMineComparisonPlot.csv")

# Figure 4a: total impacts comparison
# Plot nitrogen retention
N <- ggplot(compare, aes(x=N_bufferedPt, y=N_polygon)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Nitrogen Retention") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) + 
  scale_x_continuous(breaks = c(0, 1000000000, 2000000000), label=c("0", "1.0e+9", "2.0e+9")) + 
  scale_y_continuous(breaks = c(0, 5000000000, 10000000000, 15000000000, 20000000000), label=c("0", "5.0e+9", "1.0e+10", "1.5e+10", "2.0e+10")) + 
  xlab("") + 
  ylab("")

# Plot sediment retention
sed <- ggplot(compare, aes(x=sed_bufferedPt, y=sed_polygon)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Sediment Retention") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_continuous(breaks = c(0, 1000000000, 2000000000), label=c("0", "1.0e+9","2.0e+9")) + 
  scale_y_continuous(breaks = c(0, 20000000000, 40000000000, 60000000000, 80000000000), label=c("0", "2.0e+10", "4.0e+10", "6.0e+10", "8.0e+10")) + 
  xlab("") + 
  ylab("")

# Plot nature access
nature <- ggplot(compare, aes(x=nature_bufferedPt, y=nature_polygon)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Nature Access") +  
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_continuous(breaks = c(0, 10000, 20000, 30000), label=c("0", "10,000", "20,000", "30,000")) + 
  scale_y_continuous(breaks = c(0, 1000000, 2000000, 3000000, 4000000), label=c("0", "100,000", "200,000", "300,000", "400,000")) + 
  xlab("") + 
  ylab("")

# Plot endemic species
endemic <- ggplot(compare, aes(x=endemic_bufferedPt, y=endemic_polygon)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Endemic Species") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_continuous(breaks = c(0, 0.00005, 0.0001), label=c("0", "5.0e-5", "1.0e-4")) + 
  scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.015), label=c("0", "0.005", "0.010", "0.015")) + 
  xlab("") +
  ylab("")

# Plot Red List species
redlist <- ggplot(compare, aes(x=redlist_bufferedPt, y=redlist_polygon)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Red List Species") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125), label=c("0", "25", "50", "75", "100", "125")) + 
  scale_y_continuous(breaks = c(0, 50000, 100000, 150000, 200000, 250000), label=c("0", "50,000", "100,000", "150,000", "200,000", "250,000")) + 
  xlim(0, 125) +
  xlab("") + 
  ylab("")

# Plot species richness
richness <- ggplot(compare, aes(x=richness_bufferedPt, y=richness_polygon)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Species Richness") +   
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), label=c("0", "0.1", "0.2", "0.3", "0.4", "0.5")) + 
  scale_y_continuous(breaks = c(0, 100, 200, 300), label=c("0", "100", "200", "300")) + 
  xlab("") + 
  ylab("")

# Arrange 6 plots
ggarrange(N, sed, nature, endemic, redlist, richness)

#################################
# Figure 4b: Total impact rank comparison
# Import data
rank <- read.csv("./lithiumMineComparisonPlot_Rank.csv")

# Plot nitrogen retention rank
N_rank <- ggplot(rank, aes(x=N_bufferedPt_Rank, y=N_polygon_Rank)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Nitrogen Retention") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlim(23, 1) +
  ylim(23, 1) +
  xlab("") +
  ylab("")

# Plot sediment retention rank
sed_rank <- ggplot(rank, aes(x=sed_bufferedPt_Rank, y=sed_polygon_Rank)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Sediment Retention") +  
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlim(23, 1) +
  ylim(23, 1) +
  xlab("") +
  ylab("")

# Plot nature access rank
nature_rank <- ggplot(rank, aes(x=nature_bufferedPt_Rank, y=nature_polygon_Rank)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Nature Access") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlim(23, 1) +
  ylim(23, 1) +
  xlab("") +
  ylab("")

# Plot endemic species rank
endemic_rank <- ggplot(rank, aes(x=endemic_bufferedPt_Rank, y=endemic_polygon_Rank)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Endemic Species") +   
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlim(23, 1) +
  ylim(23, 1) +
  xlab("") +
  ylab("")

# Plot Red List species rank
redlist_rank <- ggplot(rank, aes(x=redlist_bufferedPt_Rank, y=redlist_polygon_Rank)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Red List Species") +   
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlim(23, 1) +
  ylim(23, 1) +
  xlab("") +
  ylab("")

# Plot species richness rank
richness_rank <- ggplot(rank, aes(x=richness_bufferedPt_Rank, y=richness_polygon_Rank)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Species Richness") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlim(23, 1) +
  ylim(23, 1) +
  xlab("") +
  ylab("")

# Arrange 6 plots
ggarrange(N_rank, sed_rank, nature_rank, endemic_rank, redlist_rank, richness_rank)
