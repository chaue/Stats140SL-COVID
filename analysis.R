library(readr)
data <- read.csv("cleaned_data.csv", header = TRUE)
library(dplyr)

statepop <- c(4903185, 731545, 7278717, 3017804, 39512223, 5758736, 3565287, 973764, 21477737, 10617423, 1415872, 1787065, 12671821, 6732219, 3155070, 2913314, 4467673,
              4648794, 1344212, 6045680, 6892503, 9986857, 5639632, 2976149, 6137428, 1068778, 1934408, 3080156, 1359711, 8882190, 2096829, 19453561,
              10488084, 762062, 11689100, 3956971, 4217737, 12801989, 1059361, 5148714, 884659, 6829174, 28995881, 3205958, 623989, 8535519, 7614893, 1792147,
              5822434, 578759)

# ratioconfirmed2 is the data from the last day
ratio_data <- read.csv("ratioconfirmed2.csv", header = TRUE)
ratio_data$confirm_per <- (ratio_data$Confirmed / ratio_data$statepop) * 100

# poster figures
library(ggplot2)
c <- ggplot(ratio_data, aes(x = confirm_per))
c + geom_histogram(binwidth = 0.5, fill = "orange") + 
  xlab("Percent of Confirmed Cases") + 
  ylab("Frequency") +
  theme_bw() + theme(axis.title = element_text(hjust = 0.5, size = 24),
                     axis.text.x = element_text(face="bold", size=18),
                     axis.text.y = element_text(face="bold", size=18),
                     axis.ticks = element_blank())
ggsave('cumulative.png')

new_mask <- as.factor(ifelse(ratio_data$mask == 1, "Mask Mandate", "No Mask Mandate"))
b <- ggplot(ratio_data, aes(x = new_mask))
b + geom_bar(fill = "steelblue", width = 0.5) + ylab("Frequency") + xlab("Presence of Mask Mandate") + theme_bw() +
  theme(axis.title = element_text(hjust = 0.5, size = 24),
        axis.text.x = element_text(face="bold", size=18),
        axis.text.y = element_text(face="bold", size=18),
        axis.ticks = element_blank())
ggsave('mask.png')

data$newcon <- data$New_Confirmed / rep(statepop, nrow(data) / 50)
e <- ggplot(data[which(newcon>0),], aes(x = log(newcon[which(newcon>0)])))
e + geom_histogram(binwidth = 0.5, fill = "pink", alpha = 0.8) +
  xlab("Log Proportion of Daily Confirmed Cases") + 
  ylab("Frequency") +
  theme_bw() + theme(axis.title = element_text(hjust = 0.5, size = 24),
                     axis.text.x = element_text(face="bold", size=18),
                     axis.text.y = element_text(face="bold", size=18),
                     axis.ticks = element_blank())
ggsave('logtransf.png')


# OLS models

# model for final day cumulative cases regressed on mask mandate
final <- lm(confirm_per ~ mask, data = ratio_data)
summary(final)

# model for daily new cases regressed on mask mandate
newcon <- data$New_Confirmed / rep(statepop, nrow(data) / 50)
mask <- data$mask
summary(lm(log(newcon[which(newcon > 0)]) ~ mask[which(newcon > 0)]))
plot(lm(log(newcon[which(newcon > 0)]) ~ mask[which(newcon > 0)]))

# some plots for sanity check
par(mfrow = c(1,2))
boxplot(newcon[which(mask == 1)])
boxplot(newcon[which(mask == 0)])
mean(newcon[which(mask == 1)])
mean(newcon[which(mask == 0)])
median(newcon[which(mask == 1)])
median(newcon[which(mask == 0)])
