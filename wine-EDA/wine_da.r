library(ggplot2)
library(kknn)
library(dplyr)
library(gridExtra)

winequality.red <- read.csv("./data/winequality-red.csv", sep=";")
winequality.white <- read.csv("./data/winequality-white.csv", sep=";")

winequality <- rbind(cbind(Type="white", winequality.white),cbind(Type="red", winequality.red))

theme_set(theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5, size=15, margin=margin(0,0,10,0))))

hist.sulphates <- ggplot(winequality, aes(sulphates, fill = Type)) + geom_density(alpha = 0.2) + ggtitle("Sulphates")
ggsave("./plots/hist/sulphates.png", plot= hist.sulphates)

hist.sulfurO2 <- ggplot(winequality, aes(total.sulfur.dioxide, fill = Type)) + geom_density(alpha = 0.2) + ggtitle("Total Sulfur Dioxide")
ggsave("./plots/hist/sulfurO2.png", plot= hist.sulfurO2)

hist.pH <- ggplot(winequality, aes(pH, fill = Type)) + geom_density(alpha = 0.2) + ggtitle("pH")
ggsave("./plots/hist/pH.png", plot= hist.pH)

hist.chlorides <- ggplot(winequality, aes(chlorides, fill = Type)) + geom_density(alpha = 0.2) + ggtitle("Chlorides")
ggsave("./plots/hist/chlorides.png", plot= hist.chlorides)

combined <- grid.arrange(hist.sulphates, hist.sulfurO2, hist.chlorides, hist.pH, nrow=2)
ggsave("./plots/hist/combined.png", plot=combined)

theme_set(theme_gray())
remove(hist.chlorides)
remove(hist.sulphates)
remove(hist.sulfurO2)
remove(hist.pH)


ana.class <- "Type"
ana.acols <- c("sulphates", "pH", "chlorides")
ana.tcols <- c(ana.class, ana.acols)

ana.data <- data.frame(winequality[,ana.tcols])
ana.data <- ana.data[complete.cases(ana.data),]

ana.data.sample <- ana.data[sample(nrow(ana.data), 1000),]

ana.data.sample.woclass <- data.frame(ana.data.sample[,ana.acols])

ana.kmean <- kmeans(ana.data.sample.woclass,2,iter.max = 1000)
ana.kmean.chars <- substr(ana.data.sample$Type,1,1)

ana.plot.dist <- dist(ana.data.sample.woclass)
ana.plot.mds <- cmdscale(ana.plot.dist)

ana.plot.colors <- rainbow(2)[ana.kmean$cluster]

plot(ana.plot.mds, col=ana.plot.colors, xlab="x", ylab="y", pch=ana.kmean.chars)

ana.kmean.corr <- ana.kmean$cluster == 3 - match(ana.data.sample$Type, c("white","red"))
ana.kmean.corr.char <- c("o","x")[2 - ana.kmean.corr]

plot(ana.plot.mds, col=ana.plot.colors, xlab="x", ylab="y", pch=ana.kmean.corr.char)

print(summary(ana.kmean.corr))


remove(list=ls())

