library(corrplot)
library(ggplot2)
library(ggcorrplot)


# data <- read.csv("Z:/School/f2020/da/DataAnalytics2020_Avery_Sommer/proj_rev2/data/long2.csv")
fdata <- data[data$Result != "1/2-1/2" &  data$Result != "*",]

attach(fdata)
wgames <- data.frame("COLOR"="W", "ELO" = WhiteElo, "RESULT" = as.integer(substr(Result,1,1)), "P"=wp, "KN"=wkn, "B"=wb, "K"=wk, "Q"=wq, "R"=wr)
bgames <- data.frame("COLOR"="B", "ELO" = BlackElo, "RESULT" = as.integer(substr(Result,3,3)), "P"=bp, "KN"=bkn, "B"=bb, "K"=bk, "Q"=bq, "R"=br)

games <- rbind(wgames,bgames)

remove(wgames,bgames)
detach(fdata)

games.num <- games[,2:9]

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

plot <- ggcorrplot(cor(games.num), title = "All Games",
                   type = "lower",
                   lab = TRUE,
                   ggtheme = ggplot2::theme_minimal(), tl.col="white")

ggsave("./plots/corrplot/total.png", plot = plot)

games.tophalf <- games.num[games.num$ELO > quantile(games.num$ELO,prob=1-50/100),]
games.bottomhalf <- games.num[games.num$ELO <= quantile(games.num$ELO,prob=1-50/100),]

print(paste(min(games.tophalf$ELO), max(games.tophalf$ELO)))
print(paste(min(games.bottomhalf$ELO), max(games.bottomhalf$ELO)))


plot <- ggcorrplot(cor(games.tophalf), title = "Top half of ELO",
                   type = "lower",
                   lab = TRUE,
                   ggtheme = ggplot2::theme_minimal()
)
ggsave("./plots/corrplot/thalf.png", plot = plot)
plot <- ggcorrplot(cor(games.bottomhalf), title = "Bottom half of ELO",
                              type = "lower",
                              lab = TRUE,
                              ggtheme = ggplot2::theme_minimal()
)
ggsave("./plots/corrplot/bhalf.png", plot = plot)

max <- 5

for (x in 1:max){
  val.top <- 1-((x-1)*(100/max))/100
  val.bottom <- 1-(x*(100/max))/100
  data.var <- games.num[games.num$ELO <= quantile(games.num$ELO, prob=val.top) & 
                        games.num$ELO >  quantile(games.num$ELO, prob=val.bottom),]
  title <- paste(val.bottom*100,"% - ", val.top*100, "%; ", min(data.var$ELO), "-", max(data.var$ELO), "; n=", nrow(data.var), sep="")
  
  png(paste("./plots/corrplot/4sec/", (x)*(100/max), ".png", sep = ""))
  corrplot(cor(data.var), col=col(200), diag=FALSE,
           type="upper", title=title, 
           addCoef.col = "black", 
           mar=c(0,0,5,0))
  dev.off()
  
}


for (x in 3:15){
  val <- x * 200
  png(paste("./plots/corrplot/inc200/",toString(val),".png", sep=""))
  data.use <- games.num[games.num$ELO >= val & games.num$ELO < val+199,]
  title <- paste(val, "-", val+199, ": n = ", nrow(data.use),sep="")

  corrplot(cor(data.use), col=col(200),  
               diag=FALSE, # tl.pos="d", 
               type="upper",
               title=title, 
               addCoef.col = "black", # Add coefficient of correlation
              # hide correlation coefficient on the principal diagonal
               mar=c(0,0,5,0))
  dev.off()
}

# all variables
remove(max, title, val, val.bottom, val.top, x)

# all df
remove(games.bottomhalf, games.tophalf, games.num, data.use, data.var)

# all functions
remove(col)

# 
# require(reshape2)
# df.m <- melt(games[-2], id.var = "COLOR")
# 
# require(ggplot2)
# plot <- ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=COLOR))
# 
# print(plot)