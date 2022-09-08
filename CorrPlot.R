install.packages("Hmisc")
library(corrplot)
library(Hmisc)

mydata <- read.csv(file = "02.17.2022.model.df.csv")

mydata <- mydata[,-(1:3)]


res <- cor(mydata, use = "complete.obs")
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex=.4)

view(res)
res
