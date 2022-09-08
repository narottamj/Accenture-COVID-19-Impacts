setwd("~/Desktop/Accenture/Correlation Matrix for 02.17.22 dataset")
mydata <- read.csv('02.17.22.model.df.csv')

library(ggcorrplot)
library(GGally)
library(ggplot2)

#fixing structure
mydata$CensusTract <- as.character(mydata$CensusTract)
binaries <- colnames(mydata)[4:16]
mydata[binaries] <- lapply(mydata[binaries], factor)

#subsetting for corr matrix
corrdata <- mydata[, c(20:27, 39:54)]
corrdata <- na.omit(corrdata)

#corr matrix
m <- cor(corrdata)
ggcorrplot(m, hc.order = TRUE, type = 'lower')
ggcorrplot(m, method = 'circle')

ggpairs(corrdata)

ggcorr(corrdata, nbreaks = 4, palette = 'RdGy', label = TRUE, label_size = 3,
       label_color = 'white')

ggcorr(corrdata, label = TRUE, 
       label_size = 3, label_round = 2, label_alpha = TRUE)

ggcorrplot(m, method = 'circle')

