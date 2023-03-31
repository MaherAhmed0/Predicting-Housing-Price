##################################
# Import Data
##################################
Main_DataFrame <- read.csv("train.csv",header = TRUE)
##################################
# Exploring Data
##################################
######################################################################################################
colnames(Main_DataFrame)<-Main_DataFrame[1,]
Main_DataFrame<-Main_DataFrame[-1,]

rownames(Main_DataFrame)<-Main_DataFrame$Id
Main_DataFrame<-Main_DataFrame[,-1]

str(Main_DataFrame)
######################################################################################################


dim(Main_DataFrame)

str(Main_DataFrame)

attributes(Main_DataFrame)

summary(Main_DataFrame)

hist(Main_DataFrame$SalePrice)

plot(density(Main_DataFrame$SalePrice))
##################################
# Cleaning Data
##################################
print("Count of missing values by column wise")
sapply(Main_DataFrame,function(x) sum(is.na(x)))


#library(corrplot)
#C <- cor(Main_DataFrame)
#corrplot(C, method = "pie")
