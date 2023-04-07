##################################
# Import Data
##################################
Main_DataFrame <- read.csv("train.csv",header = TRUE)

Main_DataFrame$Id <- NULL
##################################
# Exploring Data
##################################
cat("Dataset shape:(", dim(Main_DataFrame),")")

print("Dataset stucture:")
str(Main_DataFrame)

names(Main_DataFrame)

summary(Main_DataFrame)

hist(Main_DataFrame$SalePrice)

plot(density(Main_DataFrame$SalePrice))
##################################
# Cleaning Data
##################################
print("Missing or null values column wise")
sapply(Main_DataFrame,function(x) sum(is.na(x)))

NA_Cols <- which(colSums(is.na(Main_DataFrame)) > 0)
sort(colSums(sapply(Main_DataFrame[NA_Cols], is.na)), decreasing = TRUE)
cat('There are', length(NA_Cols), 'columns with missing values')

##################################
# Correlation on numeric features
##################################
Numeric_Data <- which(sapply(Main_DataFrame, is.numeric)) #index vector numeric variables
# numericVarNames <- names(Numeric_Data) #saving names vector for use later on
cat('There are', length(Numeric_Data), 'numeric variables')

All_Num_Data <- Main_DataFrame[, Numeric_Data]
Corr_Num_Data <- cor(All_Num_Data, use="pairwise.complete.obs")

Data_Sorted <- as.matrix(sort(Corr_Num_Data[,'SalePrice'], decreasing = TRUE))

# Select only high correlations
High_Corr <- names(which(apply(Data_Sorted, 1, function(x) abs(x) > 0.5)))
Corr_Table <- Corr_Num_Data[High_Corr, High_Corr]

C <- cor(Corr_Table)

corrplot(C, method="circle")

corrplot(C, method="pie")

corrplot(C, method="color")
################################################################################################

################################################################################################
