##################################
###------- Import Data --------###
##################################
Train_Data <- read.csv("train.csv", stringsAsFactors = F)
Test_Data  <- read.csv("test.csv", stringsAsFactors = F)
Test_Data$SalePrice <- NA

Main_DataFrame <- rbind(Train_Data, Test_Data)

rm(Train_Data, Test_Data)
##################################
###------ Exploring Data ------###
##################################
cat("Dataset shape:(", dim(Main_DataFrame),")")

print("Dataset stucture:")

str(Main_DataFrame)

names(Main_DataFrame)

summary(Main_DataFrame)

print("Missing or null values column wise")
NA_Cols <- which(colSums(is.na(Main_DataFrame)) > 0)
sort(colSums(sapply(Main_DataFrame[NA_Cols], is.na)), decreasing = TRUE)
cat('There are', length(NA_Cols), 'columns with missing values')

##################################
###---- Analyzing features ----###
##################################
summary(Main_DataFrame$SalePrice)

hist(Main_DataFrame$SalePrice)

##################################
###------ Cleaning Data -------###
##################################
Main_DataFrame <- Main_DataFrame[, !(colnames(Main_DataFrame)
                                     %in% c("PoolQC","MiscFeature","Alley","Fence"))]

##########################  FireplaceQu  ##########################
Main_DataFrame$FireplaceQu[is.na(Main_DataFrame$FireplaceQu)] <- 'None'
Main_DataFrame$FireplaceQu<-as.integer(revalue(Main_DataFrame$FireplaceQu,
                                               c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2,"Po" = 1, "None" = 0)))

##########################  LotFrontage  ##########################
Main_DataFrame$LotFrontage[which(is.na(Main_DataFrame$LotFrontage))] <- median(Main_DataFrame$LotFrontage,na.rm = T)

##########################  LotShape  ##########################
Main_DataFrame$LotShape<-as.integer(revalue(Main_DataFrame$LotShape,
                                            c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))

##########################  LotConfig   ##########################
Main_DataFrame$LotConfig <- as.factor(Main_DataFrame$LotConfig)
Main_DataFrame$LotConfig <- as.integer(Main_DataFrame$LotConfig)

##########################  GarageYrBlt, GarageArea, GarageCars   ##########################
cols_0 <- c('GarageYrBlt', 'GarageArea', 'GarageCars')

Main_DataFrame[cols_0][is.na(Main_DataFrame[cols_0])] <- 0

##########################  GarageType, GarageFinish, GarageQual, GarageCond   ##########################
cols_None <- c('GarageType', 'GarageFinish', 'GarageQual', 'GarageCond')

Main_DataFrame[cols_None][is.na(Main_DataFrame[cols_None])] <- 'None'

average_saleprice <- aggregate(SalePrice ~ GarageType, data = Main_DataFrame, FUN = mean)
ggplot(average_saleprice, aes(x = GarageType, y = SalePrice)) +
  geom_bar(stat = "identity") +
  xlab("Garage Type") +
  ylab("Average Sale Price") +
  ggtitle("Average Sale Price by Garage Type")

Main_DataFrame$GarageType<-as.integer(revalue(Main_DataFrame$GarageType,
                                              c('None'=0, 'CarPort'=1, 'Detchd'=2,
                                                '2Types'=3, 'Basment'=4, 'Attchd'=5, 'BuiltIn'=6)))

Main_DataFrame$GarageFinish<-as.integer(revalue(Main_DataFrame$GarageFinish,
                                            c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)))

Main_DataFrame$GarageQual<-as.integer(revalue(Main_DataFrame$GarageQual,
                                               c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2,"Po" = 1, "None" = 0)))

Main_DataFrame$GarageCond<-as.integer(revalue(Main_DataFrame$GarageCond,
                                              c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2,"Po" = 1, "None" = 0)))

########################################################################################
##################################
### ------ Correlation ------- ###
##################################
Numeric_Data <- which(sapply(Main_DataFrame, is.numeric)) #index vector numeric variables
# numericVarNames <- names(Numeric_Data) #saving names vector for use later on
cat('There are', length(Numeric_Data), 'numeric variables')

All_Num_Data <- Main_DataFrame[, Numeric_Data]
Corr_Num_Data <- cor(All_Num_Data, use="pairwise.complete.obs")

Data_Sorted <- as.matrix(sort(Corr_Num_Data[,'SalePrice'], decreasing = TRUE))

# Select only high correlations
High_Corr <- names(which(apply(Data_Sorted, 1, function(x) abs(x) > 0.3)))
Corr_Table <- Corr_Num_Data[High_Corr, High_Corr]

C <- cor(Corr_Table)

corrplot(C, method="number")

corrplot(C, method="pie")

corrplot(C, method="color")
################################################################################################

################################################################################################
