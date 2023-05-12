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
###------- Missing Data -------###
##################################
Main_DataFrame <- Main_DataFrame[, !(colnames(Main_DataFrame)
                                     %in% c("PoolQC","MiscFeature","Alley","Fence", "Utilities"))]

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

##########################  Basement Variables   ##########################
Main_DataFrame[!is.na(Main_DataFrame$BsmtFinType1) & (is.na(Main_DataFrame$BsmtCond)|is.na(Main_DataFrame$BsmtQual)
                                                      |is.na(Main_DataFrame$BsmtExposure)|
                                                        is.na(Main_DataFrame$BsmtFinType2)), 
               c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

Main_DataFrame$BsmtFinType2[333] <- names(sort(-table(Main_DataFrame$BsmtFinType2)))[1]
Main_DataFrame$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(Main_DataFrame$BsmtExposure)))[1]
Main_DataFrame$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(Main_DataFrame$BsmtCond)))[1]
Main_DataFrame$BsmtQual[c(2218, 2219)] <- names(sort(-table(Main_DataFrame$BsmtQual)))[1]

Basement_cols_None <- c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')

Main_DataFrame[Basement_cols_None][is.na(Main_DataFrame[Basement_cols_None])] <- 'None'

Main_DataFrame$BsmtQual<-as.integer(revalue(Main_DataFrame$BsmtQual,
                                              c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2,"Po" = 1, "None" = 0)))

Main_DataFrame$BsmtCond<-as.integer(revalue(Main_DataFrame$BsmtCond,
                                              c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2,"Po" = 1, "None" = 0)))

Main_DataFrame$BsmtExposure<-as.integer(revalue(Main_DataFrame$BsmtExposure,
                                            c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)))

Main_DataFrame$BsmtFinType1<-as.integer(revalue(Main_DataFrame$BsmtFinType1,
                                                c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)))

Main_DataFrame$BsmtFinType2<-as.integer(revalue(Main_DataFrame$BsmtFinType2,
                                                c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)))

Main_DataFrame$BsmtFullBath[is.na(Main_DataFrame$BsmtFullBath)] <- 0
Main_DataFrame$BsmtHalfBath[is.na(Main_DataFrame$BsmtHalfBath)] <- 0
Main_DataFrame$BsmtFinSF1[is.na(Main_DataFrame$BsmtFinSF1)] <- 0
Main_DataFrame$BsmtFinSF2[is.na(Main_DataFrame$BsmtFinSF2)] <- 0
Main_DataFrame$BsmtUnfSF[is.na(Main_DataFrame$BsmtUnfSF)] <- 0
Main_DataFrame$TotalBsmtSF[is.na(Main_DataFrame$TotalBsmtSF)] <- 0

##########################  MasVnrType , MasVnrArea  ##########################
Main_DataFrame$MasVnrType[2611] <- names(sort(-table(Main_DataFrame$MasVnrType)))[2]

Main_DataFrame$MasVnrType[is.na(Main_DataFrame$MasVnrType)] <- 'None'

average_saleprice_2 <- aggregate(SalePrice ~ MasVnrType, data = Main_DataFrame, FUN = mean)
ggplot(average_saleprice_2, aes(x = MasVnrType, y = SalePrice)) +
  geom_bar(stat = "identity") +
  xlab("MasVnr Type") +
  ylab("Average Sale Price") +
  ggtitle("Average Sale Price by MasVnr Type")

Main_DataFrame$MasVnrType<-as.integer(revalue(Main_DataFrame$MasVnrType,
                                                c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)))

Main_DataFrame$MasVnrArea[is.na(Main_DataFrame$MasVnrArea)] <- 0

##########################  MSZoning  ##########################
Main_DataFrame$MSZoning[is.na(Main_DataFrame$MSZoning)] <- names(sort(-table(Main_DataFrame$MSZoning)))[1]
Main_DataFrame$MSZoning <- as.factor(Main_DataFrame$MSZoning)

##########################  Functional  ##########################
Main_DataFrame$Functional[is.na(Main_DataFrame$Functional)] <- names(sort(-table(Main_DataFrame$Functional)))[1]

Main_DataFrame$Functional<-as.integer(revalue(Main_DataFrame$Functional,
                                              c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5,
                                                'Min1'=6, 'Typ'=7)))

##########################  Exterior  ##########################
Main_DataFrame$Exterior1st[is.na(Main_DataFrame$Exterior1st)] <- names(sort(-table(Main_DataFrame$Exterior1st)))[1]
Main_DataFrame$Exterior2nd[is.na(Main_DataFrame$Exterior2nd)] <- names(sort(-table(Main_DataFrame$Exterior2nd)))[1]

Main_DataFrame$Exterior1st <- as.factor(Main_DataFrame$Exterior1st)
Main_DataFrame$Exterior2nd <- as.factor(Main_DataFrame$Exterior2nd)

Main_DataFrame$ExterQual<-as.integer(revalue(Main_DataFrame$ExterQual,
                                             c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2,"Po" = 1, "None" = 0)))

Main_DataFrame$ExterCond<-as.integer(revalue(Main_DataFrame$ExterCond,
                                             c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2,"Po" = 1)))

##########################  Electrical  ##########################
Main_DataFrame$Electrical[is.na(Main_DataFrame$Electrical)] <- names(sort(-table(Main_DataFrame$Electrical)))[1]
Main_DataFrame$Electrical <- as.factor(Main_DataFrame$Electrical)

##########################  Kitchen Qual  ##########################
Main_DataFrame$KitchenQual[is.na(Main_DataFrame$KitchenQual)] <- names(sort(-table(Main_DataFrame$KitchenQual)))[1]
Main_DataFrame$KitchenQual<-as.integer(revalue(Main_DataFrame$KitchenQual,
                                             c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2,"Po" = 1)))

##########################  Sale Type , Sale Condition  ##########################
Main_DataFrame$SaleType[is.na(Main_DataFrame$SaleType)] <- names(sort(-table(Main_DataFrame$SaleType)))[1]

Main_DataFrame$SaleType <- as.factor(Main_DataFrame$SaleType)
Main_DataFrame$SaleCondition <- as.factor(Main_DataFrame$SaleCondition)

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
High_Corr <- names(which(apply(Data_Sorted, 1, function(x) abs(x) > 0.4)))
Corr_Table <- Corr_Num_Data[High_Corr, High_Corr]

C <- cor(Corr_Table)

corrplot(C, method="number")

corrplot(C, method="pie")

corrplot(C, method="color")
################################################################################################

################################################################################################
