# Load the "MGCV" library in R to use Generalised Additive Models
library(mgcv)

# Loading the "ReferendumResults.csv" file containing Brexit ward voting data into R as "VotingData"
VotingData <- read.csv("ReferendumResults.csv")

# Separating the dataset into two smaller datasets. One is the testing data used to test the model, called "TestData", and another is the training data used to train the model, which we call
# "VotingData".

VotingData$Leave[804:1070] <- NA
LeaveProp <- VotingData$Leave/VotingData$NVote

TestData <- cbind(VotingData[804:1070 , 1:3], LeaveProp[804:1070], VotingData[804:1070 , 6:49])
VotingData <- cbind(VotingData[1:803 , 1:3], LeaveProp[1:803], VotingData[1:803 , 6:49])
colnames(TestData)[4] <- "LeaveProp"
colnames(VotingData)[4] <- "LeaveProp"

# Get summary statistics for "LeaveProp", analyse its variance, and find the variance of its variance!

summary(VotingData$LeaveProp)
var(VotingData$LeaveProp)
LeavePropVarVar <- (VotingData$LeaveProp)*(1 - VotingData$LeaveProp)/(VotingData$Residents)
var(LeavePropVarVar)

# This function runs hierarchical clustering for any column in the VotingData dataset! The only
# argument it takes is the "column" argument, which is the name of the column in string format!

pca <- function(...) {
  
  data <- cbind(...)
  # Calculates the covariance matrix
  round(cor(data), 2)
  
  # Calculation of PCA components
  PCs <- prcomp(data, scale = TRUE)
  print(PCs, digits = 2)
  PCs
}

# Adjusting for voter turnout in each age bin

VotingData$Age_18to44 <- with(VotingData, 0.64*Age_18to19 + 0.64*Age_20to24 + 0.65*Age_25to29 + 0.65*Age_30to44)
VotingData$Age_45to64 <- with(VotingData, 0.66*Age_45to59 + 0.74*Age_60to64)
VotingData$Age_65plus <- with(VotingData, 0.90*(Age_65to74 + Age_75to84 + Age_85to89))

# Selecting the variable with the highest correlation coefficient

cor(VotingData$LeaveProp, VotingData$Age_18to44)
cor(VotingData$LeaveProp, VotingData$Age_45to64)
cor(VotingData$LeaveProp, VotingData$Age_65plus)

# Reformatting variable into "Log_BAPI"!

VotingData$Log_BAPI <- with(VotingData, log(Black + Asian + Pakistani + Indian))

# Running a PCA on the different ethnicity categories! We take only one PC, as seen here!

VotingData$EthnicPC <- pca(VotingData$White, VotingData$Log_BAPI)$x[, 1]

# Selecting the variable with the highest correlation coefficient

cor(VotingData$LeaveProp, VotingData$White)
cor(VotingData$LeaveProp, VotingData$Log_BAPI)
cor(VotingData$LeaveProp, VotingData$EthnicPC)

# Selecting the variable with the highest correlation coefficient

cor(VotingData$LeaveProp, VotingData$Owned)
cor(VotingData$LeaveProp, VotingData$OwnedOutright)
cor(VotingData$LeaveProp, VotingData$PrivateRent)
cor(VotingData$LeaveProp, VotingData$SocialRent)

# Selecting the variable with the highest correlation coefficient

cor(VotingData$LeaveProp, VotingData$L4Quals_plus)
cor(VotingData$LeaveProp, VotingData$L1Quals)
cor(VotingData$LeaveProp, VotingData$NoQuals)

# Reformatting variables into "HigherOccupOrStudent"!

VotingData$HigherOccupOrStudent <- with(VotingData, HigherOccup + log(Students))

# Selecting the variable with the highest correlation coefficient

cor(VotingData$LeaveProp, VotingData$RoutineOccupOrLTU)
cor(VotingData$LeaveProp, VotingData$HigherOccupOrStudent)

# Reformatting variables into "DeprivOnly!"

VotingData$DeprivOnly <- with(VotingData, Deprived - MultiDepriv)

# Selecting the variable with the highest correlation coefficient

cor(VotingData$DeprivOnly, VotingData$MultiDepriv)
cor(VotingData$LeaveProp, VotingData$MultiDepriv)
cor(VotingData$LeaveProp, VotingData$DeprivOnly)

# Reformatting variables into "C1Only" and "C2Only"!

VotingData$C1Only <- with(VotingData, C1C2DE - C2DE)
VotingData$C2Only <- with(VotingData, C2DE - DE)

# Selecting the variable with the highest correlation coefficient

cor(VotingData$LeaveProp, VotingData$C1Only)
cor(VotingData$LeaveProp, VotingData$C2Only)
cor(VotingData$LeaveProp, VotingData$DE)

# Calculating correlation matrix!

round(cor(cbind(VotingData$Age_18to44, VotingData$EthnicPC, VotingData$PrivateRent, VotingData$L4Quals_plus, VotingData$HigherOccupOrStudent, VotingData$MultiDepriv, VotingData$C2Only, VotingData$Density)), 2)

# Running a PCA for "L4Quals_plus", "HigherOccupOrStudent", "MultiDepriv", and "C2Only". We take two PCs, as shown!

VotingData$ReformatPC1 <- pca(VotingData$L4Quals_plus, VotingData$HigherOccupOrStudent, VotingData$MultiDepriv, VotingData$C2Only)$x[, 1]
VotingData$ReformatPC2 <- pca(VotingData$L4Quals_plus, VotingData$HigherOccupOrStudent, VotingData$MultiDepriv, VotingData$C2Only)$x[, 2]

# Running a PCA for "Age_18to44", "PrivateRent", "Density". We take only one PCs, as shown!

VotingData$ReformatPC3 <- pca(VotingData$Age_18to44, VotingData$Density)$x[, 1]

# Reformatting "AreaType" column to reduce the number of levels

# Means of covariates for each category of the column covariate
AreaMeans <- aggregate(VotingData[, c("EthnicPC", "ReformatPC1", "ReformatPC2", "ReformatPC3")], by = list(VotingData[["AreaType"]]), FUN=mean)
rownames(AreaMeans) <- AreaMeans[, 1]

# Standardise to mean 0 and standard deviation 1
AreaMeans <- scale(AreaMeans[, -1])

# Calculate pairwise distance between each category
AreaDistances <- dist(AreaMeans)

# Runs the clustering, and plots all the paired separations in a hierarchical screen.
ClusTree <- hclust(AreaDistances, method = 'complete')
par(mar = c(3, 3, 3, 1), mgp = c(2, 0.75, 0))

# Cutting the tree to form k number of groups
NewAreaGroups <- cutree(ClusTree, k = 2)
print(NewAreaGroups, width = 90)

# E06 E07 E08 E09 
# 1   1   1   2 

# Add new groups to the original data frame
VotingData <- merge(data.frame(AreaType = names(NewAreaGroups), AreaTypeReformat = NewAreaGroups), VotingData)

# Reformatting "RegionName" column to reduce the number of levels.

# Means of covariates for each category of the column covariate
RegionMeans <- aggregate(VotingData[, c("EthnicPC", "ReformatPC1", "ReformatPC2", "ReformatPC3")], by = list(VotingData[["RegionName"]]), FUN=mean)
rownames(RegionMeans) <- RegionMeans[, 1]

# Standardise to mean 0 and standard deviation 1
RegionMeans <- scale(RegionMeans[, -1])

# Calculate pairwise distance between each category
RegionDistances <- dist(RegionMeans)

# Runs the clustering, and plots all the paired separations in a hierarchical screen.
ClusTree <- hclust(RegionDistances, method = 'complete')

# Cutting the tree to form k number of groups
NewRegionGroups <- cutree(ClusTree, k = 4)
print(NewRegionGroups, width = 90)

# East Midlands          East of England                   London 
#             1                        2                        3 
#    North East               North West               South East 
#             4                        4                        2 
#    South West            West Midlands Yorkshire and The Humber 
#             4                        4                        4 

# Add new groups to the original data frame
VotingData <- merge(data.frame(RegionName = names(NewRegionGroups), RegionNameReformat = NewRegionGroups), VotingData)

# Specifying "RegionNameReformat" and "AreaTypeReformat" as categorical variables

VotingData$RegionNameReformat <- factor(VotingData$RegionNameReformat)
VotingData$AreaTypeReformat <- factor(VotingData$AreaTypeReformat)

# This is our initial model - a GAM from the binomial family of distribution with the default link function!

votingModel.binomial <- gam(LeaveProp ~ AreaTypeReformat + RegionNameReformat + s(EthnicPC) + s(ReformatPC1) + s(ReformatPC2) + s(ReformatPC3), data = VotingData, weights = Residents, family = binomial())
summary(votingModel.binomial)

# Calculating variance of Pearson residuals of our initial model to check for dispersion

sum(resid(votingModel.binomial,type="pearson")^2) / votingModel.binomial$df.residual

# Updates our initial model to change the family of distribution to beta binomial regression!

votingModel.betar <- update(votingModel.binomial, family = betar())
summary(votingModel.betar)

# Calculating variance of Pearson residuals of our betar model to check for dispersion

sum(resid(votingModel.betar, type="pearson")^2) / votingModel.betar$df.residual

# Updates our betar model to change the link function to logit!

votingModel.logit <- update(votingModel.betar, family = betar(link = logit))
summary(votingModel.logit)

# Updates our betar model to change the link function to probit!

votingModel.probit <- update(votingModel.betar, family = betar(link = probit))
summary(votingModel.probit)

# Updates our betar model to change the link function to cloglog!

votingModel.cloglog <- update(votingModel.betar, family = betar(link = cloglog))
summary(votingModel.cloglog)

# Updates our betar model to change the link function to cauchit!

votingModel.cauchit <- update(votingModel.betar, family = betar(link = cauchit))
summary(votingModel.cauchit)

# Removes the "AreaTypeReformat" categorical covariate from our betar regression model with a cauchit link function!

votingModel.initial <- update(votingModel.cauchit, .~. -AreaTypeReformat)
summary(votingModel.initial)

# Adding interaction terms to our betar regression model with a cauchit link function!

votingModel.expanded <- update(votingModel.initial, .~. + ti(EthnicPC, ReformatPC1) + ti(EthnicPC, ReformatPC2))

# Finding the AIC value of the model with the interaction terms and the nested model!

AIC(votingModel.expanded, votingModel.initial)

# Adding interaction terms to our betar regression model with a cauchit link function!

votingModel.initial <- votingModel.expanded
votingModel.expanded <- update(votingModel.expanded, .~. + ti(ReformatPC3, ReformatPC1) + ti(ReformatPC3, ReformatPC2))

# Finding the AIC value of the model with the interaction terms and the nested model!

AIC(votingModel.expanded, votingModel.initial)

votingModel <- votingModel.expanded
summary(votingModel)

# Creating Age_18to44 for TestData

Age_18to44 <- with(TestData, 0.64*Age_18to19 + 0.64*Age_20to24 + 0.65*Age_25to29 + 0.65*Age_30to44)

# Calculating EthnicPC for TestData

EthnicPC <- with(TestData, 0.71*scale(White) - 0.71*scale(log(Black + Asian + Pakistani + Indian)))

# Calculating HigherOccupOrStudent for TestData

HigherOccupOrStudent <- with(TestData, HigherOccup + log(Students))

# Calculating C2Only for TestData

C2Only <- with(TestData, C2DE - DE)

# Creating ReformatPC1 using L4Quals_plus, HigherOccupOrStudent, MultiDepriv, and C2Only for TestData

ReformatPC1 <- with(TestData, - 0.54*scale(L4Quals_plus) - 0.55*scale(HigherOccup) + 0.44*scale(MultiDepriv) + 0.46*scale(C2Only))

# Creating ReformatPC2 using L4Quals_plus, HigherOccupOrStudent, MultiDepriv, and C2Only for TestData

ReformatPC2 <- with(TestData, 0.14*scale(L4Quals_plus) - 0.11*scale(HigherOccup) + 0.72*scale(MultiDepriv) - 0.67*scale(C2Only))

# Creating ReformatPC3 using L4Quals_plus, HigherOccupOrStudent, MultiDepriv, and C2Only for TestData

ReformatPC3 <- with(TestData, -0.71*scale(Age_18to44) -0.71*scale(Density))

# Reformatting RegionName to RegionNameReformat for TestData

oldRegionNameLevels <- c("East Midlands", "East of England", "South East", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire and The Humber")
newRegionNameLevels <- factor(c("1", "2", "2", "3", "4", "4", "4", "4", "4"))

RegionNameReformat <- newRegionNameLevels[match(TestData$RegionName, oldRegionNameLevels)]

# Overriding TestData with a new dataframe containing all reformatted variables

TestData <- data.frame(
  ID = TestData$ID,
  RegionNameReformat = RegionNameReformat,
  EthnicPC = EthnicPC,
  ReformatPC1 = ReformatPC1,
  ReformatPC2 = ReformatPC2,
  ReformatPC3 = ReformatPC3
)

# Using predict.gam() and votingModel to make predictions on TestData, and calculating prediction standard deviation of prediction error

predict <- predict.gam(votingModel, newdata = TestData, type = "response", se.fit = TRUE)

# Calculating prediction standard deviation of prediction error. We use the estimated parameter of the beta regression distribution to do so!

predict.var <- predict$se.fit^2
actual.var <- predict$fit*(1 - predict$fit)/(1 + 104.062)
predictError.sd <- sqrt(predict.var + actual.var)

# Compiling ward ID, predictions, and standard deviation of prediction error

data <- data.frame(
  TestData$ID,
  predict$fit,
  predictError.sd
)
write.table(data, "ICA2_Group_AR_pred.dat", sep = " ", row.names = FALSE, col.names = FALSE)

# Generates boxplots of LeaveProp across all categories in AreaType and RegionName

# Saves all plots as "ICA2_Group_AR_Plots.

pdf(device = pdf, "ICA2_Group_AR_Plots.pdf")

# Adjusts the margin of plots and puts plots in a 1 by 2 array!

par(mfrow = c(1, 2), mar = c(6, 4, 1, 2))

# Boxplot of Leave Vote Proportion for different AreaTypes!

boxplot(LeaveProp ~ AreaType, data = VotingData, xlab = "Area Type", ylab = "Leave Vote Proportion", cex.main = 0.9, cex.ylab = 0.9)

# Plots means of leave vote proportion for each AreaType on the boxplot!

LeavePropMeansByAreaType <- tapply(VotingData$LeaveProp, VotingData$AreaType, mean)
points(LeavePropMeansByAreaType, pch = 16, cex = 1, col = "blue")

# Boxplot of Leave Vote Proportion for different RegionName!

boxplot(LeaveProp ~ RegionName, data = VotingData, xlab = "", ylab = "", cex.main = 0.9, cex.axis = 0.5, cex.ylab = 0.9, las = 2)
mtext(" ", line = 3, font = 2)
mtext("Region Name", side = 1, line = 4)

# Plots means of leave vote proportion for each RegionName on the boxplot!

LeavePropMeansByRegionName <- tapply(VotingData$LeaveProp, VotingData$RegionName, mean)
points(LeavePropMeansByRegionName, pch = 16, cex = 1, col = "blue")

# Plots LeaveProp against Mean Voter Age and percentage of white ward residents.

# Adjusts the margin of plots and puts plots in a 2 by 2 array!

par(mfrow = c(2, 2), mar = c(4, 4, 1, 2))

# Scatterplot of LeaveProp against Mean Voter Age!

plot(LeaveProp ~ AdultMeanAge, data = VotingData, xlab = "Mean Voter Age", ylab = "Leave Vote Proportion", cex.main = 0.9, cex.ylab = 0.9, pch=16)

# Scatterplot of LeaveProp against Percentage of C2Only!

plot(LeaveProp ~ C2Only, data = VotingData, xlab = "Percentage of C2Only", ylab = "", cex.main = 0.9, cex.ylab = 0.9, pch = 16)

# Scatterplot of LeaveProp against Percentage of Degree-educated ward members!

plot(LeaveProp ~ L4Quals_plus, data = VotingData, xlab = "Percentage of Degree-educated", ylab = "Leave Vote Proportion", cex.main = 0.9, cex.ylab = 0.9, pch=16)

# Scatterplot of LeaveProp against Percentage of private renting ward members!

plot(LeaveProp ~ PrivateRent, data = VotingData, xlab = "Percentage of Private Renters", ylab = "", cex.main = 0.9, cex.ylab = 0.9, pch=16)

# Plots of contour visualisations of the interactions between the covariates when adding on interaction terms

# Puts plots in a 2 by 2 array!

par(mfrow = c(2, 2))

# Plots of interaction terms "EthnicPC" and "ReformatPC1".

vis.gam(votingModel.initial, view = c("EthnicPC", "ReformatPC1"), n.grid = 100, plot.type = "contour", color = "topo")

# Plots of interaction terms "EthnicPC" and "ReformatPC2".

vis.gam(votingModel.initial, view = c("EthnicPC", "ReformatPC2"), n.grid = 100, plot.type = "contour", color = "topo")

# Plots of interaction terms "ReformatPC1 and "ReformatPC2".

vis.gam(votingModel.initial, view = c("ReformatPC1", "ReformatPC2"), n.grid = 100, plot.type = "contour", color = "topo")

# Plots of interaction terms "ReformatPC1 and "ReformatPC3".

vis.gam(votingModel.initial, view = c("ReformatPC1", "ReformatPC3"), n.grid = 100, plot.type = "contour", color = "topo")

# Plots of diagnostic plots for the final GAM

layout(matrix(1:4, nrow = 2, ncol = 2))
gam.check(votingModel, type = "pearson", pch = 16)
layout(1)

dev.off()
