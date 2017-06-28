library(class)
library(ggplot2)
library(gridExtra)


# Read the csv with headers
MyData <- read.csv("H:\\Predictive Analysis\\M405.csv", header = TRUE)

# Check the column names and the type of each field
colnames(MyData)
str(MyData)

# remove month since we don't need it
MyData$Month <- NULL

# Convert the following into numerics since they are appearing as factors
MyData$Volume.BMO.Deposits <- as.numeric(MyData$Volume.BMO.Deposits)
MyData$Total.Transaction <- as.numeric(MyData$Total.Transaction)

# Complete the data for missing values
MyData <- MyData[complete.cases(MyData),]

# Check the types again
str(MyData)

# Convert the following into factors
MyData$Depository <- factor(ifelse(MyData$Depository == 0, "Pass", "Fail"))
MyData$Dispenser <- factor(ifelse(MyData$Dispenser == 0, "Pass", "Fail"))
MyData$Card.Reader <- factor(ifelse(MyData$Card.Reader == 0, "Pass", "Fail"))

# Dividing into training Sets
trainingSet <- MyData[1:150,2:6]
trainingOutcomes <- MyData[1:150, 7]

# Dividing into Testing Sets
testSet <- MyData[151:169, 2:6]
testOutcomes <- MyData[151:169, 7]

# Using k nearest neibhour to perform prediction
predictions <- knn(train = trainingSet, cl = trainingOutcomes, k = 2,
                   test = testSet)

# Check the predictions
predictions

# Make an n-way table to see the predictions clearly
x <- table(testOutcomes, predictions)
x

# Convert the table into a data frame
y <- as.data.frame(x)

#plot the table
plot(x)

################################################################################################################################

# Main Pie
MainSlice <- c(y[1,3]+y[4,3] , y[2,3]+y[3,3])
pie(MainSlice, labels = c("Correct", "Incorrect") , main="Accuracy of Prediction")

# Pie Chart
slices <- y[,3]
lbls1 <- y[,1]
lbls2 <- y[,2]
col3 <- paste(lbls1,lbls2, sep="/")
pie(slices, labels = col3, main="Deeper view of Accuracy")

#################################################################################################################################

# Export a png image for the data frame

png("H:\\Predictive Analysis\\test.png")
output <- tableGrob(y)
grid.arrange(output)
dev.off()


