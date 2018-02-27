#read the data and assign it to the.data
the.data <- as.matrix(read.table("Journal-Ranking-0101_mod_4.txt"))
#choose sample of 100 rows and 11 columns and assign it to my.data
my.data <- the.data[sample(1:161,100), c(1,3:10,12,13)]
#converting data into numeric values
my.data <- mapply(my.data, FUN=as.numeric)
#Reconstructing the matrix and assigning to my.data
my.data <-matrix(data=my.data, ncol=11, nrow=100)
#Checking the correlation between the variable of interest and other variables
cor(my.data[,10], my.data[,1])
cor(my.data[,10], my.data[,2])
cor(my.data[,10], my.data[,3])
cor(my.data[,10], my.data[,4])
cor(my.data[,10], my.data[,5])
cor(my.data[,10], my.data[,6])
cor(my.data[,10], my.data[,7])
#Scatterplots for variable of interest( Rank ID) and the other variables
plot(my.data[,1], my.data[,10], xlab = "Journ ID", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Journ ID")
plot(my.data[,2], my.data[,10], xlab = "Number of citations", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Number of citations")
plot(my.data[,3], my.data[,10], xlab = "Impact Factor", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Impact Factor")
plot(my.data[,4], my.data[,10], xlab = "5 years IF", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and 5 years IF")
plot(my.data[,5], my.data[,10], xlab = "ii", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and ii")
plot(my.data[,6], my.data[,10], xlab = "Articles", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Articles")
plot(my.data[,7], my.data[,10], xlab = "Half-life", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Half-life")
plot(my.data[,8], my.data[,10], xlab = "Eigen Factor", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Eigen Factor")
plot(my.data[,9], my.data[,10], xlab = "Art-i", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Art-i")
plot(my.data[,11], my.data[,10], xlab = "Number", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Number")
#Histograms for all the variables
hist(my.data[,10], xlab= "Rank ID", main="Histogram for Rank ID")
hist(my.data[,10], freq=F, prob=T, xlab= "Rank ID", main="Histogram for Rank ID")
lines(density(my.data[,10]))
hist(my.data[,2], xlab= "Number of cites", main="Histogram for Number of Cites")
hist(my.data[,2], freq=F, prob=T, xlab= "Number of Cites", main="Histogram for Number of Cites")
lines(density(my.data[,2]))
hist(my.data[,3], xlab= "Impact Factor", main="Histogram for Impact Factor")
hist(my.data[,7], xlab= " Half-Life", main="Histogram for Half-Life")
hist(my.data[,1], xlab= " Journ ID", main="Histogram for Journ ID")
hist(my.data[,2], freq=F, prob=T, xlab= "Number of Cites", main="Histogram for Number of Cites")
lines(density(my.data[,2]))
#Scaling the Rank ID to unit interval 
my.data[,10] <- (my.data[,10]-min(my.data[,10]))/(max(my.data[,10])-min(my.data[,10]))
hist(my.data[,10], xlab= "Rank ID", main="Histogram for Rank ID")
hist(my.data[,10], freq=F, prob=T, xlab= "Rank ID", main="Histogram for Rank ID")
#log transformation for the Journal ID
my.data[,1] <- log(my.data[,1])
#Scaling the Journal ID to unit interval
my.data[,1]<- (my.data[,1]-min(my.data[,1]))/(max(my.data[,1])-min(my.data[,1]))
#Scatterplot for transformed Jornal ID and Transformed Rank ID
plot(my.data[,1], my.data[,10], xlab = "Journ ID", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Journ ID")
#log transformation for the Number of citations
my.data[,2] <- log(my.data[,2])
#Scaling the Number of citations to unit interval
my.data[,2]<- (my.data[,2]-min(my.data[,2]))/(max(my.data[,2])-min(my.data[,2]))
#Scatterplot for transformed Number of citations and Transformed Rank ID
plot(my.data[,2], my.data[,10], xlab = "Number of citations", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Number of citations")
#log transformation for the Impact Factor
my.data[,3] <- log(my.data[,3])
#Scaling the Impact Factor to unit interval
my.data[,3]<- (my.data[,3]-min(my.data[,3]))/(max(my.data[,3])-min(my.data[,3]))
#Scatterplot for transformed Impact Factor and Transformed Rank ID
plot(my.data[,3], my.data[,10], xlab = "Impact Factor", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Impact Factor")
#log transformation for the Half-Life
my.data[,7] <- log(my.data[,7])
#Scaling the Half-Life to unit interval
my.data[,7]<- (my.data[,7]-min(my.data[,7]))/(max(my.data[,7])-min(my.data[,7]))
#Scatterplot for transformed Half-Life and Transformed Rank ID
plot(my.data[,7], my.data[,10], xlab = "Half-life", ylab="Rank ID", las=1, main="Scatterplot for Rank ID and Half-life")
#transformed data is transfered to my.data
my.data <- my.data[1:100, c(1:3,7,10)]
#create a file for the transformed data in txt file
write.table(my.data, "Manikanta-transformed.txt")
#loading package
source("AggWaFit718.R")
#Weighted Arithmetic Mean
fit.QAM(my.data[,c(1:4,5)])
#Ordered Weoghted Averaging Function
fit.OWA(my.data[,c(1:4,5)])       
#Choquet Integral
fit.choquet(my.data[,c(1:4,5)])
#Weighted Power Mean for P=0.5
fit.QAM(my.data[,c(1:4,5)],g=PM05,g.inv=PM05)
#Weighted Power Mean for p=2
fit.QAM(my.data[,c(1:4,5)],g=QM,g.inv=QM)
#Transforming back the variables 
my.data[,5] <- sqrt(my.data[,5])
my.data[,5] <- (sqrt(my.data[,5]))^2 
#Applying the question to find out the desired rank for the given values
my.data[,1] <- log(110)
my.data[,2] <-log(116)
my.data[,3] <- log(0.694)
my.data[,4] <- log(5)
QAM(c(0,0.54298,0.884376,0.06132520),c(4.70048,4.75359,0.36528,1.609438))
OWA(c(0.552172,0.2725562,0,0.175271),c(4.70048,4.75359,0.36528,1.609438)) 
QAM(c(0,0.08829,0.8296,0.8296),c(4.70048,4.75359,0.36528,1.609438),PM05,invPM05)
choquet(c(0.07844,0.13016,0.64807,0.14330),c(4.70048,4.75359,0.36528,1.609438))
