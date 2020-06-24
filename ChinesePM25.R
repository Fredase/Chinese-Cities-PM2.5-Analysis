##This is a project to analyze PM2.5 levels in 5 Chinese Cities namely: Beijing, Chengduo, Guangzhou, Shanghai 
## and Shenyang.
##Read in the individual data to get a feel of the data
library('data.table')
library('mice')
library('VIM')
library(ggplot2)
library("lubridate")
library(rsq)
library(dplyr)
library(gridExtra)
library(dummies)
library(factoextra)
library(rpart)
##Set working directory
setwd("C:/Users/Ezinne/Desktop/ChineseCitiesPM2.5")

## Read in the datasets using the fread function
Beijing <- fread("C:\\Users\\Ezinne\\Desktop\\ChineseCitiesPM2.5\\BeijingPM2.5.csv", sep = ",", header = TRUE)
Chengdu <- fread("C:\\Users\\Ezinne\\Desktop\\ChineseCitiesPM2.5\\ChengduPM2.5.csv", sep = ",", header = TRUE)
Guangzhou <- fread("C:\\Users\\Ezinne\\Desktop\\ChineseCitiesPM2.5\\GuangzhouPM2.5.csv", sep = ",", header = TRUE)
Shanghai <- fread("C:\\Users\\Ezinne\\Desktop\\ChineseCitiesPM2.5\\ShanghaiPM2.5.csv", sep = ",", header = TRUE)
Shenyang <- fread("C:\\Users\\Ezinne\\Desktop\\ChineseCitiesPM2.5\\ShenyangPM2.5.csv", sep = ",", header = TRUE)
str(Beijing) ##Check the structure of the Beijng dataset
summary(Beijing) ##Take summary statistics


##The summary shows there is a high number of missing values, investigate further to find proportion across axes
percent_missing <- function(x){
  sum(is.na(x))/length(x) * 100
}
apply(Beijing, 1, percent_missing) ##apply the new function to the percentage of misssing values per column
apply(Beijing, 2, percent_missing) ## apply smae formula to the rows
##There are a high percentage of missing values in PM variables so we have to find a way to impute data to 
##the missing values using the MICE package
md.pattern(Beijing) ##Returns the pattern of the missing values in the dataset
md.pairs(Beijing) ## Calculates the frequency for each pattern for all variable pairs
##The VIM package helps us to obtain a visual representation of the missing values in the data
Beijing_aggr <- VIM::aggr(Beijing, col = c('navyblue', 'red'), numbers = TRUE,
                          sortVars = TRUE, labels = names(Beijing), cex.axis = 0.7, gap = 3,
                          ylab = c("Histogram of missing values", "Missing pattern"))
##The plot of the pattern shows that only 36% of the data is complete while PM_Dongsihuan, PM_Nongzhanguan
##and PM_Dongsi account for 48% 0f the missing data
##Alternatively we can use a margin plot to visualize the pairs of missing data 
##E.g. we can plot the pairs of PM_Dongsihuan and PM-Nongzhanguan like this
marginplot(Beijing[, c("PM_Dongsihuan", "PM_Nongzhanguan")], col = mdc(1:2),
           cex.numbers = 1.3, cex = 1.2, pch = 19)
##To impute the missing values for all 5 data files, we use the mice function from the mice package
tempdata <- mice(Beijing, m = 5, maxit = 5, 
                 method = ifelse(colnames(Beijing) == c("PM_Dongsi", "PM_Dongshuan",
                                                         "PM_Nongzhanguan", "PM_US Post"), "pmm", "" ), seed = 12345)

tempdata1 <- mice(Shenyang, m = 5, maxit = 5, seed = 12345,
                  method = ifelse(colnames(Shenyang) == c("PM_Taiyuanjie", "PM_US Post", "PM_Xiaoheyan", "precipitation", "Iprec"), "pmm", ""))


tempdata2 <- mice(Shanghai, m = 5, maxit = 5,
                  method = ifelse(colnames(Shanghai) == c("PM_Jingan", "PM_US Post", "PM_Xuhui"), "pmm", ""), seed = 12345)


tempdata3 <- mice(Chengdu, m = 5, maxit = 5,
                  method = ifelse(colnames(Chengdu) == c("PM_Caotangsi", "PM_Shahepu" , "PM_US Post"), "pmm", ""), seed = 12345)

tempdata4 <- mice(Guangzhou, m = 5, maxit = 5, 
                  method = ifelse(colnames(Guangzhou) == c("PM_City Station","PM_5th Middle School", "PM_US Post"), "pmm", ""), seed = 12345)


##This yields a more imputed dataset though 'PM_Dongshuan" was not imputed
summary(tempdata) ##summarizes the imputed data
##Replace the missing values (PM) in the 5 datasets using the 'complete' function
BeijingComplete <- complete(tempdata) 
ShenyangComplete <- complete(tempdata1)
ShanghaiComplete <- complete(tempdata2)
ChengduComplete <- complete(tempdata3)
GuangzhouComplete <- complete(tempdata4)
##You can investigate further to see the how the columns with missing data have been imputed
summary(BeijingComplete)

##WE can compare the distributions of the observed and imputed data using some useful plots
xyplot(tempdata, PM_Dongsi ~ DEWP + HUMI + PRES + TEMP, pch = 19, cex = 0.8)
##The shape of the imputed values matches the shape of the observed values meaning that the imputed values 
##are plausible
##Alternatively we can use 'densityplot(tempdata)' or 'stripplot(tempdata)' for same investigation

##After confirming that the imputed values are indeed plausible, and since the imputation process is time
##consuming, we save our complete datasets for the analysis
write.table(BeijingComplete, file = "BeijingComplete.csv", sep = ",", row.names = FALSE)
write.table(ShenyangComplete, file = "ShenyangComplete.csv", sep = ",", row.names = FALSE)
write.table(ShanghaiComplete, file = "ShanghaiComplete.csv", sep = ",", row.names = FALSE)
write.table(ChengduComplete, file = "ChengduComplete.csv", sep = ",", row.names = FALSE)
write.table(GuangzhouComplete, file = "GuangzhouComplete.csv", sep = ",", row.names = FALSE)

##Read in the imputed dataset for Beijing
Beijing <- read.table("BeijingComplete.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

##summary statistics
summary(Beijing)

##To investigate the variables for outliers by plotting a boxplot for the PM variables
p1 <- ggplot(Beijing, aes(season, PM_Dongsi)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "navyblue", outlier.size = 4)  + 
  ggtitle("Dongsi PM 2.5 Data") + labs(x = "Season", y = "Dongsi PM Readings")

p2 <- ggplot(Beijing, aes(season, PM_Dongsihuan))  + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "navyblue", outlier.size = 4) + ggtitle("Dongsihuan PM 2.5 Data") + 
  labs(x = "Season", y = "Dongsihuan PM Readings")

p3 <- ggplot(Beijing, aes(season, PM_Nongzhanguan))  + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "navyblue", outlier.size = 4)  + 
  ggtitle("Nongzhanguan PM 2.5 Data") + labs(x = "Season", y = "Nongzhanguan PM Readings")

p4 <- ggplot(Beijing, aes(season, PM_US.Post)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "navyblue", outlier.size = 4) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = "US Post PM Readings")

grid.arrange(p1, p2, p3, p4)

##Remove most of the outliers with the following line of code
BeijingClean <- Beijing[(Beijing$PM_Dongsi <= 300 & Beijing$PM_Dongsihuan <= 300 & Beijing$PM_Nongzhanguan <= 300 & Beijing$PM_US.Post <= 300),]
BeijingClean <- BeijingClean[!(BeijingClean$precipitation == 999990.0 & BeijingClean$Iprec == 999990.0),]
BeijingClean <- na.omit(BeijingClean)
dim(BeijingClean) ## This has a final 49260 rows
##Further Investigation of the final dataset shows a much less skewed distribution
lattice::histogram(BeijingClean$PM_Dongsi)
lattice::histogram(BeijingClean$PM_Dongsihuan)
lattice::histogram(BeijingClean$PM_Nongzhanguan)
lattice::histogram(BeijingClean$PM_US.Post)
##Alternatively we use the boxplots
p1 <- ggplot(BeijingClean, aes(season, PM_Dongsi)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "navyblue", outlier.size = 4) + 
  ggtitle("Dongsi PM 2.5 Data") + labs(x = "Season", y = "Dongsi PM Readings")

p2 <- ggplot(BeijingClean, aes(season, PM_Dongsihuan)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "navyblue", outlier.size = 4) + 
  ggtitle("Dongsihuan PM 2.5 Data") + labs(x = "Season", y = "Dongsihuan PM Readings")

p3 <- ggplot(BeijingClean, aes(season, PM_Nongzhanguan)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "navyblue", outlier.size = 4) + 
  ggtitle("Nongzhanguan PM 2.5 Data") + labs(x = "Season", y = "Nongzhanguan PM Readings")

p4 <- ggplot(BeijingClean, aes(season, PM_US.Post)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "navyblue", outlier.size = 4) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = "US Post PM Readings")

grid.arrange(p1, p2, p3, p4)

##Create a DateTime variable for use in line plots
DateTime <- with(BeijingClean, ymd_h(paste(year, month, day, hour, sep= ' ')))
par(mfrow = c(2, 2))
plot(DateTime, log(BeijingClean$PM_Dongsi), type = "l", xlab = "Year", ylab = " ", main = "Dongsi")
plot(DateTime, BeijingClean$PM_Dongsihuan, type = "l", xlab = "Year", ylab = " ", main = "Dongsihuan")
plot(DateTime, BeijingClean$PM_Nongzhanguan, type = "l", xlab = "Year", ylab = " ", main = "Nongzhanhuan")
plot(DateTime, BeijingClean$PM_US.Post, type = "l", xlab = "Year", ylab = " ", main = "US Post")

##Investigate the effect of some variables on PM readings
## Temperature
a1 <- ggplot(BeijingClean, aes(TEMP, PM_Dongsi)) + 
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) + 
  labs(x = "Temperature", y = " ") + ggtitle("Dongsi") 
a2 <- ggplot(BeijingClean, aes(TEMP, PM_Dongsihuan)) + 
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) + 
  labs(x = "Temperature", y = " ") + ggtitle("Dongsihuan")
a3 <- ggplot(BeijingClean, aes(TEMP, PM_Nongzhanguan)) + 
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) + 
  labs(x = "Temperature", y = " ") + ggtitle("Nongzhanguan") 
a4 <- ggplot(BeijingClean, aes(TEMP, PM_US.Post)) + 
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) + 
  labs(x = "Temperature", y = " ") + ggtitle("US Post") 
grid.arrange(a1, a2, a3, a4)

## Pressure

b1 <- ggplot(BeijingClean, aes(PRES, PM_Dongsi)) + 
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) + 
  labs(x = "Pressure", y = " ") + ggtitle("Dongsi") 

b2 <- ggplot(BeijingClean, aes(PRES, PM_Dongsihuan)) + 
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) + 
  labs(x = "Pressure", y = " ") + ggtitle("Dongsihuan")

b3 <- ggplot(BeijingClean, aes(PRES, PM_Nongzhanguan)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Pressure", y = " ") + ggtitle("Nongzhanguan") 

b4 <- ggplot(BeijingClean, aes(PRES, PM_US.Post)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Pressure", y = " ") + ggtitle("US Post") 

grid.arrange(b1, b2, b3, b4)

##Humidity
c1 <- ggplot(BeijingClean, aes(HUMI, PM_Dongsi)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Humidity", y = " ") + ggtitle("Dongsi") 

c2 <- ggplot(BeijingClean, aes(HUMI, PM_Dongsihuan)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Humidity", y = " ") + ggtitle("Dongsihuan")

c3 <- ggplot(BeijingClean, aes(HUMI, PM_Nongzhanguan)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Humidity", y = " ") + ggtitle("Nongzhanguan") 

c4 <- ggplot(BeijingClean, aes(HUMI, PM_US.Post)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Humidity", y = " ") + ggtitle("US Post") 

grid.arrange(c1, c2, c3, c4)

##Dew Point

d1 <- ggplot(BeijingClean, aes(DEWP, PM_Dongsi)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Dew Point", y = " ") + ggtitle("Dongsi") 

d2 <- ggplot(BeijingClean, aes(DEWP, PM_Dongsihuan)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Dew Point", y = " ") + ggtitle("Dongsihuan")

d3 <- ggplot(BeijingClean, aes(DEWP, PM_Nongzhanguan)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Dew Point", y = " ") + ggtitle("Nongzhanguan") 

d4 <- ggplot(BeijingClean, aes(DEWP, PM_US.Post)) +
  geom_point(aes(color = factor(season)),size = 1.5, alpha = 1) +
  labs(x = "Dew Point", y = " ") + ggtitle("US Post") 

grid.arrange(d1, d2, d3, d4)

##Fit a model to understand significant factors in the data
BM <- lm(PM_US.Post ~ year + factor(month) + hour + log(HUMI) + TEMP +log(PRES) + factor(cbwd) + Iws + precipitation + Iprec, data = BeijingClean)
summary(BM)
rsq(BM) ##detremines the goodness of fit
##The goodness of fit isnt encouraging so we proceed to carry out a principal component analysis(PCA)
##First we remove the dependent variables (PM readings) and some other unrequired variables
BC <- BeijingClean[-c(1, 7:10)]
str(BC) ##To confirm the variables are dropped

BC$season <- as.factor(BC$season) ##converts the season variable to a factor
BC$month <- as.factor(BC$month)
#Create a dummy dataframe
nd <- dummy.data.frame(BC, names = c("month", "season", "cbwd"))

##Split the nd dataset into train and test sets
set.seed(12345) ##set seed for reproducibility
sample <- sample.int(n = nrow(nd), size = floor(0.8 * nrow(nd)), replace = FALSE) ##Create the sample size
##Based on the sample size, create the train and test sets
train <- nd[sample, ]
test <- nd[-sample, ]
pca <- prcomp(train, scale. = TRUE) ## Run the PCA on the train set
names(pca) ## the 5 useful measures of the PCA
summary(pca)
biplot(pca, scale = 0)  ## plots the resultant principal components
fviz_eig(pca) ##Visualizes the eigenvalues (scree plot)
##Alternatively we can get the scree plot using R base
prop_varex <- (pca$sdev ^ 2)/sum(pca$sdev ^ 2)
plot(prop_varex, type = "b", xlab = "Principal Component", ylab = "Proportion of Varaince Explained" )
##plot a cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component", ylab = "Cumulative Proportion of Varaince Explained", type = "b")
##The plot shows that ~25 components result in variance close 98%. We select the number of components to proceed to the modelling stage
train.data <- data.frame(PM = BeijingClean$PM_US.Post[1:now(train)], pca$x)
head(train.data)
train.data <- train.data[, 1:25]
##Run a decision tree
rpart.model <- rpart(PM ~ ., data = train.data, method = "anova") 
summary(rpart.model)
## Transform test into PCA
test.data <- predict(pca, newdata = test)
test.data <- as.data.frame(test.data)
head(test.data)
##Select the first 25 Components
test.data <- test.data[, 1:24]
##Make prediction on the test data
rpart.prediction <- predict(rpart.model, test.data)

##Compare means of the different PM readings from 2010 to 2015
B <- aggregate(cbind(PM_Dongsi, PM_Dongsihuan, PM_Nongzhanguan, PM_US.Post) ~ year, BeijingClean, mean)
##Reshape B and then plot
BMelt <- reshape2::melt(B, id.vars = "year", )
color <- c("turquoise", "tan1", "red", "darkred")
ggplot(BMelt, aes(year, value)) + geom_line(aes(col = variable), size = 2) + 
  labs(x = "Year", y = " ") + ggtitle("Beijing PM2.5 Data 2010-2015") +
  scale_color_manual(values = color) + theme_bw()

##Shanghai
Shanghai <- read.table("ShanghaiComplete.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
str(Shanghai)
summary(Shanghai)

##Proceed to cleaning stage, first investigate the PM readings for outliers using boxplots
s1 <- ggplot(Shanghai, aes(season, PM_Jingan)) +
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2)  + 
  ggtitle("Jingan PM 2.5 Data") + labs(x = "Season", y = " ")

s2 <- ggplot(Shanghai, aes(season, PM_US.Post))  +
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = " ")

s3 <- ggplot(Shanghai, aes(season, PM_Xuhui)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) +
  ggtitle("Xuhui PM 2.5 Data") + labs(x = "Season", y = " ")

grid.arrange(s1, s2, s3,nrow = 1 )

##Remove outliers from the PM readings
ShanghaiClean <- Shanghai[(Shanghai$PM_Jingan <= 150 & Shanghai$PM_US.Post <= 150 & Shanghai$PM_Xuhui <= 150), ]
ShanghaiClean <- na.omit(ShanghaiClean)
dim(ShanghaiClean) #The cleaning process returns 45919 rows from the original 52584
##Visualize the new datasets using histograms and boxplots
histogram(ShanghaiClean$PM_Jingan)
histogram(ShanghaiClean$PM_US.Post)
histogram(ShanghaiClean$PM_Xuhui)

sc1 <- ggplot(ShanghaiClean, aes(season, PM_Jingan)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("Jingan PM 2.5 Data") + labs(x = "Season", y = " ")

sc2 <- ggplot(ShanghaiClean, aes(season, PM_US.Post)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = " ")

sc3 <- ggplot(ShanghaiClean, aes(season, PM_Xuhui)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("Xuhui PM 2.5 Data") + labs(x = "Season", y = " ")

grid.arrange(sc1, sc2, sc3)

DateTime <- with(ShanghaiClean, ymd_h(paste(year, month, day, hour, sep= ' ')))
par(mfrow = c(3, 1))
plot(DateTime, ShanghaiClean$PM_Jingan, type = "l", xlab = "Year", ylab = " ", main = "Jingan")
plot(DateTime, ShanghaiClean$PM_US.Post, type = "l", xlab = "Year", ylab = " ", main = "US Post")
plot(DateTime, ShanghaiClean$PM_Xuhui, type = "l", xlab = "Year", ylab = " ", main = "Xuhui")

##Temperature vs individual PM2.5 variables
ggplot(ShanghaiClean, aes(TEMP, PM_Jingan)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on Jingan PM2.5 readings")

ggplot(ShanghaiClean, aes(TEMP, PM_US.Post)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on US Post PM2.5 readings")

ggplot(ShanghaiClean, aes(TEMP, PM_Xuhui)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on Xuhui PM2.5 readings")

##Pressure vs individual PM2.5 variables
ggplot(ShanghaiClean, aes(PRES, PM_Jingan)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on Jingan PM2.5 readings")

ggplot(ShanghaiClean, aes(PRES, PM_US.Post)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on US Post PM2.5 readings")

ggplot(ShanghaiClean, aes(PRES, PM_Xuhui)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on Xuhui PM2.5 readings")

##Humidity Vs Individual PM2.5 variables
ggplot(ShanghaiClean, aes(HUMI, PM_Jingan)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on Jingan PM2.5 readings")

ggplot(ShanghaiClean, aes(HUMI, PM_US.Post)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on US Post PM2.5 readings")

ggplot(ShanghaiClean, aes(HUMI, PM_Xuhui)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on Xuhui PM2.5 readings")

##Dew Point Vs PM2.5 variables
ggplot(ShanghaiClean, aes(DEWP, PM_Jingan)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on Jingan PM2.5 readings")

ggplot(ShanghaiClean, aes(DEWP, PM_US.Post)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on US Post PM2.5 readings")

ggplot(ShanghaiClean, aes(DEWP, PM_Xuhui)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on Xuhui PM2.5 readings")

##Compare means of the different PM readings from 2010 to 2015
SH <- aggregate(cbind(PM_Jingan, PM_US.Post, PM_Xuhui) ~ year, ShanghaiClean, mean)
##Reshape SH and then plot
SHMelt <- reshape2::melt(SH, id.vars = "year")
color1 = c("navyblue", "turquoise", "magenta")
ggplot(SHMelt, aes(year, value)) + 
  geom_line(aes(col = variable), size = 2) + 
  labs(x = "Year", y = " ") + ggtitle("Shanghai PM2.5 Data 2010-2015") + 
  scale_color_manual(values = color1) + theme_dark()

##Chengdu
Chengdu <- read.table("ChengduComplete.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
str(Chengdu)
summary(Chengdu)
##Investigate for outliers using histograms
histogram(Chengdu$PM_Caotangsi)
histogram(Chengdu$PM_Shahepu)
histogram(Chengdu$PM_US.Post)

##or Boxplots

cc1 <- ggplot(Chengdu, aes(season, PM_Caotangsi)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("Caotangsi PM 2.5 Data") + labs(x = "Season", y = " ")

cc2 <- ggplot(Chengdu, aes(season, PM_Shahepu)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) +
  ggtitle("Shahepu PM 2.5 Data") + labs(x = "Season", y = " ")

cc3 <- ggplot(Chengdu, aes(season, PM_US.Post)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = " ")

grid.arrange(cc1, cc2, cc3)
##Clean the data by first droping most outliers
ChengduClean <- Chengdu[(Chengdu$PM_Caotangsi <= 200 & Chengdu$PM_Shahepu <= 200 & Chengdu$PM_US.Post <= 200),]
ChengduClean <- na.omit(ChengduClean)
dim(ChengduClean) ##Yields a new dataset with 46093 rows

##Visualize the cleaned datasets using histograms and boxplots
cc4 <- ggplot(ChengduClean, aes(season, PM_Caotangsi)) + geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + ggtitle("Caotangsi PM 2.5 Data") + labs(x = "Season", y = " ")
cc5 <- ggplot(ChengduClean, aes(season, PM_Shahepu)) + geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + ggtitle("Shahepu PM 2.5 Data") + labs(x = "Season", y = " ")
cc6 <- ggplot(ChengduClean, aes(season, PM_US.Post)) + geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = " ")
grid.arrange(cc4, cc5, cc6)

##Investigate the PM data over time
DateTime <- with(ChengduClean, ymd_h(paste(year, month, day, hour, sep= ' ')))
par(mfrow = c(3, 1))
plot(DateTime, ChengduClean$PM_Caotangsi, type = "l", xlab = "Year", ylab = " ", main = "Caotangsi")
plot(DateTime, ChengduClean$PM_Shahepu, type = "l", xlab = "Year", ylab = " ", main = "Shahepu")
plot(DateTime, ChengduClean$PM_US.Post, type = "l", xlab = "Year", ylab = " ", main = "US Post")

##Investigation of relationship between PM readings to different variabales over seasons
## Temperature
ggplot(ChengduClean, aes(TEMP, PM_Caotangsi)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + labs(x = "Temperature", y = " ") + 
  ggtitle("Effects of Temperature on Caotangsi PM2.5 readings")

ggplot(ChengduClean, aes(TEMP, PM_Shahepu)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on Shahepu PM2.5 readings")

ggplot(ChengduClean, aes(TEMP, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on US Post PM2.5 readings")

##Pressure
ggplot(ChengduClean, aes(PRES, PM_Caotangsi)) + 
  geom_point(aes(col = season)) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on Caotangsi PM2.5 readings")

ggplot(ChengduClean, aes(PRES, PM_Shahepu)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on Shahepu PM2.5 readings")

ggplot(ChengduClean, aes(PRES, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on US Post PM2.5 readings")

##Humidity
ggplot(ChengduClean, aes(HUMI, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on US Post PM2.5 readings")

ggplot(ChengduClean, aes(HUMI, PM_Shahepu)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on Shahepu PM2.5 readings")

ggplot(ChengduClean, aes(HUMI, PM_Caotangsi)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on Caotangsi PM2.5 readings")

##Dew Point
ggplot(ChengduClean, aes(DEWP, PM_Caotangsi)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on Caotangsi PM2.5 readings")

ggplot(ChengduClean, aes(DEWP, PM_Shahepu)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on Shahepu PM2.5 readings")

ggplot(ChengduClean, aes(DEWP, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on US Post PM2.5 readings")

##Compare means of the different PM readings from 2010 to 2015
CH <- aggregate(cbind(PM_Caotangsi, PM_Shahepu, PM_US.Post) ~ year, ChengduClean, mean)

##Reshape CH and then plot
CHMelt <- reshape2::melt(CH, id.vars = year)
color1 = c("navyblue", "turquoise", "magenta")
ggplot(CHMelt, aes(year, value)) + 
  geom_line(aes(col = variable), size = 2) + 
  labs(x = "Year", y = " ") + ggtitle("Chengdu PM2.5 Data 2010-2015") + 
  scale_color_manual(values = color1) + theme_light()

##Shenyang
Shenyang <- read.table("ShenyangComplete.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
str(Shenyang)
summary(Shenyang)

##Investigate for outliers using histograms and boxplots
histogram(Shenyang$PM_Taiyuanjie)
histogram(Shenyang$PM_US.Post)
histogram(Shenyang$PM_Xiaoheyan)

sh1 <- ggplot(Shenyang, aes(season, PM_Taiyuanjie)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("Taiyuanjie PM 2.5 Data") + labs(x = "Season", y = " ")

sh2 <- ggplot(Shenyang, aes(season, PM_US.Post)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = " ")

sh3 <- ggplot(Shenyang, aes(season, PM_Xiaoheyan)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("Xiaoheyan PM 2.5 Data") + labs(x = "Season", y = " ")

grid.arrange(sh1, sh2, sh3)

##Removes most outliers
ShenyangClean <- Shenyang[(Shenyang$PM_Taiyuanjie <= 200 & Shenyang$PM_US.Post <= 200 & Shenyang$PM_Xiaoheyan <= 200),]
dim(ShenyangClean) ##Yields data with 49166 rows

ssh1 <- ggplot(ShenyangClean, aes(season, PM_Taiyuanjie)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("Taiyuanjie PM 2.5 Data") + labs(x = "Season", y = " ")

ssh2 <- ggplot(ShenyangClean, aes(season, PM_US.Post)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = " ")

ssh3 <- ggplot(ShenyangClean, aes(season, PM_Xiaoheyan)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("Xiaoheyan PM 2.5 Data") + labs(x = "Season", y = " ")

grid.arrange(ssh1, ssh2, ssh3)

##PM Data over time
DateTime <- with(ShenyangClean, ymd_h(paste(year, month, day, hour, sep= ' ')))
par(mfrow = c(3, 1))
plot(DateTime, ShenyangClean$PM_Taiyuanjie, type = "l", xlab = "Year", ylab = " ", main = "Taiyuanjie")
plot(DateTime, ShenyangClean$PM_US.Post, type = "l", xlab = "Year", ylab = " ", main = "US Post")
plot(DateTime, ShenyangClean$PM_Xiaoheyan, type = "l", xlab = "Year", ylab = " ", main = "Xiaoheyan")

##Relationship with other variables
##Temperature
ggplot(ShenyangClean, aes(TEMP, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on US Post PM2.5 readings")

ggplot(ShenyangClean, aes(TEMP, PM_Xiaoheyan)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on Xiaoheyan PM2.5 readings")

ggplot(ShenyangClean, aes(TEMP, PM_Taiyuanjie)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on Taiyuanjie PM2.5 readings")

##Pressure
ggplot(ShenyangClean, aes(PRES, PM_Taiyuanjie)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on Taiyuanjie PM2.5 readings")

ggplot(ShenyangClean, aes(PRES, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on US Post PM2.5 readings")

ggplot(ShenyangClean, aes(PRES, PM_Xiaoheyan)) + 
  geom_point(aes(col = factor(season))) + 
  facet_wrap(~ season) + labs(x = "Pressure", y = " ") +
  ggtitle("Effects of Pressure on Xiaoheyan PM2.5 readings")

##Humidity
ggplot(ShenyangClean, aes(HUMI, PM_Xiaoheyan)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on Xiaoheyan PM2.5 readings")

ggplot(ShenyangClean, aes(HUMI, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on US Post PM2.5 readings")

ggplot(ShenyangClean, aes(HUMI, PM_Taiyuanjie)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on Taiyuanjie PM2.5 readings")

##Dew Point
ggplot(ShenyangClean, aes(DEWP, PM_Taiyuanjie)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on Taiyuanjie PM2.5 readings")

ggplot(ShenyangClean, aes(DEWP, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on US Post PM2.5 readings")

ggplot(ShenyangClean, aes(DEWP, PM_Xiaoheyan)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Dew Point", y = " ") + ggtitle("Effects of Dew Point on Xiaoheyan PM2.5 readings")

##Compare means of the different PM readings from 2010 to 2015
SY <- aggregate(cbind(PM_Taiyuanjie, PM_US.Post, PM_Xiaoheyan) ~ year, ShenyangClean, mean)

##Reshape SY and then plot
SYMelt <- reshape2::melt(SY, id.vars = "year")
color2 <- c( "tan1", "red", "darkred")
ggplot(SYMelt, aes(year, value)) + 
  geom_line(aes(col = variable), size = 2) + 
  labs(x = "Year", y = " ") + 
  ggtitle("Shenyang PM2.5 Data 2010-2015") + 
  scale_color_manual(values = color2) + theme_linedraw()
##Guangzhou
Guangzhou <- read.table("GuangzhouComplete.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
str(Guangzhou)
summary(Guangzhou)
##Impute the NAs in the US Post column with the median
Guangzhou$PM_US.Post[is.na(Guangzhou$PM_US.Post)] <- median(Guangzhou$PM_US.Post, na.rm = TRUE)

##Investigate for outliers using histograms and boxplots
histogram(Guangzhou$PM_City.Station)
histogram(Guangzhou$PM_5th.Middle.School)
histogram(Guangzhou$PM_US.Post)

gc1 <- ggplot(Guangzhou, aes(season, PM_City.Station)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("City Station PM 2.5 Data") + labs(x = "Season", y = " ")

gc2 <- ggplot(Guangzhou, aes(season, PM_5th.Middle.School)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("5th Middle School PM 2.5 Data") + labs(x = "Season", y = " ")

gc3 <- ggplot(Guangzhou, aes(season, PM_US.Post)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = " ")

grid.arrange(gc1, gc2, gc3)

##Remove outliers
GuangzhouClean <- Guangzhou[(Guangzhou$PM_City.Station <= 140 & Guangzhou$PM_5th.Middle.School <= 130 & Guangzhou$PM_US.Post <= 100),]
GuangzhouClean <- GuangzhouClean[!(GuangzhouClean$PM_US.Post < 10),]
dim(GuangzhouClean) ##Returns a new dataset with 45928 rows

gc4 <- ggplot(GuangzhouClean, aes(season, PM_City.Station)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("City Station PM 2.5 Data") + labs(x = "Season", y = " ")

gc5 <- ggplot(GuangzhouClean, aes(season, PM_5th.Middle.School)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("5th Middle School PM 2.5 Data") + labs(x = "Season", y = " ")

gc6 <- ggplot(GuangzhouClean, aes(season, PM_US.Post)) + 
  geom_boxplot(aes(fill = factor(season)), outlier.colour = "blue", outlier.size = 2) + 
  ggtitle("US Post PM 2.5 Data") + labs(x = "Season", y = " ")

grid.arrange(gc4, gc5, gc6)

##PM2.5 pattern over time
DateTime <- with(GuangzhouClean, ymd_h(paste(year, month, day, hour, sep= ' ')))
par(mfrow = c(3, 1))
plot(DateTime, GuangzhouClean$PM_City.Station, type = "l", xlab = "Year", ylab = " ", main = "City Station")
plot(DateTime, GuangzhouClean$PM_Shahepu, type = "l", xlab = "Year", ylab = " ", main = "5th Middle School")
plot(DateTime, GuangzhouClean$PM_US.Post, type = "l", xlab = "Year", ylab = " ", main = "US Post")
##Investigation of relationship between PM readings to different variabales over seasons
## Temperature
ggplot(GuangzhouClean, aes(TEMP, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on US Post PM2.5 readings")

ggplot(GuangzhouClean, aes(TEMP, PM_City.Station)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on City Station PM2.5 readings")

ggplot(GuangzhouClean, aes(TEMP, PM_5th.Middle.School)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Temperature", y = " ") + ggtitle("Effects of Temperature on 5th Middle School PM2.5 readings")

##Pressure
ggplot(GuangzhouClean, aes(PRES, PM_5th.Middle.School)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on 5th Middle School PM2.5 readings")

ggplot(GuangzhouClean, aes(PRES, PM_City.Station)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on City Station PM2.5 readings")

ggplot(GuangzhouClean, aes(PRES, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Pressure", y = " ") + ggtitle("Effects of Pressure on US Post PM2.5 readings")

##Humidity
ggplot(GuangzhouClean, aes(HUMI, PM_US.Post)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on US Post PM2.5 readings")

ggplot(GuangzhouClean, aes(HUMI, PM_City.Station)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on City Station PM2.5 readings")

ggplot(GuangzhouClean, aes(HUMI, PM_5th.Middle.School)) + 
  geom_point(aes(col = factor(season))) + facet_wrap(~ season) + 
  labs(x = "Humidity", y = " ") + ggtitle("Effects of Humidity on 5th Middle School PM2.5 readings")

####Compare means of the different PM readings from 2010 to 2015
GH <- aggregate(cbind(PM_City.Station, PM_5th.Middle.School, PM_US.Post) ~ year, GuangzhouClean, mean)
##Reshape data and then plot
GHMelt <- reshape2::melt(GH, id.vars = "year")
color3 <- c("royalblue", "green", "purple")
ggplot(GHMelt, aes(year, value)) + 
  geom_line(aes(col = variable), size = 2) + 
  labs(x = "Year", y = " ") + 
  ggtitle("Guangzhou PM2.5 Data 2010-2015") + 
  scale_color_manual(values = color3) + theme_grey()
##Proceed to work with US Post columns of the Beijing, Chengdu, Shanghai and Shenyang datasets
df1 <- BeijingClean %>% select(year, month, hour, season, PM_US.Post)
df2 <- ShanghaiClean %>% select(year, month, hour, season, PM_US.Post)
df3 <- ChengduClean %>% select(year, month, hour, season, PM_US.Post)
df4 <- ShenyangClean %>% select(year, month, hour, season, PM_US.Post)
