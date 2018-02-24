
cereals <- read.csv("cereals_c.csv", header = T)

i <- 0
for(i in 1:ncol(cereals)){
  cereals[is.na(cereals[,i]),i] <- mean(cereals[,i], na.rm = TRUE)
}

library(psych)
describe(cereals)

# briefing the summary of variables
summary(cereals$calories)
summary(cereals$protein)
summary(cereals$vitamins)
summary(cereals$rating)

plot(cereals$calories)
hist(cereals$calories) # clories are normally distributed.
hist(cereals$cups)
hist(cereals$protein)
hist(cereals$fat)
hist(cereals$sodium)
hist(cereals$vitamins)
hist(cereals$weight)
hist(cereals$rating) # normally distributed 

boxplot(cereals$fiber) # outliers are present
boxplot(cereals$carbo)
boxplot(cereals$cups)  # outliers are present
boxplot(cereals$rating)# ooutliers are present
boxplot(cereals$sugars)

plot(cereals$calories,cereals$rating, xlab = "Calories", ylab = "Ratings",main = "Histogram of Calories v/s Rating ")
plot(cereals$fat,cereals$calories ) # fat increases as calories increases    
plot(cereals$fat,cereals$rating ) # *** rating goes down as fat increases.
plot(cereals$fiber,cereals$rating ) # only low fiber values are having higher rating.
plot(cereals$carbo,cereals$rating ) # carbo > 10 are having good rating.
plot(cereals$sugars,cereals$rating ) # rating goes down as sugar increases


# Finding Correlation between these variables

library(corrplot)
col_numeric <- sapply(cereals, is.numeric)
numericalVars <- cereals[,col_numeric]
corrMat <- cor(numericalVars)
corrMat

# Correlation Plot/Correlogram
install.packages("corrplot")
library(corrplot)
corrplot(corrMat, method= "number")
corrplot(corrMat, method= "pie")
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")

corrplot(corrMat, order = "AOE")
corrplot(corrMat, order = "hclust")
corrplot(corrMat, order = "FPC")
corrplot(corrMat, order = "alphabet")
corrplot(corrMat, order = "hclust", addrect = 2)
corrplot(corrMat, order = "hclust", addrect = 3,method= "number")
corrplot(corrMat, order = "hclust", addrect = 3)



# Questions on cereals dataset

library(dplyr)
cereals %>% group_by(type,mfr) %>%
  summarise(count=n(), avgCal=mean(calories),avgRtng=mean(rating),sumProtien=sum(protein),vtmnPercnt=mean(vitamins))

# which mfr's products has high average Calories
cereals %>% group_by(mfr) %>% summarise(avgCal=mean(calories))  %>% filter(avgCal==max(avgCal)) %>% select(mfr)

# which mfr's products has low average Calories
cereals %>% group_by(mfr) %>% summarise(avgCal=mean(calories))  %>% filter(avgCal==min(avgCal)) %>% select(mfr)

# which type of product has high avg of calories, cold or hot?
cereals %>% group_by(type) %>% summarise(avgCal=mean(calories))  %>% filter(avgCal==max(avgCal)) %>% select(type)

# which cold product has got highest Rating
cereals %>% group_by(type) %>% filter(type=="H") %>% filter(rating == max(rating)) %>% select(name)



