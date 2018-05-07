
###### Session 2: Nearest Neighbor Classification #########

# import the Data given in KNN assignment
knn_Asgnmt <- read.table("C:/Users/dhirannavara/Desktop/AcadGild/ML with R - Dr.Vinod/Nearest Neighbor Classifiers/kNN_Asgnmt_data.txt", header = T,sep = "\t")
View(knn_Asgnmt)
dim(knn_Asgnmt)
str(knn_Asgnmt)

# recode class variable
knn_Asgnmt$class<- factor(knn_Asgnmt$class, levels = c('A', 'B'), 
                          labels = c('ClassA', 'ClassB'))
table(knn_Asgnmt$class)

# plot the Class variable
barplot(table(knn_Asgnmt$class))

# replot the bar plot(with colour and Description)
text(barplot(table(knn_Asgnmt$class), col = c('green', 'red'),
             main = 'Bar Plot of class'), 0, 
     table(knn_Asgnmt$class), cex = 2, pos = 3)

# No need of Normalization, as scale used for the values for each variable is same 

# train and test data sets
# since 1st column is text (Class) column we should exclude that for calculation
knn_train <- knn_Asgnmt[1:6 , 2:3]

# how we will decide the classes?
# label vector
knn_train_labels <- knn_Asgnmt[1:6, 1]
str(knn_train)
str(knn_train_labels)

install.packages('class')
library(class)

# choose k as approx sqrt of n in training data set
# sqrt 6 = approx 2
# creat a function to predict the class for different coordinates

predict_class <- function(x,y){ 
                  knn(train = knn_train, test = c(x,y),
                      cl = knn_train_labels, k= 3)
}

# predict the class of coordinates (4,4)
predict_class(4,4)

# predict the class of coordinates (3.5,3.5)
predict_class(3.5,3.5)




