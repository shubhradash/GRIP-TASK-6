#IMPORT THE DATA
Irisdata <- read.csv("C:\\Users\\HP\\Downloads\\Iris.csv", header = TRUE)

#VIEW DATA

names(Irisdata)
class('Species')
class('SepalLengthcm')

#INSTALL PACKAGES
install.packages("rpart")
install.packages("caret", dependencies = TRUE)
install.packages("rpart.plot")
install.packages("data.tree")
install.packages("caTools")

#LOAD PACKAGES
library(rpart)
library(caret)
library(rpart.plot)
library(data.tree)
library(tidyverse)
library(caTools)


#SELECT INSIGHTFUL COLUMNS
Irisdata <- Irisdata %>% select(2:6)
Irisdata <- mutate(Irisdata, Species, SepalLengthCm=as.numeric(SepalLengthCm), SepalWidthCm=as.numeric(SepalWidthCm), PetalLengthCm=as.numeric(PetalLengthCm), PetalWidthCm=as.numeric((PetalWidthCm)))

#VISUALIZE THE DATA
print(graph1 <- Irisdata %>%
  group_by(Species , PetalLengthCm, PetalWidthCm) %>%
  ggplot(aes(PetalLengthCm, PetalWidthCm, color = Species)) + 
  geom_point() +  
  theme(legend.position = "none") +
  labs(title = ""))

print(graph2 <- Irisdata %>%
        group_by(Species , SepalLengthCm, SepalWidthCm) %>%
        ggplot(aes(SepalLengthCm, SepalWidthCm, color = Species)) + 
        geom_point() +  
        theme(legend.position = "none") +
        labs(title = ""))

#INSIGHTS FROM VISUALIZATION
#petal length of species setosa < 2.5
#petal length >= 2.5 then it is versicolor or virginca
#petal width  < 1.8 then it is versicolor

#MAKE THE TREE
tree <- rpart(Species ~ ., data = Irisdata)

#CHECKING THE ACCURACY OF THE MODEL
sample <- Irisdata$PetalLengthCm
train <- Irisdata$PetalLengthCm < 2.5
test <- Irisdata$Species=='Iris-setosa'

class(train)
class(test)
train <- as.factor(train)
test <- as.factor(test)
confusionMatrix(data=train, reference=test)

#VISUALIZE THE TREE
prp(tree)

