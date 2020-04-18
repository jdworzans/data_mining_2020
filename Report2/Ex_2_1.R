library(datasets)
library(ggplot2)
library(arules)
library(e1071)

data(iris)
attach(iris)

#najlepsze zdolności dyskryminacyjne cech: Petal.Length, Petal.Width
ggplot(iris, aes(x=Species, y=Petal.Length, color=Species)) +geom_boxplot()

ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) +geom_boxplot()

#widać to również na wykresie rozrzutu tych cech: Petal.Length, Petal.Width
ggplot(iris, aes(x=Petal.Width, y=Petal.Length, color=Species))+
  geom_point(size=3)

#najgorsze zdolności dyskryminacyjne cech: Sepal.Length, Sepal.Width
ggplot(iris, aes(x=Species, y=Sepal.Length, color=Species)) +geom_boxplot()

ggplot(iris, aes(x=Species, y=Sepal.Width, color=Species)) +geom_boxplot()

#wykres rozrzutu dla cech o słabej zdolności dyskryminacyjnej: Sepal.Length, Sepal.Width
ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species))+
  geom_point(size=4)

table(Species)
#dyskretyzacja według równej szerokości wypada gorzej
