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

#dyskretyzacja nienadzorowana dla cechy Petal.Width:
d <- iris[, "Petal.Width"]

#algorytm equal width:
d.disc.eq.width <- discretize(d, method = "interval", breaks=3)
tab.d.eq.width <- table(d.disc.eq.width, Species)
tab.d.eq.width
matchClasses(tab.d.eq.width)

#algorytm equal frequency:
d.disc.eq.freq <- discretize(d, method="frequency", breaks=3)
tab.d.eq.freq <- table(d.disc.eq.freq, Species)
tab.d.eq.freq
matchClasses(tab.d.eq.freq)

#algorytm k-means clustering:
d.disc.km.clus <- discretize(d, method="cluster", breaks=3)
tab.d.km.clus <- table(d.disc.km.clus, Species)
tab.d.km.clus
matchClasses(tab.d.km.clus)

#dyskretyzacja dla zadanych przedziałów 
d.disc.fixed <- discretize(d, method="fixed", breaks=c(-Inf, 0.75, 1.65, Inf))
tab.d.fixed <- table(d.disc.fixed, Species)
tab.d.fixed
matchClasses(tab.d.fixed)

#najlepiej sprawdzają się:
#dyskretyzacja według równej szerokości; 
#dyskretyzacja oparta na algorytmie k-means;
#dyskretyzacja z przedziałami wyznaczonymi zdalnie;
#zgodność: 96%
#najgorszy wynik otrzymujemy dla dyskretyzacji według równej częstości
#zgodność: 94.67%

#dyskretyzacja według równej szerokości wypada gorzej
