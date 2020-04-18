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

#dyskretyzacja nienadzorowana dla cechy Sepal.Width:
x <- iris[, "Sepal.Width"]

#algorytm equal width:
x.disc.eq.width <- discretize(x, method = "interval", breaks=3)
tab.x.eq.width <- table(x.disc.eq.width, Species)
tab.x.eq.width
matchClasses(tab.x.eq.width)

#algorytm equal frequency:
x.disc.eq.freq <- discretize(x, method="frequency", breaks=3, )
tab.x.eq.freq <- table(x.disc.eq.freq, Species)
tab.x.eq.freq
matchClasses(tab.x.eq.freq)

#algorytm k-means clustering:
x.disc.km.clus <- discretize(x, method="cluster", breaks=3)
tab.x.km.clus <- table(x.disc.km.clus, Species)
tab.x.km.clus
matchClasses(tab.x.km.clus)

#dyskretyzacja dla zadanych przedziałów 
x.disc.fixed <- discretize(x, method="fixed", breaks=c(-Inf, 2.85, 3.20, Inf))
tab.x.fixed <- table(x.disc.fixed, Species)
tab.x.fixed
matchClasses(tab.x.fixed)

#w przypadku dyskretyzacji dla cechy Sepal.Width najlepiej sprawdził się
#algorytm k-means, który dał wynik o zgodności 56%
#najgorzej wypadła dyskretyzacja według równej szerokości
#zgodność: 50.67%


#zgodność wyników dyskretyzacji dla cechy o najlepszych zdolnościach dyskryminacyjnych
#jest o (co najmniej) 40 punktów procentowych wyższa niż dla wyników
#dyskretyzacji przeprowadzonych na zmiennej Sepal.Width


#dyskretyzacja nienadzorowana dla cechy Petal.Width z wartościami odstąjącymi:
d1 <- d
d1[which.min(d1)] <- min(d1) - 2*IQR(d1)
d1[which.max(d1)] <- max(d1) + 2*IQR(d1)

#algorytm equal width:
d1.disc.eq.width <- discretize(d1, method = "interval", breaks=3)
tab.d1.eq.width <- table(d1.disc.eq.width, Species)
tab.d1.eq.width
matchClasses(tab.d1.eq.width)

#algorytm equal frequency:
d1.disc.eq.freq <- discretize(d1, method="frequency", breaks=3)
tab.d1.eq.freq <- table(d1.disc.eq.freq, Species)
tab.d1.eq.freq
matchClasses(tab.d1.eq.freq)

#algorytm k-means clustering:
d1.disc.km.clus <- discretize(d1, method="cluster", breaks=3)
tab.d1.km.clus <- table(d1.disc.km.clus, Species)
tab.d1.km.clus
matchClasses(tab.d1.km.clus)

#dyskretyzacja dla zadanych przedziałów 
d1.disc.fixed <- discretize(d1, method="fixed", breaks=c(-Inf, 0.75, 1.65, Inf))
tab.d1.fixed <- table(d1.disc.fixed, Species)
tab.d1.fixed
matchClasses(tab.d1.fixed)

#po zamianie wartości najmniejszej i największej wartościami
#odstającymi dyskretyzacja według równej szerokości
#dała wynik o poziomie zgodności 34.67% czyli aż o 61,33 punkta mniejszym

#dyskretyzacja najmniejsza wartość zastąpiona wartością odstającą

d2 <- d
d2[which.min(d2)] <- min(d2) - 2*IQR(d2)

#algorytm equal width:
d2.disc.eq.width <- discretize(d2, method = "interval", breaks=3)
tab.d2.eq.width <- table(d2.disc.eq.width, Species)
tab.d2.eq.width
matchClasses(tab.d2.eq.width)

#algorytm equal frequency:
d2.disc.eq.freq <- discretize(d2, method="frequency", breaks=3)
tab.d2.eq.freq <- table(d2.disc.eq.freq, Species)
tab.d2.eq.freq
matchClasses(tab.d2.eq.freq)

#algorytm k-means clustering:
d2.disc.km.clus <- discretize(d2, method="cluster", breaks=3)
tab.d2.km.clus <- table(d2.disc.km.clus, Species)
tab.d2.km.clus
matchClasses(tab.d2.km.clus)

#dyskretyzacja dla zadanych przedziałów 
d2.disc.fixed <- discretize(d2, method="fixed", breaks=c(-Inf, 0.75, 1.65, Inf))
tab.d2.fixed <- table(d2.disc.fixed, Species)
tab.d2.fixed
matchClasses(tab.d2.fixed)

#dyskretyzacja według równej szerokości wypada gorzej
