##########################################################################################
# ZADANIE 2.2 CZĘŚĆ PIERWSZA: REDUKCJA WYMIARU Z WYKORZYSTANIEM ALGORYTMU PCA
##########################################################################################

library(datasets)
library(ggplot2)
library(pastecs)

data(state)
dane <- as.data.frame(state.x77)

head(dane)
str(dane)

# badanie zmienności cech:

# wskaźniki sumaryczne:
t(stat.desc(dane))
# dla zmiennych Area, Population oraz Income obserwujemy największe zróżnicowanie

# wizualizacja zmienności cech: boxplot
# dominacja zmiennej Area
ggplot(stack(dane), aes(x=ind, y=values)) + 
  geom_boxplot()


# standaryzujemy dane, wyznaczamy składowe główne:
dane.pca <- prcomp(dane, retx=T, center=T, scale.=T)

# wykresy pudełkowe dla wyznaczonych składowych głównych
# dla zmiennej PC1 rozstęp międzykwartylowy ma największą wartość
pc <- data.frame(dane.pca$x)
ggplot(stack(pc), aes(x=ind, y=values)) +
  labs(title="Analiza rozproszenia składowych głównych.", 
       x="Składowe główne", y="Wartości") +
  geom_boxplot()

# wektory ładunków dla składowych głównych PC1, PC2, PC3, PC4:
print(dane.pca$rotation[,1:4])

# I wektor ładunków przypisuje w przybliżeniu jednakową wagę zmiennym:
# Illiteracy, Life Exp, Murder oraz HS Grad, zatem PC1 można interpretować
# jako wskaźnik poziomu edukacji oraz długości życia i popełnionych morderstw.

# II wektor ładunków przypisuje największe wagi dla zmiennych 
# Area, Income, Population, czyli PC2 można uznać za wskaźnik zaludnienia 
# oraz wysokości zarobków na danym obszarze

# III wektor ładunków przypisuje największą wagę zmiennej Population oraz Area,
# zatem opisuje poziom zaludnienia

# IV wektor ładunków przypisuje największą wagę zmiennej Frost, zatem wskazuje
# na częstość występowania mrozów w danym stanie

# analiza wariancji wyjaśnionej przez składowe główne:
summary(dane.pca)
# Największy procent zmienności odpowiada składowym odpowiednio:
# PC1: 45%
# PC2: 20%
# PC3: 14%

variance <- (dane.pca$sdev ^2)/sum(dane.pca$sdev^2)
cum.variance <- cumsum(variance)
barplot(variance)
df_zmienność <- data.frame(variance, cum.variance, names(pc))

# udział wyjaśnionej wariancji: scree plot
ggplot(df_zmienność, aes(x=names.pc., y=variance)) + 
  labs(x="Składowe główne", y="Wariancja (%)", 
       title="Procent zmienności dla poszczególnych składowych.") +
  geom_bar(stat='identity')

ggplot(df_zmienność, aes(x=names.pc., y=cum.variance)) + 
  labs(x="Składowe główne", y="Kumulatywna wariancja (%)", 
       title="Udział wyjaśnionej wariancji.") +
  geom_bar(stat='identity') +
  geom_hline(yintercept = c(0.8, 0.9), color="red") +
  scale_y_continuous(breaks=c(0.00, 0.25, 0.50, 0.75, 0.80, 0.90, 1.00),
                     labels=c("0.00", "0.25", "0.50", "0.75", "0.80", "0.90", "1.00"))

# Do wyjaśnienia 80% zmienności danych są potrzebne 3 składowe główne:
# PC1, PC2 oraz PC3
# Do wyjaśnienia 90% zmienności danych potrzebujemy już 4 składowych głównych:'
# PC1, PC2, PC3 oraz PC4
