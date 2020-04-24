##########################################################################################
# ZADANIE 2.2 CZĘŚĆ PIERWSZA: REDUKCJA WYMIARU Z WYKORZYSTANIEM ALGORYTMU PCA
##########################################################################################

library(datasets)
library(ggplot2)
library(pastecs)
library(plotly)
library(ggcorrplot)

# for installation see: https://github.com/vqv/ggbiplot
library(ggbiplot)

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

wariancja <- (dane.pca$sdev ^2)/sum(dane.pca$sdev^2)
kum.wariancja <- cumsum(wariancja)
df_zmienność <- data.frame(wariancja, kum.wariancja, names(pc))

# udział wyjaśnionej wariancji: scree plot
ggplot(df_zmienność, aes(x=names.pc., y=wariancja)) + 
  labs(x="Składowe główne", y="Wariancja (%)", 
       title="Procent zmienności dla poszczególnych składowych.") +
  geom_bar(stat='identity')

ggplot(df_zmienność, aes(x=names.pc., y=kum.wariancja)) + 
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

# (e)
# Wizualizacja 2D

dane.PCA = data.frame(dane.pca$x, state.abb, state.region, state.division)

# Z podziałem na regiony:
ggplot(dane.PCA, aes(x=PC1, y=PC2, col=state.region)) + geom_point()

# Z podziałem na dywizje:
ggplot(dane.PCA, aes(x=PC1, y=PC2, col=state.division)) + geom_point()

# Wizualizacja 3D

plot_ly(
  dane.PCA, x=~PC1, y=~PC2, z=~PC3, color=~state.region,
  text=~state.abb, type="scatter3d", textfont=list(size=12)
)

plot_ly(
  dane.PCA, x=~PC1, y=~PC2, z=~PC3, color=~state.division,
  text=~state.abb, type="scatter3d", textfont=list(size=12)
)

# Na wykresach rozrzutu możemy zauważyć pewne naturalne grupy.
# Widać, że stany z południowego regionu są dość dobrze odseparowane od reszty.
# Poza południowymi stanami, pozostałe dywizje znajdują się dość blisko siebie
# i trudno byłoby je odseparować.
# Widać, że charakterystycznym stanem jest Alaska[AK], która jest zdecydowanie odseparowana od reszty zbioru.
# Na trójwymiarowym wykresie rezrzutu widać również, że wyróżniającymi się stanami
# są California [CA], Nowy York [NY], Texas [TX] oraz Floryda [FL]


# (f)

ggbiplot(dane.pca, scale=0, groups=state.region, labels.size=20, ellipse=T)

# Na dwuwykresie, możemy zauważyć, że wskaźnik morderstw może być skorelowany z analfabetyzmem.
# Ponadto, co wydaje się być dość intuicyjne, wskaźnik morderstw wygląda na ujemnie skorelowany
# z oczekiwaną długością życia. Ponadto, z wykresu wynika, że wskaźnik mroźnych dni w ciągu roku jest
# skorelowany z oczekiwaną długością życia i negatywnie skorelowany ze wskaźnikiem morderstw.
# Widać również, że populacja jest skorelowana z powierzchnią stanu, a procent wyższego wykształcenia
# jest skorelowany z przychodem.

ggcorrplot(cor(dane), colors=c("steelblue","white","darkred"), lab=T)

# Korzystając z macierzy korelacji, możemy zauważyć, że większość z naszych przewidywań jest słuszna.
# Należy jednak zaznaczyć, że współczynnik korelacji między ilością mroźnych dni, a oczekiwaną długością życia,
# jest niższy, niż moglibyśmy tego oczekiwać, po spojrzeniu na dwuwykres. Ponadto, widzimy, że wbrew dwuwykresowi,
# współczynnik korelacji pomiędzy powierzchnią stanu, a populacją jest bardzo bliski 0.



