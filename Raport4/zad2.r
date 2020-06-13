library(mlbench)
library(MASS)
library(cluster)
library(factoextra)
library(ggplot2)
library(e1071)
library(clValid)
data(Glass)

# sprawdzamy, czy w zbiorze nie ma wartości brakujących:
sum(is.na(Glass)) 
# w zbiorze nie ma wartości brakująch

# zbiór danych zawiera 214 obserwacji, zatem nie musimy losowo wybierać podzbioru

glass.cechy <- Glass[, -10]
glass.real.etykiety <- Glass[, 10]
glass.cechy.scaled <- scale(glass.cechy)

# wskaźniki wewnętrzne:
metody <- c("pam", "agnes", "kmeans", "diana",  "clara", "model")
zakres <- 2:6

internal.validation <- clValid(glass.cechy, nClust=zakres, clMethods=metody, validation="internal")

summary(internal.validation)
optimalScores(internal.validation)

par(mfrow = c(3, 1))
plot(internal.validation, lwd=2)

# stabilność:
stability.validation <- clValid(glass.cechy, nClust=zakres, clMethods=metody, validation="stability")
summary(stability.validation)
optimalScores(stability.validation)

par(mfrow = c(3,1))
plot(stability.validation, measure=c("APN","AD","ADM"), lwd=2)
# w zbiorze mamy cechy liczbowe (bez zmiennej Typ - tutaj factor)

# Nie stosujemy standaryzacji. Podczas analizy chemicznej, do rozstrzygnięcia o typie szkła, 
# istotnym jest, który z pierwiastków dominuje w składzie.

## wyznaczamy macierz niepodobieństwa dla nieustanadryzowanych danych:
diss.mtrx.glass <- daisy(glass.cechy)
# konwersja do macierzy:
glass.cechy.mtrx <- as.matrix(diss.mtrx.glass)

# wizualizacja macierzy odmienności:

# bez uporządkowania:
fviz_dist(diss.mtrx.glass, order = FALSE)
# po uporządkowaniu:
fviz_dist(diss.mtrx.glass, order = TRUE)

## macierz niepodobieństwa dla danych ustandaryzowanych:
diss.mtrx.glass.s <- daisy(glass.cechy.scaled)
# konwersja do macierzy:
glass.cechy.mtrx.s <- as.matrix(diss.mtrx.glass.s)

# wizualizacja macierzy odmienności:

# bez uporządkowania:
fviz_dist(diss.mtrx.glass.s, order = FALSE)
# po uporządkowaniu:
fviz_dist(diss.mtrx.glass.s, order = TRUE)


#######################################################################################################
############################## algorytm PAM (Partitioning Around Medoids) #############################
#######################################################################################################

############################################### algorytm PAM bez standaryzacji ########################################
glass.pam <- pam(x=diss.mtrx.glass, diss=TRUE, k=6)

summary(glass.pam)

# medoidy, które reprezentują poszczególne klastry:
glass.pam$medoids

# sprawdzamy 
glass.pam$clustering
### wizualizacje:

# wskażnik silhouette (zwartość i przestrzenność klastrów):
plot(glass.pam, main="PAM bez standaryzacji")

## wizualizacje z wykorzystaniem algorytmu PCA:
# funckja clusplot:
clusplot(glass.pam)

# funkcja fviz_cluster z biblioteki factoextra:
glass.pam.original <- glass.pam
glass.pam$data <- glass.cechy
fviz_cluster(glass.pam) + labs(title="PAM bez standaryzacji")

cont_mtrx_pam6 <- table(glass.pam$clustering, glass.real.etykiety)
matchClasses(cont_mtrx_pam6, method="exact")
## sprawdzamy, jaka jest sugerowana liczba klastrów:
fviz_nbclust(glass.cechy, pam, method = "silhouette")

# liczba skupień w naszym przypadku powinna wynosić 6 (sugerowana wynosi 3), 
# bowiem z tyloma odmianami szkła pracujemy
# być może sugerowana liczba klastrów wynika z podobnych właściwości typów szkła

################################################ algorytm PAM ze standaryzacją #####################################################
glass.pam.s <- pam(x=diss.mtrx.glass.s, diss=TRUE, k=6)

summary(glass.pam.s)

# medoidy, które reprezentują poszczególne klastry:
glass.pam.s$medoids

# sprawdzamy 
glass.pam.s$clustering
### wizualizacje:

# wskażnik silhouette (zwartość i przestrzenność klastrów):
plot(glass.pam.s, main = "PAM z ustandaryzowanymi danymi")

## wizualizacje z wykorzystaniem algorytmu PCA:
# funckja clusplot:
clusplot(glass.pam.s)

# funkcja fviz_cluster z biblioteki factoextra:
glass.pam.original.s <- glass.pam.s
glass.pam.s$data <- glass.cechy.scaled
fviz_cluster(glass.pam.s) + labs(title = "PAM ze standaryzacją")

## sprawdzamy, jaka jest sugerowana liczba klastrów:
fviz_nbclust(glass.cechy.scaled, pam, method = "silhouette")

cont_mtrx_pam6.s <- table(glass.pam.s$clustering, glass.real.etykiety)
matchClasses(cont_mtrx_pam6.s, method="exact")

#####################################################################################################
########################### AGNES (Aglomerative Nesting) ############################################
#####################################################################################################

  glass.agnes.single <- agnes(x=diss.mtrx.glass, diss=TRUE, method="single")
  glass.agnes.avg <- agnes(x=diss.mtrx.glass, diss=TRUE, method="average")
  glass.agnes.complete <- agnes(x=diss.mtrx.glass, diss=TRUE, method="complete")
  glass.agnes.ward <- agnes(x=diss.mtrx.glass, diss=TRUE, method="ward")
  
  # wizualizacja, porównanie metod łączenia klastrów:
  
  fviz_dend(glass.agnes.avg, k=6)
  fviz_dend(glass.agnes.single, k=6)
  fviz_dend(glass.agnes.complete, k=6)
  fviz_dend(glass.agnes.ward, k=6)
  
  
  
  plot(glass.agnes.single,which.plots=2,main="AGNES: single linkage")
    
  plot(glass.agnes.avg,which.plots=2,main="AGNES: average linkage")
  
  plot(glass.agnes.complete,which.plots=2, main="AGNES: complete linkage")
  
  # wydawałoby się, że najlepsze wyniki daje metoda complete linkage
  
  ## odcinamy drzewa klastrów i tworzymy wskaźniki silhouette dla każdej z metod algorytmu agnes:
  
  # complete linkage method:
  cut.glass.agnes.compl <- cutree(glass.agnes.complete, k=6)
  table(cut.glass.agnes.compl)
  
  sil.glass.agnes.compl <- silhouette(cut.glass.agnes.compl, dist=diss.mtrx.glass)
  fviz_silhouette(sil.glass.agnes)
  
  # avarage linkage method:
  cut.glass.agnes.avg <- cutree(glass.agnes.avg, k=6)
  table(cut.glass.agnes.avg)
  
  sil.glass.agnes.avg <- silhouette(cut.glass.agnes.avg, dist=diss.mtrx.glass)
  fviz_silhouette(sil.glass.agnes.avg)
  
  # single linkage mathod:
  cut.glass.agnes.sing <- cutree(glass.agnes.single, k=6)
  table(cut.glass.agnes.sing, glass.real.etykiety)
  
  sil.glass.agnes.sing <- silhouette(cut.glass.agnes.sing, dist=diss.mtrx.glass)
  fviz_silhouette(sil.glass.agnes.sing)
  
  ############# kolorowanie liści dendrogramu #####################
  etykiety.kolory <- as.numeric(glass.real.etykiety)
  
  # metoda complete linkage:
  kolory.obiektow.comp <- etykiety.kolory[glass.agnes.complete$order]
  dendrogram.agnes.comp <- as.dendrogram(glass.agnes.complete)
  fviz_dend(dendrogram.agnes.comp, cex=0.5, label_cols=kolory.obiektow.comp, main="Kolory = rzeczywiste klasy (matoda complete linkage)")
  
  # metoda average linkage:
  kolory.obiektow.avg <- etykiety.kolory[glass.agnes.avg$order]
  dendrogram.agnes.avg <- as.dendrogram(glass.agnes.avg)
  fviz_dend(dendrogram.agnes.avg, cex=0.5, label_cols=kolory.obiektow.avg, main="Kolory = rzeczywiste klasy (matoda average linkage)")
  
  # metoda single linkage:
  kolory.obiektow.sing <- etykiety.kolory[glass.agnes.single$order]
  dendrogram.agnes.sing <- as.dendrogram(glass.agnes.single)
  fviz_dend(dendrogram.agnes.sing, cex=0.5, label_cols=kolory.obiektow.sing, main="Kolory = rzeczywiste klasy (matoda single linkage)")
  
