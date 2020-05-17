library(datasets)
library(ggplot2)
library(varhandle)

data(iris)
attach(iris)

# przynależności do klas:
ggplot(data=iris, aes(x=Species)) + 
  geom_bar()

# podział danych na zbiór uczący i testowy:
smp_size <- floor(0.8 * nrow(iris))
set.seed(43)
train_ind <- sample(seq_len(nrow(iris)), size=smp_size)

X_train <- cbind(rep(1,120),iris[train_ind, 1:4])
X_test <- cbind(rep(1,30),iris[-train_ind, 1:4])
Y_train <- iris[train_ind, 5]
Y_test <- iris[-train_ind, 5]

# konwersja zmiennej kategorycznej na macierz wskaźnikową:
Y_train_bin <- to.dummy(Y_train, prefix="species")
Y_test_bin <- to.dummy(Y_test, prefix="species")

# konstrukcja klasyfikatora regresji liniowej metodą najmniejszych kwadratów:
B <- solve(t(X_train)%*%as.matrix(X_train)) %*% t(X_train) %*% as.matrix(Y_train_bin)

# wartości progonozowane:
Y_result <- as.matrix(X_test) %*% B
head(Y_result)

# sprawdzamy, czy prawdopodobieństwa sumują się do 1:
rowSums(Y_result)

# wizualizacja wyników:
  matplot(Y_result, ylab="Predykcja")
  abline(v=c(10,20), lty=2, col="grey")
  legend(x="top", legend=paste(1:3,levels(Species)), col=1:3, text.col=1:3)

# ocena skuteczności modelu:
  klasy <- levels(Species)
  maks.ind <- apply(Y_result, 1, FUN=function(x) which.max(x))
  prognozowane.etykietki <- klasy[maks.ind]
  rzeczywiste.etykietki <- Y_test
  
  conf_mtrx <- table(rzeczywiste.etykietki, prognozowane.etykietki)
  conf_mtrx
  sum(diag(conf_mtrx))/length(Y_test)

ggplot(data.frame(Y_test), aes(x=Y_test)) + geom_bar()

# sprawdzamy, czy model nie jest przetrenowany:
  Y_result.train <- as.matrix(X_train) %*% B
  
  maks.ind.train <- apply(Y_result.train, 1, FUN=function(x) which.max(x))
  prognozowane.etykietki.train <- klasy[maks.ind.train]
  rzeczywiste.etykietki.train <- Y_train
  
  conf_mtrx.train <- table(rzeczywiste.etykietki.train, prognozowane.etykietki.train)
  conf_mtrx.train
  sum(diag(conf_mtrx.train))/length(Y_train)

matplot(Y_result.train, ylab="Predykcja")
abline(v=c(40,80), lty=2, col="grey")
legend(x="top", legend=paste(1:3,levels(Species)), col=1:3, text.col=1:3)


# ZBIÓR DANYCH Z NOWYMI ZMIENNYMI:

new_iris <- cbind(Petal.Length^2, Petal.Width*Sepal.Length, iris)

X_train_n <- cbind(rep(1,120),new_iris[train_ind, 1:6])
X_test_n <- cbind(rep(1,30),new_iris[-train_ind, 1:6])
Y_train_n <- new_iris[train_ind, 7]
Y_test_n <- new_iris[-train_ind, 7]

# konwersja zmiennej kategorycznej na macierz wskaźnikową:
Y_train_bin_n <- to.dummy(Y_train_n, prefix="species")
Y_test_bin_n <- to.dummy(Y_test_n, prefix="species")

# konstrukcja klasyfikatora regresji liniowej metodą najmniejszych kwadratów:
B1 <- solve(t(X_train_n)%*%as.matrix(X_train_n)) %*% t(X_train_n) %*% as.matrix(Y_train_bin_n)

# wartości progonozowane:
Y_result_n <- as.matrix(X_test_n) %*% B1
head(Y_result_n)

# sprawdzamy, czy prawdopodobieństwa sumują się do 1:
rowSums(Y_result_n)

# wizualizacja wyników:
matplot(Y_result_n, ylab="Predykcja")
abline(v=c(10,20), lty=2, col="grey")
legend(x="top", legend=paste(1:3,levels(Species)), col=1:3, text.col=1:3)

# ocena skuteczności modelu:
klasy <- levels(Species)
maks.ind.n <- apply(Y_result_n, 1, FUN=function(x) which.max(x))
prognozowane.etykietki.n <- klasy[maks.ind.n]
rzeczywiste.etykietki.n <- Y_test_n

conf_mtrx_n <- table(rzeczywiste.etykietki.n, prognozowane.etykietki.n)
rownames(conf_mtrx_n) <- c("setosa.true", "versicolor.true", "virginica.true")
conf_mtrx_n
sum(diag(conf_mtrx_n))/length(Y_test_n)

ggplot(data.frame(Y_test_n), aes(x=Y_test_n)) + geom_bar()

# sprawdzamy, czy model nie jest przetrenowany:
Y_result.train.n <- as.matrix(X_train_n) %*% B1

maks.ind.train.n <- apply(Y_result.train.n, 1, FUN=function(x) which.max(x))
prognozowane.etykietki.train.n <- klasy[maks.ind.train.n]
rzeczywiste.etykietki.train.n <- Y_train_n

conf_mtrx.train.n <- table(rzeczywiste.etykietki.train.n, prognozowane.etykietki.train.n)
conf_mtrx.train.n
sum(diag(conf_mtrx.train.n))/length(Y_train_n)