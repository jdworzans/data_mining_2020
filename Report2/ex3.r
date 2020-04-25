library(cluster)
library(ggplot2)
library(MASS)
library(ggpubr)
library(plotly)


# Kaggle titanic dataset https://www.kaggle.com/heptapod/titanic
titanic.data <- read.csv("Report2/titanic.csv")
data <- subset(titanic.data, select=c(Pclass,Sex,Age,sibsp,Parch,Fare,Embarked))
data$Survived <- as.factor(titanic.data$X2urvived)

data$Pclass <- as.ordered(data$Pclass)
data$Sex <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)
data <- na.omit(data)

data.mds <- subset(data, select=-c(Survived))

not_duplicated <- !duplicated(data.mds)

data <- data[not_duplicated,]
data.mds <- data.mds[not_duplicated,]
str(data)

dissimilarity.object <- daisy(
  data.mds,
  stand=TRUE
)
dissimilarities.matrix <- as.matrix(dissimilarity.object)

badanie.MDS <- function (k.max, dis.matrix=dissimilarities.matrix) {

  STRESS <- numeric(k.max)
  wykresy <- list()
  
  for (k in 1:k.max) {
    mds <- isoMDS(dis.matrix, k=k)
    STRESS[k] <- mds$stress
    distances.k <- as.matrix(dist(mds$points, method="euclidean"))
    wykresy[[k]] <- ggplot() +
      geom_point(aes(x=c(distances.k), y=c(dis.matrix)), alpha=0.1) +
      labs(
        title=paste("k=", k, sep=""),
        x="nowe odległości",
        y="oryginalne odległości"
      )
  }
  return(
    list(wykresy=wykresy, STRESS=STRESS)
  )
}

wyniki <- badanie.MDS(length(data.mds))

STRESS <- wyniki$STRESS
ggplot() + geom_line(aes(x=1:length(data.mds), y=STRESS))

wykresy <- wyniki$wykresy
ggarrange(plotlist=wykresy)

# Wizualizacja ze zmienną grupującą

# 2D

mds.k2 <- isoMDS(dissimilarities.matrix, k=2)
k2.data <- data.frame(mds.k2$points)
k2.data$Survived <- data$Survived
ggplot(k2.data, aes(x=X1, y=X2, col=Survived)) + geom_point() + scale_color_discrete()

# 3D

mds.k3 <- isoMDS(dissimilarities.matrix, k=3)
k3.data <- data.frame(mds.k3$points)
k3.data$Survived <- data$Survived
plot_ly(data=k3.data, x=~X1, y=~X2, z=~X3, color=~Survived, colorscale='Viridis', type="scatter3d")

# Nie otrzymujemy dobrej separacji klas, ale to pewnie kwestia złożoności problemu.
# Możemy zauważyć 2 grupy, z których każda dzieli się na 6 podgrup.
# Nie są one jednak odseparowane pod względem przeżycia.


