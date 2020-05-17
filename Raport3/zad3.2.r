library(ggplot2)
library(ggbiplot)
library(mlbench)
library(plotly)
library(rpart)
library(rpart.plot)

data(Glass)

ggplot(Glass, aes(x=Type, y=RI)) + geom_boxplot()
ggplot(Glass, aes(x=Type, y=Na)) + geom_boxplot()
ggplot(Glass, aes(x=Type, y=Mg)) + geom_boxplot()
ggplot(Glass, aes(x=Type, y=Al)) + geom_boxplot()
ggplot(Glass, aes(x=Type, y=Si)) + geom_boxplot()
ggplot(Glass, aes(x=Type, y=K)) + geom_boxplot()
ggplot(Glass, aes(x=Type, y=Ca)) + geom_boxplot()

ggplot(Glass, aes(x=Type, y=Ba)) + geom_boxplot()
# Widać, że tylko 2 odłamki poza typem 7 mają "wysokie" stężenie Baru

ggplot(Glass, aes(x=Type, y=Fe)) + geom_boxplot()

pairs(Glass)

pairs(Glass[1:3], Glass[1:3], col=Glass$Type)
Glass[-c("Type"),]

dane <- subset(Glass, select=-c(Type))
dane.pca <- prcomp(dane, retx=T, center=T, scale.=T)
dane.pca$rotation

plot_ly(data.frame(dane.pca$x), x=~PC1, y=~PC2, z=~PC3, color=Glass$Type)

ggplot(data.frame(dane.pca$x), aes(x=PC1, y=PC2, col=Glass$Type)) + geom_point()

ggbiplot(dane.pca, groups=Glass$Type)


tree <- rpart(Type~., Glass, minsplit=1)
tree
rpart.plot(tree)

tree$variable.importance / sum(tree$variable.importance)


