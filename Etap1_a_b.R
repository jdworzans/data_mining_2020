library(dplyr)
library(tidyr)

dane <- read.csv(file="churn.txt")
head(dane)

#eksplorowanie struktury danych
paste("Liczba wierszy:", as.character(nrow(dane)))
paste("Liczba kolumn:", as.character(ncol(dane)))
str(dane) 
#według opisu danych zmienna Day.charge powinna być całkowita (?)

#usunięcie zmiennych Area.Code i Phone pełniących rolę ID klienta:
dane <- dane[,c(-3,-4)]









