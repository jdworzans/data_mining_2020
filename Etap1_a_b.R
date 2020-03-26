library(dplyr)
library(tidyr)

dane <- read.csv(file="churn.txt")
head(dane)

#eksplorowanie struktury danych
str(dane) 
#zmienne area.code i phone powinny być jakościowe (tworzą id klienta)
#według opisu danych zmienna Day.charge powinna być całkowita

#można utworzyć zmienną jakościową (numer telefonu wraz z numerem kierunkowym): id_client
#dane$Area.Code <- as.factor(dane$Area.Code)
#ID <- unite(dane[,3:4], col="id_client", sep="-")
#dane <- cbind(ID, dane)



