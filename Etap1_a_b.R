dane <- read.csv(file="churn.txt")
head(dane)

#eksplorowanie struktury danych
paste("Liczba wierszy:", as.character(nrow(dane)))
paste("Liczba kolumn:", as.character(ncol(dane)))
str(dane) 
#według opisu danych zmienna Day.charge powinna być całkowita (?)

#usunięcie zmiennych Area.Code i Phone pełniących rolę ID klienta:
dane1 <- dane[,-4]

#w zbiorze nie ma wartości brakujących
sum(is.na(dane)) #=0

#zapisywanie wstępnie przeanalizowanych danych do pliku: churn1.txt

write.table(dane1, file="churn1.txt", row.names=FALSE)








