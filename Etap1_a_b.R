dane <- read.csv(file="churn.txt")
head(dane)

#eksplorowanie struktury danych
paste("Liczba wierszy:", as.character(nrow(dane)))
paste("Liczba kolumn:", as.character(ncol(dane)))
str(dane) 

#zmiana typu zmiennej Area.Code na factor
dane$Area.Code <- as.factor(dane$Area.Code)

#usunięcie zmiennej Phone pełniącej rolę id klienta:
dane1 <- dane[,-4]

#w zbiorze nie ma wartości brakujących
sum(is.na(dane)) #=0

#zapisywanie wstępnie przeanalizowanych danych do pliku: churn1.txt

saveRDS(dane1, file="cleaned_churn.rds")







