library(ggplot2)
library(psych)

dane <- read.csv(file="churn.txt")
head(dane)
attach(dane)

Area.Code <- as.factor(Area.Code)
#  Analiza opisowa

# Podstawowe wskaźniki dla cech ilościowych
describe(dane, quant=c(0.25, 0.75), omit=TRUE, IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

#  Stan
ggplot(dane, aes(State)) + geom_bar()
table(State)
#  TODO: Możemy spróbować w przyszłości zrobić to w formie mapy leaflet lub ggplot2
#  https://www.r-graph-gallery.com/choropleth-map.html

#  Jak długo konto było aktywne
Account.Length
describe(Account.Length)
ggplot(dane, aes(Account.Length)) + geom_boxplot()
ggplot(dane, aes(Account.Length)) + geom_histogram(binwidth=5)

#  Kod regionalny
table(Area.Code)

#  Plan międzynarodowy
table(Int.l.Plan)

#  Plan poczty głosowej
table(VMail.Plan)

#  Liczba wiadomości w poczcie głosowej
table(VMail.Message)

#  Dobrze byłoby wziąć pod uwagę tylko tych użytkowników,
#  którzy mają włączoną usługę poczty głosowej

describe(VMail.Message[VMail.Plan == "yes"], quant=c(0.25, 0.75), omit=TRUE, IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]
ggplot(subset(dane, VMail.Plan == "yes"), aes(VMail.Message)) +
  geom_bar()
ggplot(subset(dane, VMail.Plan == "yes"), aes(VMail.Message)) +
  geom_boxplot()

#  Liczba minut w ciągu dnia
ggplot(dane, aes(Day.Mins)) + geom_boxplot()
ggplot(dane, aes(Day.Mins)) + geom_histogram(binwidth=5)

#  Liczba połączeń w ciągu dnia
ggplot(dane, aes(Day.Calls)) + geom_boxplot()
ggplot(dane, aes(Day.Calls)) + geom_histogram(binwidth=5)

#  Całkowita opłata za rozmowy w ciągu dnia
ggplot(dane, aes(Day.Charge)) + geom_boxplot()
ggplot(dane, aes(Day.Charge)) + geom_histogram(binwidth=1)


#  Liczba minut wieczorem
ggplot(dane, aes(Eve.Mins)) + geom_boxplot()
ggplot(dane, aes(Eve.Mins)) + geom_histogram(binwidth=5)

#  Liczba połączeń wieczorem
ggplot(dane, aes(Eve.Calls)) + geom_boxplot()
ggplot(dane, aes(Eve.Calls)) + geom_histogram(binwidth=5)

#  Całkowita opłata za rozmowy wieczorem
ggplot(dane, aes(Eve.Charge)) + geom_boxplot()
ggplot(dane, aes(Eve.Charge)) + geom_histogram(binwidth=1)


#  Liczba minut w ciągu nocy
ggplot(dane, aes(Night.Mins)) + geom_boxplot()
ggplot(dane, aes(Night.Mins)) + geom_histogram(binwidth=5)

#  Liczba połączeń w ciągu nocy
ggplot(dane, aes(Night.Calls)) + geom_boxplot()
ggplot(dane, aes(Night.Calls)) + geom_histogram(binwidth=5)

#  Całkowita opłata za rozmowy w ciągu nocy
ggplot(dane, aes(Night.Charge)) + geom_boxplot()
ggplot(dane, aes(Night.Charge)) + geom_histogram(binwidth=1)


#  Liczba minut na połączenia międzynarodowe
ggplot(dane, aes(Intl.Mins)) + geom_boxplot()
ggplot(dane, aes(Intl.Mins)) + geom_histogram(binwidth=0.5)

#  Liczba połączeń międzynarodowych
ggplot(dane, aes(Intl.Calls)) + geom_boxplot()
ggplot(dane, aes(Intl.Calls)) + geom_histogram(binwidth=1)

#  Całkowita opłata za połączeń międzynarodowych
ggplot(dane, aes(Intl.Charge)) + geom_boxplot()
ggplot(dane, aes(Intl.Charge)) + geom_histogram(binwidth=1)


#  Połączenia z biurem obsługi klienta
table(CustServ.Calls)
ggplot(dane, aes(CustServ.Calls)) +
  geom_bar()
