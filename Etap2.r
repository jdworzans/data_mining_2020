library(ggplot2)

dane <- read.csv(file="churn.txt")
head(dane)
attach(dane)

#  Analiza opisowa

#  Stan
ggplot(dane, aes(State)) + geom_bar()
table(State)
summary(State)
#  TODO: Możemy spróbować w przyszłości zrobić to w formie mapy leaflet lub ggplot2
#  https://www.r-graph-gallery.com/choropleth-map.html

#  Jak długo konto było aktywne
Account.Length
summary(Account.Length)
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
summary(VMail.Message)

#  Dobrze byłoby wziąć pod uwagę tylko tych użytkowników,
#  którzy mają włączoną usługę poczty głosowej

ggplot(subset(dane, VMail.Plan == "yes"), aes(VMail.Message)) +
  geom_bar()
ggplot(subset(dane, VMail.Plan == "yes"), aes(VMail.Message)) +
  geom_boxplot()
summary(VMail.Message[VMail.Plan == "yes"])

#  Liczba minut w ciągu dnia
summary(Day.Mins)
ggplot(dane, aes(Day.Mins)) + geom_boxplot()
ggplot(dane, aes(Day.Mins)) + geom_histogram(binwidth=5)

#  Liczba połączeń w ciągu dnia
summary(Day.Calls)
ggplot(dane, aes(Day.Calls)) + geom_boxplot()
ggplot(dane, aes(Day.Calls)) + geom_histogram(binwidth=5)

#  Całkowita opłata za rozmowy w ciągu dnia
summary(Day.Charge)
ggplot(dane, aes(Day.Charge)) + geom_boxplot()
ggplot(dane, aes(Day.Charge)) + geom_histogram(binwidth=1)


#  Liczba minut wieczorem
summary(Eve.Mins)
ggplot(dane, aes(Eve.Mins)) + geom_boxplot()
ggplot(dane, aes(Eve.Mins)) + geom_histogram(binwidth=5)

#  Liczba połączeń wieczorem
summary(Eve.Calls)
ggplot(dane, aes(Eve.Calls)) + geom_boxplot()
ggplot(dane, aes(Eve.Calls)) + geom_histogram(binwidth=5)

#  Całkowita opłata za rozmowy wieczorem
summary(Eve.Charge)
ggplot(dane, aes(Eve.Charge)) + geom_boxplot()
ggplot(dane, aes(Eve.Charge)) + geom_histogram(binwidth=1)


#  Liczba minut w ciągu nocy
summary(Night.Mins)
ggplot(dane, aes(Night.Mins)) + geom_boxplot()
ggplot(dane, aes(Night.Mins)) + geom_histogram(binwidth=5)

#  Liczba połączeń w ciągu nocy
summary(Night.Calls)
ggplot(dane, aes(Night.Calls)) + geom_boxplot()
ggplot(dane, aes(Night.Calls)) + geom_histogram(binwidth=5)

#  Całkowita opłata za rozmowy w ciągu nocy
summary(Night.Charge)
ggplot(dane, aes(Night.Charge)) + geom_boxplot()
ggplot(dane, aes(Night.Charge)) + geom_histogram(binwidth=1)


#  Liczba minut na połączenia międzynarodowe
summary(Intl.Mins)
ggplot(dane, aes(Intl.Mins)) + geom_boxplot()
ggplot(dane, aes(Intl.Mins)) + geom_histogram(binwidth=0.5)

#  Liczba połączeń międzynarodowych
summary(Intl.Calls)
ggplot(dane, aes(Intl.Calls)) + geom_boxplot()
ggplot(dane, aes(Intl.Calls)) + geom_histogram(binwidth=1)

#  Całkowita opłata za połączeń międzynarodowych
summary(Intl.Charge)
ggplot(dane, aes(Intl.Charge)) + geom_boxplot()
ggplot(dane, aes(Intl.Charge)) + geom_histogram(binwidth=1)


#  Połączenia z biurem obsługi klienta
table(CustServ.Calls)
ggplot(dane, aes(CustServ.Calls)) +
  geom_bar()
