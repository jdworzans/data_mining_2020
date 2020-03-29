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
ggplot(dane, aes(Int.l.Plan)) + geom_bar()

#  Plan poczty głosowej
table(VMail.Plan)
ggplot(dane, aes(VMail.Plan)) + geom_bar()

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

#  Analiza planu międzynarodowego

describe(Intl.Charge[Int.l.Plan=="yes"], quant=c(0.25, 0.75), omit=TRUE, IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]
describe(Intl.Charge[Int.l.Plan=="no"], quant=c(0.25, 0.75), omit=TRUE, IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

ggplot(dane, aes(x=Intl.Mins, y=Intl.Charge, col=Int.l.Plan)) +
  geom_point(alpha=0.5) +
  scale_color_discrete(name="Plan międzynarodowy", labels=c("Nie", "Tak")) +
  labs(x="Liczba minut na połączenia międzynarodowe", y="Opłata za połączenia międzynarodowe")
#  Wygląda na to, że obecnie klientom nie opłaca się korzystanie
#  z planu połączeń międzynarodowych, bo i tak płacą "tyle samo"

ggplot(dane, aes(x=Int.l.Plan, y=Intl.Charge/Intl.Mins, fill=Int.l.Plan)) +
  geom_boxplot() +
  labs(x="Plan międzynarodowy", y="Średnia opłata za minutę połączenia międzynarodowego") +
  scale_x_discrete(name="Plan międzynarodowy", labels=c("Nie", "Tak")) +
  theme(legend.position="none")

ggplot(dane, aes(x=Intl.Mins, fill=Int.l.Plan)) +
  geom_histogram() +
  labs(x="Liczba minut na połączenia międzynarodowe", y="Liczba wystąpień") +
  scale_fill_discrete(name="Plan międzynarodowy", labels=c("Nie", "Tak"))

ggplot(dane, aes(x=as.factor(Intl.Calls), y=Intl.Charge/Intl.Mins, col=Int.l.Plan)) +
  geom_boxplot() +
  scale_color_discrete(name="Plan międzynarodowy", labels=c("Nie", "Tak")) +
  labs(x="Liczba połączeń międzynarodowych", y="Średnia opłata za minutę połączenia międzynarodowego")
