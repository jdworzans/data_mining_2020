library(ggplot2)
library(psych)

dane <- readRDS("cleaned_churn.rds")
head(dane)

#podział danych na dwie grupy: group_t, group_f
group_t <- dane[which(dane$Churn.=="True."),]
group_f <- dane[which(dane$Churn.=="False."),]

#wskaźniki ilościowe dla grupy, która zrezygnowała z usług
describe(group_t, quant=c(0.25, 0.75), omit=TRUE, IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

#wskaźniki ilościowe dla grupy, która nie zrezygnowała z usług
describe(group_f, quant=c(0.25, 0.75), omit=TRUE, IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

#osoby, które zrezygnowały z usług częściej kontaktowały się biurem obsługi klienta


ggplot(group_t, aes(State)) + geom_bar() +labs(title="Liczba osób, które zrezygnowały z usług, dla poszczególnych stanów.")
table(group_t$State)
#najwięcej osób zrezygnowało w stanach : MD, NJ, TX


#okres aktywności konta dla grup
ggplot(dane, aes(x=Churn., y=Account.Length)) + geom_boxplot() +labs(title = "Okres aktywności konta dla grupy, która zrezygnowała.")

ggplot(group_f, aes(Account.Length)) + geom_boxplot() +labs(title = "Okres aktywności konta dla grupy, która nie zrezygnowała.")


#plan poczty głosowej
#rezygnujące
table(group_t$VMail.Plan)
ggplot(group_t, aes(VMail.Plan)) + geom_bar() +labs(title="Osoby rezygnujące, a udział w planie poczty głosowej")

describe(group_t$VMail.Message[group_t$VMail.Plan == "yes"], quant=c(0.25, 0.75), IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

#nierezygnujące
table(group_f$VMail.Plan)
ggplot(group_f, aes(VMail.Plan)) + geom_bar() +labs(title="Osoby nierezygnujące, a udział w planie poczty głosowej")

describe(group_f$VMail.Message[group_f$VMail.Plan == "yes"], quant=c(0.25, 0.75), IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]


#Liczba minut w ciągu dnia dla obu grup
ggplot(group_t, aes(Day.Mins)) + geom_boxplot() + labs(title="Liczba minut w ciagu dnia - osoby rezygnujące")
ggplot(group_f, aes(Day.Mins)) + geom_boxplot() + labs(title="Liczba minut w ciagu dnia - osoby nierezygnujące")

ggplot(group_t, aes(Day.Mins)) + geom_histogram(binwidth=5) + labs(title="Liczba minut w ciagu dnia - osoby rezygnujące")
ggplot(group_f, aes(Day.Mins)) + geom_histogram(binwidth=5) + labs(title="Liczba minut w ciagu dnia - osoby nierezygnujące")

#liczba połączeń w ciągu dnia
ggplot(group_t, aes(Day.Calls)) + geom_boxplot() + labs(title="Liczba połączeń w ciągu dnia - osoby rezygnujące")
ggplot(group_f, aes(Day.Calls)) + geom_boxplot() + labs(title="Liczba połączeń w ciagu dnia - osoby nierezygnujące")

ggplot(group_t, aes(Day.Calls)) + geom_histogram(binwidth=5) + labs(title="Liczba połączeń w ciagu dnia - osoby rezygnujące")
ggplot(group_f, aes(Day.Calls)) + geom_histogram(binwidth=5) + labs(title="Liczba połączeń w ciagu dnia - osoby nierezygnujące")

#opłata za rozmowy w ciągu dnia
ggplot(group_t, aes(Day.Charge)) + geom_boxplot() + labs(title="Opłata za połączenia w ciągu dnia - osoby rezygnujące")
ggplot(group_f, aes(Day.Charge)) + geom_boxplot() + labs(title="Opłata za połączenia w ciagu dnia - osoby nierezygnujące")

ggplot(group_t, aes(Day.Charge)) + geom_histogram(binwidth=5) + labs(title="Opłata za połączenia w ciagu dnia - osoby rezygnujące")
ggplot(group_f, aes(Day.Charge)) + geom_histogram(binwidth=5) + labs(title="Opłata za połączenia w ciagu dnia - osoby nierezygnujące")

#osoby rezygnujące z usług rozmawiają częściej, dłużej, więcej płacą (niewielkie różnice, ale jednak są)


#plan międzynarodowy 
table(group_t$Int.l.Plan)
table(group_f$Int.l.Plan)

ggplot(group_t, aes(Int.l.Plan)) + geom_bar() +labs(title="Osoby rezygnujące, a udział w planie międzynarodowym.")

#na plan międzynarodowy zdecydowało się 323 osoby, przy czym wśród nich było aż 137 osób, które potem zrezygnowały z usług

#liczba minut na połączenia międzynarodowe
describe(group_t$Intl.Mins[group_t$Int.l.Plan == "yes"], quant=c(0.25, 0.75), IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

describe(group_t$Intl.Mins[group_t$Int.l.Plan == "yes"], quant=c(0.25, 0.75), IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

ggplot(group_t, aes(Intl.Mins)) + geom_bar() +labs(title="Osoby rezygnujące, a połączenia międzynarodowe w minutach.")
ggplot(group_f, aes(Intl.Mins)) + geom_bar() +labs(title="Osoby nierezygnujące, a połączenia międzynarodowe w minutach.")

ggplot(group_t, aes(Intl.Mins)) + geom_boxplot() +labs(title="Osoby rezygnujące, a połączenia międzynarodowe w minutach.")
ggplot(group_f, aes(Intl.Mins)) + geom_boxplot() +labs(title="Osoby nierezygnujące, a połączenia międzynarodowe w minutach.")


#liczba połączeń międzynarodowych
describe(group_t$Intl.Calls[group_t$Int.l.Plan == "yes"], quant=c(0.25, 0.75), IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

describe(group_t$Intl.Mins[group_t$Int.l.Plan == "yes"], quant=c(0.25, 0.75), IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

ggplot(group_t, aes(Intl.Calls)) + geom_bar() +labs(title="Osoby rezygnujące, a liczba połączeń międzynarodowych.")
ggplot(group_f, aes(Intl.Calls)) + geom_bar() +labs(title="Osoby nierezygnujące, a liczba połączeń międzynarodowych.")

ggplot(group_t, aes(Intl.Calls)) + geom_boxplot() +labs(title="Osoby rezygnujące, a liczba połączeń międzynarodowych.")
ggplot(group_f, aes(Intl.Calls)) + geom_boxplot() +labs(title="Osoby nierezygnujące, a liczba połączeń międzynarodowych.")


#koszt połączeń międzynarodowych
#koszt połączeń międzynarodowych - wskaźniki opisowe:
describe(group_t$Intl.Charge[group_t$Int.l.Plan == "yes"], quant=c(0.25, 0.75), IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

describe(group_t$Intl.Charge[group_t$Int.l.Plan == "yes"], quant=c(0.25, 0.75), IQR=TRUE)[
  c("mean", "trimmed", "sd", "min", "max", "range", "Q0.25", "median", "Q0.75", "IQR", "skew", "kurtosis")]

ggplot(group_t, aes(Intl.Charge)) + geom_bar() +labs(title="Osoby rezygnujące, a koszt połączeń międzynarodowych.")
ggplot(group_f, aes(Intl.Charge)) + geom_bar() +labs(title="Osoby nierezygnujące, a koszt połączeń międzynarodowych.")



table(group_t$CustServ.Calls[group_t$Int.l.Plan=="yes"])
table(group_t$CustServ.Calls[group_t$Int.l.Plan=="no"])
ggplot(dane, aes(x=Churn., y=Intl.Charge, fill=Churn.)) +
  geom_boxplot() +
  labs(x="Decyzja o rezygnacji", y="Koszt za połączenia", title="Koszty połączeń dla klientów lojalnych i rezygnujących z usług.") +
  scale_x_discrete(name="Decyzja o rezygnacji", labels=c("Nie", "Tak")) +
  theme(legend.position="none")

#liczba telefonów do biura obsługi klienta, a liczba klientów, którzy zrezygnowali
ggplot(dane, aes(x=Churn., y=CustServ.Calls, fill=Churn.)) +
  geom_boxplot() +
  labs(x="Decyzja o rezygnacji", y="Liczba połączeń z biurem obsługi klienta") +
  scale_x_discrete(name="Decyzja o rezygnacji", labels=c("Nie", "Tak")) +
  theme(legend.position="none")
#osoby, które podjęły decyzję o rezygnacji wykonały więcej telefonów do biura obsługi klienta

#liczba telefonów do biura obsługi klienta, a plan międzynarodowy
ggplot(group_t, aes(x=Int.l.Plan, y=CustServ.Calls, fill=Int.l.Plan)) +
  geom_boxplot() +
  labs(x="Plan międzynarodowy", y="Liczba połączeń z biura obsługi klienta") +
  scale_x_discrete(name="Plan międzynarodowy", labels=c("Nie", "Tak")) +
  theme(legend.position="none")

#wzmożona liczba telefonów do biura obsługi klienta w przypadku osób, które postanowiły zrezygnować z usług
#niekoniecznie musiała wynikać z niezbyt korzystnych warunków planu międzynarodowego, czyli przyczyny można też
#dopatrywać się w braku korzystnych ofert lub niedoświadczonych pracownikach biura obsługi

#liczba połączeń krajowych z zaznaczeniem decyzji o rezygnacji z usług:
ggplot(dane, aes(x=Day.Calls+Eve.Calls+Night.Calls, fill=Churn.)) +
  geom_histogram() +
  labs(x="Liczba połączeń krajowych", y="Liczba wystąpień") +
  scale_fill_discrete(name="Decyzja o rezygnacji", labels=c("Nie", "Tak"))


#koszty połączeń krajowych na dobę:
ggplot(dane, aes(x=Churn., y=Day.Charge+Eve.Charge+Night.Charge, fill=Churn.)) +
  geom_boxplot() +
  labs(x="Decyzja o rezygnacji", y="Całodobowy koszt za połączenia") +
  scale_x_discrete(name="Decyzja o rezygnacji", labels=c("Nie", "Tak")) +
  theme(legend.position="none")

#ilość połączeń na dobę:
ggplot(dane, aes(x=Churn., y=Day.Calls+Eve.Calls+Night.Calls, fill=Churn.)) +
  geom_boxplot() +
  labs(x="Decyzja o rezygnacji", y="Całodobowa ilość połączeń") +
  scale_x_discrete(name="Decyzja o rezygnacji", labels=c("Nie", "Tak")) +
  theme(legend.position="none")

ggplot(dane, aes(x=Churn., y=Day.Charge+Eve.Charge+Night.Charge)) +
         geom_boxplot() +
         labs(x="Decyzja o rezygnacji", y="Całodobowy koszt połączeń")








