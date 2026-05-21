

#Zadanie: Wpływ czynników socjodemograficznych na dochód.

#Zebrano dane od 200 osób na temat ich dochodów oraz kilku czynników socjodemograficznych.

#Zmienne:
#wiek: wiek osoby (ilościowa)
#wyksztalcenie: poziom wykształcenia (0 = podstawowe, 1 = średnie, 2 = wyższe, kategoryczna)
#plec: płeć osoby (0 = kobieta, 1 = mężczyzna, kategoryczna)
#dochody: roczny dochód w zł (ilościowa)


regresja1 <- readRDS("C:/Users/micha/Desktop/polibuda/statystyka/laby/dane_regresja1.rds")


#1. Zamień typy zmiennych wykształcenie i płeć na factor o zdefiniowanych poziomach.
#Zbuduj model regresji wielokrotnej, który przewiduje dochody na podstawie zmiennych wiek, wyksztalcenie i plec.

C = c(regresja1$wyksztalcenie)
C1 = factor(C, levels = c("0", "1", "2"), labels = c("podstawowe", "średnie", "wyższe"))
summary(C1)

B = c(regresja1$plec)
B1 = factor(B, levels = c("0", "1"), labels = c("kobieta", "mężczyzna"))
summary(B1)

tabela_regresja <- data.frame(regresja1)

tabela_regresja$wyksztalcenie <- factor(tabela_regresja$wyksztalcenie, levels = c("0", "1", "2"),
                                        labels = c("podstawowe", "średnie", "wyższe"))

tabela_regresja$plec <- factor(tabela_regresja$plec, levels = c("0", "1"),
                               labels = c("kobieta", "mężczyzna"))

model_regresja <- lm(dochody ~ wiek + wyksztalcenie + plec, data = tabela_regresja)
summary(model_regresja)


#2. Sprawdź współczynniki regresji: jaki wpływ na dochody mają poszczególne zmienne? Które zmienne są istotne statystycznie?
#Oceń jakość dopasowania modelu (R²).


#Wiek: każdy kolejny rok życia wiąże się ze wzrostem rocznego dochodu o średnio 500,63 zł przy założeniu,
#że pozostałe czynniki pozostają niezmienne. Zmienna ta jest bardzo silnie istotna statystycznie.

#Wykształcenie średnie: osoby z wykształceniem średnim zarabiają o średnio 2347,53 zł więcej niż osoby
#z wykształceniem podstawowym, jednakże różnica ta jest statystycznie nieistotna.
#Wykształcenie wyższe: osoby z wykształceniem wyższym zarabiają średnio o 2419,97 zł wiecej niż osoby
#o wykształceniu podstawowym, ale różnica ta jest statystycznie nieistotna.
#Na podstawie naszej próby, nie da się stwierdzić, czy wykształcenie ma wpływ na wysokość dochodów.
#Otrzymane wyniki mogą być dziełem przypadku.

#Płeć: mężczyźni o tym samym wykształceniu i wieku co kobiety, zarabiają średnio o 193,32 zł mniej niż one,
#ale różnica ta jest nieistotna statystycznie.

#H_0: model jest statystycznie nieistotny.
#p-value jest bardzo bliskie zeru, dlatego odrzucamy hipotezę zerową na rzecz hipotezy alternatywnej.
#Wniosek: model jest statystycznie istotny.

#Współczynnik R^2 = 0.2864, oznacza to, że model tłumaczy 28% zmienności w wynikach wydajności. Model ten jest umiarkowanie
#przystosowany do analizowanych danych. W badaniach społeczno-ekonomicznych wynik 28% jest wynikiem
#satysfakcjonującym i typowym. Dzieje się tak, ponieważ na badane przez nas zagadanienie, wypływa
#bardzo dużo czynników, a rozpatrujemy zaledwie 3 z nich.


#3. Zweryfikuj założenia modelu regresji: sprawdź, czy występują problemy z multicollinearity (oblicz VIF)
#oraz jakie są reszty modelu. Narysuj i zinterpretuj wykresy diagnostyczne.


library(car)
vif(model_regresja)

#Interpretacja:

#Analiza współliniowości VIF dla każdej ze zmiennych w modelu okazała się być bardzo bliska wartości 1,
#która mieści się w w przedziale od 1 do 5. Oznacza to, że wszystkie ze zmiennych wykazują niską
#współliniowość. Zatem założenia modelu o braku współliniowości zostały spełnione. Pozwala to na swobodną
#i stabilną estymację wpływu poszczególnych czynników na dochód.

par(mfrow = c(2, 2))
plot(model_regresja)

#Interpretacja wykresów pod kątem reszt:

#1. Residuals vs Fitted
#Punkty są równomiernie rozproszone względem czerwonej linii na poziomie 0. Czerwona linia zaś, jest
#pozioma i płaska. Oznacza to, że model jest liniowy, a wariacja jego reszt jest jednorodna.

#2. Normal Q-Q
#Punkty układają się bardzo blisko ukośnej prostej przerywanej. Widać jednak, że punkty na końcach wykresu
#uciekają w górę lub w dół. Oznacza to, że rozkład reszt nie jest idealnie normalny.

#Dla pewności, że rozkład reszt jest normalny, warto przeprowadzić test Shapiro-Wilka:

shapiro.test(residuals(model_regresja))

#Wniosek: p-value jest równe 0.5289, czyli jego wartość jest większa niż założony próg istotności 0.05.
#Zatem można stwierdzić, że reszty pochodzą z rozkładu normalnego - spełnione zostało jedno z najważniejszych
#założeń regresji liniowej.

#3. Scale-Location

#Wykres wykazuje względnie płaską linię trendu, a punkty są rozłożone w sposób losowy.
#Potwierdza to homoskedastyczność reszt – model wykazuje podobny poziom błędu niezależnie od 
#przewidywanej wysokości dochodu.

#4. Residuals vs Leverage

#Wykres Residuals vs Leverage pozwala ocenić wpływ obserwacji odstających. Żadne z danych nie przekraczają
#granicy wyznaczonej przez odległość Cooka (Cook's distance). Oznacza to, że w próbie 200 osób nie
#występują anomalie lub pojedyncze obserwacje wpływowe, które w sztuczny sposób zniekształcałyby oszacowane współczynniki regresji.

#Podsumowanie: model spełnia założenia regresji liniowej.

#5. Wykonaj predykcję dochodu wraz z 95% przedziałem ufności dla dwóch osób:
#a) Osoba A: Kobieta, 40 lat, wyksztalcenie wyższe
#b) Osoba B: Mężczyzna, 40 lat, wyksztalcenie wyższe
#Porównaj wyniki i zinterpretuj wpływ płci na przewidywany dochód, przy identycznych innych cechach.


predykcja <- data.frame(wiek = c(40, 40),
                        wyksztalcenie = factor(c("wyższe", "wyższe"), levels = c("podstawowe", "średnie", "wyższe")),
                        plec = factor(c("kobieta", "mężczyzna"), levels = c("kobieta", "mężczyzna"))
)

rownames(predykcja) <- c("Osoba_A", "Osoba_B")

predict(model_regresja, newdata = predykcja, interval = "confidence", level = 0.95)

#Otrzymane przedziały ufności i predykcje dochodów, to:
#dla osoby_A - (49331,43; 54958,65), prognozowany roczny dochód to 52145,04 zł,
#dla osoby_B - (49110,55; 54792,88), prognozowany roczny dochód to 51951,72 zł.

#Widać zatem, że otrzymane przedziały ufności i prognozowane dochody dla kobiety (osoby_A) i
#mężczyzny (osoby_B) w tym samym wieku i o tym samym wykształceniu praktycznie nie różnią się między sobą.
#Model prognozuje, że kobieta zarobi rocznie o 193,32 zł więcej od mężczyny. Jednak z uwagi na nieistotność
#płci jako zmiennej w modelu, różnica ta nie odzwierciedla realnej dysproporcji płacowej w populacji.
#Na podstawie otrzymanych wyników, można stwierdzić, że płeć nie ma rzeczywistego wpływu na przewidywany dochód.


#6. Zrób wykres regresji z linią regresji badający zależności między zmiennymi wiek i dochody.

library(ggplot2)

par(mfrow = c(1, 1))

ggplot(tabela_regresja, aes(x = wiek, y = dochody)) +
  geom_point(alpha = 0.6, col = "black") +                
  geom_smooth(method = "lm", col = "blue", fill = "gray", se = FALSE) +
  labs(title = "Zależność dochodów od wieku",
       x = "Wiek (lata)",
       y = "Roczny dochód (zł)") +
  theme_minimal()

#Interpretacja wykresu:

#Rosnąca niebieska prosta regresji pokazuje pozytywny wpływ wieku na dochód (wraz z przesuwaniem się w prawo
#na osi wieku, wartości na osi dochodów wzrastają). Rozrzut punktów wokół niebieskiej linii odzwierciedla
#wartość współczynnika R^2 (czyli 28%). Ogólnie rzecz ujmując, wykres potwierdza wyniki uzyskane podczas
#analizy badanych danych.


#7. Przeprowadź analizę korelacji między zmiennymi. Narysuj wykres i zinterpretuj.


tabela_korelacja <- tabela_regresja
tabela_korelacja$wyksztalcenie <- as.numeric(tabela_korelacja$wyksztalcenie)
tabela_korelacja$plec <- as.numeric(tabela_korelacja$plec)

zmienne <- tabela_korelacja[, c("wiek", "wyksztalcenie", "plec", "dochody")]
cor(zmienne)

library(corrplot)

corrplot(cor(zmienne), method = "color", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, number.cex = 0.8, diag = FALSE)

#Interpretacja wykresu:

#Mapa korelacji wyraźnie pokazuje związek między wiekiem a dochodami badanych, znaczy to tyle, że większy wiek
#koreluje z wyższymi zarobkami. Zależności dochodu a płci oraz wykształcenia są bliskie zeru, co potwierdza
#ich statystyczną nieistotność wykazaną wcześniej. Korelacje między płcią, wiekiem i wykształceniem (we wszystkich wariantach)
#również są bliskie zeru, co oznacza, że zmienne te są niezwiązane ze sobą (potwierdzenie wyniku testu VIF).
#Zatem istnieje tylko jedna liniowa zależność w badanym zbiorze danych i jest to zależność pomiędzy wiekiem a wysokością dochodów.

