#Zadanie 1: Zebrano dane związane z zadeklarowanym poziomem aktywności fizycznej (Niski, Średni, Wysoki) 
#i subiektywną jakością snu (Dobra, Słaba).
#Utwórz tabelę kontyngencji, a następnie przy pomocy testu chi-kwadrat zbadaj zależność
#pomiędzy badanymi cechami. Napisz czym jest testowana hipoteza zerowa i alternatywna w tym teście.
#Następnie dokonaj interpretacji otrzymanego w teście wyniku i napisz wnioski statystyczne.

#Wykonaj wizualizację do zadania polegającą na stworzeniu wykresów słupkowych dla
#jakości snu w zależności od poziomu aktywności (funkcja ggplot). Zinterpretuj otrzymany wykres.
#Wykonaj wykres mozaikowy (funkcja mosaicplot) aby wizualnie przedstawić zależności między zmiennymi i zinterpretuj otrzymany wykres.

set.seed(123)

library(ggplot2)
library(vcd)

aktywnosc <- sample(c("Niski", "Średni", "Wysoki"), 250, replace = TRUE)
jakosc_snu <- sample(c("Dobra", "Słaba"), 250, replace = TRUE, prob = c(0.6, 0.4))

tabela_dane <- data.frame(
  aktywnosc, jakosc_snu
)

t_kontyngencji <- table(tabela_dane$aktywnosc, tabela_dane$jakosc_snu)

test <- chisq.test(t_kontyngencji)
print(test)

#Hipotezą zerową w teście chi kwadrat jest zdanie:
#"nie istnieje zależność pomiędzy poziomem aktywności fizycznej i subiektywną jakością snu?".
#Hipotezą alternatywną zaś, zaprzeczenie hipotezy zerowej, tzn.
#"istnieje zależność pomiędzy poziomem aktywności fizycznej i subiektywną jakością snu".

if (test$p.value < 0.05) {
  print("Odrzucamy hipotezę zerową: istnieje zależność pomiędzy poziomem aktywności fizycznej i subiektywną jakością snu")
} else {
  print("Nie odrzucamy hipotezy zerowej: nie istnieje istotna zależność pomiędzy poziomem aktywności fizycznej i subiektywną jakością snu")
}

#Wnioski statystyczne:

#Z naszego testu wynika zatem, że hipoteza zerowa była prawidłowa i nie istnieje istotna zależność
#pomiędzy poziomem aktywności fizycznej i subiektywną jakością snu.

wykres1 <- ggplot(data = tabela_dane, mapping = aes(x = jakosc_snu, fill = aktywnosc)) + 
  geom_histogram(stat = "count", position = "dodge", color = "snow4") +
  scale_fill_manual(values = c("Wysoki" = "aquamarine2", "Średni" = "darkslategray1", "Niski" = "lightskyblue")) +
  labs(title = "Jakość snu a poziom aktywności fizycznej", x = "Jakość snu",
       y = "Liczba osób", fill = "Poziom aktywności")

print(wykres1)

#Interpretacja wykresu:

#Z wykresu wynika, że wysoki poziom aktywności fizycznej obserwujemy u większej ilości osób, których jakość snu
#jest dobra, niż u których jest słaba. Jednakże przeprowadzony test Chi-kwadrat wykazał, że różnice
#te są statystycznie nieistotne.

wykres2 <- mosaicplot(t_kontyngencji, xlab = "Poziom aktywności fizycznej", ylab = "Jakość snu",
                      col = c("maroon2", "plum3"), "Jakość snu a poziom aktywności fizycznej")
print(wykres2)

#Interpretacja wykresu:

#Na wykresie, szerokość poszczególnych kolumn oznacza ilość osób danej kategorii, tzn. np.
#ilość osób o średnim poziomie aktywności fizycznej i dobrej jakości snu. Natomiast łączne pole powierzchni każdego
#z prostokątów, mówi o tym, jaką stanowi on część całej próby.


#Zadanie 2: Ankietowano 500 osób na temat ich wymarzonego kierunku wakacyjnego (Morze, Góry, Mazury, Zagranica). 
#Sprawdź, czy otrzymany rozkład jest zgodny z założeniem, że kierunki te nie są równocenne i ich teoretyczny rozkład 
#wynosi odpowiednio: Morze (30%), Góry (30%), Mazury (20%), Zagranica (20%).

#Podaj hipotezy w tym teście oraz napisz odpowiednie wnioski statystyczne.
#Narysuj wykres słupkowy i poddaj interpretacji.

kierunki <- c("Morze", "Góry", "Mazury", "Zagranica")
wybory <- sample(kierunki, 500, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25))

tabela1 <- table(wybory)

praw_teo <- c(Morze = 0.3, Góry = 0.3, Mazury = 0.2, Zagranica = 0.2)

test_chi <- chisq.test(tabela1, p = praw_teo)
print(test_chi)

#Hipotezą zerową w teście jest "rozkład preferencji dotyczących wymarzonego kierunku wakacyjnego
#jest zgodny z teoretycznym rozkładem (kierunki nie są równocenne)".
#Hipotezą alternatywną zaś, jest: "rozkład preferencji dotyczących wymarzonego kierunku wakacyjnego
#jest sprzeczny z teoretycznym rozkładem".

if (test_chi$p.value < 0.05) {
  print("Odrzucamy hipotezę zerową: rozkład preferencji jest sprzeczny z teoretycznym rozkładem.")
} else {
  print("brak podstaw do odrzucenia hipotezy zerowej: rozkład preferencji jest zgodny z teoretycznym rozkładem.")
}

#Wnioski statystyczne:

#Z przeprowadzonego testu wynika, że hipoteza zerowa jest błędna, przez co odrzucamy ją i przyjmujemy
#hipotezę alternatywną, tzn. rozkład preferencji jest sprzeczny z teoretycznym rozkładem.

tabela2 <- data.frame(wybory)
colnames(tabela2) <- c("Kierunki")

wykres3 <- ggplot(data = tabela2, mapping = aes(x = Kierunki, fill = Kierunki)) +
  geom_histogram(stat = "count", position = "dodge", color = "seashell4") +
  scale_fill_manual(values = c("Góry" = "lightblue1", "Mazury" = "lavenderblush", "Morze" = "lightcyan1",
                               "Zagranica" = "mistyrose")) +
  labs(title = "Rozkład wymarzonych kierunków wakacyjnych", x = "Wymarzone kierunki wakacyjne", y = "Liczba osób")
print(wykres3)

#Interpretacja wykresu:

#Wizualizacja potwierdza wynik testu Chi-kwadrat. Rozkład słupków na wykresie odbiega
#od modelu teoretycznego – szczególnie widoczna jest nadwyżka osób wybierających Morze
#oraz niedoszacowanie osób wybierających Mazury względem przyjętych proporcji. Różnice te są na tyle duże,
#że nie można uznać ich za przypadek, co uzasadnia odrzucenie hipotezy zerowej.


