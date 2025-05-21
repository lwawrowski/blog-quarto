# mój pierwszy komentarz

6/2*(2+1)

# funkcje -----------------------------------------------------------------

Sys.sleep(time = 60)

sqrt(x = 100)
sqrt(100)

5 * log(x = 27, base = 3)
5 * log(base = 3, x = 27)
5 * log(base = 3, 27)
5 * log(27, 3)
5 * log(3, 27) # źle

log(100, 10) + sqrt(121)
log10(100) + sqrt(121)

# wartości nieliczbowe

0/0
3/0


# obiekty -----------------------------------------------------------------

# wartości

liczba <- 12
liczba

wynik <- log10(100) + sqrt(121)

liczba/3

2*10*3.14
2*10*pi

pi
pi <- 5
rm(pi)

# wektory

letters
LETTERS

month.name
month.abb

samochod <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
samochod <- c(T, F, F, T, T, F, T, F, T, T, T)
samochod

oceny <- c(3,4,4.5,5,5,4,5)

ksiazki <- c("Pan Tadeusz", "Quo Vadis", "Krzyżacy", "Chłopi")
ksiazki <- c('Pan Tadeusz', 'Quo Vadis', 'Krzyżacy', 'Chłopi')
ksiazki

# indeksowanie wektora
ksiazki[3]
samochod[1:5]
samochod[c(1,4,5)]

oceny > 4 # które elementy wektora spełniają warunek
oceny[oceny > 4]

# funkcje na wektorach

length(samochod)
typeof(samochod)

is.character(ksiazki)
is.numeric(ksiazki)
is.numeric(samochod)

mieszanka1 <- c(TRUE, 99, "Pan Tadeusz")
mieszanka2 <- c(TRUE, 99)

# rzutowanie wektora
samochod_num <- as.numeric(samochod)
mieszanka3 <- c(99, "112")
mieszanka3_num <- as.numeric(mieszanka3)

# replikacja
rep(x = 8, times = 11)
rep(x = 8, length.out = 11)
rep(x = 8, each = 11)

rep(x = mieszanka1, times = 11)
rep(x = mieszanka1, length.out = 11)
rep(x = mieszanka1, each = 11)

# sekwencja
seq(from = 1, to = 49, by = 1)
seq(from = 1, to = 49, length.out = 97)
seq(1,49)

1:49
1.5:5.5
49:1
-9:9

# statystyki

srednia_ocen <- round(x = mean(x = oceny), digits = 2)

oceny_nowe <- c(oceny, 3)
oceny_nowe <- c(oceny_nowe, c(5,4,4.5))

mean(oceny_nowe)

# zadanie
wektor1_10 <- 1:10
mean(wektor1_10)
sum(wektor1_10)

wektor1_10 + 2

wektor1_10 %% 2 == 0
parzyste <- wektor1_10[wektor1_10 %% 2 == 0]
parzyste

wektor1_10[wektor1_10 %% 2 == 0]

summary(samochod)
summary(oceny)
summary(ksiazki)

# faktor

plec <- c('M', 'K', 'M', 'K', 'K', 'K')
summary(plec)

plec_fct <- as.factor(plec)
summary(plec_fct)

# macierz

macierz <- matrix(data = seq(2, 32, 2), ncol = 4, byrow = TRUE)
macierz

macierz[2,] # indeksowanie wiersza
macierz[,3] # indeksowanie kolumny
macierz[3,4] # indeksowanie wartości

macierz[1:2, 1:2]

colSums(macierz)
rowSums(macierz)

# zadanie

macierz33 <- matrix(data = 1:9, nrow = 3)

macierz33[2,]
macierz33 * 2
colSums(macierz33)
colMeans(macierz33)

# ramka danych

df <- data.frame(plec=c("m", "k", "k", "m", "k", "m", "m", "m"),
                 wzrost=c(173, 170, 163, 178, 169, 180, 175, NA),
                 wydatki=c(1200, 2340, 1900, 890, 1010, 3099, 1478, 2030),
                 pali=c(T, F, F, F, T, F, NA, T))

View(df)
summary(df)

mean(df[,3])
mean(df[,"wydatki"])
mean(df$wydatki)

mean(df$wzrost, na.rm = TRUE)

df[7, "pali"] <- FALSE

df$wydatki > 2000
df[df$wydatki > 2000,]

# uzupełnianie wszystkich braków danych
is.na(df$wzrost)
df[is.na(df$wzrost), "wzrost"] <- 173 

# nowa kolumna
df$wyjazd <- rep(TRUE, 8)

# zmiana nazwy
names(df)[4] <- "czy_pali"

# lista

roznosci <- list(wartosc = srednia_ocen, wektor = oceny, dane = df)

roznosci$wektor
roznosci$dane$wydatki

roznosci[[1]]
roznosci[["wektor"]]
