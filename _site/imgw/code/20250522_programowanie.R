# generowanie danych

rnorm(20)
hist(rnorm(20000))

# generowanie danych o wzroście
rnorm(n = 30, mean = 170, sd = 10)

runif(20)
hist(runif(20000))

dane <- data.frame(x1=rnorm(20), x2=rnorm(20), x3=runif(20), x4=runif(20))
dane

w1 <- mean(dane$x1)/median(dane$x1)
w2 <- mean(dane$x2)/median(dane$x2)
w3 <- mean(dane$x3)/median(dane$x3)
w4 <- mean(dane$x4)/median(dane$x4)

# funkcje

oblicz_wskaznik <- function(x){
  
  wskaznik <- mean(x)/sd(x)
  
  return(wskaznik)
  
}

w1 <- oblicz_wskaznik(dane$x1)
w2 <- oblicz_wskaznik(dane$x2)
w3 <- oblicz_wskaznik(dane$x3)
w4 <- oblicz_wskaznik(dane$x4)

# normalizacja danych

# sposób nr 1
normalizuj <- function(x) {
  
  return((x - min(x)) / (max(x) - min(x)))
  
}

dane$z1 <- normalizuj(dane$x1)
dane$z2 <- normalizuj(dane$x2)
dane$z3 <- normalizuj(dane$x3)
dane$z4 <- normalizuj(dane$x4)

plot(dane$x1, dane$z1)

# sposób nr 2
normalizuj <- function(x) {
  
  (x - min(x)) / (max(x) - min(x))
  
}

# sposób nr 3
normalizuj <- function(x) (x - min(x)) / (max(x) - min(x))

# sposób nr 4
normalizuj <- \(x) (x - min(x)) / (max(x) - min(x))

# warunki

wartosc <- 0

sprawdz_wskaznik <- function(wartosc){
  
  if(is.na(wartosc)){
    stop("Podana wartość to NA")
  }
  if(is.character(wartosc)){
    stop("Podana wartość to tekst")
  }
  
  if(wartosc > 0){
    print("Wartość większa od 0")
  } else if(wartosc < 0){
    print("Wartość mniejsza od 0")
  } else {
    print("Wartość równa 0")
  }
  
}

rm(wartosc)
sprawdz_wskaznik(99)
sprawdz_wskaznik(-10)
sprawdz_wskaznik(NA)
sprawdz_wskaznik("xyz")


# zadanie

sprawdz_podzielnosc <- function(liczba) {
  if (!is.numeric(liczba) || length(liczba) != 1) {
    stop("Argument musi być pojedynczą liczbą.")
  }
  
  if(liczba %% 3 == 0 && liczba %% 5 == 0) {
    return("Liczba podzielna przez 3 i 5")
  } else if (liczba %% 3 == 0) {
    return("Liczba podzielna przez 3")
  } else if (liczba %% 5 == 0) {
    return("Liczba podzielna przez 5")
  } else {
    return("Liczba niepodzielna ani przez 3, ani przez 5")
  }
}

sprawdz_podzielnosc(15)

# pętle

month.name

for(month in 1:12){
  
  print(month.name[month])
  
}

for(month in 1:length(month.name)){
  print(month.name[month])
}

for(month in seq_along(month.name)){
  print(month.name[month])
}

for(month in month.name){
  print(month)
}

# zwiększenie liter

month_name_upper <- character(length = length(month.name))

for(month in seq_along(month.name)){
  
  month_name_upper[month] <- toupper(month.name[month])
  
}

month_name_upper

toupper(month.name)

# rzut kostką

rzut_kostka <- numeric(1000000)

for(rzut in seq_along(rzut_kostka)){
  
  rzut_kostka[rzut] <- sample(x = 1:6, size = 1)
  
}

table(rzut_kostka)

rzut_kostka_vec <- sample(x = 1:6, size = 1000000, replace = TRUE)

# pętla while

eps = 0.5

while(eps > 0.1){
  
  print(eps)
  eps <- eps - 0.05
  
}

paste(month, "miesiąc")

# zadanie

for(i in 99:1){
  if(i == 1){
    print('1 bottle of beer on the wall, 1 bottle of beer!')
    print('So take it down, pass it around, no more bottles of beer on the wall!')
  } else if(i == 2){
    print('2 more bottles of beer on the wall, 2 more bottles of beer!')
    print('So take one down, pass it around, 1 more bottle of beer on the wall!')
  } else{
    print(paste(i, 'bottles of beer on the wall,', i, 'bottles of beer!'))
    print(paste('So take it down, pass it around,', i-1, 'more bottles of beer on the wall!'))
  }
}

for (i in 99:1) {
  if (i > 1) {
    cat(i, "bottles of beer on the wall,", i, "bottles of beer.\n")
    cat("Take one down and pass it around,", i - 1, 
        ifelse(i - 1 == 1, "bottle", "bottles"), "of beer on the wall.\n\n")
  } else {
    cat("1 bottle of beer on the wall, 1 bottle of beer.\n")
    cat("Take one down and pass it around, no more bottles of beer on the wall.\n\n")
  }
}

cat("No more bottles of beer on the wall, no more bottles of beer.\n")
cat("Go to the store and buy some more, 99 bottles of beer on the wall.\n")
