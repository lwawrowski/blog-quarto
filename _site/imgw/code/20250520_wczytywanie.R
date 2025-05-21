library(tidyverse)

# zadanie data.frame

dane <- data.frame(imie=c("Anna", "Tomek"),
                   wiek=c(23, 25),
                   student=c(TRUE, FALSE))

summary(dane)

# zmiana nazwy kolumny
names(dane)[2] <- "Wiek"

# nowa kolumna
dane$miasto <- c("Warszawa", "Poznań")

# filtrowanie
dane[dane$student == TRUE,]

# wczytywanie pliku csv

movies <- read_csv(file = "data/movies.csv")
movies

movies_old <- read.csv(file = "data/movies.csv")
movies_old

movies_gui <- read_csv("data/movies.csv")

wybory <- read_csv2("data/wybory2025.csv")

wybory2025 <- read_delim("data/wybory2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

lotto <- read_delim(file = "http://www.mbnet.com.pl/dl.txt", delim = " ",
                    col_names = c("id", "data", "numery"), trim_ws = TRUE,
                    col_types = "ccc")

# wczytywanie pliku netCDF

library(ncdf4)

nc <- nc_open(filename = "data/cru10min30_tmp.nc")
print(nc)

names(nc$var)
temp <- ncvar_get(nc = nc, varid = "tmp")
temp_maj <- temp[,,5]

lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

lonlat <- as.matrix(expand.grid(lon, lat))
temp_maj_vec <- as.vector(temp_maj)

lonlat_df <- data.frame(lonlat, temp_maj_vec)
names(lonlat_df) <- c("lon", "lat", "temp")

summary(lonlat_df)

complete.cases(lonlat_df)

lonlat_cc <- lonlat_df[complete.cases(lonlat_df),]

plot(lonlat_cc$lon, lonlat_cc$lat)

write_csv(x = lonlat_cc, file = "data/temp_maj.csv")

# wczytywanie plików RData

load(file = "data/gus.RData")

save(temp_maj, lotto, file = "data/temp_maj_lotto.RData")

