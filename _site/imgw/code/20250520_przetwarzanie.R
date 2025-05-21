library(tidyverse)

wybory <- read_csv2("data/wybory2025.csv")
summary(wybory)

summary(wybory$Gmina)
summary(wybory$`Teryt Gminy`)

wybory <- janitor::clean_names(wybory)
summary(wybory)

# filtrowanie

wybory_karty <- wybory |> 
  filter(liczba_kart_waznych == 777)

wybory_pszczynski <- wybory |>
  filter(powiat == "pszczyński")

# łączenie warunków

wybory_karty_woj <- wybory |> 
  filter(liczba_kart_waznych > 1000) |> 
  filter(wojewodztwo == "wielkopolskie")

wybory_karty_woj <- wybory |> 
  filter(liczba_kart_waznych > 1000, wojewodztwo == "wielkopolskie")

wybory_karty_woj <- wybory |> 
  filter(liczba_kart_waznych > 1000 & wojewodztwo == "wielkopolskie")

# alternatywa

wybory_karty_lub_woj <- wybory |> 
  filter(liczba_kart_waznych > 1000 | wojewodztwo == "wielkopolskie")
  
wybory_karty_woj2 <- wybory |> 
  filter(liczba_kart_waznych > 1000 & (wojewodztwo == "wielkopolskie" | wojewodztwo == "małopolskie"))

summary(wybory_karty_woj2$liczba_kart_waznych)  

wybory_karty_woj3 <- wybory |> 
  filter(liczba_kart_waznych > 1000 & wojewodztwo %in% c("wielkopolskie", "małopolskie", "opolskie"))

wybrane_woj <- c("wielkopolskie", "małopolskie", "opolskie")

wybory_karty_woj3 <- wybory |> 
  filter(liczba_kart_waznych > 1000 & wojewodztwo %in% wybrane_woj)

# negacja

wybory_karty_woj3_neg <- wybory |> 
  filter(liczba_kart_waznych > 1000 & !wojewodztwo %in% wybrane_woj)

# zadanie

movies <- read_csv(file = "data/movies.csv")
movies_zad <- movies |> 
  filter(genre == "Action" & year > 2010 & (duration >= 120 | rating > 8))

# braki danych

wybory_braki <- wybory |> 
  filter(!is.na(teryt_gminy))

complete.cases(wybory)

sum(complete.cases(wybory))

wybory_cc <- wybory |> 
  filter(complete.cases(wybory))

wybory_cc <- wybory |> 
  select(gmina, siedziba, liczba_kart_waznych) |> 
  filter(complete.cases(across(everything())))

# wybieranie kolumn

wybory_kolumny <- wybory |> 
  select(-teryt_gminy, -teryt_powiatu)

wybory_kolumny <- wybory |> 
  select(-c(teryt_gminy, teryt_powiatu))

wybory_kolumny <- wybory |> 
  select(gmina:siedziba, liczba_kart_waznych)

wybory_liczba <- wybory |> 
  select(starts_with("liczba"))

wybory_num <- wybory |> 
  select_if(is.numeric)

# kolejność w przetwarzaniu potokowym

wybory_siedziba <- wybory |> 
  filter(liczba_kart_waznych > 1000) |> 
  select(siedziba)

# wybory_siedziba <- wybory |> 
#   select(siedziba) |> 
#   filter(liczba_kart_waznych > 1000)

# zmiana nazwy

wybory_siedziba <- wybory |> 
  rename(adres_siedziby=siedziba)

# nowa zmienna

wybory <- wybory |> 
  rename(liczba_wyborcow=liczba_wyborcow_uprawnionych_do_glosowania_umieszczonych_w_spisie_z_uwzglednieniem_dodatkowych_formularzy_w_chwili_zakonczenia_glosowania,
         liczba_wydanych_kart=liczba_wyborcow_ktorym_wydano_karty_do_glosowania_w_lokalu_wyborczym_oraz_w_glosowaniu_korespondencyjnym_lacznie)

# dużo roboty
# wybory <- wybory |> 
#   mutate(frekwencja=liczba_wydanych_kart/liczba_wyborcow*100) |> 
#   mutate(bartoszewicz_artur_proc=bartoszewicz_artur/liczba_kart_waznych,
#          biejat_magdalena_agnieszka_proc=biejat_magdalena_agnieszka/liczba_kart_waznych)

wybory <- wybory |> 
  mutate(frekwencja=liczba_wydanych_kart/liczba_wyborcow*100) |> 
  mutate(across(.cols = bartoszewicz_artur:zandberg_adrian_tadeusz,
                .fns = ~ . /liczba_kart_waznych*100, 
                .names = "{.col}_proc"))

# zadanie

# wartość bezwzględna różnicy
wybory <- wybory |> 
  mutate(roznica_mb_az=abs(biejat_magdalena_agnieszka-zandberg_adrian_tadeusz))

wybory_roznica <- wybory |> 
  select(siedziba, biejat_magdalena_agnieszka, zandberg_adrian_tadeusz, roznica_mb_az)

wybory <- wybory |> 
  mutate(wygrana_az=if_else(condition = zandberg_adrian_tadeusz > biejat_magdalena_agnieszka, 
                            true = "Wygrana AZ", false = "Przegrana AZ")) |> 
  mutate(frekwencja_kat = case_when(
    frekwencja < 33 ~ "niska",
    frekwencja >= 33 & frekwencja < 67 ~ "średnia",
    frekwencja >= 67 ~ "wysoka",
    .default = as.character(frekwencja)
  ))

save(wybory, file = "data/wybory_analiza.rda")




