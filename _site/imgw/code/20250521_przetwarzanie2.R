library(tidyverse)

load("data/wybory_analiza.rda")

wybory_stat <- wybory |> 
  summarise(srednia=mean(liczba_wyborcow, na.rm=TRUE),
            mediana=median(liczba_wyborcow, na.rm=TRUE),
            q3=quantile(liczba_wyborcow, 0.75, na.rm=TRUE),
            liczebnosc=n())

q3 <- wybory_stat$q3

wybory_summ <- summary(wybory$liczba_wyborcow)
wybory_summ[5]

wybory |> 
  filter(!is.na(frekwencja)) |> 
  summarise(srednia=mean(frekwencja),
            odch=sd(frekwencja))

# grupowanie

wyborcy_woj <- wybory |> 
  group_by(wojewodztwo) |> 
  summarise(n=n(),
            mediana=median(liczba_wyborcow)) |> 
  arrange(desc(mediana))

wybory
wyborcy_woj

# zliczanie wartości

wyborcy_woj_count <- wybory |> 
  count(wojewodztwo)

wyborcy_woj_count2 <- wybory |> 
  group_by(wojewodztwo) |> 
  summarise(n=n())

# wiele kolumn

poparcie_woj <- wybory |> 
  # filter_at(vars(biejat_magdalena_agnieszka_proc:zandberg_adrian_tadeusz_proc), ~ !is.nan(.)) |> 
  # drop_na() |> 
  filter(liczba_kart_waznych > 0) |> 
  group_by(wojewodztwo) |> 
  summarise(across(biejat_magdalena_agnieszka_proc:zandberg_adrian_tadeusz_proc, mean))

wybrane_woj <- wybory |> 
  filter(wojewodztwo == "lubelskie") |> 
  select(siedziba, biejat_magdalena_agnieszka, liczba_kart_waznych, biejat_magdalena_agnieszka_proc)


# łączenie danych ---------------------------------------------------------

load("data/gus.RData")

przyrost_16_17 <- left_join(x = przyrost2016, y = przyrost2017)
przyrost_16_17 <- left_join(x = przyrost2016, y = przyrost2017, by = join_by(id, name))
przyrost_16_17 <- left_join(x = przyrost2016, y = przyrost2017, by = join_by(id))
przyrost_16_17 <- left_join(x = przyrost2016, y = przyrost2017, by = c("id" = "id", "name"))

przyrost_16_17_r <- right_join(x = przyrost2017, y = przyrost2016)

przyrost_15_17 <- inner_join(x = przyrost2015, y = przyrost2017)

przyrost_15_17_anti <- anti_join(x = przyrost2015, y = przyrost2017)
przyrost_17_15_anti <- anti_join(x = przyrost2017, y = przyrost2015)

przyrost_15_17_full <- full_join(x = przyrost2015, y = przyrost2017)

# wszystko razem

# 1 sposób
przyrost_15_16 <- full_join(przyrost2015, przyrost2016)
przyrost_15_16_17 <- full_join(przyrost_15_16, przyrost2017)

# 2 sposób
przyrost_15_16_17 <- full_join(x = przyrost2015, y = full_join(x = przyrost2016, y = przyrost2017))

# 3 sposób
przyrost_15_16_17 <- full_join(przyrost2015, przyrost2016) |> 
  full_join(przyrost2017)

przyrost_15_16_17 <- przyrost2015 |> 
  full_join(przyrost2016) |> 
  full_join(przyrost2017)

# zadanie

bezr_podmioty <- inner_join(bezrobocie, pod_10tys, by = c("id_powiat" = "pow"))

bezr_podmioty <- bezr_podmioty |> 
  mutate(id_powiat_num=as.numeric(id_powiat))

bezr_podmioty_wyn <- inner_join(bezr_podmioty, wyn, by = c("id_powiat_num" = "pow"))

bezr_podmioty_wyn <- bezr_podmioty_wyn |> 
  select(-id_powiat_num)

# konsolidacja
bezr_podmioty_wyn <- inner_join(bezrobocie, pod_10tys, by = c("id_powiat" = "pow")) |> 
  mutate(id_powiat_num=as.numeric(id_powiat)) |> 
  inner_join(wyn, by = c("id_powiat_num" = "pow")) |> 
  select(-id_powiat_num)

# długa i szeroka reprezentacja danych

dane_long <- bezr_podmioty_wyn |> 
  pivot_longer(stopa_bezr:wynagrodzenie)

bezr_podmioty_wyn |> 
  summarise(srednia_stopa=mean(stopa_bezr),
            srednia_pod=mean(pod_10tys),
            srednia_wyn=mean(wynagrodzenie))

dane_long |> 
  group_by(name) |> 
  summarise(srednia=mean(value))

dane_wide <- dane_long |> 
  pivot_wider()

dane_wide <- dane_long |> 
  pivot_wider(names_from = name, values_from = value, names_prefix = "rok2018_")

rm(q3)

popracie_woj_long <- poparcie_woj |> 
  pivot_longer(biejat_magdalena_agnieszka_proc:zandberg_adrian_tadeusz_proc)









