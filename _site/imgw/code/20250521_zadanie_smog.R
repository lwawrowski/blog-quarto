library(tidyverse)

smog <- read_csv2(file = "data/smog_pszczyna.csv")

smog_clean <- janitor::clean_names(smog)

smog_braki <- smog_clean |> 
  drop_na()

smog_analiza <- smog_clean |> 
  filter(!is.na(pm10))

smog_analiza <- smog_analiza |> 
  select(data, t2m_mean_daily, tmin_daily, tmax_daily, pm10, poziom, cloud_mean_daily, ws_mean_daily) 

smog_analiza <- smog_analiza |> 
  rename(tmean_daily=t2m_mean_daily, wiatr = ws_mean_daily, zachmurzenie=cloud_mean_daily)

# smog_analiza <- smog_analiza |> 
#   mutate(kategoria_temp = if_else(tmean_daily < 0, "zimno", "ciepło"))

smog_analiza <- smog_analiza |>
  mutate(kategoria_temp = case_when(
    tmean_daily < -10 ~ "ekstremalnie zimno",
    tmean_daily < 0 ~ "zimno",
    tmean_daily < 10 ~ "chłodno",
    tmean_daily < 20 ~ "umiarkowanie",
    tmean_daily >= 20 ~ "ciepło"
  ))

# wszystko razem
smog_analiza <- smog_clean |> 
  filter(!is.na(pm10)) |> 
  select(data, t2m_mean_daily, tmin_daily, tmax_daily, pm10, poziom, cloud_mean_daily, ws_mean_daily) |> 
  rename(tmean_daily=t2m_mean_daily, wiatr = ws_mean_daily, zachmurzenie=cloud_mean_daily) |> 
  mutate(kategoria_temp = case_when(
    tmean_daily < -10 ~ "ekstremalnie zimno",
    tmean_daily < 0 ~ "zimno",
    tmean_daily < 10 ~ "chłodno",
    tmean_daily < 20 ~ "umiarkowanie",
    tmean_daily >= 20 ~ "ciepło"
  ))
