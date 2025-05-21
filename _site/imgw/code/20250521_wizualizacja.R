library(tidyverse)
library(readxl)

# rossmann <- read_excel(path = "data/rossmann.xlsx")
rossmann <- read_xlsx(path = "data/rossmann.xlsx")

sklep <- rossmann |> 
  filter(sklep_id == 21, czy_otwarty == "Tak")

# proste wykresy

plot(x = sklep$sprzedaz)
plot(x = sklep$sprzedaz, y = sklep$liczba_klientow, type = "p", pch = 19, 
     xlab = "Sprzedaż", ylab = "Liczba klientów", col = "blue")

hist(x = sklep$sprzedaz, main = "Histogram sprzedaży", 
     xlab = "Sprzedaż", ylab = "Liczebność", col = "lightskyblue", breaks = 6)

promocja <- table(sklep$czy_promocja)
barplot(promocja, main = "Promocja")

# ggplot2

p <- ggplot(data = sklep, mapping = aes(x = sprzedaz, y = liczba_klientow, 
                                        color = czy_promocja, label = data)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Sprzedaż") +
  ylab("Liczba klientów") +
  scale_color_discrete(name = "Promocja") +
  ggtitle("Sprzedaż i liczba klientów w roku 2014") +
  labs(subtitle = "Sklep nr 21", caption = "Opracowanie własne") +
  ylim(200,1000) +
  theme_light() +
  # theme(legend.position = "top")
  theme(legend.position = "inside", legend.position.inside = c(0.9,0.3))

p

ggsave("figs/sklep21_ggplot.png")
ggsave("figs/sklep21_ggplot.pdf", device = cairo_pdf)
ggsave("figs/sklep21_ggplot.png", width = 6, height = 4) # w calach
ggsave("figs/sklep21_ggplot.png", width = 1800, height = 1000, units = "px") # w pikselach

ggsave(filename = "figs/sklep21_ggplot.png", plot = p)

plotly::ggplotly(p)















