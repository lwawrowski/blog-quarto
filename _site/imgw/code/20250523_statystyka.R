library(tidyverse)

smog <- read_csv2("data/smog_pszczyna.csv")

smog <- smog |> 
  mutate(pm10_poziom=if_else(condition = pm10 < 50, true = "w normie", false = "podwyższony"),
         tmin_zero=if_else(tmin_daily < 0, "poniżej 0", "powyżej 0"))

# smog <- smog |> 
#   mutate(pm10_poziom = case_when(
#     pm10 < 50 ~ "w normie",
#     pm10 >= 50 ~ "podwyższony"
#   ))

plot(smog$tmax_daily, smog$tmin_daily)

ggplot(data = smog, mapping = aes(x = tmax_daily, y = tmin_daily)) +
  geom_point()

smog2017 <- smog |> 
  filter(rok == 2017)

# statystyka opisowa

summary(smog$t2m_mean_daily)

smog |> 
  group_by(rok) |> 
  summarise(srednia_temp=mean(t2m_mean_daily),
            odch_temp=sd(t2m_mean_daily),
            wsp_zm=odch_temp/srednia_temp*100)

smog |> 
  group_by(kwartal) |> 
  summarise(srednia_temp=mean(t2m_mean_daily),
            odch_temp=sd(t2m_mean_daily),
            wsp_zm=odch_temp/srednia_temp*100)

# cechy jakościowe

table(smog$poziom)

table(smog$cloud_mean_daily)
mean(smog$cloud_mean_daily)
median(smog$cloud_mean_daily)

# korelacja ciągła

# współczynnik korelacji liniowej Pearsona wynosi 0,8959 co oznacza 
# istnienie silnej korelacji dodatniej
cor(x = smog$tmax_daily, y = smog$tmin_daily)
cor(x = smog$tmax_daily, y = smog$tmin_daily, use = "complete.obs", method = "pearson")

# współczynnik korelacji rang Spearmana wynosi 0,8952 co oznacza
# istnienie silnej korelacji dodatniej
cor(x = smog$tmax_daily, y = smog$tmin_daily, method = "spearman")

# korelacja pm10 i temp
plot(x = smog$pm10, y = smog$tmax_daily)
cor(x = smog$pm10, y = smog$tmax_daily, use = "complete.obs")

# korelacja jakościowa
table(smog$pm10_poziom, smog$tmin_zero)

library(confintr)

# współczynnik korelacji V-Cramera wynosi 0,4368
# co oznacza umiarkowaną zależność między poziomem pm10 a kategorią temperatury
cramersv(table(smog$pm10_poziom, smog$tmin_zero))

# testy statystyczne

# Test istotności współczynnika korelacji
# H0: r = 0
# H1: r != 0

# p = 95%
# alpha = 0.05

# wartość p < alpha = 0.05 - odrzucamy hipotezę zerową
# współczynnik korelacji istotnie różni się od zera
cor.test(smog$tmin_daily, smog$tmax_daily)

# Test niezależności chi2
# H0: poziom pm10 i kategoria temperatury są niezależne
# H1: poziom pm10 i kategoria temperatury nie są niezależne

# p = 95%
# alpha = 0.05

# wartość p < alpha = 0.05 - odrzucamy hipotezę zerową
# poziom pm10 i kategoria temperatury nie są niezależne
chisq.test(table(smog$pm10_poziom, smog$tmin_zero))
qchisq(p = 0.95, df = 1)

# przedział ufności dla średniej
set.seed(123)
smog2017_proba <- smog2017 |> 
  sample_frac(0.3) |> 
  filter(!is.na(pm10))

mean(smog2017$pm10, na.rm = TRUE)
mean(smog2017_proba$pm10)

# z prawdopodobieństwem 95% prawdziwa wartość Pm10 zawiera się w przedziale
# od 43,78 do 74,77
t.test(smog2017_proba$pm10)

# z prawdopodobieństwem 90% prawdziwa wartość Pm10 zawiera się w przedziale
# od 46,3 do 72,24
t.test(smog2017_proba$pm10, conf.level = 0.9)

# przedział ufności dla odsetka
table(smog2017_proba$poziom)
# 28/108
sum(smog2017_proba$poziom == "dopuszczalny")/nrow(smog2017_proba)

sum(smog2017$poziom == "dopuszczalny", na.rm = TRUE)/nrow(smog2017)

#prop.test(x = 28, n = 108)
# z prawdopodobieństwem 95% prawdziwa wartość odsetka zawiera się w przedziale
# od 18% do 35%
prop.test(x = sum(smog2017_proba$poziom == "dopuszczalny"), n = nrow(smog2017_proba))

# test normalności

hist(smog2017$t2m_mean_daily)
# alpha = 0.05
# temperatura nie ma rozkładu normalnego
shapiro.test(x = smog2017$t2m_mean_daily)

ggplot(data = smog2017, mapping = aes(sample = t2m_mean_daily)) +
  stat_qq() +
  stat_qq_line()

# test dla dwóch grup

# próby zależne
# H0: średnia temp min = średnia temp max
# H1: średnia temp min != średnia temp max

shapiro.test(x = smog2017$tmin_daily)
shapiro.test(x = smog2017$tmax_daily)

# wartość p < 0.05 - odrzucamy hipotezę zerową
# wartości temp min różnią się istotnie od wartości temp max
wilcox.test(x = smog2017$tmin_daily, y = smog2017$tmax_daily, paired = TRUE)

t.test(x = smog2017$tmin_daily, y = smog2017$tmax_daily, paired = TRUE)

# próby niezależne
# H0: średnia temp nie różni się w 2 grupach poziomu pm10
# H1: średnia temp różni się w 2 grupach poziomu pm10

# wartość p < 0.05 - odrzucamy hipotezę zerową
# średnia temp różni się w 2 grupach poziomu pm10
wilcox.test(t2m_mean_daily ~ pm10_poziom, data = smog2017)
wilcox.test(smog2017$t2m_mean_daily ~ smog2017$pm10_poziom)

ggplot(smog2017, aes(x=t2m_mean_daily, y=pm10_poziom)) +
  geom_boxplot()

t.test(t2m_mean_daily ~ pm10_poziom, data = smog2017)

# wiele prób
ggplot(smog2017, aes(x=t2m_mean_daily, y=kwartal)) +
  geom_boxplot()

# H0: temperatura we wszystkich kwartałach jest taka sama
# H1: istnieje co najmniej jedna para kwartałów o różnej temperaturze

# wartość p < 0.05 - odrzucamy hipotezę zerową
# istnieje co najmniej jedna para kwartałów o różnej temperaturze
kruskal.test(t2m_mean_daily ~ kwartal, data = smog2017)

# wszystkie pary kwartałów różnią się istotnie
pairwise.wilcox.test(x = smog2017$t2m_mean_daily, g = smog2017$kwartal)

# ANOVA
temp_kw_anova <- aov(t2m_mean_daily ~ kwartal, data = smog2017)
summary(temp_kw_anova)

TukeyHSD(temp_kw_anova)

broom::tidy(TukeyHSD(temp_kw_anova))

# zadanie
ggplot(smog, aes(x=pm10, y=as.factor(rok))) +
  geom_boxplot()

kruskal.test(smog$pm10 ~ as.factor(smog$rok))
pairwise.wilcox.test(x = smog$pm10, g = as.factor(smog$rok))
