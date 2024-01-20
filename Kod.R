
# Potrzebne pakiety
library(readr)
library (dplyr)
library(cowplot)
library(moments)
library(ggplot2)
library(tidyr)



# Pobieranie danych z pliku csv, wszytskie dane zostały pobrane ze strony stooq.pl
# Uwaga ! Prosze pamietac o zmianie śxciezki aby dane sie poprawnie załadowały


eat_w <- read_csv("~/eat_w.csv") # Tygodniowe dane AmRest Holdings SE mWIG40

cdr_d <- read_csv("~/cdr_d.csv") # Dzienne dane CDPROJECT WIG20
cdr_w <- read_delim("~/cdr_w.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE) # Tygodniowe dane CDPROJECT WIG20
eat_d <- read_delim("~/eat_d.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE) # Dzienne dane AmRest Holdings SE mWIG40

# Losowanie próbek 1/3 dłuigości 

probka_ew <- sample (eat_w$Zamkniecie, 35)
probka_ed <- sample (eat_d$Zamkniecie, 167)
probka_cd <- sample (cdr_d$Zamkniecie, 167)
probka_cw <- sample (cdr_w$Zamkniecie, 35)



#  Obliczanie stóp zworotu

# stopy zwortu 

sz_e_w <- log(eat_w[2:106,5]) - log(eat_w[1:105,5])
sz_e_d <- log(eat_d[2:501,5]) - log(eat_d[1:500,5])
sz_c_d <- log(cdr_d[2:501,5]) - log(cdr_d[1:500,5])
sz_c_w <- log(cdr_w[2:106,5]) - log(cdr_w[1:105,5])



probka_sz_e_w <- log(probka_ew[2:35]) - log(probka_ew[1:34])
probka_sz_e_d <- log(probka_ed[2:167]) - log(probka_ed[1:166])
probka_sz_c_d <- log(probka_cd[2:167]) - log(probka_cd[1:166])
probka_sz_c_w <- log(probka_cw[2:35]) - log(probka_cw[1:34])

# Stworzenie tabel z wynikami 

wyniki_e_w <- data.frame( eat_w[2:106,1],sz_e_w )
wyniki_e_d <- data.frame(eat_d[2:501,1], sz_e_d  )
wyniki_c_d <- data.frame( cdr_d[2:501,1],sz_c_d )
wyniki_c_w <- data.frame(cdr_w[2:106,1], sz_c_w )

colnames(wyniki_e_w) <- c( "Dzień", "stopy zwortu")
colnames(wyniki_e_d) <- c("Dzień", "stopy zwortu")
colnames(wyniki_c_w) <- c("Dzień","stopy zwortu")
colnames(wyniki_c_d) <- c("Dzień", "stopy zwortu")

# Wizaulizacja 
wyniki_e_w %>%
  ggplot(aes(x = Dzień , y = `stopy zwortu`))+
  geom_line(size = 1) +
  geom_line( y = 0, color = "red")

wyniki_e_d %>%
  ggplot(aes(x = Dzień , y = `stopy zwortu`))+
  geom_line(size = 1) +
  geom_line( y = 0, color = "red")

wyniki_c_w %>%
  ggplot(aes(x = Dzień , y = `stopy zwortu`))+
  geom_line(size = 1) +
  geom_line( y = 0, color = "red")

wyniki_c_d %>%
  ggplot(aes(x = Dzień , y = `stopy zwortu`))+
  geom_line(size = 1) +
  geom_line( y = 0, color = "red")

# Podstawowe charakterystyki rozkładówdla całości danych

Średnia <- c( mean(sz_e_w$Zamkniecie), mean(sz_e_d$Zamkniecie), mean (sz_c_w$Zamkniecie), mean(sz_c_d$Zamkniecie))

Odchylenie_stan <- c( sd(sz_e_w$Zamkniecie), sd(sz_e_d$Zamkniecie), sd(sz_c_w$Zamkniecie), sd(sz_c_d$Zamkniecie))

Kurtoza <- c(kurtosis(sz_e_w$Zamkniecie), kurtosis(sz_e_d$Zamkniecie), kurtosis(sz_c_w$Zamkniecie), kurtosis(sz_c_d$Zamkniecie))

Skośność <-  c( skewness(sz_e_w$Zamkniecie), skewness(sz_e_d$Zamkniecie), skewness(sz_c_w$Zamkniecie), skewness(sz_c_d$Zamkniecie))

Statystyki_opisowe <- round (data.frame( Średnia , Odchylenie_stan , Skośność, Kurtoza) , 4 )

row.names(Statystyki_opisowe) <- c( " AmRest tygodniowe", "AmRest dzienne", " CDProjekt tygodniowe", "CDProjket dzienne")

# Podstawowe charakterystyki rozkładów dla próbek 

Średnia <- c( mean(probka_sz_e_w), mean(probka_sz_e_d), mean (probka_sz_c_w), mean(probka_sz_c_d))

Odchylenie_stan <- c( sd(probka_sz_e_w), sd(probka_sz_e_d), sd(probka_sz_c_w), sd(probka_sz_c_d))

Kurtoza <- c(kurtosis(probka_sz_e_w), kurtosis(probka_sz_e_d), kurtosis(probka_sz_c_w), kurtosis(probka_sz_c_d))

Skośność <-  c( skewness(probka_sz_e_w), skewness(probka_sz_e_d), skewness(probka_sz_c_w), skewness(probka_sz_c_d))

Statystyki_opisowe_1 <- round (data.frame( Średnia , Odchylenie_stan , Skośność, Kurtoza) , 4 )

row.names(Statystyki_opisowe_1) <- c( " AmRest tygodniowe", "AmRest dzienne", " CDProjekt tygodniowe", "CDProjket dzienne")





# Wizualizacja rozkładów 

# Pełne dane 

boxplot (sz_e_w$Zamkniecie , sz_e_d$Zamkniecie, sz_c_w$Zamkniecie,  sz_c_d$Zamkniecie , names = c( "A tyg", "A d", " C tyg", "C d"))

# próbka 

boxplot (probka_sz_e_w , probka_sz_e_d, probka_sz_c_w,  probka_sz_c_d , names = c( "A tyg", "A d", " C tyg", "C d"))

# Pełne dane  
D <- function(x , kolor) {
  
  ggplot() +
    geom_density(aes(x = x), fill = kolor , col= "black", alpha = 0.6) +
    xlim(-0.3 , 0.3)+
    theme_light() +
    labs (y = NULL,
          x = NULL) 
  
}

D1 <- D(sz_e_w$Zamkniecie, "darkgreen")
D2 <- D( sz_e_d$Zamkniecie , "#9ACD32")
D3 <- D( sz_c_w$Zamkniecie , "navy")
D4 <- D(sz_c_d$Zamkniecie , "steelblue")

plot_grid(D1, D2, D3, D4, align = "hv",
          label_x = 0.1, label_y = 0.95, labels = c(" AmRest tygodniowe", "AmRest dzienne", " CDProjekt tygodniowe", "CDProjket dzienne"), label_size = 13) 


# próbka 

D <- function(x , kolor) {
  
  ggplot() +
    geom_density(aes(x = x), fill = kolor , col= "black", alpha = 0.6) +
    xlim(-0.6 , 0.6)+
    theme_light() +
    labs (y = NULL,
          x = NULL) 
  
}


D1 <- D(probka_sz_e_w, "darkgreen")
D2 <- D( probka_sz_e_d , "#9ACD32")
D3 <- D( probka_sz_c_w , "navy")
D4 <- D(probka_sz_c_d , "steelblue")

plot_grid(D1, D2, D3, D4, align = "hv",
          label_x = 0.1, label_y = 0.95, labels = c(" AmRest tygodniowe", "AmRest dzienne", " CDProjekt tygodniowe", "CDProjket dzienne"), label_size = 13) 

# Badanie normalności 

# Pełne dane

# H0 : Rozkład jest normlany
# H1 : Rozkład nie jest normalny

# Poziom istotności = 0.1

# Test Shapiro - Wilka 

Shapiro.Wilk <- c ( shapiro.test(sz_e_w$Zamkniecie)$p.value , 
            shapiro.test( sz_e_d$Zamkniecie)$p.value, 
            shapiro.test(sz_c_w$Zamkniecie)$p.value,
            shapiro.test(sz_c_d$Zamkniecie)$p.value
            )

# Test Kołmogorowa 
Kolmogorowa <- c ( ks.test(sz_e_w$Zamkniecie, "pnorm" , mean = mean(sz_e_w$Zamkniecie), sd = sd (sz_e_w$Zamkniecie))$p.value,
                    ks.test(sz_e_d$Zamkniecie, "pnorm" , mean = mean(sz_e_d$Zamkniecie), sd = sd (sz_e_d$Zamkniecie))$p.value,
                    ks.test(sz_c_w$Zamkniecie, "pnorm" , mean = mean(sz_c_w$Zamkniecie), sd = sd (sz_c_w$Zamkniecie))$p.value,
                    ks.test(sz_c_d$Zamkniecie, "pnorm" , mean = mean(sz_c_d$Zamkniecie), sd = sd (sz_c_d$Zamkniecie))$p.value
                    )
# Test chi- kwadrat 

# Funkcja dzieląca na klasy względem wynzaczonych kwartyli 

Klasy <- function(x) {
  
k1 <- x[x< q1]
k2 <- x[ x < q2 & x > q1]
k3 <- x[x < q3 & x> q2]
k4 <- x[x < q4 & x> q3]
k5 <- x[x < q5 &x > q4]
k6 <- x[x < q6 & x > q5]
k7 <- x[x < q7 & x> q6]
k8 <- x[x < q8 & x > q7]
k9 <- x[x < q9 & x> q8]
k10 <- x[x< q10 & x> q9]

liczebnosc <- c(length(k1) , length(k2), length(k3), length(k4), length(k5), length(k6), length(k7), length(k8), length(k9), length(k10))

return (liczebnosc)
}

prob <- rep ( 0.1, 10)

m <- mean(sz_e_w$Zamkniecie)
s <- sd(sz_e_w$Zamkniecie)

q1 <- qnorm (0.1 , mean = m, sd = s)
q2 <-qnorm (0.2 , mean = m , sd = s)
q3 <-qnorm (0.3 , mean = m , sd = s)
q4 <-qnorm (0.4 , mean = m , sd = s)
q5 <-qnorm (0.5 , mean = m , sd = s)
q6 <-qnorm (0.6 , mean = m , sd = s)
q7 <-qnorm (0.7 , mean = m , sd = s)
q8 <-qnorm (0.8 , mean = m , sd = s)
q9 <-qnorm (0.9 , mean = m , sd = s)
q10 <- qnorm( 1, mean = m , sd = s)


chi_s_w <- chisq.test(Klasy(sz_e_w$Zamkniecie), p = prob )$p.value

m <- mean(sz_e_d$Zamkniecie)
s <- sd(sz_e_d$Zamkniecie)

q1 <- qnorm (0.1 , mean = m, sd = s)
q2 <-qnorm (0.2 , mean = m , sd = s)
q3 <-qnorm (0.3 , mean = m , sd = s)
q4 <-qnorm (0.4 , mean = m , sd = s)
q5 <-qnorm (0.5 , mean = m , sd = s)
q6 <-qnorm (0.6 , mean = m , sd = s)
q7 <-qnorm (0.7 , mean = m , sd = s)
q8 <-qnorm (0.8 , mean = m , sd = s)
q9 <-qnorm (0.9 , mean = m , sd = s)
q10 <- qnorm( 1, mean = m , sd = s)



chi_s_d <- chisq.test(Klasy(sz_e_d$Zamkniecie), p = prob )$p.value

m <- mean(sz_c_w$Zamkniecie)
s <- sd(sz_c_w$Zamkniecie)

q1 <- qnorm (0.1 , mean = m, sd = s)
q2 <-qnorm (0.2 , mean = m , sd = s)
q3 <-qnorm (0.3 , mean = m , sd = s)
q4 <-qnorm (0.4 , mean = m , sd = s)
q5 <-qnorm (0.5 , mean = m , sd = s)
q6 <-qnorm (0.6 , mean = m , sd = s)
q7 <-qnorm (0.7 , mean = m , sd = s)
q8 <-qnorm (0.8 , mean = m , sd = s)
q9 <-qnorm (0.9 , mean = m , sd = s)
q10 <- qnorm( 1, mean = m , sd = s)


chi_c_w <- chisq.test(Klasy(sz_c_w$Zamkniecie), p = prob )$p.value

m <- mean(sz_c_d$Zamkniecie)

s <- sd(sz_c_d$Zamkniecie)


q1 <- qnorm (0.1 , mean = m, sd = s)
q2 <-qnorm (0.2 , mean = m , sd = s)
q3 <-qnorm (0.3 , mean = m , sd = s)
q4 <-qnorm (0.4 , mean = m , sd = s)
q5 <-qnorm (0.5 , mean = m , sd = s)
q6 <-qnorm (0.6 , mean = m , sd = s)
q7 <-qnorm (0.7 , mean = m , sd = s)
q8 <-qnorm (0.8 , mean = m , sd = s)
q9 <-qnorm (0.9 , mean = m , sd = s)
q10 <- qnorm( 1, mean = m , sd = s)


chi_c_d <- chisq.test(Klasy(sz_c_d$Zamkniecie), p = prob )$p.value

Chi_kwadrat <- c (chi_s_w, chi_s_d,chi_c_w ,chi_c_d)

Testy_normalnosc <- round (data.frame(Shapiro.Wilk,Kolmogorowa,Chi_kwadrat) , 5)
 
#  próbka

# H0 : Rozkład jest normlany
# H1 : Rozkład nie jest normalny

# Poziom istotności = 0.1

# Test Shapiro - Wilka 

Shapiro.Wilk <- c ( shapiro.test(probka_sz_e_w)$p.value , 
                    shapiro.test(probka_sz_e_d)$p.value, 
                    shapiro.test(probka_sz_c_w)$p.value,
                    shapiro.test(sz_c_d$Zamkniecie)$p.value
)

# Test Kołmogorowa 
Kolmogorowa <- c ( ks.test(probka_sz_e_w, "pnorm" , mean = mean(probka_sz_e_w), sd = sd (probka_sz_e_w))$p.value,
                   ks.test(probka_sz_e_d, "pnorm" , mean = mean(probka_sz_e_d), sd = sd (probka_sz_e_d))$p.value,
                   ks.test(probka_sz_c_w, "pnorm" , mean = mean(probka_sz_c_w), sd = sd (probka_sz_c_w))$p.value,
                   ks.test(sz_c_d$Zamkniecie, "pnorm" , mean = mean(sz_c_d$Zamkniecie), sd = sd (sz_c_d$Zamkniecie))$p.value
)
# Test chi- kwadrat 

# Funkcja dzieląca na klasy względem wynzaczonych kwartyli 

Klasy <- function(x) {
  
  k1 <- x[x< q1]
  k2 <- x[ x < q2 & x > q1]
  k3 <- x[x < q3 & x> q2]
  k4 <- x[x < q4 & x> q3]
  k5 <- x[x < q5 &x > q4]
  k6 <- x[x < q6 & x > q5]
  k7 <- x[x < q7 & x> q6]
  k8 <- x[x < q8 & x > q7]
  k9 <- x[x < q9 & x> q8]
  k10 <- x[x< q10 & x> q9]
  
  liczebnosc <- c(length(k1) , length(k2), length(k3), length(k4), length(k5), length(k6), length(k7), length(k8), length(k9), length(k10))
  
  return (liczebnosc)
}

prob <- rep ( 0.1, 10)

m <- mean(probka_sz_e_w)
s <- sd(probka_sz_e_w)

q1 <- qnorm (0.1 , mean = m, sd = s)
q2 <-qnorm (0.2 , mean = m , sd = s)
q3 <-qnorm (0.3 , mean = m , sd = s)
q4 <-qnorm (0.4 , mean = m , sd = s)
q5 <-qnorm (0.5 , mean = m , sd = s)
q6 <-qnorm (0.6 , mean = m , sd = s)
q7 <-qnorm (0.7 , mean = m , sd = s)
q8 <-qnorm (0.8 , mean = m , sd = s)
q9 <-qnorm (0.9 , mean = m , sd = s)
q10 <- qnorm( 1, mean = m , sd = s)


chi_s_w <- chisq.test(Klasy(probka_sz_e_w), p = prob )$p.value

m <- mean(probka_sz_e_d)
s <- sd(probka_sz_e_d)

q1 <- qnorm (0.1 , mean = m, sd = s)
q2 <-qnorm (0.2 , mean = m , sd = s)
q3 <-qnorm (0.3 , mean = m , sd = s)
q4 <-qnorm (0.4 , mean = m , sd = s)
q5 <-qnorm (0.5 , mean = m , sd = s)
q6 <-qnorm (0.6 , mean = m , sd = s)
q7 <-qnorm (0.7 , mean = m , sd = s)
q8 <-qnorm (0.8 , mean = m , sd = s)
q9 <-qnorm (0.9 , mean = m , sd = s)
q10 <- qnorm( 1, mean = m , sd = s)



chi_s_d <- chisq.test(Klasy(probka_sz_e_d), p = prob )$p.value

m <- mean(probka_sz_c_w)
s <- sd(probka_sz_c_w)

q1 <- qnorm (0.1 , mean = m, sd = s)
q2 <-qnorm (0.2 , mean = m , sd = s)
q3 <-qnorm (0.3 , mean = m , sd = s)
q4 <-qnorm (0.4 , mean = m , sd = s)
q5 <-qnorm (0.5 , mean = m , sd = s)
q6 <-qnorm (0.6 , mean = m , sd = s)
q7 <-qnorm (0.7 , mean = m , sd = s)
q8 <-qnorm (0.8 , mean = m , sd = s)
q9 <-qnorm (0.9 , mean = m , sd = s)
q10 <- qnorm( 1, mean = m , sd = s)


chi_c_w <- chisq.test(Klasy(probka_sz_c_w), p = prob )$p.value

m <- mean(probka_sz_c_d)

s <- sd(probka_sz_c_d)


q1 <- qnorm (0.1 , mean = m, sd = s)
q2 <-qnorm (0.2 , mean = m , sd = s)
q3 <-qnorm (0.3 , mean = m , sd = s)
q4 <-qnorm (0.4 , mean = m , sd = s)
q5 <-qnorm (0.5 , mean = m , sd = s)
q6 <-qnorm (0.6 , mean = m , sd = s)
q7 <-qnorm (0.7 , mean = m , sd = s)
q8 <-qnorm (0.8 , mean = m , sd = s)
q9 <-qnorm (0.9 , mean = m , sd = s)
q10 <- qnorm( 1, mean = m , sd = s)


chi_c_d <- chisq.test(Klasy(probka_sz_c_d), p = prob )$p.value

Chi_kwadrat <- c (chi_s_w, chi_s_d,chi_c_w ,chi_c_d)

Testy_normalnosc <- round (data.frame(Shapiro.Wilk,Kolmogorowa,Chi_kwadrat) , 5)

# Testy zgodności z rozkładem t-studenta

# na poczatek przemnazam przez 100 stopy zwrotu 

szew100 <- 100 * probka_sz_e_w
szed100 <- 100 * probka_sz_e_d
szcw100 <- 100 * probka_sz_c_w
szcd100 <- 100 * probka_sz_c_d

# Estymacja stopni swobody 

# Korzystam ze wzoru wariancja = stopnie swobody/ (stopnie swobody - 2 )
 
df <- function (x) { 
  
wariancja <- sd(x) ^2

round ( 2*wariancja/ (wariancja-1) , 0 )
}
df(szew100)

s <- 3

Kolmogorowa_1 <- c ( ks.test(szew100, "pt", df = df(szew100))$p.value,
                   ks.test(szed100, "pt" , df = df(szed100))$p.value,
                   ks.test(szcw100, "pt" , df = df(szcw100))$p.value,
                   ks.test(szcd100, "pt" , df = df(szcd100))$p.value )

# Test chi- kwadrat

s <- df(szew100)

q1 <- qt (0.1 , df = s )
q2 <-qt (0.2 , df = s)
q3 <-qt(0.3 , df = s)
q4 <-qt (0.4 , df = s)
q5 <-qt (0.5 , df = s)
q6 <-qt (0.6 , df = s)
q7 <-qt (0.7 , df = s)
q8 <-qt (0.8 , df = s)
q9 <-qt (0.9 , df = s)
q10 <- qt( 1, df = s)

chi_s_w <- chisq.test(Klasy(szew100), p = prob )$p.value

s <- df(szed100)

q1 <- qt (0.1 , df = s )
q2 <-qt (0.2 , df = s)
q3 <-qt(0.3 , df = s)
q4 <-qt (0.4 , df = s)
q5 <-qt (0.5 , df = s)
q6 <-qt (0.6 , df = s)
q7 <-qt (0.7 , df = s)
q8 <-qt (0.8 , df = s)
q9 <-qt (0.9 , df = s)
q10 <- qt( 1, df = s)

chi_s_d <- chisq.test(Klasy(szed100), p = prob )$p.value


s <- df(szcw100)

q1 <- qt (0.1 , df = s )
q2 <-qt (0.2 , df = s)
q3 <-qt(0.3 , df = s)
q4 <-qt (0.4 , df = s)
q5 <-qt (0.5 , df = s)
q6 <-qt (0.6 , df = s)
q7 <-qt (0.7 , df = s)
q8 <-qt (0.8 , df = s)
q9 <-qt (0.9 , df = s)
q10 <- qt( 1, df = s)

chi_c_w <- chisq.test(Klasy(szcw100), p = prob )$p.value


s <- df(szcd100)

q1 <- qt (0.1 , df = s )
q2 <-qt (0.2 , df = s)
q3 <-qt(0.3 , df = s)
q4 <-qt (0.4 , df = s)
q5 <-qt (0.5 , df = s)
q6 <-qt (0.6 , df = s)
q7 <-qt (0.7 , df = s)
q8 <-qt (0.8 , df = s)
q9 <-qt (0.9 , df = s)
q10 <- qt( 1, df = s)

chi_c_d <- chisq.test(Klasy(szcd100), p = prob )$p.value

Chi_kwadrat <- c (chi_s_w, chi_s_d,chi_c_w ,chi_c_d)

Testy_student <- round (data.frame(Kolmogorowa_1,Chi_kwadrat) , 5)

Testy_student
