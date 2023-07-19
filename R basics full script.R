#######################################################
########### Einführung in R Gesamte Skripte ########### 
#######################################################

library(rio) # Import, Export and Convert Data Files 
library(tidyverse) # Funktion glimpse()
library(carData) # Dataset Davis
library(moments) # Funktionen skewness() und kurtosis(): Schiefe und Exzess
library(Hmisc) # Funktion describe()	
library(pastecs) # Funktion stat.desc()
library(psych) # Funktionen describe(), describeBy(), geometric.mean() 
library(QuantPsyc) # Schiefe und Exzess

## set wd 2023
setwd("")

# Einlesen von Dateien
d <- Davis
glimpse(d)
head(d)

sum(d$weight)
mean(d$weight) 
median(d$weight)
which.max(table(d$weight))
quantile(d$weight)
quantile(d$weight, 0.25)
sd(d$weight)
var(d$weight)
max(d$weight)
range(d$weight)
min(d$weight)
diff(range((d$weight)))
IQR(d$weight)
moments::skewness(d$weight)
moments::kurtosis(d$weight)


quantile(d$weight)
summary(d$weight)

round(stat.desc(d$weight),2)
psych::describe(d$weight)
Hmisc::describe(d$weight)	
?describe

# Hochladen der Arbeitsdatei
a <- allbus2018_sample800 <- import("a18_s800.rds"); a
head(a)
glimpse(a)

# Lagemaße

# Arithmetisches Mittel
mean(a$alter) # geht nicht! NA!
?mean # Information über mean() Funktion

mean(a$alter, na.rm = TRUE)
round(mean(a$alter, na.rm = TRUE), 2)

# Getrimmtes Mittel 
mean(a$alter, trim = 0.5, na.rm = TRUE)

# Median
median(a$alter, na.rm = TRUE)
?median

# Grafische Darstellung des Medians
boxplot(a$alter)
?boxplot
boxplot(a$alter, horizontal = TRUE)
abline(v = 51.3567, col = "red", lwd = 2)

# Modalwert für die Variable gsnd (Gesundheit)
table(a$gsnd)

?which.max
x <- c(1:4, 0:4, 4); x
which.min(x)
which.max(x)

which.max(table(a$gsnd)) # Gibt den Index des größten Werts eines Vektors zurück
max(table(a$gsnd)) # VORSICHTIG! Funktion max() geht hier nicht!
# Funktion max() zeigt die absolute Häufigkeit für die Kategorie mit der höchsten Häufigkeit

# Modalwert für die Variable alter
table(a$alter)
which.max(table(a$alter))
max(table(a$alter))



# Eigene Funktion MODE() erstellen

MODE <- function(x){
  a <- table(x) # numerischer Vektor x
  return(a[which.max(a)]) # Gibt den Modus der ersten 
  #häufigsten Zahl und deren Häufigkeit zurück
}

MODE(a$gsnd)
barplot(table(a$gsnd))

MODE(a$alter)
barplot(table(a$alter))

# Unimodale und Bimodale Verteilungen

?rnorm # Erzeugung von Zufallsvariablen 
# r + norm
# r = random = Zufallsvariable, norm = Normal Distribution
# n = Anzahl der Variablen, mean = mu, sd = sigma


set.seed(775) # Unimodale Verteilung

uni.v <- round(rnorm(n = 20, mean = 8, sd = 2))
barplot(table(uni.v), main ="Unimodale Verteilung")
uni.v

set.seed(777) # Bimodale Verteilung

# Perzentile und Quartile

# Quartile
?quantile
quantile(a$alter, na.rm = TRUE)

# Dezile
quantile(a$alter, na.rm = TRUE, p = seq(0, 1, 0.1))
# Das Argument p (Wahrscheinlichkeit) gibt einen Vektor von 0 bis 1
# mit einem Schritt von 0,1 an

# Perzentile
# p = 13%
quantile(a$alter, na.rm = TRUE, p = 0.13, names = FALSE)
# alle Perzentile
quantile(a$alter, na.rm = TRUE, p = seq(0, 1, 0.01))

# Geometrisches Mittel
exp(mean(log(c(1.4,0.6))))
geometric.mean(c(1.4,0.6))

# Streuungsmaße

# Fünf-Punkte-Zusammenfassung
quantile(a$alter, na.rm = TRUE)
summary(a$alter)
boxplot(a$alter, horizontal = TRUE)

# Minimum und Maximum
min(a$alter, na.rm = TRUE)
max(a$alter, na.rm = TRUE)

# Wertebereich bzw. Range
range(a$alter, na.rm = TRUE)

# Die Spannweite des Wertebereichs
diff(range(a$alter, na.rm = TRUE))

diff(range(quantile(a$alter, na.rm = TRUE))) # alternativ

# Interquartilsabstand 
IQR(a$alter, na.rm = TRUE)

a$alter # Erinnerung über die Variable alter

# Varianz und Standardabweichung
# Varianz
?var
var(a$alter, na.rm = TRUE)
round(var(a$alter, na.rm = TRUE),2)

# Standardabweichung
?sd
sd(a$alter, na.rm = TRUE)
round(sd(a$alter, na.rm = TRUE),2)

# Standardfehler des Mittelwert und Variationskoeffizient in R
?stat.desc
stat.desc(a$alter)
round(stat.desc(a$alter),2)

# Eigene Funktion SE() erstellen

# 1 Variante
SE <- function(x) {
  res <- sd(x)/sqrt(length(x))
  return(res)}
SE(a$alter) # geht leider nicht, wegen NAs
SE(d$weight) # das geht

# 2. (verbesserte) Variante

SE <- function(x, na.rm = FALSE) {
  if(na.rm == TRUE){ 
    x <- na.omit(x)
  }
  res <- sd(x)/sqrt(length(x))
  return(res)}

SE(a$alter)
SE(a$alter, na.rm = TRUE)

#Überprüfung
SE.U <- sd(a$alter, na.rm = TRUE)/sqrt(29)
SE.U

#Schiefe und Exzess
skewness(a$alter, na.rm = TRUE) # moments::skewness()
# [1] -0.0009483903
kurtosis(a$alter, na.rm = TRUE) # moments::kurtosis()
# [1] 2.089275

norm(a$alter) # QuantPsyc::norm()
# Skewness -0.000950175
# Kurtosis -0.908902811

round(norm(a$alter), 3)

# Histogramm
hist(a$alter)
# Kerndichte-Diagramm
plot(density(a$alter, na.rm = TRUE))
curve(dnorm(x, mean(a$alter, na.rm = TRUE), 
            sd(a$alter, na.rm = TRUE )), add = TRUE, col ="blue")

# Universelle Funktionen
# 1. Die Hmisc::describe() Funktion

?describe
Hmisc::describe(a$alter) # eine metrische Variable
Hmisc::describe(a$sex) # eine nominale Variable

# 2. Die psych::describe() Funktion
psych::describe(a$alter) # eine metrische Variable
psych::describe(data.frame(a$alter,a$eink)) # mehrere metrische Variablen

# 3. Die pastecs::stat.desc() Funktion 
?stat.desc
# stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
stat.desc(a$alter) # eine metrische Variable
# round(stat.desc(a$alter),2)
stat.desc(data.frame(a$alter,a$eink)) # mehrere metrische Variablen
# round(stat.desc(data.frame(a$alter,a$eink)),2)

stat.desc(a$alter, norm=TRUE) 
# round(stat.desc(a$alter, norm=TRUE),2)
export(a$alter, "alter.xlsx")
dir()
# 4. Die summary() Funktion

?summary
summary(a$alter) # eine metrische Variable

summary(a$sex) # eine nominale Variable

summary(a) # Kombination von Variablen verschiedener Typen

summary(data.frame(a$eink, a$eink.K)) # eine Auswahl der Variablen

summary(lm(eink ~ alter, data = a)) # ein lineares Modell

# Gruppenvergleiche
# Die describeBy() Funktion

?describeBy
describeBy(a$alter, a$sex)

describeBy(a$alter, a$sex, mat = TRUE)

# Die tapply() Funktion
?tapply

tapply(a$alter, a$sex, mean, na.rm = TRUE)

tapply(a$alter, a$sex, summary)

##################################################################
##################################################################

library(rio) # Funktion import()
library(tidyverse) # Funktion glimpse() und viel mehr

# rm(list = ls())

# Datenmanagement in R

# Einlesen von Dateien

setwd("")
a <- allbus2010 <- import("Daten/a18_s800.rds"); a
str(a)
head(a)

# as_tibble(): Data Frame in Tibble konvertieren 
a <- as_tibble(a); a
glimpse(a)

# Ein neuen Tibble erstellen
# tibble() = data_frame(): ein neuen Tibble (tbl_df) erstellen
# hkb = Häufigkeit von Kinobesuchen
hkb <- tibble(
  id = 1:5,
  geschlecht = c("w", "m", "m", "w", "w"),
  alter = c(32, 15, 60, 42, 17),
  hkb = c(5, 7, 1, 3, 15)); hkb

## Datenmanipulationen mit dyplr

# Die fünf Hauptfunktionen (+etwas dazu):
# 1. select(): Auswahl von Variable(n)
# 2. filter(): Auswahl der Fälle
# 3. arrange(): Fälle sortieren
# 4. mutate(): Erzeugung von neuen Variablen
# 5. summarise(): Zusammenfassung von Fällen

# 6. group_by(): Gruppierung nach Faktorstufen 

## Eigenschaften von Funktionen

# Man kann die Haupfunktionen verwenden:
# 1. als eigenständige Befehle (das erste Argument ist in diesem Fall ein Data Frame).
select(hkb, alter)

# 2.als Teil eines “Satzes” (innerhalb einer sogenannten Pipe).
hkb %>%
  select(alter)

#Eigenschaften von Funktionen:

#* Sie gelten nur für Data Frames, und das Ergebnis ist immer auch ein Data Frame.
#* Spalten lassen sich direkt mit Variablennamen abrufen (ohne $).
#* Es gibt keine Änderungen in Originaldaten, d.h. man soll die Ergebnisse
# immer in einem Objekt speichern.

# Zuweisungen

ndf <- select(hkb, alter); ndf

# oder
ndf <- hkb %>%
  select(alter) %>% 
  print(ndf)

# oder
hkb %>% 
  select(alter) -> ndf; ndf

## Einige Beispiele

# 1. Funktion select(): Auswahl von Variablen
# Syntax: select(.data, ...)
?select

# um das Ergebnis zu überprüfen
head(a, 3) 
print(a, n = 3) # ist besser

select(a, sex) # Auswahl aus dem Data Frame (DF) a der Variable sex
select(a, sex:eink) # Auswahl aller Variablen von sex bis eink
select(a, c(2, 4, 7)) # Auswahl die Variablen mit den Nummern 2, 4 und 7
select(a, -alter.K, -eink.K) # Auswahl aller Variablen außer alter.K und eink.K

# 2. Funktion filter(): Auswahl der Fälle

?filter

filter(a, alter >= 50) # nur die Befragten mit dem Alter größer oder gleich 50 Jahre
filter(a, alter >= 18 & alter <= 30) # nur die Befragten aus dieser Altersgruppe
filter(a, sex =="Mann", alter > 30) # nur die männlichen Befragten mit dem Alter von 31 Jahre alt

# 3. Funktion arrange(): Umordnung von Reihen (Fälle sortieren)

?arrange

arrange(a, sex) # Nach Geschlecht in aufsteigender Reihenfolge sortieren
arrange(a, desc(alter)) # Nach Alter in absteigender Reihenfolge sortieren
arrange(a, sex, desc(alter)) # mehrstufige Sortierung

# 4. mutate(): Erzeugung von neuen Variablen
?mutate

# Zwischenschritte
a_jahr <- mutate(a, jahr = 2018 - alter) # erzeugt eine neue Spalte namens "jahr",
# die das Geburtsjahr bezeichnet
glimpse(a_jahr) # und setzt sie am Ende des Data Frames

a_jahr_1 <- select(a_jahr, alter, jahr); a_jahr_1 

# 5. summarise(): Zusammenfassung von Fällen
?summarise

# Zusammenfassungs-Funktionen:
# Lage: mean(), median()
# Streuung: sd(), IQR(), mad()
# Range: min(), max(), quantile()
# Position: first(), last(), nth()
# Anzahl: n(), n_distinct()
# Logical: any(), all()

# das arithmetische Mittel
summarise(a, mean(alter)) # geht leider nicht, NAs (fehlende Werte)!  
summarise(a, mean(alter, na.rm = TRUE)) # das geht
summarise(a, mean = mean(alter, na.rm = TRUE)) # das geht

# 6. Funktion group_by(): Gruppierung nach Faktorstufen 
?group_by()

# 1. Schritt
group_by(a, sex) 
# Gruppierung nach den Faktorstufen: "Mann", "Frau" für die Faktorvariable sex)

# A tibble: 800 x 19
# Groups:   sex [2] ! (Einige Änderungen im Output)

# Aber das ist nicht genug! Man braucht noch eine gruppierende Variable.

# 2. Schritt

# Das Mittelalter den Befragten getrennt nach Geschlecht
summarise(group_by(a, sex), 
          mean(alter, na.rm = TRUE)) 
# => Verschachtelte Funktion

# Pipes ("Pipe"-Operator "%>%")

# Wenn man gleichzeitig auswählen und filtern möchten, gibt es drei Möglichkeiten:
# 1. Zwischenschritte verwenden. 
# 2. Verschachtelte Funktionen.
# 3. Pipes.

# Variante  Nr.1 (ein traditioneller Weg => Zwischenschritte verwenden)
a_jahr <- mutate(a, jahr = 2018 - alter) # erzeuge eine neue Spalte namens "jahr",
# die das Geburtsjahr berechnet
glimpse(a_jahr) # und setzt sie am Ende des Data Frames

a_jahr_2 <- arrange(a_jahr, jahr); a_jahr_2 
glimpse(a_jahr_2)

# Mit Zwischenschritten erstellt man einen temporären Daten Frame und verwenden
# diesen als Eingabe für die nächste Funktion.

# Variante  Nr.2 (ein traditioneller Weg => verschachtelte Funktion)
print(select(mutate(a, jahr = 2018 - alter), alter, jahr), n = 3)

# Variante  Nr.3 (mit dem "Pipe"-Operator: %>%)
# %>% = Ctrg + Schift + M

a %>% # nehmen Sie den Data Frame "a"
  group_by(sex) %>% # teilen Sie diesen DF in (zwei) Gruppen nach Faktor-Variable "sex" 
  summarise(mean(alter, na.rm = TRUE)) # berechnen Sie das arithmetische Mittel für die numerische Variable "alter"

# traditionelle Schreibweise (Variante 1 und 2):
# function(DataFrame, Spalte)

# "Pipe"-Operator-Schreibweise (Variante 3):
# DataFrame %>% function(Spalte)

# Die Bedeutung: Nehmen Sie aus diesem Dataframe diese Spalte
# und wenden Sie diese Funktion darauf an.

# x %>% f() ist äquivalent zu f(x)

# x %>% f() %>% g() %>% h() ist äquivalent zu h(g(f(x)))

# Verschachtelte Funktion
1:20
(1:20)^3
cos((1:20)^3)
abs(cos((1:20)^3))
sqrt(abs(cos((1:20)^2)))

# Verschachtelte Funktion mit einem zusätzlichen Parameter
sort(sqrt(abs(cos((1:20)^2))), decreasing = FALSE)

# %>% = Ctrg + Schift + M

(1:20)^3 %>% 
  cos() %>% 
  abs() %>% 
  sqrt() %>% 
  sort()

# "Pipe"-Operator mit einem zusätzlichen Parameter
(1:20)^2 %>% 
  sin() %>% 
  abs() %>% 
  sqrt() %>% 
  sort(x = ., decreasing = TRUE)


# Beschreibung von Arbeitsdatei 
glimpse(a)

# Auswahl von Variablen (Wiederholung)
a$sex
head(a$sex, 10)
a$sex[1:10]
print(a$sex, n = 10) # geht leider nicht
print(a, n = 10) # das geht

# Einige Beispiele
# 1. Funktion select(): Auswahl von Variable(n)
# ! Das Ergebnis ist immer ein Tibble

a %>%         # Auswahl aus dem Dataframe "a"
  select(sex) # der Variable "sex"

a %>% 
  select(sex) %>% 
  print(n = 5)

a %>% 
  select(sex, alter, ausb, eink) %>% 
  print(n = 5) -> a1

a1 %>% 
  select(-sex) %>% 
  print(n = 5)

a %>%              # Auswahl aus dem DF a
  select(sex:eink) # aller Variablen von sex bis eink

a %>%                      # Auswahl aus dem DF a
  select(sex:eink, partei) # aller Variablen von sex bis eink + partei

a %>%             # Auswahl aus dem DF a
  select(c(3, 5)) # die Variablen mit den Nummern 3 und 5

a %>%             # Auswahl aus dem DF a
  select(3:5) # die Variablen mit den Nummern von 3 bis 5

a %>%               # Auswahl aus dem DF a
  select(c(2:4, 7)) # die Variablen mit den Nummern 2, 3, 4 und 7

a %>%                       # Auswahl aus dem DF a
  select(-alter.K, -eink.K) # aller Variablen außer alter.K und eink.K

a %>%                 # Auswahl aus dem Dataframe a
  select(-c(3, 5, 7)) # aller Variablen außer 3., 5. und 7.

a %>%                       # Auswahl aus dem DF a
  select(-(alter.K:eink.K)) # aller Variablen außer dem Interval von alter.K bis eink.K

a %>%                     
  select(-alter.K:eink.K) # das geht leider nicht! warum?

#----------------------------
# Zusätzliche Informationen
# starts_with()
# ends_with()
# contains()

a %>% 
  select(starts_with("a")) %>% 
  print(n = 5)

a %>% 
  select(ends_with("k")) %>% 
  print(n = 5)

a %>% 
  select(contains("k")) %>% 
  print(n = 5)

#-----------------------------

# Umbenennen von Spalten (Variablen)
# 1. Funktion select()

a %>% 
  select(geschl = sex,
         erhgbt = ew) %>% 
  print(n = 5)
# Nachteil: Es werden nur zwei geänderte Variablen angezeigt,
# es ist beser Funktion rename() benutzen

# Umbenennen von Spalten mit Erhaltung von Data Frame
# Funktion rename()

a %>% 
  rename(geschl = sex,
         erhgbt = ew) %>% 
  print(n = 5)

# Funktion distinct(): Auswahl von eindeutigen Zeilen
?distinct

a %>% 
  distinct(ausb) %>% # alle eindeutige Zeilen (Faktorstufen) aus die ausgewählten Spalte
  print(n = 5) -> a2
# head() hier geht nicht 

# 2. Funktion filter(): Auswahl der Fälle

a %>% 
  filter(sex == "Mann") %>% 
  print(n = 5) 

a %>% 
  filter(sex == "Mann", alter >= 30) %>% 
  print(n = 5) 

a %>% 
  filter(sex == "Mann", alter >= 30, partei == "Linke") %>% 
  print(n = 5) 

# Logische Operatoren
# ! logisches NICHT
# & logisches UND
# | logisches ODER

a %>% 
  filter(sex == "Mann" & alter >= 30) %>% 
  print(n = 5)

a %>% 
  filter(partei == "Linke" | partei == "SPD")

a %>% 
  filter(!partei == "AFD") 

a %>% 
  filter(sex == "Mann" & alter >= 30 | 
           sex == "Frau" & partei == "SPD") %>% 
  print(n = 20) 

a %>% 
  filter(between(alter, 40, 59)) %>% 
  print(n = 20) 

#----------------------------
# Zusätzliche Informationen

library(stringr)
#str_starts() # Beachten Sie! Die andere Funktionen als für select()!
#str_ends()
?str_starts

a %>% 
  filter(str_starts(ew, "O")) %>% 
  print(n = 5) 

a %>% 
  filter(str_ends(partei, "D")) %>% 
  print(n = 5) 

#----------------------------

?filter_if
?all_varsa
?any_vars  
?any_vars(is.na(.))

# filter() und select() kombinieren

a %>% 
  filter(sex == "Mann",
         alter < 40,
         partei == "Linke") %>% 
  select(sex, 
         alter, 
         ew) -> a3; a3 

# 3. Funktion arrange(): Umordnung von Reihen
?arrange

# einfache Sortierung in aufsteigender Reihenfolge
a %>% arrange(sex) # Nach Geschlecht in aufsteigender Reihenfolge

# einfache Sortierung in absteigender Reihenfolge, Funktion desc() ("descending")
a %>% arrange(desc(alter)) # Nach Alter in absteigender Reihenfolge

# mehrstufige Sortierung
a %>% 
  arrange(sex, alter) # Nach Geschlecht und Alter in aufsteigender Reihenfolge

a %>% 
  arrange(sex, alter) %>% 
  select(sex, alter, partei)

a %>%
  arrange(sex, alter, ausb) # Nach Geschlecht, Alter und Ausbildung

# mehrstufige Sortierung in entgegengesetzten Richtungen

a %>%
  arrange(sex, desc(alter)) # Nach Geschlecht in auf-, nach Alter in absteigender Reihenfolgen

a %>%
  arrange(sex, desc(alter)) %>%
  select(sex, alter)

# 4. Funktion mutate(): Erzeugung von neuen Variablen
?mutate

# Zwischenschritte
a_jahr <- mutate(a, jahr = 2018 - alter) # eine neue Spalte namens "jahr" erzeigen
a_jahr_1 <- select(a_jahr, alter, jahr); a_jahr_1 

# Äquivalent mit dem "Pipe"-Operator
a %>% 
  mutate(jahr = 2018 - alter) %>% 
  select(alter, jahr) %>% 
  print(n = 3)

# Alter Verktor überschreiben:

?women

plot(women, xlab = "Height (in)", ylab = "Weight (lb)",
     main = "women data: American women aged 30-39")

w <- as_tibble(women)
print(w, n = 3)

w %>% 
  mutate(height = 2.54 * height, 
         weight = weight / 2.205) %>% 
  round() -> wn

print(wn, n = 3)

# Alter Verktor überschreiben, neue Variablennamen definieren:

w %>% 
  mutate(height_cm = 2.54 * height, 
         weight_kg = weight / 2.205) %>% 
  round() %>% 
  select(height, height_cm, weight, weight_kg) -> wn2
print(wn2, n = 3)


plot(wn, xlab = "Height (cm)", ylab = "Weight (kg)",
     main = "women data: American women aged 30-39")

# Beispiel: Normalgewicht nach Broca und BMI berechnen

# Normalgewicht (in kg) = Körpergröße (in cm) – 100 
# Normalgewicht = 170 - 100 = 70 (kg)

# BMI = kg-Gewicht / (m-Größe x m-Größe)
# BMI = 70 / (1,70 x 1,70) =  70 / 2,89 = 24,2

#| BMI-Index | Kathegorie | Beschreibung                |
#|-----------|------------|-----------------------------|
#| unter 18.5|    1       | Untergewicht                | 
#| 18.5 – 25 |    2       | Normalgewicht               | 
#| 25 – 30   |    3       | Übergewicht                 |
#| über 30   |    4       | Adipositas (Fettleibigkeit) |   

# Datendatei laden   
g <- gewicht <- import("gewichtR.rds"); g
glimpse(g)

# Beschreibung der Variablen
# Observations: 50
# Variables: 5
# $ nr   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,...
# $ sex  <fct> weiblich, männlich, weiblich, ...
# $ gr   <dbl> 168, 155, 177, 180, 148, 173, ...
# $ gew0 <dbl> 59.4, 67.6, 68.5, 71.2, 75.1, ...
# $ gew1 <dbl> 60.4, 63.8, 70.2, 73.2, 69.2, ...

# Normalgewicht nach Broca berechnen

# Normalgewicht (in kg) = Körpergröße (in cm) – 100 
g %>% 
  mutate(normal = gr - 100) %>%
  select(nr:gew1, normal) -> g
print(g, n = 5)
# Idealgewicht nach Broca:
# Bei Frauen:
# Idealgewicht (in kg) = Normalgewicht – 15 Prozent
# Bei Männern:
# Idealgewicht (in kg) = Normalgewicht – 10 Prozent 

# https://www.figurbetont.com/idealgewicht-berechnen-broca-formel-und-bmi-im-vergleich/

# Funktion ifelse()
g %>% 
  mutate(ideal = ifelse(sex == "männlich", 
                        normal - 0.15 * normal, 
                        normal - 0.1 * normal)) %>%
  select(nr:gew1, normal, ideal) -> g
print(g, n = 5)
# BMI berechnen
g %>% 
  mutate(bmi = gew0/((gr/100)^2)) %>%
  select(nr:gew1, bmi) -> bmi_0

#----------------------------
# Zusätzliche Information

library(units)
g %>% 
  mutate(
    gr = units::as_units(gr, "cm"),
    bmi = gew0/((gr/100)^2),
    gew0 = units::as_units(gew0, "kg"),
    gew1 = units::as_units(gew1, "kg")) %>%
  select(nr:gew1, bmi) -> bmi_1 
#%>%  mutate(bmi=round(bmi)) -> bmi_2
#----------------------------

# Funktion ifelse()

bmi_0 %>%
  select(nr:bmi) %>% 
  mutate(bmi.K = ifelse(bmi < 18.5, 1, 
                        ifelse(bmi < 25, 2, 
                               ifelse(bmi < 30, 3, 4)))
  ) %>% print(n = 5)

?case_when

# 1. Variante 
bmi_0 %>%
  select(nr:bmi) %>% 
  mutate(
    bmi.K = case_when(
      bmi < 18 ~ 1,
      bmi >= 18.5 & bmi < 25 ~ 2,
      bmi >= 25 & bmi < 30 ~ 3,
      bmi >= 30 ~ 4)) %>% print(n = 5)

# 2. Variante 
bmi_0 %>%
  select(nr:bmi) %>% 
  mutate(
    bmi.K = case_when(
      bmi < 18 ~ "Untergewicht",
      bmi >= 18.5 & bmi < 25 ~ "Normalgewicht",
      bmi >= 25 & bmi < 30 ~ "Übergewicht",
      TRUE ~ "Adipositas")) %>% print(n = 5)


# 5. summarise(): Zusammenfassung von Fällen
?summarise

# Zusammenfassungs-Funktionen
# Lage: mean(), median()
# Streuung: sd(), IQR(), mad()
# Range: min(), max(), quantile()
# Position: first(), last(), nth(),
# Anzahl: n(), n_distinct()
# Logical: any(), all()

# das arithmetische Mittel
a %>% 
  summarise(mean(alter)) # geht leider nicht, NAs (fehlende Werte)!  

a %>% 
  summarise(mean(alter, na.rm = TRUE)) # das geht

a %>% 
  summarise(mean = mean(alter, na.rm = TRUE))

a %>% 
  summarise(mean = mean(alter, na.rm = TRUE), 
            sd = sd(alter, na.rm = TRUE))

# Verwendendung der Funktion mutate() anstelle von summarize()
a %>% 
  mutate(mean = mean(alter, na.rm = TRUE), 
         sd = sd(alter, na.rm = TRUE)) %>% 
  glimpse() 

a %>% 
  summarise(
    n = n(),
    mean = mean(alter, na.rm = TRUE), 
    sd = sd(alter, na.rm = TRUE)) 

# 6. Funktion group_by(): Gruppierung nach Faktorstufen 
?group_by()

a %>% 
  group_by(sex) %>% 
  summarise(mean(alter, na.rm = TRUE)) # group_by(sex)

a %>% 
  group_by(sex) %>% 
  summarise(
    n = n(),
    mean = mean(alter, na.rm = TRUE), 
    sd = sd(alter, na.rm = TRUE))

# Verwendendung der Funktion mutate() anstelle von summarize()

a %>% 
  group_by(sex) %>% 
  mutate(mean = mean(alter, na.rm = TRUE), 
         sd = sd(alter, na.rm = TRUE)) %>% 
  glimpse() 

# zweistufige Gruppierung

a %>% 
  group_by(sex,
           ew) %>% 
  summarise(n = n(), 
            mean = mean(alter, na.rm = TRUE), 
            sd = sd(alter, na.rm = TRUE)) -> a4

# Gruppierung aufheben
a4 %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(n2 = n(),
         mean_2 = mean(mean))

#####################################################################
#####################################################################

library(rio) # R Input/Output
library(tidyverse)
library(lubridate) # Verschiedene Datums- und Zeitmanipulationen
vignette("lubridate") # Info über lubridate-Package

##----------------------------------------
# Warum dieses Thema?

# 1. Beispiel: Aktuelles Datum und aktuelle Uhrzeit in den Dateinamen einsetzen
paste0(paste(format(Sys.Date(),"%Y"), 
             format(Sys.Date(),"%m"), 
             format(Sys.Date(),"%d"), sep = "_" ), ".csv")

# 2. Beispiel: Apple Aktienkurse

library(quantmod)

# Apple Aktienkurse:
getSymbols(Symbols = "AAPL", from = "2020-01-01", to = "2020-04-16", src = "yahoo")
head(AAPL)
tail(AAPL)

# Einige Diagramme
# !Vorsichtig! Es wird jetzt besser Standadtparameter für Grafiken speichern!
opar <- par(no.readonly=TRUE) # Kopie der aktuellen Parameter

plot(AAPL)
autoplot(AAPL[, 1:4])
autoplot(AAPL[, 1:4], facets = NULL)
chartSeries(AAPL)

par(opar) # zurück zu Standadtparameter
##----------------------------------------

# Der Datumstyp in R

# Das aktuelle Datum und die aktuelle Uhrzeit abrufen
# Funktionen: Sys.Date(), Sys.time(), Sys.timezone()

?Sys.Date

d <- heute <- Sys.Date() # Aktuelles Datum (wird aktualisiert)
print(d)

t <- jetzt <- Sys.time() # Aktuelles Datum und Uhrzeit (wird aktualisiert)
print(t)
# Central European Summer Time (CEST)

lubridate::now() # Aktuelles Datum und Uhrzeit (wird aktualisiert)
# wie "jetzt"

zz <- Sys.timezone() # Zeitzone am aktuellen Standort
zz

# Typ und Class überprüfen
typeof(d) # double (numerisch)
d+5 # => die Berechnungen sind möglich
class(d) # "Date"

typeof(t) # double (numerisch)
t+5 # => die Berechnungen sind möglich
class(t) # "POSIXct" "POSIXt" 

typeof(zz) # "character"
class(zz) # "character"

str(d) # was ist drin?
glimpse(d) # alternativ

str(t)
glimpse(t)

# Ein Datum kann als Anzahl der Tage (Stunden) seit einem Ursprungsdatum
# angegeben werden
# Upsprungsdatum: "1970-01-01 00:00:00 UTC"
print(as.numeric(d)) # heute
print(as.numeric(t)) # jetzt

# Beispiel: Wie lange lebe ich schon?
mgbt <- as.Date("1970-06-24"); mgbt # mein Geburtsdatum
print(as.numeric(heute)-as.numeric(mgbt))
cat("Ich lebe auf der Erde" , as.numeric(heute)-as.numeric(mgbt), "Tage","\n")

# Datum aus Zeichenkette

# Funktion as.Date(), man muss jedoch das Format der Zeichenkette kennen
# Voreinstellung für das Datumsformat: yyyy-mm-dd
# zusätzliches Parameter: format = "%Y-%m-%d" # Jahr-Monat-Tag

?as.Date
Sys.Date() # zeigt immer ein 'richtiges' Format

# Standardformat: yyyy-mm-dd:
as.Date("2020-03-31")
as.Date("2020/03/31")

as.Date("31.03.2020")
#Fehler in charToDate(x) : 
#  Zeichenkette ist nicht in einem eindeutigen Standardformat

# Ein neues Datumsformat verwenden
as.Date("31.03.2020", format = "%d.%m.%Y") 

# format() als eine Funktion
# d <- as.Date(Sys.Date()) 
print(d) # Datumsformat ohne Wechsel
format(d,"%d.%m.%Y") # ein neues Datumsformat

# https://de.wikipedia.org/wiki/Datumsformat

# Datum aus Zeichenkette

# Funktion format()
format(Sys.Date())
as.character(Sys.Date()) # alternativ

?format
?strftime # vollständige Liste der Kürzeln für das Formatwechsel

# d <- as.Date(Sys.Date())
print(paste("Monat:",format(d,"%m")))
print(paste("3-stelliger Monatsname:",format(d,"%b")))
print(paste("Monatsname:",format(d,"%B")))
print(paste("Tag:",format(d,"%d")))
print(paste("Tag vom Jahr:",format(d,"%j")))
print(paste("2-stelliges Jahr:",format(d,"%y")))
print(paste("4-stelliges Jahr:",format(d,"%Y")))
print(paste("Jahrhundert:",format(d,"%C")))
print(paste("Kalenderwoche (DE):",format(d,"%W")))
print(paste("Datum (Systemeinstellung):",format(d,"%x")))
print(paste("Datum MM/TT/JJ:",format(d,"%D")))
print(paste("Wochentag (ab Montag):",format(d,"%u")))
print(paste("3-stelliger Wochentag:",format(d,"%a")))
print(paste("Wochentag:",format(d,"%A")))

# Berechnungen mit Daten

d1 <- as.Date("2020-04-01")
d2 <- mgbt <- as.Date("2020-06-24")
bis_mgbt <- d2 - d1; bis_mgbt
# Time difference of 84 days

str(bis_mgbt)
# 'difftime' num 84
# - attr(*, "units")= chr "days"
attributes(bis_mgbt)
#$class
#[1] "difftime"
#$units
#[1] "days"

?difftime
difftime(d2, d1, units = "weeks")
# Time difference of 12 weeks

difftime(d2, d1, units = "monats") # geht nicht
# Fehler in match.arg(units) : 
# 'arg' sollte eines  von '"auto", "secs", "mins", "hours", "days", "weeks"' sein

# Beispiel: Bestimmung des ersten Tages des Monats

# 1. Variante

# wie funktioniert as.Date()?
as.Date(24, origin = d1)
as.Date(24, origin = "2020-04-01")
as.Date(24, "2020-04-01")
as.Date(as.numeric(format(d2,"%d")), origin = d1)

?as.Date
## S3 method for class 'numeric'
# as.Date(x, origin, ...)

ntage <- as.numeric(format(d2,"%d")) - 1; ntage # 23
as.Date(-ntage, d2) # -23 + 24
# [1] "2020-06-01"

# Schritt nach Schritt
d2
# [1] "2020-06-24"
format(d2,"%d")
# [1] "24"
as.numeric(format(d2,"%d"))
# [1] 24
as.numeric(format(d2,"%d")) - 1
# [1] 23
ntage <- as.numeric(format(d2,"%d")) - 1; ntage
# [1] 23
as.Date(-ntage, d2)
# [1] "2020-06-01"

# 2. Variante (als eine Funktion)
etdm <- function(Datum) { # erster Tag des Monats
  return(as.Date(-as.numeric(format(Datum,"%d")) + 1, Datum))
}
etdm(d2)

# 3. Variante (Funktion cut())
as.POSIXlt(cut(d2, "month"))

# Datum mit dem Package lubridate

# Ein Datum erstellen
# Funktionen ymd(), mdy(), dmy(): y = Jahr, m = Monat, d = Day 
# Analysieren Daten mit Komponenten für Jahr, Monat und Tag
d0 <- ymd(20200401); d0 # wie eine numerische Variable: y = 2020, m = 04, d = 01
d0 <- ymd("20200401"); d0 # wie eine Zeichenkette: y = 2020, m = 04, d = 01

d1 <- mdy("4/1/20"); d1
d2 <- mdy("Apr 1 20"); d2
d3 <- dmy("01042020"); d3
?ymd
# Es gibt auch andere Funktionen: ydm(), myd(), dym()

# Man kann verschiedene Datatypen in einem Vektor kombinieren
x <- c("20-04-01", 20200402) 
ymd(x)

# und verschiedene Datumsformate auch
x <- c("20-04-01", "20 Apr 2", "2020/April/3")
ymd(x)

# Abruf der einzelnen Teile des Datums

# Funktionen year(), month(), day() usw.
# library(lubridate)

year("2020-06-24") # 2020
month("2020-06-24") # 6
day("2020-06-24") # 24
wday("2020-06-24") # 4

# Funktion year(): year(Datum)
?year
year(d)
year(d) <- 2019 # Modifikation
year(d) <- 2020
year(d) >= 2019 # eine Relationale Operation 
# [1] TRUE
year(ymd(x)) # eine vektorisierte Funktion
which(year(ymd(x)) >= 2019)


# Funktion month()
?month
# Syntax: month(x, label = FALSE, abbr = TRUE)
month(d)
month(d, label = TRUE)
month(d, label = TRUE, abbr = FALSE)

# Funktion day()
?day

day("2020-06-24")
mday("2020-06-24")
qday("2020-06-24")
yday("2020-06-24")

# Wochentag
# wday(x, label = FALSE, abbr = TRUE, week_start = getOption("lubridate.week.start", 7))
wday(d)
wday(d,label = TRUE)
wday(d,label = TRUE, abbr = FALSE)

# Option week_start
wday(d,label = TRUE, abbr = FALSE, week_start = 7) # ab Sonntag (Voreinstellung)
wday(d,label = TRUE, abbr = FALSE)
wday(d,label = TRUE, abbr = FALSE, week_start = 1) # ab Montag

# Datum Modifikationen
d1
year(d1) <- 2019
month(d1) <- 2
day(d1) <- 5
d1
#[1] "2019-02-05"
Datum <- c(year(d1), month(d1), day(d1)); Datum

# Umwandlung von Jahr, Monat und Tag im Datum

# 1. Möglichkeit (in zwei Schritten)
# Funktion base::ISOdate(): Umwandlung von Jahr, Monat und Tag im Datum
# Syntax: ISOdate(year, month, day)
# Das Ergebnis ist ein POSIXct-Objekt, das Sie in ein Date-Objekt
# konvertieren können: as.Date(ISOdate(year, month, day))

base::ISOdate(year = 2020, month = 4, day = 16)
ISOdate(2020, 4, 16)
class(ISOdate(2020, 4, 16))

as.Date(ISOdate(2020, 4, 16)) # Umwandlung im Date-Objekt
class(as.Date(ISOdate(2020, 4, 16)))

# Der Versuch, ein ungültiges Datum zu konvertieren, führt zu NA:
ISOdate(2018, 2, 29) # 2018 ist kein Schaltjahr

# 2. Möglichkeit
# Funktion lubridate::make_date()
# Syntax: make_date(year, month, day)

lubridate::make_date(year = 2020, month = 4, day = 16)
make_date(2020, 4, 16)
class(make_date(2020, 4, 16))

# Der Versuch, ein ungültiges Datum zu konvertieren, führt zu NA:
make_date(2018, 2, 29) # 2018 ist kein Schaltjahr

# Erstellen von Datumsfolgen
ISOdate(2019, 1:12, 1)
make_date(2019:2020, 1:6, 5:15)

sort(make_date(2019:2020, 1:6, 5:15))

# Noch eine Funktion: make_datetime()
make_datetime() # Ursprungsdatum

# Beispiel: Geburtstage

gb <- import("E:/HUB R/R HUB/geburtstag.sav")
glimpse(gb)
head(gb)

gb$tag
gb$monat
gb$jahr

ISOdate(gb$jahr, gb$monat, gb$tag)
as.Date(ISOdate(gb$jahr, gb$monat, gb$tag))

geb_datum <- make_date(gb$jahr, gb$monat, gb$tag); geb_datum

## Einige nützliche Funktionen

# Funktion floor_date(): Rundet auf die nächste Einheit ab
# Syntax: round_date(Datum, unit = "second", week_start = 7)
# unit = second, minute, hour, day, week, month, bimonth, 
# quarter, season, halfyear and year. 

?floor_date
# springt zum Anfang des Monats
floor_date(d, unit="month")

# Funktion ceiling_date(): Rundet auf die nächste Einheit auf
# Syntax: ceiling_date(Datum, unit = "second", week_start = 7,
# change_on_boundary = NULL,)

# springt zum Anfang des nächsten Monats
ceiling_date(d, unit="month")

# Funktion round_date(): Rundet auf die nächste Einheit
# Syntax: round_date(Datum, unit = "second", week_start = 7)

# Rundet auf Monate, also bis zum 15. zum Monatsanfang, 
# ab dem 16. zum nächsten Monatsanfang
round_date(as.Date("2020-04-14"), unit="month") # [1] "2020-04-01"
round_date(as.Date("2020-04-16"), unit="month") # [1] "2020-05-01"
round_date(dmy(14042020), unit = "month") # [1] "2020-04-01"
round_date(dmy(16042020), unit = "month") # [1] "2020-05-01"

# Erstellen von Datumsfolgen

?seq
# Funktion seq() 
# Syntax: seq(from, to, by, length.out) 
# Das Argument "by" kann in Tagen, Wochen, Monaten oder Jahren angegeben werden

start <- as.Date("2020-01-01")
end <- as.Date("2020-04-16")

# Tägliche Zeitintervalle
seq(from = start, to = end, by = 1) # blättern durch die Tage 
seq(from = start, by = 1, length.out = 7) # jeden Tag einer Woche 

# Monatliche Zeitintervalle
seq(from = start, by = "month", length.out = 12) # Erster Tag jedes Monats des Jahres

# Vierteljährliche Zeitintervalle
seq(from = start, by="3 months", length.out = 4) # Vierteljährliche Daten von einem Jahr 

# Jährliche Zeitintervalle
seq(from=start, by="year", length.out = 10) # Jahresanfänge für ein Jahrzehnt

# Seien Sie vorsichtig mit by = "month" in der Nähe des Monatsendes!
seq(as.Date("2018-01-29"), by = "month", len = 3)

## Uhzeit

# aktuelle Systemzeit 
t <- jetzt <- Sys.time(); t # POSIXct-Objekt
str(t) 
typeof(t)
class(t)

# Ausgabe der Minuten
as.numeric(format(t,"%M"))

# Funktion as.POSIXlt(): Umwandlung im POSIXlt-Objekt
t_lt <- as.POSIXlt(t); t_lt
str(t_lt)
typeof(t_lt)
class(t_lt)
attributes(t_lt)

# Ausgabe der Minuten
t_lt$min

# origin und tz (timezone) sind optional
# sie sind nur dann nötig, wenn nicht der Standardwert genommen werden soll
zz1 <- as.POSIXct("16042020205320", format="%d%m%Y%H%M%S", 
                  origin="1970-01-01", tz="CET")
zz1

zz2 <- as.POSIXct("16.04.2020 20:53:20", format="%d.%m.%Y %H:%M:%S")
zz2

# Zeichenkette aus Zeit (2.Teil)

?strftime # vollständige Liste der Kürzeln für das Formatwechsel

t <- Sys.time(); t

print(paste("Datum und Zeit im System-Format:",format(t,"%c")))
print(paste("Stunden (00-24):",format(t,"%H")))
print(paste("Stunden (01-12):",format(t,"%I")))
print(paste("Minuten:",format(t,"%M")))
print(paste("Sekunden:",format(t,"%S")))
print(paste("Zeit (Systemeinstellungen):",format(t,"%X")))
print(paste("Abweichung von GMT:",format(t,"%z")))
print(paste("Zeitzone:",format(t,"%Z")))

# Länderspezifische Version des Datums
# Beispiel für Deutschland
# "Mo, der 20. April 2020, 12:14:34 Uhr"
format(Sys.time(), "%a, der %d. %B %Y, %X %Z Uhr")

# Kombinieren Datum-Urzeit-Info zusammen (in Format 'm/d/y h:m:s')
dates <- c("05/23/19", "06/30/19", "07/14/19", "08/11/19", "09/01/19")
times <- c("16:30:10", "17:15:12", "14:02:32", "17:20:55", "15:03:15")
x <- paste(dates, times)
strptime(x, "%m/%d/%y %H:%M:%S")


## Berechnungen mit der Uhrzeit
# Funktion difftime()
?difftime
tdif1 <- Sys.time() - zz1; tdif1
str(tdif1)
#  'difftime' num 29.774017850558
#  - attr(*,"units")= chr "hours"
tdif2 <- difftime(Sys.time(), zz1, units = "days"); tdif2
#Time difference of 0.02198203 days

# Abruf der einzelnen Teile der Uhrzeit
# Funktionen ymd_hms(), ymd_hm(), ymd_h(), hour(), minute(), second() usw.
# library(lubridate)

?ymd_hms
?hour

z1 <- ymd_hms("2020-04-16 21:40:20"); z1
z2 <- ymd_hm("2020-04-16 21:40"); z2 # ohne Sekunden
z3 <- ymd_h("2020-04-16 21"); z3 # nur Stunden

year(z1) # 2020
hour(z1) # 21
minute(z1) # 40
second(z1) # 20

hour(z1) <- 22
minute(z1) <- 13
second(z1) <- 59
z1
#[1] "2020-04-16 22:13:59 UTC"

z2 <- ymd_hm("2020-04-16  21:40"); z2

OlsonNames()

# 
Sys.timezone() # Zeitzone am aktuellen Standort
class(Sys.timezone()) 

OlsonNames()
str(OlsonNames())

OlsonNames()[431]
# [1] "Europe/Berlin"
startsWith(OlsonNames(), "Eur")
OlsonNames()[startsWith(OlsonNames(), "Eur")]

# Manipulationen mit Zeitzonen

?with_tz
?force_tz

# Funktion with_tz(): gibt ein Datum und eine Uhrzeit zurück, 
# wie es in einer anderen Zeitzone erscheinen würde.
# Syntax: with_tz(time, tzone = "")

b_time <- now(tz = "Europe/Berlin"); b_time
# [1] "2020-04-20 15:46:39 CEST"

# Entsprechende "Europa/Riga"-Zeit
with_tz(b_time, tzone = "Europe/Riga")
# [1] "2020-04-20 16:46:39 EEST"

# Funktion force_tz(): gibt ein Datum und eine Uhrzeit zurück, 
# das dieselbe Uhrzeit wie x in der neuen Zeitzone hat.
# Syntax: force_tz(time, tzone = "", roll = FALSE)

b_time <- now("Europe/Berlin"); b_time
# [1] "2020-04-20 15:43:25 CEST"
force_tz(b_time, tzone = "Europe/Riga") # Es gibt nur Zeitzonenänderungen
# [1] "2020-04-20 15:43:25 EEST"


# Plotten zeitabhängiger Daten
x <- sort(make_date(2019, 1:6, 1:20))
x
set.seed(777)
y <- round(runif(20, 0, 15))
plot(y ~ x, type = "l", xlab="Jahr 2020", ylab="Temperatura C") # ein Liniendiagramm

####################################################################
####################################################################

library(rio)
library(psych) # Funktionen describeBy() 
library(graphics)
library(ggplot2) # Grafiken
library(datasauRus)

# Anscombe-Quartett und datasauRus

# 1. Anscombe-Quartett

a <- import("E:/HUB R/R HUB/anscombe3.txt")
head(a)
str(a)
a$dataset <- factor(a$dataset)

describeBy(a$x, a$dataset, mat = TRUE)
describeBy(a$x, a$dataset, mat = TRUE)[,c(4,5,6)]

?anscombe
head(anscombe)
str(anscombe)

# Beispiel aus Dokumentation

ggplot(a, aes(x = x, y = y, color = dataset)) + 
  geom_point() + 
  facet_wrap(~dataset, ncol = 2) +
  theme(legend.position = "none") 

# 2. datasauRus
?datasauRus

ggplot(datasaurus_dozen, aes(x = x, y = y,color = dataset)) + 
  geom_point() + 
  facet_wrap(~dataset, ncol = 3) +
  theme(legend.position = "none") +
  theme_void()


library(gifski)
library(gganimate)

ggplot(datasaurus_dozen, aes(x = x, y = y)) +
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 1)

dd <- datasaurus_dozen
head(dd)
describeBy(dd$x, dd$dataset, mat = TRUE)
describeBy(dd$x, dd$dataset, mat = TRUE)[,c(4,5,6)]

####################################################################
####################################################################

# 1. Funktion apply(): Wendet die Funktion (Statistik)
# zeilen- oden spaltenweise an.
# Syntax: apply(x, Margin, Funktion)
# x: eine Matrix
# Margin: 1 oder 2 um zu definieren, wo die Funktion angewendet werden soll:
# Margin = 1: zeilenweise
# Margin = 2: spaltenweise
# Funktion = eine statistische Funktion wie mean(), sum() ...

?apply
A <- matrix(rep(1:2, 5), nrow = 2); A

a1_A <- apply(A, 1, sum); a1_A
a2_A <- apply(A, 2, sum); a2_A

# 2. Funktion lapply(): Wendet die Funktion (Statistik)
# auf alle Variablen der Liste an
# Syntax: lapply(list(X,Y,...), Funktion): 

?lapply

sqrt(1:5)

la <- lapply(1:5, sqrt); la
str(la)

ula <- unlist(la); ula
str(ula)

# 3. Funktion sapply(): Erzeugt eine Tabelle von statistiken für X
# pro Werten für Y
# sapply(split(X,Y), Funktion) 

?sapply

sa <- sapply(1:5, sqrt); sa
str(sa)

B <- matrix(1:12, nrow = 2); B
C <- matrix(1:12, nrow = 3); C
D <- matrix(1:12, nrow = 4); D

meine.liste <- list(B,C,D); meine.liste
lapply(meine.liste, "[",1,) # erste Zeile aus jeder Matrix
lapply(meine.liste, "[",,1) # erste Spalte aus jder Matrix


# 4.Funktion tapply()
# tapply(X, Funktion): Erzeugt eine Tabelle von statistiken für X  
# tapply(X, Y, Funktion): Erzeugt eine Tabelle von statistiken für X pro Werten für Y

?tapply


####################################################################
####################################################################

# install.packages("ggplot2")
# install.packages("maps")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rio)
library(maps)
library(GGally) # Streudiagramm-Matrix ggpairs()
library(ggfortify) # Regressionsdiagnostik autoplot() 

# Warum es ist notwendig Daten zu visualisieren?
setwd("")

my.df <- import("Daten/ANSUR2.rds")
glimpse(my.df)
summary(my.df)

ggplot() # Schritt 1
ggplot(data = my.df, mapping = aes(x = height, y = weight)) # Schritt 2: Grundebene erstellen 
ggplot(my.df, aes(x = height, y = weight)) # Schritt 2 (alternativ)

ggplot(my.df, aes(x = height, y = weight)) + geom_point() # Schritt 3: neue Ebene hinzufügen
ggplot(my.df, aes(x = height, y = weight)) + geom_line() # Schritt 3

# Wichtige Bemerkungen
# + muss am Ende einer Zeile stehen und nicht am Anfang
# das ist korrekt:
plot1 <- ggplot(my.df, aes(x = height, y = weight)) + 
  geom_point()
plot1
# das funktioniert nicht!
plot2 <- ggplot(my.df, aes(x = height, y = weight))
+ geom_point()
plot2


# Eindimensionale Grafiken
# Diskrete Variablen
p1 <- ggplot(my.df, aes(x = race)) # Grundebene für die Variable race erstellen
p1 + geom_bar() # Ebene mit dem Balkendiagramm hinzufügen

# Zusätzliche Optionen:
# alpha = die Transparenz, liegt zwischen 0 und 1
p1 + geom_bar(alpha = 0.25)
p1 + geom_bar(alpha = 0.50)
p1 + geom_bar(alpha = 0.75)
# color = die Umrissfarbe
p1 + geom_bar(color = "red")
p1 + geom_bar(color = "red", size = 2) # machen ein bisschen dicker
# fill = die Säulen-/Balkenfarbe
p1 + geom_bar(fill = "blue")
# eine Kombination von zusätzlichen Optionen:
p1 + geom_bar(alpha = 0.5, fill = "blue", color = "red")

# Gestappeltes Diagramm
p2 <- ggplot(my.df, aes(x = race, fill = gender)) # Grundebene für die Variable race mit der Trennung nach gender erstellen
p2 + geom_bar() # Ebene mit dem Balkendiagramm hinzufügen

# Äquivalent
p2 + geom_bar(position = "stack")

# Gestappeltes Diagramm mit 100% Skala
p2 + geom_bar(position = "fill")

# Gruppiertes Diagramm
p2 + geom_bar(position = "dodge")

# Änderung der Farben
p1 + geom_bar(aes(fill = race))

# Änderung der Farbpalette
# für die fertigten Farbpaletten scale_fill_brewer()

?scale_fill_brewer()

p2 + geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2")

# noch einen Variant
p2 + geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1")

# Sie können seinen eigene Farbpalette definieren

?scale_fill_manual()

p2 + geom_bar(position = "dodge") +
  scale_fill_manual(values = c("pink","violetred"))

# Graustufen mit scale_fill_grey()

?scale_fill_grey()

p1 + geom_bar(aes(fill = race)) +
  scale_fill_grey(start = 0.0, end = 0.8, na.value = "red") 

# Eindimensionale Grafiken
# Stetige Variablen 

# Histogramm
p3 <- ggplot(my.df, aes(x = weight)) # Grundebene für die Variable weight erstellen 
p3 + geom_histogram() # Ebene mit dem Histogramm hinzufügen

p3 + geom_histogram(binwidth = 10, fill = "white", colour = "black")
p3 + geom_histogram(binwidth = 20, fill = "white", colour = "black")

p3 + geom_histogram(binwidth = 20, fill = "white", colour = "black", boundary = 40)
p3 + geom_histogram(binwidth = 10, fill = "white", color = "black", boundary = 40)

# Gruppiertes Histogramm

p4 <- ggplot(my.df, aes(x = weight, fill = gender)) # Grundebene für die Variable weight nach gender erstellen 
p4 + geom_histogram(binwidth = 10, boundary = 40, alpha = 0.5) # mit Transparenz
p4 + geom_histogram(binwidth = 10, boundary = 40) # ohne Transparenz

# Grafiken kombinieren
# Histograqmm plus Häufigkeitspolygon
p3 <- ggplot(my.df, aes(weight)) 

p3 + geom_histogram() +
  geom_freqpoly()

# Verschiedene Diagrammtypen für die stetigen Variablen
p3 + geom_histogram(binwidth = 5) # Histograqmm
p3 + geom_histogram(binwidth = 10) # Histograqmm
p3 + geom_density() # Kerndichte-Diagramm
p3 + geom_density(kernel = "gaussian") # Kerndichte-Diagramm
p3 + geom_area(stat = "bin") # Flächendiagramm
p3 + geom_dotplot() # Punktendiagramm
p3 + geom_freqpoly() # Häufigkeitspolygon

# Facetten
f <- p3 + geom_histogram(binwidth = 10, fill = "white", color = "black", boundary = 40)
f + facet_grid(gender ~ .) # In Zeilen aufteilen, basierend auf gender 
f + facet_grid(. ~ gender) # In Spalten aufteilen, basierend auf gender 
f + facet_grid(gender ~ race) # In Zeilen und Spalten aufteilen
f + facet_wrap(~ race) # Aufteilung in rechteckige Anordnung

# Kerndichte-Diagramm
p3 <- ggplot(my.df, aes(weight)) # Grundebene für die stetige Variable weight erstellen 
p3 + geom_density() # Ebene mit dem Kerndichte-Diagramm hinzufügen
# Äquivalent
p3 + geom_density(kernel = "gaussian")

# Kerndichte-Diagramm mit verschiedenen Optionen
p3 + geom_density(fill = "blue")
p3 + geom_density(fill = "blue", alpha = 0.2)
p3 + geom_density(colour = "red")
p3 + geom_density(colour = "red", size = 1.5)

# Gruppiertes Kerndichte-Diagramm
# 1. Hintergrundfarbunterschied
p4 <- ggplot(my.df, aes(x = weight, fill = gender))
p4 + geom_density(alpha = 0.3)

# 2. Unterschied in der Umrissfarbe
p5 <- ggplot(my.df, aes(x = weight, colour = gender))
p5 + geom_density()

# Grafiken kombinieren
# Histograqmm plus Kerndichte-Diagramm
p3 <- ggplot(my.df, aes(weight)) 
p3 + geom_histogram(aes(y = ..density..), binwidth = 10, boundary = 40, fill = "violet", color = "black") +
  geom_density(col = "darkblue")

# Zweidimensionale Grafiken 1. X = Diskret und Y = Stetig

# Haupttyp: Boxplot

# Gruppiertes Boxplot
p5 <- ggplot(my.df, aes(x = gender, y = weight)) # Grundebene für die Variablen weight und gender erstellen 
p5 + geom_boxplot()

# Boxplot nur für einzelne Variable
p6 <- ggplot(my.df, aes(y = weight)) # Grundebene für die Variable weight erstellen 
p6 + geom_boxplot()

# Violine-Plot
# Gruppiertes Violine-Plot
p5 + geom_violin()

# Grafiken kombinieren
# # Gruppiertes Boxplot plus Gruppiertes Violine-Plot
p5 + geom_violin() +
  geom_boxplot(width = 0.1) 

# Hinzufügen von Barthaare zum Boxplot
p5 + stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot()

# Kooordinaten vertauschen
p5 + geom_boxplot() +
  coord_flip()

# Zweidimensionale Grafiken: 2. X = Stetig und Y = Stetig 

# Haupttyp: Streudiagramm

# Grundebene für die Variablen height (stetig) und weight (stetig) erstellen 
p7 <- ggplot(my.df, aes(x = height, y = weight))
p7 + geom_point() # Streudiagramm

# Streudiagramme mit verschiedenen Optionen

# Einige zusätzliche Parameter für aes(): 
# A. Die abhängige Variable ist Diskret (gender)

ggplot(my.df, aes(y = height, x = weight, colour = gender)) + geom_point()
ggplot(my.df, aes(x = height, y = weight, shape = gender)) + geom_point()
ggplot(my.df, aes(x = height, y = weight, colour = gender, shape = gender)) + geom_point()

# B. Die abhängige Variable ist Stetig (age)

ggplot(my.df, aes(x = height, y = weight, colour = age)) + geom_point()
ggplot(my.df, aes(x = height, y = weight, size = age)) + geom_point()
ggplot(my.df, aes(x = height, y = weight, alpha = age)) + geom_point()

# Einige interresante Kombinationen von A und B
ggplot(my.df, aes(y = height, x = weight, colour = gender, size = age)) + geom_point()
ggplot(my.df, aes(y = height, x = weight, shape = gender, colour = age)) + geom_point()

# Hinzufügen der Regressiongrade
# p7 <- ggplot(my.df, aes(x = height, y = weight)) # Errinerung
p7 + geom_point() +
  geom_smooth(method = "lm", se = FALSE) # Lineare Regression, ohne Konfidenzintervall

p7 + geom_point() +
  geom_smooth(method = "lm") # Lineare Regression, mit Konfidenzintervall (se = TRUE)

# Regressiongraden getrennt nach Geschlecht
p7 + geom_point() +
  geom_smooth(aes(linetype = gender),method = "lm", se = FALSE) # Lineare Regression, ohne Konfidenzintervall

p7 + geom_point() +
  geom_smooth(aes(linetype = gender),method = "lm") # Lineare Regression, mit Konfidenzintervall

# Glättung
p7 + geom_point() +
  geom_smooth() # Glättungskurve hinzufügen

# Äquivalent:
p7 + geom_point() +
  geom_smooth(method = "loess", se = TRUE)

# Grafiken kombinieren
# Regressiongrade und Glättungskurve

# ohne Konfidenzintervallen
p7 + geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour ="red") +
  geom_smooth(method = "loess", se = FALSE)

# mit Konfidenzintervallen
p7 + geom_point() +
  geom_smooth(aes(linetype = gender), method = "lm", se = FALSE, colour ="red") +
  geom_smooth(aes(linetype = gender), method = "loess", se = TRUE)

# mit unterschiedlichen Farben getrennt nach gender
p8 <- ggplot(my.df, aes(y = height, x = weight, colour = gender))
p8 + geom_point() +
  geom_smooth(aes(linetype = gender), method = "lm", se = FALSE, colour = "blue") +
  geom_smooth(aes(linetype = gender), method = "loess", se = TRUE)

# Zweidimensionale Grafiken: 3. Stetige Funktion
# Liniendiagramme

p9 <- ggplot(my.df, aes(x = age, y = weight)) 
p9 + geom_line()

# Linien und Punkte können kombiniert werden
p9 + geom_line() + 
  geom_point()

# Farben und Typen von Punkten und Linien können geändert werden
?geom_point
?geom_line

p9 + geom_line(color = "blue") +
  geom_point(size = 0.5)

p9 + geom_line(color = "red", linetype = 2) +
  geom_point(size = 2, shape = 17)

# Liniendiagramme getrennt nach gender 
p10 = ggplot(my.df, aes(x = age, y = weight, color = gender))
p10 + geom_line()

# Andere Linien
# Vertikale und Horizontale

# Grundebene und ein Histogramm erstellen
p11 <- ggplot(my.df, aes(x = age)) + 
  geom_histogram(binwidth = 5, boundary = 15, fill = "yellowgreen", colour = "black")

# Horizontale
p11 + geom_hline(yintercept = 15, colour = "red")

# Vertikale  
p11 + geom_vline(xintercept = median(my.df$age), colour = "red", lty = 2)

# Median berechenen
median(my.df$age)

# Änderung der Achsen-, Legenden- und Diagrammbeschriftungen
# p7 <- ggplot(my.df, aes(x = height, y = weight)) # Errinerung
plot <- p7 + geom_point() # die Kerngrafik
plot 

# die Kopftitel und Untertitel
?labs

plot + labs(title ="Meine Grafik", subtitle = "Untertitel")

# Achsenlabels

plot + labs(x = "Größe", y = "Gewicht")

# alles zusammen
plot + labs(title ="Meine Grafik", subtitle = "Untertitel", x = "Größe", y = "Gewicht")

# Keine Achsenlabels

plot +  labs(x = "", y = "")

# Achsen anpassen
# Achsenlimits

plot + 
  xlim(min = 0, max = 2) + # Achsenlimit für x
  ylim(0, 130) # Achsenlimit für y: min = 0, max = 130

# alternativ  scale_*_continuous() Funktion
?scale_x_continuous  

plot +   
  scale_x_continuous(name = "Größe", limits = c(0, 2), breaks = seq(0, 2, 0.2)) +
  scale_y_continuous(name = "Gewicht", limits = c(0, 130), breaks = seq(0, 130, 10))

#Keine Achsenskala
plot + 
  scale_x_continuous(limits = c(0, 2), breaks = NULL)

# Legende
# p8 <- ggplot(my.df, aes(y = height, x = weight, colour = gender)) # Errinerung

plot1 <- p8 + geom_point()
plot1 +
  labs(colour = "Meine Legende") 

# p2 <- ggplot(my.df, aes(x = race, fill = gender)) # Errinerung
plot2 <- p2 + geom_bar(position = "dodge")
plot2 + 
  labs(fill = "Meine Legende")

# Weltkarte
ggplot(map_data("world"), aes(x = long, y = lat, group = group)) +
  geom_polygon()

# Streudiagramm-Matrix
# Funktion: GGally::ggrairs(DF)
ggpairs(data.frame(my.df$age, my.df$height, my.df$weight))

# Regressionsdiagnostik
# Funktion: ggfortify::autoplot(LM)
autoplot(lm(weight ~ age, my.df))


####################################################################
####################################################################
## remove all packages
# create a list of all installed packages
# ip <- as.data.frame(installed.packages())
# head(ip)
# # if you use MRO, make sure that no packages in this library will be removed
# ip <- subset(ip, !grepl("MRO", ip$LibPath))
# # we don't want to remove base or recommended packages either\
# ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# # determine the library where the packages are installed
# path.lib <- unique(ip$LibPath)
# # create a vector with all the names of the packages you want to remove
# pkgs.to.remove <- ip[,1]
# head(pkgs.to.remove)
# # remove the packages
# sapply(pkgs.to.remove, remove.packages, lib = path.lib)
