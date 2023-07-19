library(rio)
library(tidyverse)

a18 <- import("E:/HUB R/R HUB/allbus2018.rds")
str(a18)
glimpse(a18)

df <- a18_auswahl <- data.frame(respid = a18$respid, ew = a18$eastwest,
      dst = a18$german, partei = a18$pv01, sex = a18$sex, alter = a18$age,
      alter.K = a18$agec, ausb = a18$educ, eink = a18$inc, eink.K = a18$incc,
      work = a18$work, gsnd = a18$hs01, famst = a18$mstat, ausb.F = a18$feduc,
      ausb.M = a18$meduc, hheink = a18$hhinc, hheink.K = a18$hhincc, konf = a18$rd01,
      lzuf = a18$ls01)

head(df)
str(df)
glimpse(df)

# ew: Erhebungsgebiet

attributes(df$ew)
df$ew <- factor(df$ew, levels = 1:2, labels = c("Ost","West"))
table(df$ew)

# dst: Deutsche Staatsangehoerigkeit

df$dst
str(df$dst)
attributes(df$dst)

df$dst[which(df$dst < 1)] <- NA
df$dst[which(df$dst == 2)] <- 1
df$dst[which(df$dst == 3)] <- 2

df$dst <- factor(df$dst, levels = 1:2, labels = c("Ja", "Nein"))
table(df$dst)
summary(df$dst)
sum(summary(df$dst))

# partei

df$partei
str(df$partei)
table(df$partei)
attributes(df$partei)

df$partei[which(df$partei < 1)] <- NA
df$partei[which(df$partei == 42)] <- 5 # AFD
df$partei[which(df$partei == 90)] <- 7 # ANDERE PARTEI
df$partei[which(df$partei == 91)] <- 8 # WUERDE NICHT WAEHLEN

df$partei <- factor(df$partei, levels = 1:8, labels = c("CDU-CSU", "SPD", "FDP", "Die Gruenen",
          "AFD", "Linke", "Andere Partei", "Wuerde nicht waehlen"))
table(df$partei)
summary(df$partei)
sum(summary(df$partei))

# sex: Geschlecht

df$sex
str(df$sex)
table(df$sex)
attributes(df$sex)

df$sex[which(df$sex < 1)] <- NA
df$sex <- factor(df$sex, levels = 1:2, labels = c("Mann", "Frau"))

table(df$sex)
summary(df$sex)
sum(summary(df$sex))

# alter

df$alter
str(df$alter)
df$alter[which(df$alter < 1)] <- NA
attributes(df$alter)$labels <- NULL

# alter.K: Alter kategorisiert

df$alter.K
str(df$alter.K)
table(df$alter.K)
attributes(df$alter.K)

df$alter.K[which(df$alter.K < 1)] <- NA
df$alter.K <- factor(df$alter.K, levels = 1:6, labels = c("18-29", "30-44", "45-59", "60-74",
                                                        "75-89", "89+"))
table(df$alter.K)
summary(df$alter.K)

sum(summary(df$alter.K))

# ausb: Allgemeiner Schulabschluss

df$ausb
str(df$ausb)
table(df$ausb)
attributes(df$ausb)

df$ausb[which(df$ausb < 1)] <- NA
df$ausb <- factor(df$ausb, levels = 1:7, labels = c("Ohne Abschluss",
          "Volks-, Hauptschule", "Mittlere Reife", "Fachhochschulereife",
          "Hohschulereife", "Anderer Abschluss", "Noch Schuler"))
table(df$ausb)
summary(df$ausb)
sum(summary(df$ausb))

# eink: Nettoeinkommen

df$eink
str(df$eink)
attributes(df$eink)

df$eink[which(df$eink < 0)] <- NA
attributes(df$eink)$labels <- NULL

# eink.K: Nettoeinkommen kategorisiert

df$eink.K
str(df$eink.K)
table(df$eink.K)
attributes(df$eink.K)

df$eink.K[which(df$eink.K < 1)] <- NA
df$eink.K <- factor(df$eink.K, levels = 1:22, labels = c("<200 EUR", "200-299 EUR",
             "300-399 EUR", "400-499 EUR", "500-624 EUR", "625-749 EUR", "750-874 EUR",
             "875-999 EUR", "1000-1124 EUR", "1125-1249 EUR", "1250-1374 EUR", 
             "1375-1499 EUR", "1500-1749 EUR", "1750-1999 EUR", "2000-2249 EUR",
             "2250-2499 EUR", "2500-2749 EUR", "2750-2999 EUR", "3000-3999 EUR",
             "4000-4999 EUR", "5000-7499 EUR", ">=7500 EUR"))
table(df$eink.K)
summary(df$eink.K)

sum(summary(df$eink.K))

# work: Berufstätigkeit

df$work
str(df$work)
table(df$work)
attributes(df$work)

df$work[which(df$work < 1)] <- NA
df$work[which(df$work == 2)] <- 1 # HAUPTBERUFL.HALBTAGS
df$work[which(df$work == 3)] <- 1 # NEBENHER BERUFSTAE.
df$work[which(df$work == 4)] <- 2 # NICHT ERWERBSTAETIG

df$work <- factor(df$work, levels = 1:2, labels = c("Erwerbstaetig", "Nicht Erwerbstaetig"))

table(df$work)
summary(df$work)

sum(summary(df$work))

# gesundheit: Gesundheitszustand

df$gsnd
str(df$gsnd)
table(df$gsnd)
attributes(df$gsnd)

df$gsnd[which(df$gsnd < 1)] <- NA

df$gsnd <- factor(df$gsnd, levels = 1:5, labels = c("sehr gut", "gut", "zufriedenstellend",
           "weniger gut", "schlecht"))

table(df$gsnd)
summary(df$gsnd)

sum(summary(df$gsnd))

# famst: Familienstand
df$famst
str(df$famst)
table(df$famst)
attributes(df$famst)

df$famst[which(df$famst < 1)] <- NA

df$famst <- factor(df$famst, levels = 1:7, labels = c("Verh.zusam.leb.", "Verh.getr.leb.",
            "Verwitwet", "Geschieden", "Ledig", "Lebensp.zusam.leb.", "Lebensp.getr.leb."))

table(df$famst)
summary(df$famst)

sum(summary(df$famst))

# Vater: Allgemeiner Schulabschluss
# ausb.F = a18$feduc
df$ausb.F
str(df$ausb.F)
table(df$ausb.F)
attributes(df$ausb.F)

df$ausb.F[which(df$ausb.F < 1)] <- NA
df$ausb.F <- factor(df$ausb.F, levels = 1:6, labels = c("Ohne Abschluss",
             "Volks-, Hauptschule", "Mittlere Reife", "Fachhochschulereife",
             "Hohschulereife", "Anderer Abschluss"))
table(df$ausb.F)
summary(df$ausb.F)
sum(summary(df$ausb.F))

# Mutter: Allgemeiner Schulabschluss
# ausb.F = a18$feduc
#ausb.M = a18$meduc
df$ausb.M
str(df$ausb.M)
table(df$ausb.M)
attributes(df$ausb.M)

df$ausb.M[which(df$ausb.M < 1)] <- NA
df$ausb.M <- factor(df$ausb.M, levels = 1:6, labels = c("Ohne Abschluss",
                                                        "Volks-, Hauptschule", "Mittlere Reife", "Fachhochschulereife",
                                                        "Hohschulereife", "Anderer Abschluss"))
table(df$ausb.M)
summary(df$ausb.M)
sum(summary(df$ausb.M))

# Haushaltseinkommen
# hheink = a18$hhinc

df$hheink
str(df$hheink)
sort(str(df$hheink))

df$hheink[which(df$hheink < 1)] <- NA
attributes(df$hheink)$labels <- NULL

# Haushaltseinkommen, Kat.
# hheink.K = a18$hhincc

df$hheink.K
str(df$hheink.K)
table(df$hheink.K)
attributes(df$hheink.K)

df$hheink.K[which(df$hheink.K < 1)] <- NA
df$hheink.K <- factor(df$hheink.K, levels = 1:22, labels = c("<200 EUR", "200-299 EUR",
               "300-399 EUR", "400-499 EUR", "500-624 EUR", "625-749 EUR", "750-874 EUR",
               "875-999 EUR", "1000-1124 EUR", "1125-1249 EUR", "1250-1374 EUR", 
               "1375-1499 EUR", "1500-1749 EUR", "1750-1999 EUR", "2000-2249 EUR",
               "2250-2499 EUR", "2500-2749 EUR", "2750-2999 EUR", "3000-3999 EUR",
               "4000-4999 EUR", "5000-7499 EUR", ">=7500 EUR"))
table(df$hheink.K)
summary(df$hheink.K)

sum(summary(df$hheink.K))

# Konfession
# konf = a18$rd01

df$konf
str(df$konf)
table(df$konf)

df$konf[which(df$konf < 1)] <- NA
df$konf <- factor(df$konf, levels = 1:6, labels = c("Ev.ohne Freikirch", "Ev. Freikirch",
           "R.Katholisch", "And.Chr.Religion", "And.Nicht-Chr.Religion", "Keiner Religionsgem."))
table(df$konf)
summary(df$konf)
sum(summary(df$konf))

# Allgemeine Lebenszufriedenheit
# lzuf = a18$ls01

str(df$lzuf)
table(df$lzuf)
attributes(df$lzuf)

# 0 = GANZ UNZUFRIEDEN ... 10 = GANZ ZUFRIEDEN
df$lzuf[which(df$lzuf < 0)] <- NA
df$lzuf <- factor(df$lzuf, levels = 0:10, ordered =TRUE)

?factor
table(df$lzuf)
summary(df$lzuf)
sum(summary(df$lzuf))

# Überprüfung

glimpse(df)

# 1.Speicherung: vollständige Stichprobe

export(df, "a18_auswahl_full.rds")
dir() # Überprüfung

# Ein neuer Data Frame
set.seed(777)
ndf <- sample_n(df, size = 800, replace = FALSE)
glimpse(ndf)
summary(ndf)

# 2.Speicherung: Stichprobe mit 800 Beobachtungen
export(ndf, "a18_s800.rds")
dir() # Überprüfung