library(rio)
library(tidyverse)

# Hochladen der Arbeitsdatei
a <- allbus2018_sample800 <- import("Daten/a18_s800.rds"); a
glimpse(a)
table(a$ew)
levels(a$ew) <- c("West","Ost")
table(a$ew)
export(a, "a18_s800_neu.rds")
dir()
