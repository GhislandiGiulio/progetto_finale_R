library(readxl)
library(dplyr)

setwd("INSERISCI PATH SCRIPT QUI")

DatiX <- read_excel("dataset_french_bakery.xlsx")

head(DatiX)
names(DatiX)
str(DatiX)
summary(DatiX)

head(is.na(DatiX), 7)

apply(DatiX, 2, function(dataset) sum(is.na(dataset)))

unique(DatiX$social)
unique(DatiX$lang_value)

DatiInutili <- sum(is.na(DatiX$text) & is.na(DatiX$score_rating))
head(DatiInutili)

sum(DatiX$lang_value == 'fr')

DatiBaguette <- filter(DatiX, lang_value == 'fr')
sum(is.na(DatiBaguette$text))
sum(is.na(DatiBaguette$score_rating))

cat('Togliamo due colonne perché inutili e una perché vuota(riscrivere)')

DatiFR <- select(DatiBaguette, -social, -likes, -lang_value)

head(DatiFR)

unique(DatiFR$Players)

DatiFR$Players <- as.factor(DatiFR$Players)
