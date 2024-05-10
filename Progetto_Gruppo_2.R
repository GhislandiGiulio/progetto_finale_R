library(readxl)
library(writexl)
library(dplyr)

setwd(getwd())

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

### Classificazione manuale del training set

# reimportazione del dataset dopo valutazione manuale
training_set = read_excel("commenti_addestramento_fr_training.xlsx")

### Creazione del test set
test_set <- anti_join(DatiFR, training_set, 
                      by = c("text", "date_created_at", "score_rating", "Players"))