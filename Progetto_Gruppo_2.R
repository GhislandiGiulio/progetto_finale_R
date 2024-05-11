library(readxl)
library(writexl)
library(dplyr)

# impostazione della cartella di lavoro
setwd(getwd())

# impostazione del seed per replicare i risultati
set.seed(1213)

# importazione del dataset
df_bakeries <- read_excel("dataset_bakery.xlsx")

# seleziono solo le righe delle recensioni in lingua francese (fr)
df_french_bakeries <- filter(df_bakeries, lang_value == "fr")

# seleziono solo le colonne che mi interessano
df_french_bakeries <- select(df_french_bakeries, -social, -likes, -lang_value)

# rimozione righe con dei valori mancanti (NA)
clean_df_french_bakeries <- na.omit(df_french_bakeries)

# stampa numero di righe rimanenti dopo filtri
print("numero di righe rimanenti dopo pulizia: ")
print(nrow(clean_df_french_bakeries))

# aggiunta della colonna "sentiment"
clean_df_french_bakeries$sentiment <- NA

### estrazione di un sample da valutare
training_sample <- clean_df_french_bakeries[sample(nrow(clean_df_french_bakeries), 200), ]

# creazione dell'excel per riempire valori
write_xlsx(training_sample, path = "df_before_sentiment.xlsx")

# reimportazione del dataset dopo valutazione manuale
training_set <- read_excel("df_after_sentiment.xlsx")

### Creazione del test set
test_set <- anti_join(clean_df_french_bakeries, training_set)

# mostriamo i risultati
library(kableExtra)

print(kbl(clean_df_french_bakeries[1:5, ], longtable = T, booktabs = T, 
    caption = "Il dataset") %>%
    kable_styling(c("bordered", "condensed", "hover"), 
                  full_width = F, font_size = 11) %>%
    row_spec(0, color = "black", bold = T, background = "#b8daba") %>%
    column_spec(2, width = "20em", width_min="2in", color = "black" ))


    #### SEZIONE DI PRE-PROCESSING

### Librerie per pre-processing
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)


### Creazione corpus e DFM 

## Training set 
# Corpus
corpus_training_set <- corpus(training_set)

# dfm
# creazione dei token
tokens <- tokens(corpus_training_set, 
                 remove_punct = TRUE, 
                 remove_symbols = TRUE,
                 remove_numbers = TRUE) %>% tokens_tolower()

tokens <- tokens_select(tokens, pattern = stopwords("french"), selection = "remove")

# creazione dfm dai token
dfm_training_set <- dfm(tokens)

# conversione dfm in dataFrame per stampa con KBL
dfm_df <- quanteda::convert(dfm_training_set, to = "data.frame")


## Test set 
# Corpus
corpus_test_set <- corpus(test_set)

# dfm
# creazione dei token
tokens <- tokens(corpus_test_set, 
                 remove_punct = TRUE, 
                 remove_symbols = TRUE,
                 remove_numbers = TRUE) %>% tokens_tolower()

tokens <- tokens_select(tokens, pattern = stopwords("french"), selection = "remove")

# creazione dfm dai token
dfm_test_set <- dfm(tokens)

### FEATURES MATCH
# utilizzo la funzione dfm_match() per rendere uguali le features del test set a quelle del training set
dfm_test_set <- dfm_match(dfm_test_set, 
                features = featnames(dfm_training_set))



### CREAZIONE DELLE MATRICI

#Training set
matrice_training_set <- as.matrix(dfm_training_set) 

#Test set
# NOTA: l'oggetto della funzione è la dfm che abbiamo creato nello step 3
# in cui il numero di features è stato ridotto per renderlo uguale a quello
# del training set
matrice_test_set <- as.matrix(dfm_test_set)


#La trasformiamo in factor
dfm_training_set@docvars$sentiment <- as.factor(dfm_training_set@docvars$sentiment)


    #### TRAINING E CLASSIFICAZIONE

### Naive Bayes Model
library(naivebayes)

# avviamento del modello
system.time(NaiveBayesModel <- multinomial_naive_bayes
            (x=matrice_training_set,
             y=dfm_training_set@docvars$sentiment,
             laplace = 1))

# mostriamo le caratteristiche del modello
print(summary(NaiveBayesModel))

# memorizzazione della previsione in un oggetto
NB_test_predicted <- predict(NaiveBayesModel,
                            matrice_test_set)

# visualizzazione dei risultati della previsione NB con valori assolluti e proporzioni
print(table(NB_test_predicted))
print(round(prop.table(table(NB_test_predicted )), 2))

### Random Forest Model
library(randomForest)

# avviamento del modello
system.time(RF <- randomForest(y= dfm_training_set@docvars$sentiment, 
                               x= matrice_training_set, 
                               importance=TRUE,  
                               do.trace=FALSE, 
                               ntree=500))


# visualizzazione dei risultati della previsione RF
print(RF)

# visualizzazione grafico dei risultati 
plot(RF, type = "l", col = c("black", "steelblue4","violetred4", "springgreen4"),
     main = "Random Forest Model Errors: sentiment variable")

legend("topright", horiz = FALSE, cex = 0.7,
       fill = c("springgreen4", "black", "steelblue4", "violetred4"),
       c("Positive error", "Average error", "Negative error", "Neutral error"))


# memorizzazione degli errori in un data frame
rf_errori <- as.data.frame(RF$err.rate)

# estrazione del numero di tree associati con l'errore più basso
print(min_tree <- which.min(rf_errori$OOB)) # = 1

# avviamento del il modello con ntree ottimizzato
system.time(RF2 <- randomForest(y = dfm_training_set@docvars$sentiment, 
                                x=matrice_training_set,
                                importance=FALSE, 
                                ntree=min_tree, 
                                do.trace=FALSE))

# visualizzazione dei risultati della previsione RF
print(RF2)

# visualizzazione grafico dei risultati 
plot(RF2, type = "l", col = c("black", "steelblue4","violetred4", "springgreen4"),
     main = "Random Forest Model Errors: sentiment variable")

legend("topright", horiz = FALSE, cex = 0.7,
       fill = c("springgreen4", "black", "steelblue4", "violetred4"),
       c("Positive error", "Average error", "Negative error", "Neutral error"))


# predizione dei risultati
system.time(RF_test_predicted <- predict(RF2, matrice_test_set ,type="class"))

# visualizzazione del sentiment in valori assoluti e in valori relativi
print(table(RF_test_predicted))
print(round(prop.table(table(RF_test_predicted)), 2))


library(iml)
library(future)
library(future.callr)
library(e1071)


# avviamento del modello
system.time(support_vector_machine <- svm(
  y= dfm_training_set@docvars$sentiment,
  x=matrice_training_set, kernel='linear', cost = 1))

# visualizzazione numero di support vectors considerati
length(support_vector_machine$index)

# predizione dei dati del test set
system.time(SV_test_predicted <- predict(support_vector_machine, matrice_test_set))

# visualizzazione della distribuzione del sentiment in valori assoluti e relativi
print(table(SV_test_predicted))
print(round(prop.table(table(SV_test_predicted))))

# aggiunta della variabile nel test set
dfm_test_set$PREDICTION_SV <- SV_test_predicted

