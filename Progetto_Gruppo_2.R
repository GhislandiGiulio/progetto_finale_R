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


  #### CROSS VALIDATION

library(cvTools)
library(caret)

# duplicazione della matrice del training set 
matrice_training_set2 <- matrice_training_set

# definizione di un oggetto k che indichi il numero di folders
k <- 5

# divisione della matrice in k folders
folds <- cvFolds(NROW(matrice_training_set2), K = k)


## NAIVE BAYES
#loop
system.time(for(i in 1:k){
  
  matrice_training_set <-
    matrice_training_set2 [folds$subsets[folds$which != i], ]
  
  validation_set <-
    matrice_training_set2 [folds$subsets[folds$which == i], ]
  
  NaiveBayesModel <- multinomial_naive_bayes(
    y= dfm_training_set[folds$subsets[folds$which != i], ]
    @docvars$sentiment ,
    x=matrice_training_set, laplace = 1)
  
  NB_predictions <- predict(NaiveBayesModel, 
                            newdata= validation_set, 
                            type = "class")
  
  class_table <- table("Predictions"= NB_predictions,
                       "Actual"=dfm_training_set[folds$subsets[folds$which == i], ]@docvars$sentiment)
  
  print(class_table)
  df<-confusionMatrix( class_table, mode = "everything")
  df_measures_NB<-paste0("conf.mat.nb",i)
  assign(df_measures_NB,df)
})


#Creiamo un dataset vuoto da riempire con le informazioni della cross-validation
df_NB_prediction <- data.frame(col1=vector(), col2=vector(), col3=vector(), col4=vector())

# riempimento il dataset con i valori di accuracy e f1 
for(i in mget(ls(pattern = "conf.mat.nb")) ) {
  Accuracy <-(i)$overall[1]
  p <- as.data.frame((i)$byClass)
  F1_negative <- p$F1[1]
  F1_neutral <- p$F1[2]
  F1_positive <- p$F1[3]
  df_NB_prediction <- rbind(df_NB_prediction , cbind(Accuracy , F1_negative ,
                                               F1_neutral, F1_positive ))
  
}

# sostituzione dei valori mancanti con 0
df_NB_prediction [is.na(df_NB_prediction )] <- 0

# estrazione del valore medio di accuracy e f1
AverageAccuracy_NB <- mean(df_NB_prediction[, 1] )
AverageF1_NB<- mean(colMeans(df_NB_prediction[-1] ))

# visualizzazione dei valori medi
print(AverageAccuracy_NB)
print(AverageF1_NB)


## RANDOM FOREST
#loop
system.time(for(i in 1:k){
  matrice_training_set <-
    matrice_training_set2 [folds$subsets[folds$which != i], ]
  validation_set <-
    matrice_training_set2 [folds$subsets[folds$which == i], ]
  RandomForest <- randomForest(
    y= dfm_training_set[folds$subsets[folds$which != i], ]
                               @docvars$sentiment ,
    x=matrice_training_set, do.trace=FALSE, ntree=96)
  df_RF_prediction <- predict(RandomForest, 
                            newdata= validation_set, 
                            type="class")
  class_table <- table("Predictions"= df_RF_prediction,
                       "Actual"=dfm_training_set[folds$subsets[folds$which == i], ]@docvars$sentiment)
  print(class_table)
  df<-confusionMatrix( class_table, mode = "everything")
  df_measures_RF<-paste0("conf.mat.rf",i)
  assign(df_measures_RF,df)
})

#Dataset vuoto
df_RF_prediction <- data.frame(col1=vector(), col2=vector(), col3=vector(), col4 = vector())

#riempiamo il dataset con le performance metrics
for(i in mget(ls(pattern = "conf.mat.rf")) ) { #conf.mat.rf
  Accuracy <-(i)$overall[1]
  p <- as.data.frame((i)$byClass)
  F1_negative <- p$F1[1]
  F1_neutral <- p$F1[2]
  F1_positive <- p$F1[3]
  df_RF_prediction <- rbind(df_RF_prediction , cbind(Accuracy , F1_negative ,
                                                 F1_neutral, F1_positive ))
  
}

# sostituzione dei valori mancanti con 0
df_RF_prediction [is.na(df_RF_prediction )] <- 0

# estrazione del valore medio di accuracy e f1
AverageAccuracy_RF <- mean(df_RF_prediction[, 1] )
AverageF1_RF<- mean(colMeans(df_RF_prediction[-1] ))

# visualizzazione dei valori medi
print(AverageAccuracy_RF)
print(AverageF1_RF)

## SUPPORT VECTOR MACHINE
#loop
system.time(for(i in 1:k){
  matrice_training_set <-
    matrice_training_set2 [folds$subsets[folds$which != i], ]
  validation_set <-
    matrice_training_set2 [folds$subsets[folds$which == i], ]
  support_vector_machine <- svm(
    y= dfm_training_set[folds$subsets[folds$which != i], ]
    @docvars$sentiment ,
    x=matrice_training_set, do.trace=FALSE, ntree=96)
  df_SV_prediction <- predict(support_vector_machine, 
                              newdata= validation_set, 
                              type="class")
  class_table <- table("Predictions"= df_SV_prediction,
                       "Actual"=dfm_training_set[folds$subsets[folds$which == i], ]@docvars$sentiment)
  print(class_table)
  df<-confusionMatrix( class_table, mode = "everything")
  df_measures_SV<-paste0("conf.mat.rf",i)
  assign(df_measures_SV,df)
})

#Dataset vuoto
df_SV_prediction <- data.frame(col1=vector(), col2=vector(), col3=vector(), col4 = vector())

#riempiamo il dataset con le performance metrics
for(i in mget(ls(pattern = "conf.mat.rf")) ) { #conf.mat.rf
  Accuracy <-(i)$overall[1]
  p <- as.data.frame((i)$byClass)
  F1_negative <- p$F1[1]
  F1_neutral <- p$F1[2]
  F1_positive <- p$F1[3]
  df_SV_prediction <- rbind(df_SV_prediction , cbind(Accuracy , F1_negative ,
                                                     F1_neutral, F1_positive ))
  
}

# sostituzione dei valori mancanti con 0
df_SV_prediction [is.na(df_SV_prediction )] <- 0

# estrazione del valore medio di accuracy e f1
AverageAccuracy_SV <- mean(df_SV_prediction[, 1] )
AverageF1_SV<- mean(colMeans(df_SV_prediction[-1] ))

# visualizzazione dei valori medi
print(AverageAccuracy_SV)
print(AverageF1_SV)


  ####  COMPARAZIONE RISULTATI

#Comparazione
library(reshape2)

# ACCURACY 
#Creo un dataframe per NB
AccNB <- as.data.frame(AverageAccuracy_NB )
#Rinomino la colonna
colnames(AccNB)[1] <- "NB"

#Creo un dataframe per RF
AccRF <- as.data.frame(AverageAccuracy_RF )
#Rinomino la colonna
colnames(AccRF)[1] <- "RF"

#Creo un dataframe per SV
AccSV<- as.data.frame(AverageAccuracy_SV )
#Rinomino la colonna
colnames(AccSV)[1] <- "SV"

#Unisco in un unico dataframe i valori di accuracy dei tre modelli
Accuracy_models <- cbind(AccNB, AccRF, AccSV)
Accuracy_models


Accuracy_models_Melt <-melt(Accuracy_models)
str(Accuracy_models_Melt)

#Creo un grafico per i valori di accuracy 
plot_accuracy <- ggplot(Accuracy_models_Melt, aes(x=variable, y=value, color = variable)) +
  geom_boxplot() + xlab("Algorithm") + ylab(label="Values of accuracy") +
  labs(title = "Cross-validation with k =5: values of accuracy") + coord_flip() +
  theme_bw() +
  guides(color=guide_legend(title="Algorithms")) +
  theme(plot.title = element_text(color = "black", size = 12, face = "italic"),
        axis.title.x =element_text(size=12,face="bold"),
        axis.title.y =element_text(size=12, face = "plain"),
        axis.text= element_text(size =10, face = "italic")
  )


# F1 SCORE 
# Replico gli stessi step per f1 score

#NB
F1NB <- as.data.frame(AverageF1_NB)
colnames(F1NB)[1] <- "NB"
#RF
F1RF<- as.data.frame(AverageF1_RF )
colnames(F1RF)[1] <- "RF"
#SV
F1SV <- as.data.frame(AverageF1_SV)
colnames(F1SV)[1] <- "SV"
#DATAFRAME
f1_models <- cbind(F1NB, F1RF, F1SV)
f1_models

f1_models_melt <-melt(f1_models)
str(f1_models_melt)

#Creo il grafico
plot_f1 <- ggplot(f1_models_melt, aes(x=variable, y=value, color = variable)) +
  geom_boxplot() + xlab("Algorithm") + ylab(label="Values of f1") +
  labs(title = "Cross-validation with k =5: values of f1") + coord_flip() +
  theme_bw() +
  guides(color=guide_legend(title="Algorithms")) +
  theme(plot.title = element_text(color = "black", size = 12, face = "italic"),
        axis.title.x =element_text(size=12,face="bold"),
        axis.title.y =element_text(size=12, face = "plain"),
        axis.text= element_text(size =10, face = "italic")
  )


library(gridExtra)
#Visualizzo i due grafici 
grid.arrange(plot_accuracy, plot_f1, nrow=2)


  #### DRIVER ANALYSIS

# Creazione del dataframe con sentiment previsto, calcolato al momento con NB (modello migliore)
predicted_df <- transform(
  test_set, 
  sentiment = (NB_test_predicted <- predict(NaiveBayesModel, matrice_test_set))) %>%
  rbind(training_set, .)

predicted_df$id <- 1:nrow(predicted_df)

### Creazione corpus e DFM 

## DF dell'intero df: test_set+training_set

# Corpus
corpus_predicted_df <- corpus(predicted_df)

# dfm
dfm_predicted_df <- corpus_predicted_df %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(pattern = stopwords("french")) %>%
  tokens_tolower() %>%
  tokens_wordstem() %>%
  dfm()

## Creazione del dizionario per la DRIVER ANALYSIS
# Costruzione del dizionario
rating_drivers <- dictionary(list(Personale = c("gentil*", "cordial*", "accueillant*", "disponible*",
                                        "personnel*", "malpoli*", "impoli*", "courtois*",
                                        "personne*"),
                          Qualità = c("bon*", "qualité*", "excellent*", "optimal*"),
                          Prezzo = c("prix*", "coût*", "tarif*"),
                          Location = c("beau*", "propre*", "sale*", "soigné*", "mignon*",
                                          "local", "emplacement", "position*", "au centre",
                                          "Vomero")))

# applicazione del dizionario sulla dfm
#Applicazione del dizionario alla dfm

bakeries_drivers <- dfm_lookup(dfm_predicted_df, #dfm su cui lo applico
                              rating_drivers)  #dizionario

# conversione dell'output in data frame
df_bakeries_drivers <- convert(bakeries_drivers, to = "data.frame")

# aggiunta un id che verrà utilizzato per il merging
df_bakeries_drivers$id <- 1:nrow(df_bakeries_drivers)

# unione dei due dataset
driver_analysis <- full_join(df_bakeries_drivers, predicted_df, by = join_by("id"))

driver_analysis <- driver_analysis %>%
  mutate(sentiment = case_when(
    sentiment == "positive" ~ 100.0,
    sentiment == "negative" ~ 0.0,
    sentiment == "neutral" ~ 50.0
  ))

# selezione delle variabili rilevanti
driver_analysis <- select(driver_analysis, id, Players, text, Personale, Qualità, Prezzo, Location, score_rating, sentiment, sentiment)

# visualizzazione dataset con sentiment e driver analysis
print(kbl(driver_analysis[1:15, ], longtable = T, booktabs = T, 
    caption = "I driver menzionati nelle recensioni") %>%
  kable_styling(c("bordered", "condensed", "hover"), 
                full_width = F, font_size = 8) %>%
  row_spec(0, color = "black", bold = T, background = "#b8daba", font_size = 11))


  #### CREAZIONE TABELLA FINALE
# separazione del df driver_analysis in un df per ogni player
list_of_dfs <- split(driver_analysis, driver_analysis$Players)

# creazione di una variabile per ogni pasticceria
for (player_name in names(list_of_dfs)) {
  # Remove spaces from player_name
  clean_player_name <- gsub(" ", "_", player_name)
  # Assign dataframe to a variable with cleaned name
  assign(paste0("df_", clean_player_name), list_of_dfs[[player_name]])
}


df_Ange_Boulangerie$n_recensioni <- nrow(`df_Ange_Boulangerie`)
df_Ange_Boulangerie$n_recensioni_qualita <- sum(df_Ange_Boulangerie$Qualità > 0)
df_Ange_Boulangerie$n_recensioni_prezzo <- sum(df_Ange_Boulangerie$Prezzo > 0)
df_Ange_Boulangerie$n_recensioni_location <- sum(df_Ange_Boulangerie$Location > 0)
df_Ange_Boulangerie$n_recensioni_personale <- sum(df_Ange_Boulangerie$Personale > 0)

df_Ange_Boulangerie$rating_medio <- mean(df_Ange_Boulangerie$score_rating)
df_Ange_Boulangerie$sentiment_medio <- mean(df_Ange_Boulangerie$sentiment)

df_Ange_Boulangerie$rating_medio_qualita <- df_Ange_Boulangerie %>%
  filter(Qualità > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)
df_Ange_Boulangerie$rating_medio_prezzo <- df_Ange_Boulangerie %>%
  filter(Prezzo > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)
df_Ange_Boulangerie$rating_medio_location <- df_Ange_Boulangerie %>%
  filter(Location > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)
df_Ange_Boulangerie$rating_medio_personale <- df_Ange_Boulangerie %>%
  filter(Personale > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_Ange_Boulangerie$sentiment_medio_qualita <- df_Ange_Boulangerie %>%
  filter(Qualità > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)
df_Ange_Boulangerie$sentiment_medio_prezzo <- df_Ange_Boulangerie %>%
  filter(Prezzo > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)
df_Ange_Boulangerie$sentiment_medio_location <- df_Ange_Boulangerie %>%
  filter(Location > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)
df_Ange_Boulangerie$sentiment_medio_personale <- df_Ange_Boulangerie %>%
  filter(Personale > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

# Counting the number of rows
df_la_croissanterie$n_recensioni <- nrow(df_la_croissanterie)
df_la_croissanterie$n_recensioni_qualita <- sum(df_la_croissanterie$Qualità > 0)
df_la_croissanterie$n_recensioni_prezzo <- sum(df_la_croissanterie$Prezzo > 0)
df_la_croissanterie$n_recensioni_location <- sum(df_la_croissanterie$Location > 0)
df_la_croissanterie$n_recensioni_personale <- sum(df_la_croissanterie$Personale > 0)

# Calculating mean rating
df_la_croissanterie$rating_medio <- mean(df_la_croissanterie$score_rating)
df_la_croissanterie$sentiment_medio <- mean(df_la_croissanterie$sentiment)

# Calculating mean rating and sentiment for each aspect
df_la_croissanterie$rating_medio_qualita <- df_la_croissanterie %>%
  filter(Qualità > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_la_croissanterie$rating_medio_prezzo <- df_la_croissanterie %>%
  filter(Prezzo > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_la_croissanterie$rating_medio_location <- df_la_croissanterie %>%
  filter(Location > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_la_croissanterie$rating_medio_personale <- df_la_croissanterie %>%
  filter(Personale > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_la_croissanterie$sentiment_medio_qualita <- df_la_croissanterie %>%
  filter(Qualità > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_la_croissanterie$sentiment_medio_prezzo <- df_la_croissanterie %>%
  filter(Prezzo > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_la_croissanterie$sentiment_medio_location <- df_la_croissanterie %>%
  filter(Location > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_la_croissanterie$sentiment_medio_personale <- df_la_croissanterie %>%
  filter(Personale > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

# Counting the number of rows
df_Brioche_Doree$n_recensioni <- nrow(df_Brioche_Doree)
df_Brioche_Doree$n_recensioni_qualita <- sum(df_Brioche_Doree$Qualità > 0)
df_Brioche_Doree$n_recensioni_prezzo <- sum(df_Brioche_Doree$Prezzo > 0)
df_Brioche_Doree$n_recensioni_location <- sum(df_Brioche_Doree$Location > 0)
df_Brioche_Doree$n_recensioni_personale <- sum(df_Brioche_Doree$Personale > 0)

# Calculating mean rating
df_Brioche_Doree$rating_medio <- mean(df_Brioche_Doree$score_rating)
df_Brioche_Doree$sentiment_medio <- mean(df_Brioche_Doree$sentiment)

# Calculating mean rating and sentiment for each aspect
df_Brioche_Doree$rating_medio_qualita <- df_Brioche_Doree %>%
  filter(Qualità > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_Brioche_Doree$rating_medio_prezzo <- df_Brioche_Doree %>%
  filter(Prezzo > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_Brioche_Doree$rating_medio_location <- df_Brioche_Doree %>%
  filter(Location > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_Brioche_Doree$rating_medio_personale <- df_Brioche_Doree %>%
  filter(Personale > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_Brioche_Doree$sentiment_medio_qualita <- df_Brioche_Doree %>%
  filter(Qualità > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_Brioche_Doree$sentiment_medio_prezzo <- df_Brioche_Doree %>%
  filter(Prezzo > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_Brioche_Doree$sentiment_medio_location <- df_Brioche_Doree %>%
  filter(Location > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_Brioche_Doree$sentiment_medio_personale <- df_Brioche_Doree %>%
  filter(Personale > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)



# Counting the number of rows
df_La_mie_Caline$n_recensioni <- nrow(df_La_mie_Caline)
df_La_mie_Caline$n_recensioni_qualita <- sum(df_La_mie_Caline$Qualità > 0)
df_La_mie_Caline$n_recensioni_prezzo <- sum(df_La_mie_Caline$Prezzo > 0)
df_La_mie_Caline$n_recensioni_location <- sum(df_La_mie_Caline$Location > 0)
df_La_mie_Caline$n_recensioni_personale <- sum(df_La_mie_Caline$Personale > 0)

# Calculating mean rating
df_La_mie_Caline$rating_medio <- mean(df_La_mie_Caline$score_rating)
df_La_mie_Caline$sentiment_medio <- mean(df_La_mie_Caline$sentiment)

# Calculating mean rating and sentiment for each aspect
df_La_mie_Caline$rating_medio_qualita <- df_La_mie_Caline %>%
  filter(Qualità > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_La_mie_Caline$rating_medio_prezzo <- df_La_mie_Caline %>%
  filter(Prezzo > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_La_mie_Caline$rating_medio_location <- df_La_mie_Caline %>%
  filter(Location > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_La_mie_Caline$rating_medio_personale <- df_La_mie_Caline %>%
  filter(Personale > 0) %>%
  summarize(mean_score_rating = mean(score_rating)) %>%
  pull(mean_score_rating)

df_La_mie_Caline$sentiment_medio_qualita <- df_La_mie_Caline %>%
  filter(Qualità > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_La_mie_Caline$sentiment_medio_prezzo <- df_La_mie_Caline %>%
  filter(Prezzo > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_La_mie_Caline$sentiment_medio_location <- df_La_mie_Caline %>%
  filter(Location > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)

df_La_mie_Caline$sentiment_medio_personale <- df_La_mie_Caline %>%
  filter(Personale > 0) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  pull(mean_sentiment)


# unione dei dati dei df
final_df <- rbind(df_Ange_Boulangerie, df_Brioche_Doree, df_La_mie_Caline, df_la_croissanterie)

# rimozione colonne superflue
final_df <- select(final_df, -text, -Qualità, -Personale, -Prezzo, -Location, -score_rating, -sentiment, -id)

# raggruppamento
final_df <- final_df %>%
  group_by(Players) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# visualizzazione dataset finale raggruppato
print(kbl(final_df, longtable = T, booktabs = T, 
          caption = "Riassunto finale delle Recensioni") %>%
        kable_styling(c("bordered", "condensed", "hover"), 
                      full_width = F, font_size = 8) %>%
        row_spec(0, color = "black", bold = T, background = "#b8daba", font_size = 11))


