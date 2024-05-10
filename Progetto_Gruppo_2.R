library(readxl)
library(writexl)
library(dplyr)

# impostazione della cartella di lavoro
setwd(getwd())

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
set.seed(12)
training_sample <- clean_df_french_bakeries[sample(nrow(clean_df_french_bakeries), 200), ]

# creazione dell'excel per riempire valori
write_xlsx(training_sample, path = "df_before_sentiment.xlsx")

# reimportazione del dataset dopo valutazione manuale
training_set <- read_excel("df_after_sentiment.xlsx")

### Creazione del test set
test_set <- anti_join(clean_df_french_bakeries, training_set)

# mostriamo i risultati
library(kableExtra)
library(dplyr)

print(kbl(clean_df_french_bakeries[1:5, ], longtable = T, booktabs = T, 
    caption = "Il dataset") %>%
    kable_styling(c("bordered", "condensed", "hover"), 
                  full_width = F, font_size = 11) %>%
    row_spec(0, color = "black", bold = T, background = "#b8daba") %>%
    column_spec(2, width = "20em", width_min="2in", color = "black" ))


    #### Sezione di pre-processing

### Librerie per pre-processing
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)

### creazione del corpus
# creazione del corpus
corpus_french_bakeries <- corpus(clean_df_french_bakeries)

# analizziamo le caratteristiche del corpus
text_stat <- textstat_summary(corpus_french_bakeries)

# mostriamo le caratteristiche del corpus
print(kbl(text_stat[1:5, ], longtable = T, booktabs = T, 
    caption = "Le caratteristiche del corpus") %>%
    kable_styling(c("bordered", "condensed", "hover"), 
                  full_width = F, font_size = 11) %>%
    row_spec(0, color = "black", bold = T, background = "#b8daba"))

    
###  creazione della DFM

# creazione dei token
tokens <- tokens(corpus_french_bakeries, 
                 remove_punct = TRUE, 
                 remove_symbols = TRUE,
                 remove_numbers = TRUE) %>% tokens_tolower()

tokens <- tokens_select(tokens, pattern = stopwords("french"), selection = "remove")

# creazione dfm dai token
dfm_french_bakeries <- dfm(tokens)

# conversione dfm in dataFrame per stampa con KBL
dfm_df <- quanteda::convert(dfm_french_bakeries, to = "data.frame")

# stampa della dfm
print(kbl(dfm_df[1:5, 1:15], longtable = TRUE, booktabs = TRUE, 
    caption = "Subset of DFM") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, font_size = 10) %>%
  row_spec(0, bold = TRUE, background = "#b8daba", color = "black", font_size = 11))

#Creiamo una wordcloud
print(textplot_wordcloud(dfm_french_bakeries, 
                   min_size = 1.5,  
                   max_size = 4,    
                   min.count = 10,   
                   max_words = 50,  
                   random.order = FALSE,  
                   random_color = FALSE,
                   rotation = 0,    #rotazione delle parole
                   colors = RColorBrewer::brewer.pal(8,"Dark2")))