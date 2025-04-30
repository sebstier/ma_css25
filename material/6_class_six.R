#' class: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Text Analysis and Validation"
#' author: "Sebastian Stier"
#' lesson: 6
#' institute: University of Mannheim & GESIS
#' date: "2025-04-30"

library(quanteda)
library(caret)

# Validation of text classification ----

# read in party manifestos of German parties in 2013 and 2017
corp_ger <- read_rds("https://www.dropbox.com/s/uysdoep4unfz3zp/data_corpus_germanifestos.rds?dl=1")
summary(corp_ger)
docvars(corp_ger)

# Remove German stopwords, use only features that occur at least 50 times and create a dfm
dfm_ger <- corp_ger %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_select(pattern = stopwords("de"), selection = "remove") %>%
  dfm() %>%
  dfm_trim(min_termfreq = 30)

# Rooduijn populism dictionary
dict_rooduijn <- c("elit*",
                   "konsens*",
                   "undemokratisch*",
                   "referend*",
                   "korrupt*",
                   "propagand*",
                   "politiker*",
                   "täusch*",
                   "betrüg*",
                   "betrug*",
                   "*verrat*",
                   "scham*",
                   "schäm*",
                   "skandal*",
                   "wahrheit*",
                   "unfair*",
                   "unehrlich*",
                   "establishm*",
                   "*herrsch*",
                   "lüge*")

# Build dictionary using quanteda
pop_dict <- dictionary(list(rooduijn = dict_rooduijn,
                            populism_own = c("elite*", "volk*", "korrupt*", "*deutsch*") 
                                             ))

# Build dfm and apply dictionary
toks <- corp_ger %>% 
  tokens(remove_punct = TRUE, 
         remove_numbers = TRUE) %>% 
  tokens_select(pattern = c(stopwords("de")), selection = "remove") 
toks %>% 
  dfm() %>% 
  dfm_lookup(pop_dict) %>% 
  convert(to = "data.frame")

# Refine keyword lists
head(kwic(pattern = "*deutsch*", toks, window = 5), 5)
kwic(pattern = "*volk*", toks, window = 5)

# Hand code a paragraph or sentence sample for validation
df_manifesto_paragraphs <- corp_ger %>% 
  corpus_reshape(to = "paragraphs") %>% #sentences
  convert(to = "data.frame")
# write_csv(df_manifesto_paragraphs, file = "df_manifesto_paragraphs.csv")
# read_csv(file = "df_manifesto_paragraphs.csv")

# Check frequency of paragraphs
table(df_manifesto_paragraphs$party, df_manifesto_paragraphs$year)

# Create a document-frequency matrix, with German stopwords removed
#df_manifesto_paragraphs <- 

# Assign predictions
df_manifesto_dict <-  df_manifesto_paragraphs %>% 
  dfm_lookup(pop_dict) %>% 
  convert(to = "data.frame") %>% 
  as_tibble()

# Recover the text
df_manifesto_dict$text <- as.character(corp_parag)

# Inspect the data frame
df_manifesto_dict

# Binary cross-tab of the two dictionaries
tab_class <- table(rooduijn = df_manifesto_dict$rooduijn >= 1, populism_own = df_manifesto_dict$populism_own >= 1)
tab_class

# Confusion matrix and F1 scores
confusionMatrix(tab_class, mode = "everything") 


# LDA Topic Model ----
library(seededlda)
# Restrict the number of features, otherwise running the LDA will take long
dfm_trimmed <- df_manifesto_paragraphs %>% 
  dfm_trim(min_termfreq = 20) #only features that appear at least X times
dfm_trimmed

# set a seed in order to keep the output consistent
set.seed(111)

# run the LDA Topic Model
tmod_lda <- textmodel_lda(dfm_trimmed, k = 30)
terms(tmod_lda, 10)
df_terms <- terms(tmod_lda, 15)
View(df_terms)

# Assign topic as a new variable
dfm_trimmed$topic <- topics(tmod_lda)

# Cross-table the topic frequency
table(dfm_trimmed$topic)

# Calculate the mean topic average by party
df_party <- dfm_trimmed %>% 
  convert(to = "data.frame") %>% 
  as_tibble() %>% 
  bind_cols(docvars(dfm_trimmed)) %>% #add document vars as new columns
  group_by(party, topic) %>% 
  summarise(sum_topic = n())

# Create a barchart
df_party %>% 
  ggplot(aes(x = topic, y = sum_topic, color = party, fill = party)) + 
  geom_col()

# Visualize topic model on the web ----
library(LDAvis)
phi <- tmod_lda$phi  # topic-term distribution
theta <- tmod_lda$theta  # document-topic distribution
vocab <- featnames(dfm_trimmed) # vocabulary
doc_length <- rowSums(dfm_trimmed)  # length of each document
term_frequency <- colSums(dfm_trimmed)  # term frequency

# Create the JSON object for visualization
json <- LDAvis::createJSON(phi = phi, theta = theta, vocab = vocab, 
                           doc.length = doc_length, term.frequency = term_frequency)

# Visualize
LDAvis::serVis(json)


# Wordfish ----
# read in party manifestos of German parties in 2013 and 2017
summary(corp_ger)
docvars(corp_ger)

# Remove German stopwords, use only features that occur at least 50 times and create a dfm
dfm_ger <- corp_ger %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_select(pattern = stopwords("de"), selection = "remove") %>%
  dfm() %>%
  dfm_trim(min_termfreq = 30)

# Run a Wordfish model
model_wf <- textmodel_wordfish(dfm_ger)
textplot_scale1d(model_wf)
