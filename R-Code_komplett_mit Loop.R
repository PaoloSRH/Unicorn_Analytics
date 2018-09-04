##### Remove variables if they exist #####
rm(list=ls())

##### Load packages #####
library(mongolite)
library(dplyr) 
library(tm)
library(corpustools)
library(fastrtext)
library(quanteda)
library(caret)
library(e1071)
library(RTextTools)

##### Choose option (MongoDB or CSV import) #####
# MongoDB (=1) or csv import (=2)
type <- 1

##### Set FastRText settings (Specifiy range for loop, results will be saved in variable 'results') #####
epoche <- 128:128
dime <- 27:27
lr <- c(1)
ngram <- 4:4
verbose <-22:22

##### Option 1: MongoDB #####
if (type == 1) {
  ### Connection to categories
  #conn <- mongo("categories", url = "mongodb://192.168.2.135:27017/mails")  
  
  ### Connection to mails
  #conn <- mongo("interactions", url = "mongodb://192.168.2.135:27017/mails")
  conn <- mongo("interactions", url = "mongodb://localhost:27017/mails")
  getdata <- conn
  
  ### Count how many records are available
  getdata$count('{}')
  
  ### Clean texts and categories of MongoDB
  # Categories
  listid <- getdata$find(query = '{}', fields = '{"categories.group_id" : true, "_id": false}')
  listid <- data.frame(lapply(listid, function(x) {gsub("list\\(group_id = \"", "", x)}))
  listid <- data.frame(lapply(listid, function(x) {gsub("\"\\)", "", x)}))
  listid <- data.frame(lapply(listid, function(x) {gsub("list\\(group_id = c\\(\"", "", x)}))
  listid <- data.frame(lapply(listid, function(x) {gsub("\"", "", x)}))
  listid <- data.frame(lapply(listid, function(x) {gsub("\\)", "", x)}))
  listid <- data.frame(lapply(listid, function(x) {gsub("\n", "", x)}))
  
  # Relevant text modules
  listtext <- getdata$find(query = '{}',fields = '{"categories.text" : true, "_id": false}')
  listtext <- data.frame(lapply(listtext, function(x) {gsub("list\\(text = \"", "", x)}))
  listtext <- data.frame(lapply(listtext, function(x) {gsub("\"\\)", "", x)}))
  listtext <- data.frame(lapply(listtext, function(x) {gsub("\n", "", x)}))
  listtext <- data.frame(lapply(listtext, function(x) {gsub("\\)", "", x)}))

  # Adapt and unite data frames
  textframe <- data.frame(listid, listtext)
  colnames(textframe) <- c("id", "text")
}

##### Option 2: csv file #####
if (type == 2) { 
  csvimport = read.csv("maildaten2.csv", header = TRUE, sep=";", encoding = "UTF-8",stringsAsFactors = FALSE)
  textframe <- data.frame(csvimport[,10], csvimport[,12])
  colnames(textframe) <- c("id", "text")
}


##### Load 2nd column in data frame and clean with tm #####
corpus <- VCorpus(VectorSource(textframe[,2]))


#inspect(corpus)
# remove whitespace
corpus <- tm_map(corpus, stripWhitespace)
# convert to lowercases
corpus <- tm_map(corpus, content_transformer(tolower))
# remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("german"))
# remove Punctuation
corpus <- tm_map(corpus, removePunctuation)
# replace contraction
#corpus <- tm_map(corpus, replace_contraction)
# replace abbreviation
#corpus <- tm_map(corpus, replace_abbreviation)
# replace symbol
#corpus <- tm_map(corpus, replace_symbol)
# stemming
tm_map(corpus, stemDocument)

corpusdataframe <- data.frame(text_new = sapply(corpus, as.character), stringsAsFactors = FALSE)

#inspect(corpus)
#print(textframe)

### corpusmatrix is not needed for FastRText
# Term document matrices
#corpusmatrix <- DocumentTermMatrix(corpus)
#inspect(corpusmatrix)

# Frequency matrix
#findFreqTerms(corpusmatrix, 10)

# Correlation between single words
#findAssocs(corpusmatrix, "fehler", 0.5)

# Remove rare words
#removeSparseTerms(corpusmatrix, 0.4)
#inspect(corpusmatrix)

# Add new column with edited mail texts
textframe[,3] <- corpusdataframe

##### START OF: FASTRTEXT #####

### Data is needed in a specific format, load necessary columns in new dataframe
textframe2 <- subset(textframe, select=c("id", "text_new"))
names(textframe2) <- c("class.text", "text")

### Mix up dataset
#textframe2 <- textframe2[sample(nrow(textframe2)),]


if (type == 1) { #MongoDB
  ### Delete rows with multiple IDs
  textframe2 <- textframe2[!grepl(",", textframe2$class.text),]
  
  ### If there are not enough mail texts per category, categories will be deleted
  # TODO: Should be automated
  #textframe3 <- aggregate(cbind(count = text) ~ class.text, data = textframe2, FUN = function(x){NROW(x)})
  # delete ids 59, 70, 72, 85, 86, 87, 92, 97 and group ids 1, 13 and 18
  textframe2 <- textframe2[!grepl("1", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("13", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("18", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("59", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("70", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("72", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("85", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("86", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("87", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("92", textframe2$class.text),]
  textframe2 <- textframe2[!grepl("97", textframe2$class.text),]
}

### START OF training and test dataset generation ###

### Aufteilung NEU:
help_df1 <- data.frame(class.text=character(),text=character(),stringsAsFactors=FALSE)
help_df2 <- data.frame(class.text=character(),text=character(),stringsAsFactors=FALSE)


for(i in 1:length(unique(textframe2[,1]))){
  temp_head <- head(subset(textframe2, class.text == unique(textframe2[,1])[i]),.7*nrow(subset(textframe2, class.text == unique(textframe2[,1])[i])))
  temp_tail <- tail(subset(textframe2, class.text == unique(textframe2[,1])[i]),.3*nrow(subset(textframe2, class.text == unique(textframe2[,1])[i])))
  
  help_df1 <- rbind(help_df1, temp_head)
  help_df2 <- rbind(help_df2, temp_tail)
  
}

train_sentences <- help_df1
test_sentences <- help_df2

### END OF training and test dataset generation ###

# TODO: Has to be improved
#textframe2 %>% 
#add_count(class.text)


#for (year in c(2010,2011,2012,2013,2014,2015)){
#  print(paste("The year is", year))
#}

### Create dataframe to save results of the models
results <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("id", "time", "dim","lr","ngram", "epoch","verbose", "result"))
id <- 1

### Setting seed
set.seed(42)

##### FASTRTEXT Loop, ranges can be specified at the beginning of script #####
for (i in epoche){ 
  for (j in dime){ 
    for (k in lr){ 
      for (l in ngram){ 
        for (m in verbose){ 
          
          # Convert data in right format for model
          tmp_file_model <- tempfile()
          
          ### Train data set
          # Add "label" to categories
          train_labels <- paste0("__label__", train_sentences[,"class.text"])
          # Convert mail text in small letters
          train_texts <- tolower(train_sentences[,"text"])
          # View(train_texts)
          # Kategorie mit Prefix + Text aus Kleinbuchstaben
          train_to_write <- paste(train_labels, train_texts)
          # View(train_to_write)
          train_tmp_file_txt <- tempfile()
          # Write data in tempfile
          writeLines(text = train_to_write, con = train_tmp_file_txt)
          
          
          ### Apply same procedure for test data set
          test_labels <- paste0("__label__", test_sentences[,"class.text"])
          test_labels_without_prefix <- test_sentences[,"class.text"]
          test_texts <- tolower(test_sentences[,"text"])
          test_to_write <- paste(test_labels, test_texts)
          
          ###setting seed
          set.seed(42)
          
          ### Train model and save in tempfile, parameter can be adjusted
          execute(commands = c("supervised", "-input", train_tmp_file_txt, "-output", tmp_file_model, "-dim", j, "-lr", k, "-epoch", i, "-wordNgrams", l, "-verbose", m))
          #execute(commands = c("supervised", "-input", train_tmp_file_txt, "-output", tmp_file_model, "-dim", 200, "-lr", 1, "-epoch", 20, "-wordNgrams", 2, "-verbose", 5))
          
          ### Load model
          model <- load_model(tmp_file_model)
          
          ### Apply model for test data set
          predictions <- predict(model, sentences = test_to_write)
          
          ### Splitting predictions in predictions filtered (without bin = category "999") and predictions bin (category "999")
          prediction_splitter <- 0.2
          predictions_renamed <- lapply(predictions, function(prediction){
            if(prediction<prediction_splitter){
              c("999" = prediction[[1]][[1]])
            }else{
              prediction
            }
          })
          
          predictions_bin <- predictions_renamed[predictions_renamed<prediction_splitter]
          predictions_filtered <- predictions_renamed[predictions_renamed>=prediction_splitter]
          
          ### Vorhergesagte Kategorie und Wahrscheinlichkeit ausgeben
          #print(head(predictions,100))
          #summary(unlist(predictions))
          ### Prozentualer Anteil, in dem das Model richtig lag
          #result <- mean(names(unlist(predictions)) == test_labels_without_prefix)
          result <- mean(unlist(predictions_filtered))
          
          ### Because there is only one category by observation, hamming loss will be the same
          #get_hamming_loss(as.list(test_labels_without_prefix), predictions)
          
          # You can get flat list of results when you are retrieving only one label per observation
          #print(head(predict(model, sentences = test_to_write, simplify = TRUE)))
          
          ### Save results
          results[nrow(results) + 1,] = list(id,format(Sys.time(), format="%d.%m.%Y - %H:%M:%S"),j,k,l,i,m,result)
          
          id <- id+1
          
          show(result)
          
          ### Free memory and delete files in temp folder
          # mit tempdir() auslesen welcher der Temp-Ordner ist, dann den Inhalt im nächsten Schritt immer löschen
          do.call(file.remove, list(list.files(tempdir(), full.names = TRUE)))
          unlink(train_tmp_file_txt)
          unlink(tmp_file_model)
          gc()
          
        }}}}}
##### END OF: FASTRTEXT #####

##### START OF MODEL 2: QUANTEDA (NAIVE BAYES CLASSIFIER) #####

#Traindaten - in Corpus dann Labels dazu und DFM erstellen. 
model2.train.corpus <- corpus(train_sentences$text) 
docvars(model2.train.corpus) <- train_sentences$class.text
model2.train.dfm <- dfm(model2.train.corpus, tolower = TRUE,stem=TRUE)

#Das gleiche für Testdaten
model2.test.corpus <- corpus(test_sentences$text) 
docvars(model2.test.corpus) <- test_sentences$class.text
model2.test.dfm <- dfm(model2.test.corpus, tolower = TRUE,stem=TRUE)

#summary(model2.train.corpus, 5)
# Model trainieren
model2.nb <- textmodel_nb(model2.train.dfm, docvars(model2.train.dfm, "docvar1"))
#summary(model2.nb)

#Feature von Trainings- und Testdaten verwenden
model2.test_dfm <- dfm_select(model2.test.dfm, model2.train.dfm)

#Und jetzt prüfen wie gut das Model funktioniert
model2.actual_class <- docvars(model2.test_dfm, "docvar1")
model2.predicted_class <- predict(model2.nb, model2.test_dfm)
#Mit union
u <- union(model2.actual_class, model2.predicted_class)
model2.class_table <- table(factor(model2.actual_class, u), factor(model2.predicted_class, u))
#Ohne Union
#model2.class_table <- table(model2.actual_class, model2.predicted_class)

#model2.class_table


confusionMatrix(model2.class_table, mode = "everything")

##### END OF: QUANTEDA #####


##### START OF MODEL 3: SVM (Nicht gut aber mal schauen was man mit machen kann) #####
# vor dem ausfuehren
trace("create_matrix", edit=T) #eingeben un in Zeile 42 Acronym in acronym Ã¤ndern
# wir benÃ¶tigen eine Matrix

model3.dtMatrix <- create_matrix(train_sentences$text)

# und einen Container
model3.container <- create_container(model3.dtMatrix, train_sentences$class.text, trainSize=1:length(train_sentences$class.text), virgin=FALSE)

# Dann trainieren wir das Modell
model3.model <- train_model(model3.container, "SVM", kernel="linear", cost=1)

# Testdaten vorbereiten
model3.predictionData <-test_sentences$text

# Prediction Matrix
model3.predMatrix <- create_matrix(model3.predictionData, originalMatrix=model3.dtMatrix)

# Prediction Container
predSize = length(model3.predictionData);
model3.predictionContainer <- create_container(model3.predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

# Ergebnisse
model3.results <- classify_model(model3.predictionContainer, model3.model)

# Und den Durchschnitt um das Modell zu bewerten
mean(model3.results$SVM_PROB)

# stimmt noch nicht ganz, testlabels zum vergleich heranziehen.

##### END OF: SVM #####

##### START OF MODEL 4: xgBoost #####
library(xgboost)




##### Start of Ensembling #####


#Predicting the probabilities
test_sentences$pred_fastrtext<-predict(object = model,test_sentences$class.text,type='prob')
test_sentences$pred_quanteda<-predict(object = model2,test_sentences$class.text,type='prob')
test_sentences$pred_mod3<-predict(object = model3,test_sentences$class.text,type='prob')

#Taking average of predictionsa
test_sentences$pred_avg<-(test_sentences$pred_fastrtext$Y+test_sentences$pred_quanteda$Y+test_sentences$pred_mod3$Y)/3

#Splitting into binary classes at 0.5
test_sentences$pred_avg<-as.factor(ifelse(test_sentences$pred_avg>0.5,'Y','N'))

