
# OPTIONEN

# MongoDB (=1) or csv Import (=2)
type <- 2

# FastRText Einstellungen (Fuer Loop range angeben, Ergebnisse werden in Variable results gespeichert.)
epoche <- 128:128
dime <- 27:27
lr <- c(1)
ngram <- 4:4
verbose <-22:22

#Einfach immer alles ausführen



#install.packages("tm") 
#install.packages("libxml2") 
#install.packages("corpustools") 
#install.packages("mongolite") 
#install.packages("dplyr") 
#install.packages("fastrtext") 

library(mongolite)
library(dplyr) 
library(tm)
library(corpustools)
library(fastrtext)

# Remove Variables if they exist
if (exists("conn")) {rm(conn)}
if (exists("getdata")) {rm(getdata)}
if (exists("listid")) {rm(listid)}
if (exists("listtext")) {rm(listtext)}
if (exists("textframe")) {rm(textframe)}
if (exists("corpus")) {rm(corpus)}
if (exists("corpusmatrix")) {rm(corpusmatrix)}
if (exists("model")) {rm(model)}
if (exists("predictions")) {rm(predictions)}
if (exists("test_labels_without_prefix")) {rm(test_labels_without_prefix)}
if (exists("test_sentences")) {rm(test_sentences)}
if (exists("textframe2")) {rm(textframe2)}
if (exists("train_sentences")) {rm(train_sentences)}
if (exists("corpusdataframe")) {rm(corpusdataframe)}
if (exists("id")) {rm(id)}
if (exists("result")) {rm(result)}
if (exists("results")) {rm(results)}
if (exists("help_df1")) {rm(help_df1)}
if (exists("help_df2")) {rm(help_df2)}
if (exists("temp_head")) {rm(temp_head)}
if (exists("temp_tail")) {rm(temp_tail)}
if (exists("csvimport")) {rm(csvimport)}
if (exists("model")) {rm(model)}

#rm(textframe3)

if (type == 1) {
# Verbindung zu Kategorien, wird aktuell nicht gebraucht
# conn <- mongo("categories", url = "mongodb://192.168.2.135:27017/mails")  

# Verbindung zur MongoDB zu den Mails:
conn <- mongo("interactions", url = "mongodb://192.168.2.135:27017/mails")  
getdata <- conn

# Durchzählen wie viele Datensätze vorhanden sind
getdata$count('{}')

# Texte und Kategorien aus monodb und berinigen (da aktuell nicht sauber gespeichert)
# Kategorien
listid <- getdata$find(query = '{}', fields = '{"categories.id" : true, "_id": false}')
listid <- data.frame(lapply(listid, function(x) {gsub("list\\(id = \"", "", x)}))
listid <- data.frame(lapply(listid, function(x) {gsub("\"\\)", "", x)}))
listid <- data.frame(lapply(listid, function(x) {gsub("list\\(id = c\\(\"", "", x)}))
listid <- data.frame(lapply(listid, function(x) {gsub("\"", "", x)}))
listid <- data.frame(lapply(listid, function(x) {gsub("\\)", "", x)}))
listid <- data.frame(lapply(listid, function(x) {gsub("\n", "", x)}))



# Relevante Textbausteine
listtext <- getdata$find(query = '{}',fields = '{"categories.text" : true, "_id": false}')
listtext <- data.frame(lapply(listtext, function(x) {gsub("list\\(text = \"", "", x)}))
listtext <- data.frame(lapply(listtext, function(x) {gsub("\"\\)", "", x)}))
listtext <- data.frame(lapply(listtext, function(x) {gsub("\n", "", x)}))
listtext <- data.frame(lapply(listtext, function(x) {gsub("\\)", "", x)}))


#Dataframes zusammenführen und anpassen
textframe <- data.frame(listid, listtext)
colnames(textframe) <- c("id", "text")

}

if (type == 2) {
csvimport = read.csv("maildaten2.csv", header = TRUE, sep=";", encoding = "UTF-8",stringsAsFactors = FALSE)
textframe <- data.frame(csvimport[,10], csvimport[,12])
colnames(textframe) <- c("id", "text")
}



# wir Laden die 2te Spalte in den Dataframe und verwenden tm zur Breinigung
corpus <- VCorpus(VectorSource(textframe[,2]))

# inspect(corpus)
#whitespace entfernen
corpus <- tm_map(corpus, stripWhitespace)
#convert to lowercases
corpus <- tm_map(corpus, content_transformer(tolower))
#remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("german"))
#remove Punctuation
corpus <- tm_map(corpus, removePunctuation)
#replace contraction
corpus <- tm_map(corpus, replace_contraction)
#replace abbreviation
corpus <- tm_map(corpus, replace_abbreviation)
#replace symbol
#corpus <- tm_map(corpus, replace_symbol)
#stemming
tm_map(corpus, stemDocument)

corpusdataframe <- data.frame(text_new = sapply(corpus, as.character), stringsAsFactors = FALSE)



#inspect(corpus)
#print(textframe)

## corpusmatrix erst einmal weglassen, wird nicht benötigt für fastrtext
##Term document Matrices
#corpusmatrix <- DocumentTermMatrix(corpus)
#inspect(corpusmatrix)

##Häufigkeitsmatrix
#findFreqTerms(corpusmatrix, 10)

##Korrelationen zwischen einzelnen Wörtern
#findAssocs(corpusmatrix, "fehler", 0.5)

##wir entfernen sehr seltene wörter, die unwichtig bzgl. der korrelation sind
#removeSparseTerms(corpusmatrix, 0.4)
#inspect(corpusmatrix)

#wir schreiben in eine neue spalte die editierten texte
textframe[,3] <- corpusdataframe




#FASTRTEXT

#fuer fastrtext benötigen wir die daten in einem bestimmten format, dafür laden wir die benötigten spalten in 
#ein neues dataframe

textframe2 <- subset(textframe, select=c("id", "text_new"))
names(textframe2) <- c("class.text", "text")

# Wir mixen den Datensatz durch vor dem splitten
# hier wäre besser 70/30 pro kategorie aber das muss man coden
# wird nicht mehr benötigt: textframe2 <- textframe2[sample(nrow(textframe2)),]


if (type == 1) { #nur wenn MongoDB
# Alle Zeilen mit mehreren IDs löschen, nicht sauber aber die müssen erst einmal weg
textframe2 <- textframe2[!grepl(",", textframe2$class.text),]

#hier zähle ich einmalig die anzahl der texte pro kategorie um sie danach zu löschen
#das muss man automatisieren
#textframe3 <- aggregate(cbind(count = text) ~ class.text, data = textframe2, FUN = function(x){NROW(x)})
# delete cats 59, 70, 72, 85, 86, 87, 92, 97
textframe2 <- textframe2[!grepl("59", textframe2$class.text),]
textframe2 <- textframe2[!grepl("70", textframe2$class.text),]
textframe2 <- textframe2[!grepl("72", textframe2$class.text),]
textframe2 <- textframe2[!grepl("85", textframe2$class.text),]
textframe2 <- textframe2[!grepl("86", textframe2$class.text),]
textframe2 <- textframe2[!grepl("87", textframe2$class.text),]
textframe2 <- textframe2[!grepl("92", textframe2$class.text),]
textframe2 <- textframe2[!grepl("97", textframe2$class.text),]

}

#trainings und test datensätze
#jede Kategorie wird 70/30 aufgeteilt

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


#das muss verbessert werden
#textframe2 %>% 
#  add_count(class.text)



#for (year in c(2010,2011,2012,2013,2014,2015)){
#  print(paste("The year is", year))
#}

#create dataframe to save results of the models
results <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("id", "time", "dim","lr","ngram", "epoch","verbose", "result"))
id <- 1


for (i in epoche){ 
  for (j in dime){ 
    for (k in lr){ 
      for (l in ngram){ 
        for (m in verbose){ 




# Daten in richtiges Format für Model
tmp_file_model <- tempfile()
?tempfile()

# label vor die kategorien
train_labels <- paste0("__label__", train_sentences[,"class.text"])


# Text in Kleinbuchstaben
train_texts <- tolower(train_sentences[,"text"])
# View(train_texts)
# Kategorie mit Prefix + Text aus Kleinbuchstaben
train_to_write <- paste(train_labels, train_texts)
# View(train_to_write)
train_tmp_file_txt <- tempfile()
# Daten in tempfile schreiben
writeLines(text = train_to_write, con = train_tmp_file_txt)


# gleiches Vorgehen für Testdatensatz
test_labels <- paste0("__label__", test_sentences[,"class.text"])
test_labels_without_prefix <- test_sentences[,"class.text"]
test_texts <- tolower(test_sentences[,"text"])
test_to_write <- paste(test_labels, test_texts)


# Modell trainieren und in tempfile abspeichern; Parameter können angepasst werden 
execute(commands = c("supervised", "-input", train_tmp_file_txt, "-output", tmp_file_model, "-dim", j, "-lr", k, "-epoch", i, "-wordNgrams", l, "-verbose", m))

#execute(commands = c("supervised", "-input", train_tmp_file_txt, "-output", tmp_file_model, "-dim", 200, "-lr", 1, "-epoch", 20, "-wordNgrams", 2, "-verbose", 5))

# Modell laden
model <- load_model(tmp_file_model)

# Modell auf Testdatensatz anwenden
predictions <- predict(model, sentences = test_to_write)
# Vorhergesagte Kategorie und Wahrscheinlichkeit ausgeben
#print(head(predictions,100))
#summary(unlist(predictions))
# Prozentualer Anteil, in dem das Model richtig lag
result <- mean(names(unlist(predictions)) == test_labels_without_prefix)

# because there is only one category by observation, hamming loss will be the same
#get_hamming_loss(as.list(test_labels_without_prefix), predictions)

# you can get flat list of results when you are retrieving only one label per observation
#print(head(predict(model, sentences = test_to_write, simplify = TRUE)))

#save results
results[nrow(results) + 1,] = list(id,format(Sys.time(), format="%d.%m.%Y - %H:%M:%S"),j,k,l,i,m,result)


id <- id+1

show(result)

# free memory and delete Files in Temp-Ordner
# mit tempdir() auslesen welcher der Temp-Ordner ist, dann den Inhalt im nächsten Schritt immer löschen
do.call(file.remove, list(list.files(tempdir(), full.names = TRUE)))
unlink(train_tmp_file_txt)
unlink(tmp_file_model)
gc()

}}}}}
