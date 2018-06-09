rm(list=ls())

# install.packages("fastrtext")
library("fastrtext")

# getwd()
# setwd("/Users/felix/RKurs/work")

# Einlesen der vorbereiteten Testdatei
d <- read.csv("categoriestest.csv", sep=";")
# Spalten umbennen, damit Tutorial Code nicht angepasst werden muss
names(d) <- c("class.text", "text")
# View(d)

# Datensätze in Trainings- und Testdatensatz aufteilen
train_sentences <- head(d, 124)
test_sentences <- tail(d, 48)

# Im Paket vorinstallierte Daten nutzen
# data("test_sentences")
# data("train_sentences")

# View(test_sentences)
# View(train_sentences)
# Daten in richtiges Format für Model
tmp_file_model <- tempfile()
?tempfile()
# Prefix "__label__" vor Kategorie wird benötigt, damit das Modell die Kategorie erkennt
train_labels <- paste0("__label__", train_sentences[,"class.text"])
# View(train_labels)
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
# Kategorien für Überprüfen der Einordnungen
test_labels_without_prefix <- test_sentences[,"class.text"]
test_texts <- tolower(test_sentences[,"text"])
test_to_write <- paste(test_labels, test_texts)
# View(test_to_write)

# Modell trainieren und in tempfile abspeichern; Parameter können angepasst werden 
execute(commands = c("supervised", "-input", train_tmp_file_txt, "-output", tmp_file_model, "-dim", 20, "-lr", 1, "-epoch", 20, "-wordNgrams", 2, "-verbose", 1))

# Modell laden
model <- load_model(tmp_file_model)

# Modell auf Testdatensatz anwenden
predictions <- predict(model, sentences = test_to_write)

# Vorhergesagte Kategorie und Wahrscheinlichkeit ausgeben
print(head(predictions,100))
summary(unlist(predictions))
# Prozentualer Anteil, in dem das Model richtig lag
mean(names(unlist(predictions)) == test_labels_without_prefix)

# because there is only one category by observation, hamming loss will be the same
get_hamming_loss(as.list(test_labels_without_prefix), predictions)

# you can get flat list of results when you are retrieving only one label per observation
print(head(predict(model, sentences = test_to_write, simplify = TRUE)))

# free memory
unlink(train_tmp_file_txt)
unlink(tmp_file_model)
rm(model)
gc()
