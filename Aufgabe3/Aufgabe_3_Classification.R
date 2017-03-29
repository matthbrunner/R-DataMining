#-------------------------------------------------------------------------------
# Name:        Aufgabe_3_Classification.R
# Purpose:     Aufgabe Datenaufbereitung
#
# Author:      Matthias Brunner
#
# Created:     28.03.2017
#-------------------------------------------------------------------------------

## Uebung 3 - Classification
## Das MNIST Datenset ist sozusagen das HelloWorld Program wenn es um Classification mit DeepLearning
## geht. Wir missbrauchen hier dieses Datenset für traditionelle Classification.
## 1. Führen Sie u.g. Source Code aus und beantworten Sie folgende Fragen
## a) Wieviele Bilder sind in der Matrix mnist_matrix encodiert 

# Antwort a). [1] 42000

## b) Da es sich um einen Supervised Machine Learning task handelt muss ein Label (Target Variable) 
## bereitgestellt sein - welche Spalte der Matrix enthält das Label?

# Antwort b.) Spalte label

## c) Wieviele Pixel haben die Bilder?

# Antwort c.) 784 pixel

## d) Wie hoch/breit sind die Bilder?

# Antwort d.) 28 x 28 pixel


mnist_matrix = read.csv( 'https://github.com/romeokienzler/developerWorks/raw/master/train.csv' )

# Ausgabe Anzahl reihen
nrow(mnist_matrix)

# Ausgabe der Dimension des Data Frame
dim(mnist_matrix)

# Ausgabe der nummern im Label
sort(unique(mnist_matrix[,1]))

par( mfrow = c(10,10), mai = c(0,0,0,0))

# Darstellen einer Teilmenge von 1 bis 100 Zeile
for(i in 1:100){
  y = as.matrix(mnist_matrix[i, 2:785])
  dim(y) = c(28, 28)
  image( y[,nrow(y):1], axes = FALSE, col = gray(255:0 / 255))
  text( 0.2, 0, mnist_matrix[i,1], cex = 3, col = 2, pos = c(3,4))
}


## 2. Nehmen Sie einen Classifier Ihrer Wahl und trainieren Sie Ihn mit der bereitgestellten Matrix.
## a) Teilen Sie die Matrix in ein sinnvolles Training und Test set auf, lesen Sie hierzu diesen
## Thread: http://stats.stackexchange.com/questions/19048/what-is-the-difference-between-test-set-and-validation-set
## (Ein Validation Set wird hier nicht benötigt da nicht erwartet wird Parameter des
## Classifiers zu tunen)

# Train with 50% Test with 25% Execute with 25%.
# Benutze die library caret um die Datensätze nach Prozent zu filtern!
# Nach absprache mit Romeo aber Training 70% und Test 30% wählen
library(caret)

# Erstellen Training Data Frame
training.index <- createDataPartition(mnist_matrix$label, p = .07, list = FALSE, times = 2)
training.df <- mnist_matrix[training.index,]

# Erstellen Test Data Frame
test.index <- createDataPartition(mnist_matrix$label, p = .03, list = FALSE, times = 1)
test.df <- mnist_matrix[test.index,]

## Erstellen des Application Data Frame diese wird jedoch nicht genutzt
# application.index <- createDataPartition(mnist_matrix$label, p = .25, list = FALSE, times = 1)
# application.df <- mnist_matrix[application.index,]


## b) Verwenden Sie nun das Training Set um einen Classifier Ihrer Wahl zu trainieren
# Vom Training Data Frame das label entferenen
x.training <- subset(training.df, select=-training.df$label)
dim(x.training)
# [1] 58806   776

# Bei der oberigen Methode hat es die Anzahl spalten Reduziert! Darum hier nocheinmal
x.training <- training.df[-1]
dim(x.training)
# [1] 58806   784

# Erstellen des label vektors
x.training.label <- training.df[1]

# Erstellen des test Data Frame
x.test <- subset(test.df, select=-test.df$label)
dim(x.test)
# [1] 12601   776

x.test <- test.df[-1]
dim(x.test)
# [1] 58806   784

# Erstellen des label vektors
x.test.label <- test.df[1]

# -- Erstellen einer funktion welch die Resultate Auswertet, für == und !=
# da diese bei jedem Datensatz unterschiedlich behandelt werden können. --------
get.good.bad <- function(prediction, to.test){
  truthVector <- prediction == to.test
  good <- length(truthVector[truthVector==TRUE])
  bad <- length(truthVector[truthVector==FALSE])
  result <- good/(good + bad)
  message(sprintf("Result == : %s", result))
  
  truthVector <- prediction != to.test
  good <- length(truthVector[truthVector==TRUE])
  bad <- length(truthVector[truthVector==FALSE])
  result <- good/(good + bad)
  message(sprintf("Result != : %s", result))
}

# Laden der Library "e1071" für das svm
# -- Variante 1 mit svm --------------------------------------------------------
# Modell Trainieren
library("e1071")
svm_model.training <- svm(x.training, training.df$label)
summary(svm_model.training)

pred.model.training <- predict(svm_model.training, x.training)
table(pred.model.training,test.df$label)

# Funktion welche die Werte für Gut und Schlecht ausgibt!
get.good.bad(pred, test.df$label)



## c) Berechnen Sie den Prozentsatz der richtig klassifizierten Daten indem Sie Ihren
## trainierten Classifier auf das Test Set anwenden (Hinweis: Die Qualität des Classifiers
## wird nicht bewertet)

# Berechnung mit dem test Data Frame
pred <- predict(svm_model, x.test)


# Funktion welche die Werte für Gut und Schlecht ausgibt!
get.good.bad(pred, y)

# Somit kommt immer nur 0 oder 1 heraus.

table(pred, x.test.label)
# Mit dem SVM bin ich auf bei der Berechnung bin ich auf keinen bei der Prediction
# ein ähnliches Resultat gekommen wie dieses
#                  y.training.label
# pred                  0    1    2    3    4    5    6    7    8    9
# 3.41120407420946 2889 3221 2986 3069    0    0    0    0    0    0
# 4.09948532420946    0    0    0    0 1091    0    0    0    0    0
# 4.10026657420946    0    0    0    0 1737    0    0    0    0    0
# 4.89948532420946    0    0    0    0    0  785    0    0    0    0
# 4.90026657420946    0    0    0    0    0 1885    0    0    0    0
# 5.41120407420946    0    0    0    0    0    0 2877 3087 2846 2930
# ich habe es nicht geschaft richtig zu machen!

# -- Variante 2 mit knn --------------------------------------------------------
# Ich habe mich entschieden das ganze mit dem knn nochmals zu erstellen.
library(class)

# Bei einem 70 / 30% Verhältnis
# mit k = 1 kommt bei einem 
prediction.knn <- knn(train = x.training, test = x.test, cl = training.df$label, k = 1)

# Funktion welche die Werte für Gut und Schlecht ausgibt!
get.good.bad(prediction.knn, y)

# Resultat
# Result == : 0.988969129434172
# Result != : 0.0110308705658281


# Bei einem 70 / 30% Verhältnis
# mit k = 6 kommt bei einem 
prediction.knn <- knn(train = x.training, test = x.test, cl = training.df$label, k = 6)

# Funktion welche die Werte für Gut und Schlecht ausgibt!
get.good.bad(prediction.knn, y.validation.label)

# Result == : 0.97198635028966
# Result != : 0.0280136497103404

# Bei Tests mit 7/3 % des Data Frame hat sich gezeigt, dass ein k mit 6 ein besseres
# Resultat ergeben hat wie wenn der ganze Datensatz verwendet wird.









