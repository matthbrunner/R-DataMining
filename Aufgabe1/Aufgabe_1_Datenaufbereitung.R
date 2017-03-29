#-------------------------------------------------------------------------------
# Name:        Aufgabe_1_Datenaufbereitung.R
# Purpose:     Aufgabe Datenaufbereitung
#
# Author:      Matthias Brunner
#
# Created:     28.03.2017
#-------------------------------------------------------------------------------

# Aufgabe 1 --------------------------------------------------------------------
# 
# 1. Datenaufbereitung
# 
# 
# Ein findiger Programmierer hat den Apache HTTPD server über ein Modul so 
# angepasst dass neben normalen Seitenzugriffen auch mitgelogged wird um welchen 
# Mitarbeiter es sich handelt, zu welcher Abteilung er gehört und welchen Kunden 
# er gerade zugegriffen hat. Leider hat er sich keine grossen Gedanken gemacht 
# wie ein Data Scientist die Daten verarbeitet.
# 
# 
# Der Link zur Datei ist hier: 
# https://raw.githubusercontent.com/romeokienzler/developerWorks/master/log
# 
# Hier wurde einfach über ein Apache HTTPD modul für jeden Request eine 
# 2. Zeile eingefügt in der der Payload die gewünschten Informationen enthält, 
# in folgerner Reihenfolge: departmentid, employeeid, clientid
# 
# a) Lesen Sie die LOG Datei mittels R ein und bereiten Sie so auf, dass daraus
# ein Data Frame entsteht welcher folgendes Format hat: Spalte 1> employeeid, 
# Spalte 2> departmentid, Spalte 3> clientid

# Setzen des Workdirectory Pfad
setwd("C:/Workspace/Weiterbildung/DataMining/R-Projects/DataMining/Aufgabe1")
library(dplyr)
library(stringr)

# Laden der Daten von der Webseite
data <- read.table("https://raw.githubusercontent.com/romeokienzler/developerWorks/master/log")
head(data)
write.csv(data, "logging/testdata.csv" ,row.names=TRUE, na="",col.names=TRUE, sep=";")

# Es gibt zwei Varianten um die Daten zu filtern Variante 1 ist ein Schritt für
# Schritt Weg. Mit der Variante 2 geht es genau so in dyplr auf einer Zeile!
# Variante 1:
# --- START------
# make an index eg. every 2th
# ind <- seq(1, nrow(data), by=2)
# head(ind)
# 
# # make subset --> this would choose every `ind` row
# #df.log <- data[ind, ]
# 
# # --> this would exclude ale `ind` row
# df.log <- data[-ind, ]
# 
# head(df.log)
# 
# drops <- c("V1", "V2", "V3", "V5", "V7", "V8")
# df.log.f <- df.log[ , !(names(df.log) %in% drops)]
# head(df.log.f)
# 
# substr(df.log.f$V4, 12, 2)
# head(df.log.f)
# 
# # res <- sapply(strsplit(toString(df.log.f$V4),split= "\\:"),'[',2)
# 
# res <- strsplit(toString(df.log.f$V6),split= "\\,")[[1]]
# 
# df.log.f$departmentid <- trimws(res[seq(1, length(res), by=3)])
# df.log.f$employeeid <- trimws(res[seq(2, length(res), by=3)])
# df.log.f$clientid <- trimws(res[seq(3, length(res), by=3)])
# 
# drops <- c("V4", "V6")
# df.log.f <- df.log.f[ , !(names(df.log.f) %in% drops)]
# --- END------


# Variante 2:
# --- START------
df.log.data <- data %>%
  mutate(employeeid = trimws(str_replace(str_extract(V6, " [0-9]+ ,"),",", " ")),
         departmentid = trimws(str_replace_all(str_extract(V6, ", [0-9]+ ,"),",", " ")),
         clientid = trimws(str_replace(str_extract(V6, ", [0-9]+ "),",", " "))) %>%
  filter(grepl(" \\d",V6)) %>% 
  select(employeeid, departmentid, clientid)
# --- END------
# 
# 
# b) Erweitern Sie Ihr R Script dass nun auch die Stunde des Zugriffsdatums aus 
# der LOG Datei in der ersten Zeile des Data Frame erscheint. Das Format ist 
# nun Spalte 1 > hour, Spalte 2> employeeid, Spalte 3> departmentid, 
# Spalte 4> clientid
# 

# # Variante 1:
# # --- START------
# res <- strsplit(toString(df.log$V4),split= "\\:")[[1]]
# df.log.f$hour <- res[seq(2, length(res), by=3)]
# df.log.f[c("hour", "departmentid", "employeeid", "clientid")]
# --- END------

# Variante 2:
# --- START------
df.log.data <- data %>%
  mutate(hour = str_sub(str_extract(V4, "\\d{4}[:]\\d{2}?"),start = -2),
         employeeid = trimws(str_replace(str_extract(V6, " [0-9]+ ,"),",", " ")),
         departmentid = trimws(str_replace_all(str_extract(V6, ", [0-9]+ ,"),",", " ")),
         clientid = trimws(str_replace(str_extract(V6, ", [0-9]+ "),",", " "))) %>%
  filter(grepl(" \\d",V6)) %>% 
  select(hour, employeeid, departmentid, clientid)

# --- END------

# Daten Ausgabe in eine CSV datei
write.csv(df.log.data, "logging/cleanupData.csv" ,row.names=TRUE, na="",col.names=TRUE, sep=",")


# Aufgabe 2 --------------------------------------------------------------------
# 2. Die bekommen nun das aus Aufgabe 1 extrahierte CSV file von Ihrem 
# Junior Data Scientist geliefert. Die Forensik Abteilung möchte wissen ob in 
# diesem Trace anomales Verhalten auftritt. Können Sie helfen?
# 
# Der Link zur Datei ist hier: https://raw.githubusercontent.com/romeokienzler/developerWorks/master/testdata.csv
# 
data.forensik <- read.csv("https://raw.githubusercontent.com/romeokienzler/developerWorks/master/testdata.csv", sep = ",", header = TRUE, stringsAsFactors=FALSE)
head(data.forensik)

# Ändern der Columns name in folgende Kategorien

# ID mit Zahlen 1-1000 = ClientID
# ID mit Zahlen 1-100 = EmployeeID
# ID mit Zahlen 1-10 = DepartmentID
keeps <- c("X","hour","departmentid","employeeid","clientid")
colnames(data.forensik) <- keeps

class(data.forensik$employeeid)

# 1. Histogramm erstellen über die Zeiten
hist(data.forensik$hour)

# filtern nach den frühen Stunden
early.hour <- data.forensik %>% 
  filter(hour < 1)

head(early)

# 2. Herausfinden bei welchem 
employee.most.access <- early %>%
  group_by(employeeid) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n == max(n))

# Der Employee 23 hat am meisten zugriffe in den frühen Stunden.
