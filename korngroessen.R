# Skript: Korngrössenanalyse am GfZ Potsdam 
# Aufgabe: Transformation der Ausgabe
# 17-03-09, https://keybase.io/larsschulz

# Libraries
install.packages("aqp") #momentan nicht benötigt aber interessante Funktionen
library("aqp")
library("gdata")

# Arbeitsverzeichnis auswählen
setwd("~/Dropbox/Jobs Active/Studium/TU-1611 Kiecker Naturschutzgebiet/1-Entwicklung/9-Laborarbeiten/Korngroessen")


# Daten einlesen
# https://martinsbioblogg.wordpress.com/2014/03/06/using-r-common-errors-in-table-import/
korngroessen <- read.delim("data/all kieker samples.txt", fileEncoding="UCS-2LE", header = T, dec=",", stringsAsFactors=F)

str(korngroessen)
summary(korngroessen)
names(korngroessen)
colnames(korngroessen[-1,45:320])

# Simplify data
korngroessen_data <- cbind(korngroessen[,'Sample.Name'], korngroessen[,45:320])
colnames(korngroessen_data)[1] <- "Sample.Name"
str(korngroessen_data)
attach(korngroessen_data)

# Umbenennen
zeile_1 <- korngroessen_data[1,]
names(zeile_1) <- NULL
zeile_1
new_headers <- paste (head( c("CHECK",zeile_1[,-1]), -1 ), "µm")

names(korngroessen_data) <- c("Sample.Name", new_headers)
str(korngroessen_data)

# modulo, jede dritte Spalte auswählen (Spalte q in Rohdaten)
c <- 1:ncol(korngroessen_data)
korngroessen_data[, c%%3==0]

korngroessen_clean <- cbind(korngroessen_data[,1], korngroessen_data[, c%%3==0])

# Spalten für die Statistik ausgeben
variables_in_cols <- names(korngroessen_clean[,-1])

# Mittel pro Faktorstufe
korngroessen_stats <- aggregate(korngroessen_clean[, variables_in_cols], list(Sample.Name), mean)

korngroessen_stats$ton <- rowSums(korngroessen_stats[, 2:40])
korngroessen_stats$schluff <- rowSums(korngroessen_stats[, 41:65])
korngroessen_stats$sand <- rowSums(korngroessen_stats[, 66:93])

# Drop columns not needed
korngroessen_overview <- korngroessen_stats[,c(1,94:96)]

# Klassifikation nach KA-5
# TODO

write.csv(korngroessen_stats, file="korngroessen_overview.csv")

