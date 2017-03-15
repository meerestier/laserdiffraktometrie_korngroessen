# Skript: Korngrössenanalyse am GfZ Potsdam 
# Aufgabe: Transformation der Ausgabe
# 17-03-09, https://keybase.io/larsschulz

# Libraries
# install.packages("aqp") #momentan nicht benötigt aber interessante Funktionen
# library("aqp") # Für Horizont-Plots
library("gdata") # Für Datenimport und -manipulation etc.

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
colnames(korngroessen_data)[1] <- "Sample"
str(korngroessen_data)
attach(korngroessen_data)

# Umbenennen
zeile_1 <- korngroessen_data[1,]
names(zeile_1) <- NULL
zeile_1
new_headers <- paste (head( c("CHECK",zeile_1[,-1]), -1 ), "µm")

names(korngroessen_data) <- c("Sample", new_headers)
str(korngroessen_data)

# modulo, jede dritte Spalte auswählen (Spalte q in Rohdaten)
c <- 1:ncol(korngroessen_data)
korngroessen_data[, c%%3==0]

korngroessen_clean <- cbind(korngroessen_data[,1], korngroessen_data[, c%%3==0])

# Spalten für die Statistik ausgeben
variables_in_cols <- names(korngroessen_clean[,-1])

# Mittel pro Faktorstufe
korngroessen_stats <- aggregate(korngroessen_clean[, variables_in_cols], list(Sample), mean)

# Create overview of classes
korngroessen_overview <- as.data.frame(korngroessen_stats[,1])
colnames(korngroessen_overview)[1] <- "Sample"

korngroessen_overview$ton <- rowSums(korngroessen_stats[, 2:40]) # ton insgesamt
korngroessen_overview$schluff <- rowSums(korngroessen_stats[, 41:65]) # schluff insgesamt
korngroessen_overview$sand <- rowSums(korngroessen_stats[, 66:93]) # sand insgesamt

# Mittel pro Faktorstufe (fein)
names(korngroessen_stats)
korngroessen_overview$fT <- rowSums(korngroessen_stats[, 2:23]) # feinton
korngroessen_overview$mT <- rowSums(korngroessen_stats[, 24:31]) # mittelton
korngroessen_overview$gT <- rowSums(korngroessen_stats[, 32:40]) # grobton

korngroessen_overview$fU <- rowSums(korngroessen_stats[, 41:48]) # feinschluff
korngroessen_overview$mU <- rowSums(korngroessen_stats[, 49:57]) # mittelschluff
korngroessen_overview$gU <- rowSums(korngroessen_stats[, 58:65]) # grobschluff

korngroessen_overview$fS <- rowSums(korngroessen_stats[, 66:74]) # feinsand
korngroessen_overview$mS <- rowSums(korngroessen_stats[, 75:82]) # mittelsand
korngroessen_overview$gS <- rowSums(korngroessen_stats[, 83:93]) # grobsand


      # Optional ----
      # Anreichern mit zusätlichen Informationen (auskommentieren, falls nicht erforderlich)
      horizont_infos <- read.xls("~/Dropbox/Jobs Active/Studium/TU-1611 Kiecker Naturschutzgebiet/1-Entwicklung/9-Laborarbeiten/Probenliste v3.xls", 6) # Probennummern holen
      colnames(horizont_infos)[3] <- "Sample"
      korngroessen_overview <- merge(korngroessen_overview, horizont_infos[2:3], by = "Sample")
      korngroessen_overview$Sample <- korngroessen_overview$Probe
      korngroessen_overview$Probe <- NULL
      
      # Reihenfolge nach Probennummer
      korngroessen_overview <- korngroessen_overview[with(korngroessen_overview, order(Sample)), ]
      attach(korngroessen_overview)

# Klassifikation nach KA-5
# TODO

write.csv(korngroessen_overview, file="korngroessen_overview.csv", fileEncoding = "UTF-8")

# Darstellung ----

matrix_names <- t (korngroessen_overview[,1])
korngroessen_matrix <- t (korngroessen_overview[,c(2,8:13)]) # matrix und transposed: Ton als eine Faktorstufe, Schluff und Sand aufgesplittet
str(korngroessen_matrix)
barplot(korngroessen_matrix )


x <- barplot(korngroessen_matrix, col=c("red", "cadetblue2", "cadetblue3", "cadetblue4", "goldenrod2", "goldenrod3", "goldenrod4"), 
             legend=TRUE, border=NA, xlim=NULL, args.legend=
               list(bty="n", border=NA), 
             ylab="Cumulative percentage", xlab="Sample", las=2, names=matrix_names)
text(x, ton/2, labels=round(korngroessen_overview$ton), col="black", srt = 90)
text(x, ton+fU/2, labels=round(korngroessen_overview$fU), col="black", srt = 90)
text(x, ton+fU+mU/2, labels=round(korngroessen_overview$mU), col="black", srt = 90)
text(x, ton+fU+mU+gU/2, labels=round(korngroessen_overview$gU), col="black", srt = 90)
text(x, ton+fU+mU+gU+fS/2, labels=round(korngroessen_overview$gU), col="black", srt = 90)
text(x, ton+fU+mU+gU+fS+mS/2, labels=round(korngroessen_overview$gU), col="black", srt = 90)
text(x, ton+fU+mU+gU+fS+mS+gS/2, labels=round(korngroessen_overview$gU), col="black", srt = 90)

dev.copy2pdf(file = "plots/korngroessen_plot.pdf")

