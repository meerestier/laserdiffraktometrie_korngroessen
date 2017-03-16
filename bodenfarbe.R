# Skript: Bodenfarbe(nach KA-5, Feld 28)
# Aufgabe: Ermitteln der RGB Werte
# 17-03-09, https://keybase.io/larsschulz

# set wd
setwd("~/Dropbox/Jobs Active/Studium/TU-1611 Kiecker Naturschutzgebiet/1-Entwicklung/9-Laborarbeiten/Korngroessen")

# install.packages("aqp") #momentan nicht benötigt aber interessante Funktionen
library("aqp")

# Daten einlesen
horizonte <- read.delim("data/horizonte.txt", fileEncoding="UCS-2LE", header = T, dec=",", stringsAsFactors=F)
data("munsell") # munsell farbwerte

attach(horizonte)
str(horizonte)
bodenfarbe <- horizonte[,c(1,37)] # Probe und Bodenfarbe
colnames(bodenfarbe)[2] <- "munsell"

bodenfarbe$munsell <- gsub("/", "_", bodenfarbe$munsell)
bodenfarbe$munsell <- gsub(",", ".", bodenfarbe$munsell)

bodenfarbe$hue <- sapply(strsplit(bodenfarbe$munsell, split=' ', fixed=TRUE), '[',1)
bodenfarbe$chroma_value <- sapply(strsplit(bodenfarbe$munsell, split=' ', fixed=TRUE), '[',2)
bodenfarbe$value <- sapply(strsplit(bodenfarbe$chroma_value, split='_', fixed=TRUE), '[',1)
bodenfarbe$chroma <- sapply(strsplit(bodenfarbe$munsell, split='_', fixed=TRUE), '[',2)
bodenfarbe$chroma_value <- NULL
bodenfarbe$munsell <- NULL

rgb_values_hex <- munsell2rgb(bodenfarbe$hue, bodenfarbe$value, bodenfarbe$chroma, alpha=1, 
            maxColorValue=1, return_triplets=F)


bodenfarbe <- cbind(bodenfarbe, rgb_values_hex)
