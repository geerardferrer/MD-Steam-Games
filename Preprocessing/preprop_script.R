setwd("PATH")

#Load full dataset
dd <- read.csv("st_gms.csv", header = TRUE, sep = ",", dec = ".")

#Clean /" character
dd$additional_content <- gsub("'", "", dd$additional_content, fixed=T)
dd$additional_content <- gsub("\"", "'", dd$additional_content, fixed=T)

dd$developers <- gsub("\"", "'", dd$developers, fixed=T)

dd$publishers <- gsub("\"", "'", dd$publishers, fixed=T)

#Discard missing values (we have to choose 5000 out of 70000 rows, so we start discarding)
dd <- dd[dd$publishers != "[]", ]
dd <- dd[dd$developers != "[]", ]

#Select games from more repeated publishers until get 5000
publisher_counts <- table(dd$publishers)

sorted_publishers <- names(sort(publisher_counts, decreasing = TRUE))

selected_games <- data.frame()
for (pub in sorted_publishers) {
  games_from_pub <- dd[dd$publishers == pub, ]
  selected_games <- rbind(selected_games, games_from_pub)
  if (nrow(selected_games) >= 5000) {
    selected_games <- selected_games[1:5000, ]
    break
  }
}
unique(selected_games$developers)
unique(selected_games$publishers)

#Select games from more repeated developers until get 5000
developer_counts <- table(dd$developers)

sorted_developers <- names(sort(developer_counts, decreasing = TRUE))

selected_games_2 <- data.frame()
for (dev in sorted_developers) {
  games_from_dev <- dd[dd$developers == dev, ]
  selected_games_2 <- rbind(selected_games_2, games_from_dev)
  if (nrow(selected_games_2) >= 5000) {
    selected_games_2 <- selected_games_2[1:5000, ]
    break
  }
}
unique(selected_games_2$developers)
unique(selected_games_2$publishers)

#Save games into csv
write.csv(selected_games, "PATH/stm_gms_pub.csv", row.names = FALSE)
write.csv(selected_games_2, "PATH/stm_gms_dev.csv", row.names = FALSE)

#Preprocess stm_gms_pub.csv or stm_gms_dev.csv
dd <- read.csv("PATH/stm_gms_pub.csv", header = TRUE, sep = ",", dec = ".")
dd <- read.csv("PATH/stm_gms_dev.csv", header = TRUE, sep = ",", dec = ".")

#Multivalued categories into bool variables
revisar <- c()
for(i in 1:5000) {
  valor <- dd$categories[i]
  valor <- gsub("]", "", valor, fixed = T)
  valor <- gsub("[", "", valor, fixed = T)
  valorsFinal <- strsplit(valor, split = ", ")[[1]]
  valorsFinal <- gsub("'", "", valorsFinal)
  revisar <- c(revisar, valorsFinal)  
}
unique(revisar)

for (cat in revisar) {
  dd[grep(cat, dd$categories), cat] <- 1; dd[which(is.na(dd[,cat])), cat] <- 0
}

#Multivalued genres into bool variables
revisar <- c()
for(i in 1:5000) {
  valor <- dd$genres[i]
  valor <- gsub("]", "", valor, fixed = T)
  valor <- gsub("[", "", valor, fixed = T)
  valorsFinal <- strsplit(valor, split = ", ")[[1]]
  valorsFinal <- gsub("'", "", valorsFinal)
  revisar <- c(revisar, valorsFinal)  
}
unique(revisar)

for (gen in revisar) {
  dd[grep(gen, dd$genres), gen] <- 1; dd[which(is.na(dd[,gen])), gen] <- 0
}

#Multivalued platforms into bool variables
revisar <- c()
for(i in 1:5000) {
  valor <- dd$platforms[i]
  valor <- gsub("]", "", valor, fixed = T)
  valor <- gsub("[", "", valor, fixed = T)
  valorsFinal <- strsplit(valor, split = ", ")[[1]]
  valorsFinal <- gsub("'", "", valorsFinal)
  revisar <- c(revisar, valorsFinal)  
}
unique(revisar)

for (plat in revisar) {
  dd[grep(plat, dd$platforms), plat] <- 1; dd[which(is.na(dd[,plat])), plat] <- 0
}

#Count number of additional contents
revisar <- c()
for(i in 1:5000) {
  valor <- dd$additional_content[i]
  valor <- gsub("]", "", valor, fixed = T)
  valor <- gsub("[", "", valor, fixed = T)
  valorsFinal <- strsplit(valor, split = ", ")[[1]]
  valorsFinal <- gsub("'", "", valorsFinal)
  revisar <- c(revisar, valorsFinal)  
}
revisar <- gsub("\"", "", revisar, fixed=T)
unique(revisar)

dd$num_ac <- 0

for (adc in revisar) {
  dd$num_ac <- dd$num_ac + 
    as.integer(grepl(adc, dd$additional_content, fixed = TRUE))
}

#Count number of developers
contar_desarrolladores <- function(x) {
  # Eliminar corchetes y comillas
  x <- gsub("^\\[|\\]$", "", x)
  # Dividir por comillas y coma
  desarrolladores <- strsplit(x, "', '")[[1]]
  # Eliminar comillas restantes
  desarrolladores <- gsub("^'|'$", "", desarrolladores)
  # Contar el número de elementos
  return(length(desarrolladores))
}
dd$num_dev <- sapply(dd$developers, contar_desarrolladores)

#Save stm_gms_pub.csv or stm_gms_pub.csv
write.csv(dd, "PATH/stm_gms_pub_prep.csv", row.names = FALSE)
write.csv(dd, "PATH/stm_gms_dev_prep.csv", row.names = FALSE)