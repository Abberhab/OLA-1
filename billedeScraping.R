library(rvest)
library(tidyverse)
library(stringr)

# Opratter mappe til at gemme billeder i hvis den ikke eksisterer
if (!file.exists("Teams")) {
  dir.create("Teams")
}

# Kode til webscraping af billeder
# Ekstraklasa (1. liga)
baseurl <- "https://www.ekstraklasa.org/"
index <- read_html(baseurl)

# Yeet 
for (i in 1:18) {
  
  teamTag <- paste0(".duration-100:nth-child(", i, ") .h-full")
  
  imageLink <- index %>% html_nodes(teamTag) %>% html_attr("src")
  imageLink <- imageLink[[2]]
  
  imageName <- index %>% html_nodes(teamTag) %>% html_attr("alt")
  imageName <- imageName[[2]]
  imageName <- gsub("\\s", "", imageName)
  
  download.file(imageLink, paste0("Teams/",imageName, ".png"), mode = "wb")
}

# Webscraping af anden polsk liga
# 1 liga (2. liga)
base_url2 <- "https://www.1liga.org/tabela-sezon-2023-2024"
index2 <- read_html(base_url2)

tag <- ".team-logo-container"
teamList <- index2 %>% html_nodes(tag)
urlPattern <- 'src="([^"]*)"'
namePattern <- 'alt=\\"([^"]+)'

# double Yeet
for (i in 1:length(teamList)) {
  result <- str_extract(as.character(teamList[i]), urlPattern)
  result <- str_match(result, 'src="(.*)"')[, 2]
  imageLink <- paste0("https://www.1liga.org/", result)
  
  imageName <- str_extract(as.character(teamList[i]), namePattern)
  imageName <- substr(imageName, start = 6, stop = nchar(test) - 7)
  imageName <- gsub("\\s", "", imageName)
  
  download.file(imageLink, paste0("Teams/",imageName, ".png"), mode = "wb")
}




