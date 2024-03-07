library(mongolite)
library(tidyverse)
library(png)
library(grid)
library(gridGraphics)
library(ggsoccer)

setwd("~/Desktop/Dataanalyse 2. Semester/Projekter/OLA 1/Scripts OLA1, OPG1")
########

con <-  mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "games")
con1 <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "matches")

games <- con$find(query = '{"type.primary": "pass"}')
matches <- con1$find(query = '{"seasonId": {"$in": [188088, 186215]}}', fields = '{"_id": 1}')


#### temp file ##################
saveRDS(games, "games")####
saveRDS(matches, "matches")####
games <- readRDS("games")####
matches <- readRDS("matches")####
#################################

#sammenligner matchid's ud fra de korrekte ligaer.
polsk_liga <- games[games$matchId %in% matches$`_id`,]

mean_length_pass <- polsk_liga %>%
  group_by(team$name) %>%
  summarize(mean_pass = mean(pass$length)) 

colnames(mean_length_pass) <- c("team","avgPassLength")

################################# Opgave 1.2 #################################
# Bestemmer farve til plots
plotColor <- "#c7343b"

avg_pass_length <- ggplot(mean_length_pass, aes(x = reorder(team, -avgPassLength), y = avgPassLength)) + 
  geom_col(fill = plotColor) +
  geom_text(aes(label = round(avgPassLength,2)),
            vjust = 2,
            size = 2.8,
            color = "white")+
  labs(title = "Korona Kielce laver de længste gennemsnitlige afleveringer",
       subtitle = "Polske liga (Ekstraklasa) sæson 2021/22 - 2022/23",
       y = "Gennemsnitlig afleveringslængde",
       x = "",
       caption = "Datakilde: WyScout") +
  scale_y_continuous(breaks = seq(min(18), max(24), by = 1),
                     labels = scales::label_number(suffix = "m")) +
  coord_cartesian(ylim = c(18,24)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"))

avg_pass_length
ggsave(plot = avg_pass_length, file = "Plots/Gennemsnitlig afleveringslængde.jpg", dpi = 300, width = 8, height = 5)


######### Mest modtagende spiller pr. hold

modtagende_df <- polsk_liga %>%
  group_by(team$name) %>%
  summarize(ptp = mean(pass$accurate == TRUE) *100)

colnames(modtagende_df) <- c("team","ptp")

avg_pass_percent <- ggplot(modtagende_df, aes(x = reorder(team, -ptp), y = ptp)) + 
  geom_col(fill = plotColor) +
  geom_text(aes(label = round(ptp,2)),
            vjust = 2,
            size = 2.8,
            color = "white")+
  labs(title = "Korona Kielce laver de værste gennemsnitlige afleveringsprocenter",
       subtitle = "Polske liga (Ekstraklasa) sæson 2021/22 - 2022/23",
       y = "Gennemsnitlig afleveringsprocent",
       x = "Klubnavne",
       caption = "Datakilde: WyScout") +
  scale_y_continuous(breaks = seq(min(70), max(86), by = 4),
                     labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(70, 86)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"))

avg_pass_percent
ggsave(plot = avg_pass_percent, file = "Plots/Gennemsnitlig afleveringsprocent.jpg", dpi = 300, width = 8, height = 5)


####### VI GÅR DYBERE!!!!
# Finder ud af hvor mange kampe hver spiller har spillet
spiller_antal_kampe <- polsk_liga %>% 
  group_by(player$name, team$name) %>% 
  summarise(antal_kampe = n_distinct(matchId)) %>% 
  na.omit()

# Finder ud af hvor mange kampe hver hold har spillet
# MUY IMPORTANTE - mangler data fra en kamp
hold_antal_kampe <- polsk_liga %>%
  group_by(team$name) %>% 
  summarise(antal_kampe = n_distinct(matchId)) %>% 
  na.omit()

# Merger de to dataframes
samlet_antal_kampe <- merge(spiller_antal_kampe, hold_antal_kampe, by = "team$name", all.x = T)
colnames(samlet_antal_kampe) <- c("hold", "spiller", "spiller_kampe", "hold_kampe")

# Laver ny kolonne til at finde ud af hvor mange procent af kampe spillere har spillet
samlet_antal_kampe$kampe_procent_spillet <- round(samlet_antal_kampe$spiller_kampe / samlet_antal_kampe$hold_kampe * 100, 0)

# Laver kolonne kun med de ønskede spiller som har spillet over threshold antal kampe
godkendte_spillere <- samlet_antal_kampe[samlet_antal_kampe$kampe_procent_spillet > 40, ]

# Finder spiller med bedste aflevering 
bedste_aflevringsspiller_df <- polsk_liga %>%
  group_by(team$name, player$name) %>%
  summarize(passes_made = n(),
            ptp = mean(pass$accurate == TRUE) *100)

# Ændrer kolonnenavne
colnames(bedste_aflevringsspiller_df) <- c("team","player","passes","ptp")

# Vælger kun godkendte spillere
bedste_aflevringsspiller_df <- bedste_aflevringsspiller_df[bedste_aflevringsspiller_df$player %in% godkendte_spillere$spiller, ]

# Finder bedste afleveringsspiller fra hvert hold
bedste_aflevringsspiller <- bedste_aflevringsspiller_df %>%
  group_by(team) %>%
  slice(which.max(ptp)) %>%
  ungroup()

# Laver mapping til spillere og hold-logo
bedste_aflevringsspiller$holdLogo <- c("Teams/LechiaGdań.png", "Teams/WidzewŁódź.png","Teams/WisłaPłock.png",
                                       "Teams/PogońSzczecin.png", "Teams/RadomiakRadom.png", "Teams/LechPoznań.png",
                                       "Teams/LegiaWarszawa.png", "Teams/WisłaKrakó.png", "Teams/Bruk-BetTe.png",
                                       "Teams/JagielloniaBiałystok.png", "Teams/WartaPoznań.png", "Teams/KGHMZagłębieLubin.png",
                                       "Teams/GórnikZabrze.png", "Teams/PiastGliwice.png", "Teams/ŚląskWrocław.png",
                                       "Teams/GórnikŁęcz.png", "Teams/StalRzeszó.png", "Teams/Cracovia.png",
                                       "Teams/RakówCzęstochowa.png", "Teams/KoronaKielce.png", "Teams/MiedźLegni.png")

# Laver plot
playerPlot <- ggplot(bedste_aflevringsspiller, aes(x = reorder(player, -ptp), y = ptp)) +
  geom_col(fill = plotColor) +
  geom_text(aes(label = round(ptp,2)),
            vjust = 2,
            size = 2.8,
            color = "white") +
  labs(title = "M. Maloča fra Lechia Gdańsk laver de mest præcise afleveringer*",
       subtitle = "Polske topliga (Ekstraklasa) - sæson 2021/22 & 2022/23",
       y = "Gennemsnitlig afleveringsprocent",
       x = "",
       caption = "Datakilde: WyScout \n*Min. spillerdeltagelse i 40% af holdkampe") +
  scale_y_continuous(breaks = seq(min(80), max(94), by = 2),
                     labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(80, 94)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Tilføjer alle billeder
#g = list()
#for (i in 1:length(bedste_aflevringsspiller$holdLogo)) {
#  img <- readPNG(bedste_aflevringsspiller$holdLogo[i])
#  g[[i]] <- rasterGrob(img, interpolate = TRUE)
#  
#  playerPlot = playerPlot +
#    annotation_custom(grob = g[[i]],
#                      xmin = -3 + i,
#                      xmax = 3 + i,
#                      ymin = 1.5,
#                      ymax = 8.5)
#}

playerPlot
ggsave(plot = playerPlot, filename = "Plots/playerplot.jpeg", width = 8, height = 5, dpi = 300)

# Finder spiller med flest antal afleveringer 
spiller_aflevering <- polsk_liga %>%
  group_by(team$name, player$name) %>%
  summarize(passes_made = n())

# Laver plot af spiller med flest passes
spiller_aflevering <- spiller_aflevering %>%
  group_by(`team$name`)

# Samlet antal afleveringer for hvert hold
samlet_antal_holdaflevering <- polsk_liga %>% 
  group_by(team$name) %>% 
  summarize(team_passes = n())

# Merger samlede antal holdafleveringer med antal spiller afleveringer
aflevering_antal_kampe <- merge(samlet_antal_holdaflevering, spiller_aflevering, by.x = "team$name", by.y = "team$name", all.x = TRUE)

# Udregner procentandelen for afleveringer baseret ud fra den samlede mængde af afleveringer hvert hold har foretaget sig
aflevering_antal_kampe$procent_antal_pass <- round((aflevering_antal_kampe$passes / aflevering_antal_kampe$team_passes) * 100, 2)

# Threshold defineres ud fra den gennemsnitlige procentandel af afleveringer
godkendte_aflevering_spillere <- aflevering_antal_kampe[aflevering_antal_kampe$procent_antal_pass > mean(aflevering_antal_kampe$procent_antal_pass), ]

# Finder spiller med fleste afleveringer for hvert hold
flest_passes <- godkendte_aflevering_spillere %>%
  group_by(`team$name`) %>%
  slice(which.max(procent_antal_pass)) %>%
  ungroup()

# Laver plot
flesteAfleveringsPlot <- ggplot(flest_passes, aes(x = reorder(`player$name`, -procent_antal_pass), y = procent_antal_pass)) +
  geom_col(fill = plotColor) +
  labs(title = "D. Dąbrowski har den højeste andel af afleveringer blandt de polske modstandere",
       subtitle = "Polske topliga (Ekstraklasa) - sæson 2021/22 & 2022/23",
       y = "Procentandel af afleveringer",
       x = "",
       caption = "Datakilde: WyScout") +
  scale_y_continuous(breaks = seq(min(8), max(ceiling(max(flest_passes$procent_antal_pass) / 2) * 2), by = 1),
  labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(8, ceiling(max(flest_passes$procent_antal_pass) / 0.5) * 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

flesteAfleveringsPlot
ggsave(filename = "Plots/flesteAfleveringsPlot.jpeg", plot = flesteAfleveringsPlot, width = 10, height = 5, dpi = 300)

# Laver plot af modtagende spillere
top_pass_receiver <- polsk_liga %>%
  filter(pass$accurate == TRUE) %>% 
  group_by(team$name, pass$recipient$name) %>% 
  summarize(received = n())

# Ændrer kolonnenavne 
colnames(top_pass_receiver) <- c("hold","spiller","recieved_spiller")

# Samlet antal holdafleveringer 
samlet_antal_modtagneaflevering <- polsk_liga %>% 
  filter(pass$accurate == TRUE) %>%
  group_by(team$name) %>% 
  summarize(team_passes = n())

# Ændrer kolonnenavne 
colnames(samlet_antal_modtagneaflevering) <- c("hold","afleveringer_hold")

# Merger samlede antal holdafleveringer med antal spiller afleveringer
recieved_antal_kampe <- merge(samlet_antal_modtagneaflevering, top_pass_receiver, by = "hold")

# Normaliserer data baseret ud fra andelen af modtagne afleveringer pr. spiller ud fra den samlede antal af afleveringer for holdet
recieved_antal_kampe$procent_andel_recieved <- round((recieved_antal_kampe$recieved_spiller / recieved_antal_kampe$afleveringer_hold) * 100, 2)

# Opdeler observationer baseret ud fra maksimum værdi for modtagne afleveringer af hver holdspiller
recieved_antal_kampe <- recieved_antal_kampe %>%
  group_by(hold) %>%
  slice(which.max(procent_andel_recieved)) %>%
  ungroup()

# Laver plot
flesteModtagneAfleveringsPlot <- ggplot(recieved_antal_kampe, aes(x = reorder(`spiller`, -procent_andel_recieved), y = procent_andel_recieved)) +
  geom_col(fill = plotColor) +
  labs(title = paste0(recieved_antal_kampe[which.max(recieved_antal_kampe$procent_andel_recieved),3,drop = T]," har den højeste andel af modtagne afleveringer blandt de polske holdmodstandere"),
       subtitle = "Polske topliga (Ekstraklasa) - sæson 2021/22 & 2022/23",
       y = "Procentandel af modtagne afleveringer",
       x = "",
       caption = "Datakilde: WyScout") +
  scale_y_continuous(breaks = seq(min(7), max(ceiling(max(recieved_antal_kampe$procent_andel_recieved) / 1) * 1), by = 1),
  labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(7, ceiling(max(recieved_antal_kampe$procent_andel_recieved) / 1) * 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

flesteModtagneAfleveringsPlot
ggsave(filename = "Plots/flesteModtagneAfleveringsPlot.jpeg", plot = flesteModtagneAfleveringsPlot, width = 10, height = 5, dpi = 300)





























