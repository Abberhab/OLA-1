library(tidyverse)
library(useful)
library(plotly)
# Indhenter data
aflevering <- readRDS("Alt_data_flatten")


############### Opgave 3.1 ############### 
# Agreggerer afleveringer pr. spiller
spiller_aflevering <- aflevering %>%
  # Medtager både den polske og holldanske liga
  filter(seasonId %in% c(187502, 186215)) %>%
  select(player.id, pass.length, pass.accurate) %>%
  group_by(player.id) %>%
  summarize(average.pass.length = round(mean(pass.length), 2),
            average.pass.accuracy = round(mean(pass.accurate), 2),
            total.passes = n()) %>%
  na.omit() %>%
  filter(total.passes >= 100) 

# Laver k-means
set.seed (4)
kmeans_spiller <- kmeans(spiller_aflevering[,3:4], centers = 2, nstart = 20)
spiller_aflevering$cluster <- kmeans_spiller$cluster

# ggplot
clustering_pass_length <- ggplot(spiller_aflevering, aes(x = average.pass.length, y = total.passes, col = as.factor(cluster))) +
  geom_point()

ggplotly(clustering_pass_length)

clustering_pass_accuracy <- ggplot(spiller_aflevering, aes(x = average.pass.accuracy, y = total.passes, col = as.factor(cluster))) +
  geom_point()

ggplotly(clustering_pass_accuracy)

# assist
spiller_assist <- aflevering %>%
  # Medtager både den polske og holldanske liga
  filter(seasonId %in% c(187502, 186215),sapply(type.secondary, function(i) "assist" %in% i)) %>% 
  select(player.id, pass.length) %>% 
  group_by(player.id) %>% 
  summarize(avg.pass.length = round(mean(pass.length),2),
            assist = n()) %>% 
  na.omit() %>% 
  filter(assist < 18)

# Laver k-means
set.seed (3)
kmeans_spillerassist <- kmeans(spiller_assist[,2:3], centers = 3, nstart = 20)
spiller_assist$cluster <- kmeans_spillerassist$cluster

clustering_pass_assist <- ggplot(spiller_assist, aes(x = avg.pass.length, y = assist, col = as.factor(cluster))) +
  geom_point()

ggplotly(clustering_pass_assist)

#TEST
# Agreggerer afleveringer pr. spiller
spiller_aflevering.test <- aflevering %>%
  # Medtager både den polske og holldanske liga
  filter(seasonId %in% c(187502, 186215)) %>%
  select(pass.length, pass.accurate) %>%
  na.omit()

# Laver k-means
set.seed (4)
kmeans_spiller_test <- kmeans(spiller_aflevering.test, centers = 2, nstart = 20)
spiller_aflevering.test$cluster <- kmeans_spiller_test$cluster

# ggplot
clustering_pass_length_test <- ggplot(spiller_aflevering.test, aes(x = pass.length, y = pass.accurate, col = as.factor(cluster))) +
  geom_point()

ggplotly(clustering_pass_length_test)

############### Opgave 3.2 ############### 
# Indhenter data
df_s <- readRDS("shot_flat")

# Laver koordinat til meter
længde <- 105
bredde <- 68

scale.x <- længde / 100
scale.y <- bredde / 100 

df_s$længde.m <- df_s$location.x * scale.x
df_s$bredde.m <- df_s$location.y * scale.y

# Udregner distance fra hver spiller til målets centerposition
df_s$dist_mål <- round(sqrt((længde - df_s$længde.m)^2 + (bredde / 2 - df_s$bredde.m)^2),2)

# K-means
# Agreggerer afleveringer & skud pr. spiller
spillerstats_afleveringer <- aflevering %>%
  # Medtager kun den polske 
  filter(seasonId %in% 186215) %>% 
  select(player.id,pass.length, pass.accurate) %>%
  group_by(player.id) %>%
  summarize(average.pass.length = round(mean(pass.length), 2),
            average.pass.accuracy = round(mean(pass.accurate), 2),
            total.passes = n()) %>%
  na.omit() %>%
  filter(total.passes >= 100)

spillerstats_skud <- df_s %>%
  # Medtager kun den polske 
  filter(seasonId %in% 186215) %>% 
  select(player.id,dist_mål,shot.xg) %>%
  group_by(player.id) %>%
  summarize(average.shot.length = round(mean(dist_mål), 2),
            average.xG = round(mean(shot.xg), 2),
            total.shots = n()) %>%
  na.omit() 

# Merger afleveringer og skud
spillerstats <- left_join(spillerstats_afleveringer,spillerstats_skud, by = "player.id")

#Fjerner NA-værdier
spillerstats <- na.omit(spillerstats)















