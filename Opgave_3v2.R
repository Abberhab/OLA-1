library(tidyverse)
library(useful)
library(plotly)
library(scales)
# Indhenter data
aflevering <- readRDS("Alt_data_flatten")


############### Opgave 3.1 ###############

# Agreggerer afleveringer pr. spiller
spiller_aflevering <- aflevering %>%
  filter(seasonId %in% c(187502, 186215)) %>%
  select(player.name, pass.length, pass.accurate, pass.angle) %>%
  group_by(player.name) %>%
  summarize(average.pass.length = round(mean(pass.length), 2),
            average.pass.accuracy = round(mean(pass.accurate), 2),
            average.pass.angle = round(mean(pass.angle)),
            total.passes = n()) %>%
  na.omit() %>%
  filter(total.passes >= 100) 

# Finder average passes her spiller laver pr. kamp
average_passes_per_game <- aflevering %>%
  group_by(player.name, matchId) %>%
  summarise(average_passes = n()) %>%
  group_by(player.name) %>%
  summarise(avg_passes_per_game = mean(average_passes))

# Joiner average passes her spiller laver pr. kamp på 
spiller_aflevering <- merge(spiller_aflevering, average_passes_per_game)

##### Laver k-means clustering
set.seed (4)
kmeans_spiller <- kmeans(spiller_aflevering[,c(2:4,6)], centers = 4, nstart = 20)
spiller_aflevering$cluster <- as.factor(kmeans_spiller$cluster)


# Laver ny df til visualisering af cluster - 1. variable
cluster_avg_pass_made <- spiller_aflevering %>%
  group_by(cluster) %>%
  summarize(avg_pass_made = mean(avg_passes_per_game))

# ggplot - visualiserer clustering
cluster_1x <- ggplot(data = cluster_avg_pass_made, aes(x = cluster, y = avg_pass_made, fill = cluster)) +
  geom_col() +
  scale_fill_manual(values = c("1"= "#1b4e6b", "2" = "#5c63a2", "3" = "#c068a8", "4" = "#ec7176")) +
  labs(title = "Spillernes antal afleveinger per kamp har markant inflydelse på indeling af clustering",
       subtitle = "Spillere i cluster 3 laver det højeste antal af afleveringer pr. kamp",
       x = "Cluster",
       y = "Gennemsnitlig antal afleveringer",
       caption = "Data: WyScout") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        legend.title = element_blank())

cluster_1x


# Laver ny df til visualisering af cluster - 2. variable
cluster_avg_pass_length <- spiller_aflevering %>%
  group_by(cluster) %>%
  summarize(avg_pass_length = mean(average.pass.length))

# ggplot - visualiserer clustering
cluster_2x <- ggplot(data = cluster_avg_pass_length, aes(x = cluster, y = avg_pass_length, fill = cluster)) +
  geom_col() +
  scale_fill_manual(values = c("1"= "#1b4e6b", "2" = "#5c63a2", "3" = "#c068a8", "4" = "#ec7176")) +
  labs(title = "Afleveringslængde har ikke stor betydning for indeling af clusters",
       subtitle = "",
       x = "Cluster",
       y = "Gennemsnitlig afleveringslængde",
       caption = "Data: WyScout") +
  scale_y_continuous(labels = label_number(suffix = "m")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        legend.title = element_blank())

cluster_2x


# Laver ny df til visualisering af cluster - 3. variable
cluster_avg_pass_angle <- spiller_aflevering %>%
  group_by(cluster) %>%
  summarize(avg_pass_angle = mean(average.pass.angle))

# ggplot - visualiserer clustering
cluster_3x <- ggplot(data = cluster_avg_pass_angle, aes(x = cluster, y = avg_pass_angle, fill = cluster)) +
  geom_col() +
  scale_fill_manual(values = c("1"= "#1b4e6b", "2" = "#5c63a2", "3" = "#c068a8", "4" = "#ec7176")) +
  labs(title = "Afleveringens vinkel har stor betydning for indeling af clusters",
       subtitle = "Cluster 1 har ",
       x = "Cluster",
       y = "Gennemsnitlig vinkel",
       caption = "Data: WyScout") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        legend.title = element_blank())

cluster_3x



# Laver ny df til visualisering af cluster - 4. variable
cluster_avg_pass_accuracy <- spiller_aflevering %>%
  group_by(cluster) %>%
  summarize(avg_pass_accuracy = mean(average.pass.accuracy))

# ggplot - visualiserer clustering
cluster_4x <- ggplot(data = cluster_avg_pass_accuracy, aes(x = cluster, y = avg_pass_accuracy, fill = cluster)) +
  geom_col() +
  scale_fill_manual(values = c("1"= "#1b4e6b", "2" = "#5c63a2", "3" = "#c068a8", "4" = "#ec7176")) +
  labs(title = "Afleveringspræcisionen har ikke stor betydning for indelling af cluster",
       subtitle = "",
       x = "Cluster",
       y = "Gennemsnitlig afleveringspræcsision",
       caption = "Data: WyScout") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        legend.title = element_blank())

cluster_4x



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

# Agreggerer afleveringer pr. spiller
spiller_aflevering2 <- aflevering %>%
  filter(seasonId %in% 186215) %>%
  select(player.name, pass.length, pass.accurate, pass.angle) %>%
  group_by(player.name) %>%
  summarize(average.pass.length = round(mean(pass.length), 2),
            average.pass.accuracy = round(mean(pass.accurate), 2),
            average.pass.angle = round(mean(pass.angle)),
            total.passes = n()) %>%
  na.omit() %>%
  filter(total.passes >= 100) 

# Finder average passes her spiller laver pr. kamp
average_passes_per_game2 <- aflevering %>%
  group_by(player.name, matchId) %>%
  summarise(average_passes = n()) %>%
  group_by(player.name) %>%
  summarise(avg_passes_per_game = mean(average_passes))

# Joiner average passes her spiller laver pr. kamp på 
spiller_aflevering_pl <- merge(spiller_aflevering2, average_passes_per_game2)

spiller_skud_pl <- df_s %>%
  filter(seasonId %in% 186215) %>% 
  select(player.name,dist_mål,shot.xg) %>%
  group_by(player.name) %>%
  summarize(average.shot.length = round(mean(dist_mål), 2),
            average.xG = round(mean(shot.xg), 2),
            total.shots = n()) %>%
  filter(total.shots >= 5) %>% 
  na.omit() 

# Finder average skud spiller laver pr. kamp
average_shot_per_game2 <- df_s %>%
  group_by(player.name, matchId) %>%
  summarise(average_shot = n()) %>%
  group_by(player.name) %>%
  summarise(avg_shots_per_game = mean(average_shot))

# Tilføjer average skud til spiller_skud_pl
spiller_skud_pl <- merge(spiller_skud_pl, average_shot_per_game2)

# Merger afleveringer og skud
spillerstats <- merge(spiller_aflevering_pl, spiller_skud_pl)

# Fjerner NA værdier
spillerstats <- na.omit(spillerstats)

# Fjerner unødvendige kolonner
spillerstats <- spillerstats[,c(2:4,6,7,8,10)]

#### Laver clustering på afleveringer og skud
kmeans_afl_skud <- kmeans(spillerstats, centers = 4, nstart = 20)

# Tilføjer clusters til DF
spillerstats$cluster <- as.factor(kmeans_afl_skud$cluster)

# Ændrer colnames
colnames(spillerstats) <- c("Gennemsnitlig skudlængde (m)", "Gennemsnitlig afleveringspræcision (%)", "Gennemsnitlig afleveringsvinkel",
                            "Gennemsnitlig antal afleveringer pr. kamp", "Gennemsnitlig skud længde (m)", "Gennemsnitlig xG",
                            "Gennemsnitlig antal skud pr. kamp", "Cluster")

plotList <- list()

for (i in 1:(ncol(spillerstats)-1)) {
  
  avg <- spillerstats %>%
    group_by(Cluster) %>%
    summarize(avg = mean(.data[[colnames(spillerstats)[i]]]))
  
  
  plot <- ggplot(data = avg, aes(x = Cluster, y = avg, fill = Cluster, color = Cluster)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("1"= "#1b4e6b", "2" = "#5c63a2", "3" = "#c068a8", "4" = "#ec7176")) +
    scale_color_manual(values = c("1"= "#1b4e6b", "2" = "#5c63a2", "3" = "#c068a8", "4" = "#ec7176")) +
    labs(title = paste0("Cluster fordeling på ",colnames(spillerstats)[i]),
         subtitle = "",
         x = "Cluster",
         y = colnames(spillerstats)[i],
         caption = "Data: WyScout") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          text = element_text(family = "Verdana"),
          legend.title = element_blank(),
          legend.position = "none")
  
  plotList[[i]] <- plot
  
  ggsave(plot = plotList[[i]], filename = paste0("clusterPlots/clusterPlot", i,".jpg"), width = 8, height = 5, dpi = 300)
  
}
















