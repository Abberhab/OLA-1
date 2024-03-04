library(ggsoccer)
library(tidyverse)
library(randomForest)
library(tree)
source("util.R")

# Indhenter data
df_s <- readRDS("Shot_flat")

# Fjerner NA
df_s <- df_s[!is.na(df_s$matchId), ]

############ Udarbejder XG-model ############

# Laver koordinat til meter
længde <- 120
bredde <- 80 

scale.x <- længde / 100
scale.y <- bredde / 100 

df_s$længde.m <- df_s$location.x * scale.x
df_s$bredde.m <- df_s$location.y * scale.y

df_s$længde.s <- df_s$possession.startLocation.x * scale.x
df_s$bredde.s <- df_s$possession.startLocation.y * scale.y

# Koordinater for målstolperne
målstolpe1.x <- (120 / længde) * 120
målstolpe1.y <- (80 / bredde) * 36

målstolpe2.x <- (120 / længde) * 120
målstolpe2.y <- (80 / bredde) * 44

# Udregner distance fra hver spiller til målets centerposition
df_s$dist_mål <- round(sqrt((målstolpe1.x - df_s$længde.m)^2 + (bredde / 2 - df_s$bredde.m)^2),2)

# Udregner vinkel
get_angle <- function(shot_x, shot_y) {

  c <- sqrt((målstolpe1.x-shot_x)^2+(målstolpe1.y-shot_y)^2)
  b <- sqrt((målstolpe2.x-shot_x)^2+(målstolpe2.y-shot_y)^2)
  a <- sqrt((målstolpe1.x-målstolpe2.x)^2+(målstolpe1.y-målstolpe2.y)^2)

  scorings_vinkel <- acos((b^2+c^2-a^2)/(2*b*c))*180/pi
  
  return(scorings_vinkel)
}

df_s$vinkel <- sapply(1:nrow(df_s), function(i) get_angle(df_s$længde.m[i], df_s$bredde.m[i]))

#Spillers_scorings_vinkel <- paste("Spilleren skyder på mål, hvor han har en scoringsvinkel på", round(scorings_vinkel,2),"grader")

df_s$bodypart <-  ifelse(df_s$shot.bodyPart == "head_or_other", "other", "foot")

# Funktion til indeling af positioner
spillerPos <- function(position) {
  if (grepl("CB|RCB|RCB3|LCB|LCB3|GK|RB|LB|RB5|LB5|RWB|LWB", position)) {
    return("Forsvar")
  } else if (grepl("DMF|LDMF|RDMF|RCMF|LCMF|RCMF3|LCMF3|AMF|LAMF|RAMF", position)) {
    return("Midtbane")
  } else if (grepl("CF|SS|RWF|LWF|RW|LW", position)) {
    return("Angreb")
  } else {
    return(NA)
  }
}

# Laver ny kolonne i df_s baseret på spiller positionen
df_s$poss <- sapply(1:nrow(df_s), function(i) spillerPos(df_s$player.position[i]))


df_xG <- df_s %>% 
  select("shot.isGoal","minute","poss","bodypart", "possession.duration",
                 "possession.team.formation","længde.m","bredde.m","shot.goalZone","vinkel","dist_mål",
                 "længde.s","bredde.s")

df_xG <- na.omit(df_xG)


# Træningsdata
træningsdata <- df_xG[sample(1:nrow(df_xG),round(nrow(df_xG)/5*4), replace = FALSE),]

# Testdata
testdata <- anti_join(df_xG, træningsdata)

# Random Forest
bag.træning <- randomForest(data = træningsdata, shot.isGoal ~., mtry = 12, importance = TRUE)

forudsigelse <- round(as.numeric(format(predict(bag.træning, newdata = testdata)),scientific = FALSE),4)

bag.træning$importance

varImpPlot(bag.træning)

# Beslutningstræ
tree.træning <- tree(shot.isGoal~., træningsdata)

plot(tree.træning)
text(tree.træning , pretty = 0)

# Multiple logistisk regressionsmodel //ÆNDR DATATYPER
træningslm <- glm(shot.isGoal ~.,data = træningsdata[1:100,])
summary(træningslm)
