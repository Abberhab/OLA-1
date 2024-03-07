#basic ui og server
library(shiny)
library(ggplot2)
library(skimr)
library(ggsoccer)
library(plotly)
library(shinythemes)
library(mongolite)
library(jsonlite)
library(tidyverse)

#setwd("~/Desktop/Dataanalyse 2. Semester/Projekter/OLA 1/Scripts OLA1, OPG1")

#Indhenter og laver DF
#forbinder til mongoDB
cong <-  mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout",collection = "games")
conp <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "players")
conm <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "matches")
#indhenter dataen
df_players <- conp$find(fields ='{}')
df_games <- cong$find(query = '{"type.primary": "shot"}')
df_matchid <- conm$find(fields ='{}')
#gør så der ikke er lister i DF
df_players <- fromJSON(toJSON(df_players), flatten = T)
df_games <- fromJSON(toJSON(df_games), flatten = T)
df_matchid <- fromJSON(toJSON(df_matchid), flatten = T)
#joiner dem, så vi får en samlet DF
df_pg <-  left_join(df_games,df_players, by = c("player.id" = "_id"))
df_s <- left_join(df_pg,df_matchid, by = c("matchId"="_id"))
#udregner BMI og alder
df_s$bmi <- round(df_s$weight/((df_s$height/100)^2),0)
df_s$birthDate <- as.Date(df_s$birthDate)
df_s$Age <- floor(as.numeric(Sys.Date()-df_s$birthDate)/365)

#Fjerner alt data hvor BMI = NA
df <- df_s[complete.cases(df_s$bmi),]
df <- df[df$bmi>5,]

#Laver procent tabeller
bmi_pct <- as.data.frame(prop.table(table(df$bmi))*100)
colnames(bmi_pct) <- c("bmi","pct")
alder_pct <- as.data.frame(prop.table(table(df$Age))*100)
colnames(alder_pct) <- c("alder","pct")


ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Statistik over fodboldspillere",
                           tabPanel("Skudforsøg",
                                    sidebarPanel(
                                      selectInput("Land",
                                                  label = " Vælg land",
                                                  choices = c("Holland", "Polen"),
                                                  selected = "Polen"),
                                      selectInput("plname",
                                                  label = "Vælg spiller",
                                                  choices = ""),
                                      selectInput("Skud",
                                                  label = "Skal der være scoret",
                                                  choices = c("Ja", "Nej", "Blandet"),
                                                  selected = "Blandet"),
                                      tableOutput("playerstat")
                                    ),
                                    mainPanel(
                                      plotOutput("bane"),
                                      fluidRow(
                                        column(width = 8,
                                               dataTableOutput("playertab")
                                        )
                                      )
                                    )
                           ),
                           tabPanel("Målramme",
                                    sidebarPanel(
                                      selectInput("Land1",
                                                  label = " Vælg land",
                                                  choices = c("Holland", "Polen"),
                                                  selected = "Polen"),
                                      selectInput("plname1",
                                                  label = "Vælg spiller",
                                                  choices = ""),
                                      selectInput("Skud1",
                                                  label = "Skal den være indenfor målrammen?",
                                                  choices = c("Ja", "Nej", "Blandet"),
                                                  selected = "Blandet"),
                                      tableOutput("playerstat1")
                                    ),
                                    mainPanel(
                                      plotOutput("målramme"),
                                      fluidRow(
                                        column(width = 8,
                                               dataTableOutput("playertab1")  
                                        )
                                      )
                                    )
                           ),
                                        tabPanel("Stats",
                                                 sidebarPanel(
                                                   selectInput("Land2",
                                                               label = " Vælg land",
                                                               choices = c("Holland", "Polen"),
                                                               selected = "Polen"),
                                                   selectInput("plname2",
                                                               label = "Vælg spiller",
                                                               choices = "")
                                                                        ),
                                                 mainPanel(
                                                   dataTableOutput("playertab2")
                                                   )
                                             )
                                         )
                                    )



server <- function(input,output,session) {
  #dette er fanen Skud
  observe({
    Land <- input$Land
    landekode <- ifelse(Land == "Polen",692,635)
    ctpl <- df %>% filter(competitionId == landekode) %>% select(player.id,player.name,shortName,lastName) %>% unique()
    updateSelectInput(session, "plname",
                      choices = ctpl[["player.name"]],
                      selected = "")
  })
  output$playertab <- renderDataTable({
    pval <- input$plname
    df %>% filter(player.name==pval) %>% select("Mål"=shot.isGoal,"Minut"=minute,"Spillerposition"=player.position,"Holdnavn"=team.name,"Modstander"=opponentTeam.name)
  })
  #laver fodboldbanen til Skud fanen
  output$bane <- renderPlot({
    
    pval <- input$plname
    pskud <- input$Skud
    
    if (pskud == "Ja") {
      skud_valg <- TRUE
    } else if (pskud == "Nej") {
      skud_valg <- FALSE
    } else {
      skud_valg <- c(TRUE, FALSE)
    }
    
    ny_df <- df %>% filter(player.name== pval, shot.isGoal %in% skud_valg) %>% select(location.x, location.y, shot.isGoal)
    skud_df <- df %>% filter(player.name== pval) %>% select(shot.isGoal)
    
    antal_mål <- sum(skud_df$shot.isGoal==TRUE)
    antal_skud <- nrow(skud_df)
    ggplot(ny_df) +
      annotate_pitch(colour = "white",
                     dimensions = pitch_wyscout,
                     fill   = "steelblue4",
                     limits = FALSE) +
      geom_point(aes(x = location.x, y = location.y, color = shot.isGoal),
                 size = 2) +
      scale_color_manual(values = c("TRUE"="#68a1fc", "FALSE" = "#0f3675"), 
                         labels = c("TRUE" = "Mål","FALSE" ="Ikke mål"),
                         name = "") +
      theme_pitch() +
      
      labs(title = paste(pval, "har scoret", antal_mål, "mål, ud af", antal_skud, "skud forsøg"),
           subtitle = paste0("Scoringsrate på ",round(antal_mål/antal_skud*100,2),"%")) +
      theme(panel.background = element_rect(fill = "steelblue4"),
            plot.title = element_text(face = "bold"),
            text = element_text(family = "Verdana")) 
  })
  #laver stat tabel
  output$playerstat <- renderTable({
    pval <- input$plname
    ny_df1_final <- df %>%
      filter(player.name == pval) %>%
      select(Mål = shot.isGoal, Minut = minute) %>%
      mutate(Minutter = cut(Minut, breaks = seq(0, 100, by = 10), labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"))) %>%
      group_by(Minutter, Mål) %>%
      count() %>% 
      pivot_wider(names_from = Mål, values_from = n, values_fill = list(Mål=0, "Ikke mål"=0)) %>% 
      right_join(data.frame(Minutter = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")), by = "Minutter") %>%
      replace(is.na(.), 0)
    
    if (!"TRUE" %in% colnames(ny_df1_final)) {
      ny_df1_final <- mutate(ny_df1_final, "TRUE" = 0)  # Create column "TRUE" with default value 0
    }
    if (!"FALSE" %in% colnames(df)) {
      df <- mutate(df, "FALSE" = 0)  # Create column "FALSE" with default value 0
    }
    ny_df1_final <- rename(ny_df1_final, "Mål" = "TRUE", "Ikke mål" = "FALSE")
  })
  #laver fanen målramme
  observe({
    Land1 <- input$Land1
    landekode <- ifelse(Land1 == "Polen",692,635)
    ctpl <- df %>% filter(competitionId == landekode) %>% select(player.id,player.name,shortName,lastName) %>% unique()
    updateSelectInput(session, "plname1",
                      choices = ctpl[["player.name"]],
                      selected = "")
  })
  output$playertab1 <- renderDataTable({
    pval <- input$plname1
    df %>% filter(player.name==pval) %>% select("Indenfor målramme"=shot.onTarget,"Minut"=minute,"Spillerposition"=player.position,"Holdnavn"=team.name,"Modstander"=opponentTeam.name)
  })
  #laver målramme
  output$målramme <- renderPlot({
    
    pval <- input$plname1
    pskud <- input$Skud1
    
    if (pskud == "Ja") {
      skud_valg <- TRUE
    } else if (pskud == "Nej") {
      skud_valg <- FALSE
    } else {
      skud_valg <- c(TRUE, FALSE)
    }
    
    ny_df <- df %>% filter(player.name== pval, shot.onTarget %in% skud_valg) %>% select(location.x, location.y, shot.onTarget)
    skud_df <- df %>% filter(player.name== pval) %>% select(shot.onTarget)
    
    antal_mål <- sum(skud_df$shot.onTarget==TRUE)
    antal_skud <- nrow(skud_df)
    ggplot(ny_df) +
      annotate_pitch(colour = "white",
                     dimensions = pitch_wyscout,
                     fill   = "steelblue4",
                     limits = FALSE) +
      geom_point(aes(x = location.x, y = location.y, color = shot.onTarget),
                 size = 2) +
      scale_color_manual(values = c("TRUE"="#68a1fc", "FALSE" = "#0f3675"), 
                         labels = c("TRUE" = "Indenfor målramme","FALSE" ="Udenfor målramme"),
                         name = "") +
      theme_pitch() +
      
      labs(title = paste(pval, "har haft", antal_skud, "forsøg på mål,", antal_mål, "var indenfor målrammen"),
           subtitle = paste0("On-Target på ",round(antal_mål/antal_skud*100,2),"%")) +
      theme(panel.background = element_rect(fill = "steelblue4"),
            plot.title = element_text(face = "bold"),
            text = element_text(family = "Verdana")) 
  })
  #laver stat tabel
  output$playerstat1 <- renderTable({
    pval <- input$plname1
    ny_df1_final <- df %>%
      filter(player.name == pval) %>%
      select(Mål = shot.onTarget, Minut = minute) %>%
      mutate(Minutter = cut(Minut, breaks = seq(0, 100, by = 10), labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"))) %>%
      group_by(Minutter, Mål) %>%
      count() %>% 
      pivot_wider(names_from = Mål, values_from = n, values_fill = list(Mål=0, "Uden for målramme"=0)) %>% 
      right_join(data.frame(Minutter = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")), by = "Minutter") %>%
      replace(is.na(.), 0)
    
    if (!"TRUE" %in% colnames(ny_df1_final)) {
      ny_df1_final <- mutate(ny_df1_final, "TRUE" = 0)  # Create column "TRUE" with default value 0
    }
    if (!"FALSE" %in% colnames(df)) {
      df <- mutate(df, "FALSE" = 0)  # Create column "FALSE" with default value 0
    }
    ny_df1_final <- rename(ny_df1_final, "Indenfor målramme" = "TRUE", "Udenfor målramme" = "FALSE")
  })
  #laver ny player-stat-tabel
  observe({
    Land <- input$Land2
    landekode <- ifelse(Land == "Polen",692,635)
    ctpl <- df %>% filter(competitionId == landekode) %>% select(player.id,player.name,shortName,lastName) %>% unique()
    updateSelectInput(session, "plname2",
                      choices = ctpl[["player.name"]],
                      selected = "")
  })
  output$playertab1 <- renderDataTable({
    pval <- input$plname2
    df %>% filter(player.name==pval) %>% select("Indenfor målramme"=shot.onTarget, "Skudforsøg" = shot.isGoal)
  })
  
}

shinyApp(ui=ui, server=server)
