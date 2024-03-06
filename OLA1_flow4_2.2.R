library(shiny)
library(ggplot2)
library(skimr)
library(ggsoccer)
library(plotly)
library(shinythemes)
library(mongolite)
library(jsonlite)
library(tidyverse)
library(reshape2)

# Indhenter data
df_skud <- readRDS("Shot_flat")

# Fjerner NA
df_skud <- df_skud[!is.na(df_skud$matchId), ]


df_skud <- df_skud %>% rowwise() %>% mutate(match=str_split(label,",")[[1]][1],
                                  score=str_split(label,",")[[1]][2])
df_skud <- df_skud %>% rowwise() %>% mutate(home=str_split(match," - ")[[1]][1],
                                  away=str_split(match," - ")[[1]][2])
df_skud <- df_skud %>% rowwise() %>% mutate(homescore=str_split(score,"-")[[1]][1],
                                  awayscore=str_split(score,"-")[[1]][2])
df_skud <- df_skud %>% mutate(longinfo=paste0(match,"_",`matchId`))

#####Opdeling af data#####

#Laver vektor for top 5 & bund 5 hold 
holdandskeliga_top <- c("Ajax","PSV","Feyenoord","Twente","AZ")
holdandskeliga_bund <- c("Sparta Rotterdam","Fortuna Sittard", "Heracles", "Willem II", "PEC Zwolle")
polskeliga_top <- c("Lech Poznań", "Raków Częstochowa", "Pogoń Szczecin", "Lechia Gdańsk","Piast Gliwice")
polskeliga_bund <- c("Stal Mielec", "Śląsk Wrocław","Nieciecza","Wisła Kraków","Górnik Łęczna")


# Filtrerer efter hold
df_skud <- df_skud %>%
  filter(team.name %in% c(holdandskeliga_top, holdandskeliga_bund,polskeliga_top,polskeliga_bund))


# Angiver om hold er top eller bund
df_skud$plads <- ifelse(df_skud$team.name %in% c(polskeliga_top,holdandskeliga_top), "Top", "Bund")

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

# Laver ny kolonne i df_skud baseret på spiller positionen
df_skud$poss <- sapply(1:nrow(df_skud), function(i) spillerPos(df_skud$player.position[i]))

# Fjerner NA
df_skud <- df_skud[!is.na(df_skud$poss), ]

# Laver koordinat til meter
længde <- 105
bredde <- 68

scale.x <- længde / 100
scale.y <- bredde / 100 

df_skud$længde.m <- df_skud$location.x * scale.x
df_skud$bredde.m <- df_skud$location.y * scale.y

# Udregner distance fra hver spiller til målets centerposition
df_skud$shot_length <- round(sqrt((længde - df_skud$længde.m)^2 + ((bredde / 2) - df_skud$bredde.m)^2),2)

######UI######
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Statistik over fodboldspillere",
    tabPanel(
      "Skudforsøg",
      sidebarPanel(
        selectInput("land1",
                    label = " Vælg land",
                    choices = c("Holland", "Polen"),
                    selected = "Holland"),
        selectInput("season1",
                    label = "Vælg sæson",
                    choices = c("21/22"),
                    selected = "21/22"),
        sliderInput("minut1", "Vælg interval:",
                    min = 0, max = 90, value = c(0, 90),
                    step = 1)
      ),
      mainPanel(
        plotOutput("TopBundPlot"),
        fluidRow(
          column(width = 8,
                 dataTableOutput("succes1")
          )
        )
      )
    ),
    tabPanel(
      "Sammenligning",
      sidebarPanel(
        selectInput("land2",
                    label = " Vælg land",
                    choices = c("Holland", "Polen"),
                    selected = "Holland"),
        selectInput("season2",
                    label = "Vælg sæson",
                    choices = c("21/22"),
                    selected = "21/22")
      ),
      mainPanel(
        plotOutput("sammenlign_plot"),
        fluidRow(
          column(width = 8,
                 dataTableOutput("pass_table")
          )
        )
      )
    ),
    tabPanel(
      "Topspillere",
      sidebarPanel(
        selectInput("land3", label = " Vælg land", choices = c("Holland", "Polen"), selected = "Holland"),
        selectInput("season3", label = "Vælg sæson", choices = c("21/22"), selected = "21/22")
      ),
      mainPanel(
        plotOutput("top10plot"),
        fluidRow(
          column(width = 8, dataTableOutput("top10"))
        )
      )
    ),
    tabPanel(
      "Sammenligning af xG",
      sidebarPanel(
        selectInput("land4",
                    label = " Vælg land",
                    choices = c("Holland", "Polen"),
                    selected = "Holland"),
        selectInput("season4",
                    label = "Vælg sæson",
                    choices = c("21/22"),
                    selected = "21/22"),
        sliderInput("minut41", "Vælg første interval:",
                    min = 0, max = 90, value = c(0, 90),
                    step = 1),
        sliderInput("minut42", "Vælg andet interval:",
                    min = 0, max = 90, value = c(0, 90),
                    step = 1)
      ),
      mainPanel(
        plotOutput("sammenlign_plot1"),
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$TopBundPlot <- renderPlot({
    
    #Vælger land og finder top og bund
    
    Land1 = input$land1
    season1 = input$season1
    if (Land1 == "Holland") {
      holdvalg <- c(holdandskeliga_bund, holdandskeliga_top)
    } else {
      holdvalg <- c(polskeliga_bund, polskeliga_top)
    }
    
    #finder top_bund ud fra sæson i df
    
    sæsonid<- ifelse(Land1 == "Holland",187502,186215)
    min1 <- input$minut1[1]
    min2 <- input$minut1[2]
    
    df_mean_pass <- df_skud %>%
      filter(team.name %in% holdvalg, seasonId == sæsonid, minute >= min1 & minute <= min2) %>% 
      group_by(team.name, plads) %>%
      summarise(average_length = mean(shot_length))
    print(df_mean_pass)
    #ggplot over de længste afleveringer bund 5 og top 5-holdende 
    ggplot(df_mean_pass, aes(x = reorder(team.name, -average_length), y = average_length, fill = plads)) +
      geom_col(position = "dodge") +
      labs(title = "Top 5 holdende tager skud fra kortere afstand end bund 5 holdende",
           subtitle = ifelse(Land1 == "Holland","I den hollandske liga (Eredivisie) sæson 21/22", "I den polske liga (Ekstraklasa) sæson 21/22"),
           x = "Fodboldklub",
           y = "Gennemsnitlig skudlængde",
           caption = "Datakilde: WyScout") +
      scale_y_continuous(breaks = seq(min(0), max(ceiling(max(df_mean_pass$average_length) / 5) * 5), by = 5),
                         labels = scales::label_number(suffix = "m")) +
      coord_cartesian(ylim = c(0, ceiling(max(ceiling(max(df_mean_pass$average_length) / 5) * 5)))) +
      scale_fill_manual(values = c("Bund" = "#d42a30", "Top" = "steelblue4"), 
                        name = "Niveau") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            text = element_text(family = "Verdana"),
            panel.grid.major.x = element_blank())  
    
  })
  output$succes1 <- renderDataTable({
    Land1 = input$land1
    season1 = input$season1
    if (Land1 == "Holland") {
      holdvalg <- c(holdandskeliga_bund, holdandskeliga_top)
    } else {
      holdvalg <- c(polskeliga_bund, polskeliga_top)
    }
    sæsonid<- ifelse(Land1 == "Holland",187502,186215)
    min1 <- input$minut1[1]
    min2 <- input$minut1[2]
    
    df_skud %>% 
      filter(team.name %in% holdvalg, seasonId == sæsonid, minute >= min1 & minute <= min2) %>%
      group_by(Holdnavn = team.name, Placering = plads) %>%
      summarise(
        Succesfulde = sum(shot.isGoal == TRUE),
        Misset = sum(shot.isGoal == FALSE),
        Pct = paste0(round((Succesfulde / (Succesfulde + Misset)) * 100), "%"),
        Gennemsnitslængde = round(mean(shot_length),2)
      ) %>% 
      arrange(-Gennemsnitslængde)
  })   
  ###### Opgave 2.2.2 - Ny graf ######
  output$sammenlign_plot <- renderPlot({
    Land2 = input$land2
    season2 = input$season2
    assist2 = input$assist2
    
    sæsonid <- ifelse(Land2 == "Holland",187502,186215)

    
      df_mean_pass_1 <- df_skud %>% 
        filter(seasonId == sæsonid,shot.isGoal == TRUE) %>% 
        group_by(team.name) %>%
        summarise(average_length_1 = mean(shot_length))
      
      df_mean_pass_2 <- df_skud %>% 
        filter(seasonId == sæsonid, shot.isGoal == FALSE) %>% 
        group_by(team.name) %>%
        summarise(average_length_2 = mean(shot_length))
    
    df_hold_2 <- df_skud %>% 
      ungroup %>% 
      filter(seasonId == sæsonid) %>% 
      select(team.name) %>% 
      distinct()
    
    merge_df <- merge(df_hold_2, df_mean_pass_1, by = "team.name", all.x = TRUE)
    merge_df <- merge(merge_df, df_mean_pass_2, by = "team.name", all.x = TRUE)
    merge_df[is.na(merge_df)] <- 0
    
    df_mean_pass_4 <- melt(merge_df, id.vars = "team.name")
    
    #ggplot over de længste afleveringer bund 5 og top 5-holdende 
    
    ggplot(df_mean_pass_4, aes(x = team.name)) +
      geom_col(aes(y = value, fill = variable), position = "dodge") +
      labs(title = paste0("Den gennemsnitlige skudlængde for klubberne er markant lavere på skud som resulterer i mål"),
           subtitle = ifelse(Land2 == "Holland","I den hollandske liga (Eredivisie) sæson 21/22", "I den polske liga (Ekstraklasa) sæson 21/22"),
           x = "Fodboldklub",
           y = "Gennemsnitlig skudlængde",
           caption = "Datakilde: WyScout")+
      scale_y_continuous(breaks = seq(min(0), max(ceiling(max(df_mean_pass_4$value) / 5) * 5), by = 5),
                         labels = scales::label_number(suffix = "m")) +
      coord_cartesian(ylim = c(0, ceiling(max(ceiling(max(df_mean_pass_4$value) / 5) * 5)))) + # Corrected coordinate limits
      scale_fill_manual(values = c("average_length_1" = "steelblue4", "average_length_2" = "#d42a30"), 
                        name = "",
                        labels = c(paste0("Mål "), paste0("Ikke mål "))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            text = element_text(family = "Verdana"),
            panel.grid.major.x = element_blank())  
  })
  # Laver tabel til afleveringer på positioner
  output$pass_table <- renderDataTable({
    
    # Definerer valg fra UI
    Land2 = input$land2
    season2 = input$season2
    assist2 = input$assist2
    
    sæsonid<- ifelse(Land2 == "Holland",187502,186215)
    
      result <- df_skud %>%
        filter(seasonId == sæsonid, shot.isGoal == TRUE) %>% 
        group_by(team.name, poss) %>%
        summarise(count = n()) %>%
        group_by(team.name) %>%
        mutate(percentage = paste(round(count/sum(count)*100,2), "%")) %>%
        select(-count) %>%
        spread(key = poss, value = percentage, fill = "0%")
      
      result1 <- df_skud %>%
        filter(seasonId == sæsonid, shot.isGoal == FALSE) %>% 
        group_by(team.name, poss) %>%
        summarise(count = n()) %>%
        group_by(team.name) %>%
        mutate(percentage = paste(round(count/sum(count)*100,2), "%")) %>%
        select(-count) %>%
        spread(key = poss, value = percentage, fill = "0%") %>% 
        select(team.name, Forsvar, Midtbane, Angreb)
      
      Christians_ide <- left_join(result, result1, by = "team.name")
      
      Christians_ide <- Christians_ide %>% 
        select(team.name, Forsvar.x, Forsvar.y, Midtbane.x, Midtbane.y, Angreb.x, Angreb.y)
      
      colnames(Christians_ide) <- c("Holdnavn", 
                                    paste0("Forsvar - Mål" ),
                                    paste0("Forsvar - Ikke Mål"),
                                    paste0("Midtbane - Mål"),
                                    paste0("Midtbane - Ikke Mål"),
                                    paste0("Angreb - Mål"),
                                    paste0("Angreb - Ikke Mål"))
       return(Christians_ide)
  })  
  output$top10plot <- renderPlot({
    #Vælger land og finder top 10 spillere
    Land3 <- input$land3
    season3 <- input$season3
    if (Land3 == "Holland") {
      holdvalg <- c(holdandskeliga_bund, holdandskeliga_top)
    } else {
      holdvalg <- c(polskeliga_bund, polskeliga_top)
    }
    sæsonid <- ifelse(Land3 == "Holland", 187502, 186215)
    
    top10 <- df_skud %>%
      filter(seasonId == sæsonid) %>%
      select(player.name, shot.isGoal, shot.xg) %>% 
      group_by(player.name) %>%
      summarise(antal_mål = sum(shot.isGoal == TRUE),
                sum_af_xg = sum(shot.xg)) %>% 
      arrange(desc(antal_mål)) %>%
      slice_head(n = 10) 
    print(top10)
    
    # Laver variable til ggplot
    spiller <- top10$player.name[which.max(top10$antal_mål)]
    max_mål <- max(top10$antal_mål)
    forv_xg <- round(top10[top10$player.name == spiller,]["sum_af_xg"], 2)
    trust <- ifelse(max_mål > forv_xg, "højere", "lavere")
    
    
    # ggplot
    ggplot(top10, aes(x = reorder(player.name, -antal_mål), y = antal_mål)) +
      geom_col(position = "dodge", fill = "steelblue4") +
      geom_point(aes(y = sum_af_xg, color = "Sum af xG"), position = position_dodge(width = 0.9), size = 3) +
      labs(title = paste0("Spilleren ", spiller, " har scoret flest antal mål på ", max_mål,
                          ",\nhvilket er ", trust," end hans forventede xG på ", forv_xg, 2),
           subtitle = ifelse(Land3 == "Holland", "I den hollandske liga (Eredivisie) sæson 21/22", "I den polske liga (Ekstraklasa) sæson 21/22"),
           x = "Spillernavn",
           y = "Antal mål",
           caption = "Datakilde: WyScout") +
      scale_color_manual(values = c("Sum af xG" = "#d42a30"),
                         name = "") +
      scale_y_continuous(breaks = seq(0, max(ceiling(max(top10[,2:3]) / 2) * 2), by = 2),
                         limits = c(0, ceiling(max(ceiling(max(top10[,2:3]) / 2) * 2)))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            text = element_text(family = "Verdana"),
            panel.grid.major.x = element_blank())
  })
  ###### Opgave 2.2.2 - Ny graf ######
  output$sammenlign_plot1 <- renderPlot({
    Land4 = input$land4
    season4 = input$season4
    
    sæsonid <- ifelse(Land4 == "Holland",187502,186215)
    min41_1 <- input$minut41[1]
    min41_2 <- input$minut41[2]
    
    min42_1 <- input$minut42[1]
    min42_2 <- input$minut42[2]
    
      df_mean_pass_1 <- df_skud %>% 
        filter(seasonId == sæsonid, minute >= min41_1 & minute <= min41_2) %>% 
        group_by(team.name) %>%
        summarise(average_length_1 = mean(shot.xg))
      
      df_mean_pass_2 <- df_skud %>% 
        filter(seasonId == sæsonid, minute >= min42_1 & minute <= min42_2) %>% 
        group_by(team.name) %>%
        summarise(average_length_2 = mean(shot.xg))
    
    df_hold_2 <- df_skud %>% 
      ungroup %>% 
      filter(seasonId == sæsonid) %>% 
      select(team.name) %>% 
      distinct()
    
    merge_df <- merge(df_hold_2, df_mean_pass_1, by = "team.name", all.x = TRUE)
    merge_df <- merge(merge_df, df_mean_pass_2, by = "team.name", all.x = TRUE)
    merge_df[is.na(merge_df)] <- 0
    
    df_mean_pass_4 <- melt(merge_df, id.vars = "team.name")
    print(df_mean_pass_4)
    #ggplot over de længste afleveringer bund 5 og top 5-holdende 
    
    ggplot(df_mean_pass_4, aes(x = team.name)) +
      geom_col(aes(y = value, fill = variable), position = "dodge") +
      labs(title = paste0("Den gennemsnitlige xG for minut ", min41_1, "-", min41_2, " er ", round(mean(df_mean_pass_1$average_length_1),2), "\n",
                          "og for minut ", min42_1, "-", min42_2, " er ", round(mean(df_mean_pass_2$average_length_2), 2)),
           subtitle = ifelse(Land4 == "Holland","I den hollandske liga (Eredivisie) sæson 21/22", "I den polske liga (Ekstraklasa) sæson 21/22"),
           x = "Fodboldklub",
           y = "Gennemsnitlig xG",
           caption = "Datakilde: WyScout")+
      scale_y_continuous(breaks = seq(min(0), max(ceiling(max(df_mean_pass_4$value) / 0.01) * 0.01), by = 0.02)) +
      #coord_cartesian(ylim = c(0, ceiling(max(ceiling(max(df_mean_pass_4$value) / 0.01) * 0.01)))) + # Corrected coordinate limits
      scale_fill_manual(values = c("average_length_1" = "#d42a30", "average_length_2" = "steelblue4"), 
                        name = "",
                        labels = c(paste0("Minut ", min41_1,"-", min41_2), paste0("Minut ", min42_1, "-", min42_2))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            text = element_text(family = "Verdana"),
            panel.grid.major.x = element_blank())  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
