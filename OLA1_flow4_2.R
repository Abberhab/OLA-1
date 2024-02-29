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

#####Indhentning af data#####

con <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "games")
mongoquery <- con$find(query = '{"type.primary": "shot"}')

con1 <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "matches")
mongoquery1 <- con1$find(query = '{"seasonId": {"$in": [187502, 186215]}}')
df_matchid <- fromJSON(toJSON(mongoquery1), flatten = T)


#afleveringer <- readRDS("Alt data flatten")
#skud <- fromJSON(toJSON(mongoquery), flatten = T)
setwd("/Users/marius/Documents/DataAnalyseProjekter/2. Semester/OLA'er/OLA-1")
df_s <- readRDS("Alt_data_flatten")

# Fjerner NA
df_s <- df_s[!is.na(df_s$matchId), ]

# Laver minutter
#df_s <- df_s %>%  group_by(matchId) %>% mutate(Minutter = cut(minute, breaks = seq(-1, 99, by = 10), 
                                                            #  labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", 
                                                                 #        "60-69", "70-79", "80-89", "90+"))) 

#####Opdeling af data#####

#Laver vektor for top 5 & bund 5 hold 
holdandskeliga_top <- c("Ajax","PSV","Feyenoord","Twente","AZ")
holdandskeliga_bund <- c("Sparta Rotterdam","Fortuna Sittard", "Heracles", "Willem II", "PEC Zwolle")
polskeliga_top <- c("Lech Poznań", "Raków Częstochowa", "Pogoń Szczecin", "Lechia Gdańsk","Piast Gliwice")
polskeliga_bund <- c("Stal Mielec", "Śląsk Wrocław","Nieciecza","Wisła Kraków","Górnik Łęczna")


# Filtrerer efter hold
df <- df_s %>%
  filter(team.name %in% c(holdandskeliga_top, holdandskeliga_bund,polskeliga_top,polskeliga_bund))


# Angiver om hold er top eller bund

df$plads <- ifelse(df$team.name %in% c(polskeliga_top,holdandskeliga_top), "Top", "Bund")

# Funktion til indeling af positioner
spillerPos <- function(position) {
  if (grepl("CB|RCB|LCB|GK", position)) {
    return("Forsvar")
  } else if (grepl("DMF|LDMF|RDMF|RCMF|LCMF", position)) {
    return("Midtbane")
  } else if (grepl("CF|SS|AMF|LAMF|RAMF|RWF|LWF|RW|LW", position)) {
    return("Angreb")
  } else {
    return(NA)
  }
}

# Laver ny kolonne i df_s baseret på spiller positionen
df_s$poss <- sapply(1:nrow(df_s), function(i) spillerPos(df_s$player.position[i]))



######UI######
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),navbarPage("Statistik over fodboldspillere",
                                                        tabPanel("Skudforsøg",
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
                                                        tabPanel("Sammenligning",
                                                                 sidebarPanel(
                                                                   selectInput("land2",
                                                                               label = " Vælg land",
                                                                               choices = c("Holland", "Polen"),
                                                                               selected = "Holland"),
                                                                   selectInput("season2",
                                                                               label = "Vælg sæson",
                                                                               choices = c("21/22"),
                                                                               selected = "21/22"),
                                                                   checkboxInput("assist2",
                                                                                 value = FALSE,
                                                                                 label = "Aflevering skal være assist"),
                                                                   sliderInput("minut21", "Vælg første interval:",
                                                                               min = 0, max = 90, value = c(0, 90),
                                                                               step = 1),
                                                                   sliderInput("minut22", "Vælg andet interval:",
                                                                               min = 0, max = 90, value = c(0, 90),
                                                                               step = 1)
                                                                 ),
                                                                 mainPanel(
                                                                   plotOutput("sammenlign_plot"),
                                                                 )
                                                        ),
                                                        tabPanel("Målramme",
                                                                 sidebarPanel(
                                                                   selectInput("L",
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
                                                                   selectInput("hold",
                                                                               label = "Vælg hold",
                                                                               choices = ""),
                                                                   selectInput("pval3",
                                                                               label = "Vælg spiller",
                                                                               choices = "")
                                                                 ),
                                                                 mainPanel(
                                                                   plotOutput("Heat_map"),
                                                                   dataTableOutput("hold")
                                                                   
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

        df_mean_pass <- df %>% 
          filter(holdvalg == team.name, sæsonid == seasonId, minute >= min1 & minute <= min2) %>% 
          group_by(team.name, plads) %>%
          summarise(average_length = mean(pass.length))

      #ggplot over de længste afleveringer bund 5 og top 5-holdende 
      ggplot(df_mean_pass, aes(x = reorder(team.name, -average_length), y = average_length, fill = plads)) +
        geom_col(position = "dodge") +
        labs(title = "Bund 5 holdene udfører klart længere afleveringer sammenlignet med top 5 holdene",
             subtitle = "Polske liga (Ekstraklasa) Sæson 2021/22",
             x = "Fodboldklub",
             y = "Gennemsnitlig afleveringslængde",
             caption = "Datakilde: WyScout") +
        scale_y_continuous(breaks = seq(min(0), max(ceiling(max(df_mean_pass$average_length) / 5) * 5), by = 5),
                           labels = scales::label_number(suffix = "m")) +
        coord_cartesian(ylim = c(0, ceiling(max(ceiling(max(df_mean_pass$average_length) / 5) * 5)))) + # Corrected coordinate limits
        scale_fill_manual(values = c("Bund" = "#d42a30", "Top" = "steelblue4"), 
                          name = "Niveau") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold"),
              text = element_text(family = "Verdana"))  
      
      
      
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
      
      df %>% 
        filter(holdvalg == team.name, sæsonid == seasonId, minute >= min1 & minute <= min2) %>%
        group_by(Holdnavn = team.name, Placering = plads) %>%
        summarise(
          Succesfulde = sum(pass.accurate == TRUE),
          Misset = sum(pass.accurate == FALSE),
          Pct = paste0(round((Succesfulde / (Succesfulde + Misset)) * 100), "%"),
          Gennemsnitslængde = round(mean(pass.length),2)
        ) %>% 
        arrange(-Gennemsnitslængde)
      
    })   
      ###### Opgave 2.1.2 - Ny graf ######
    output$sammenlign_plot <- renderPlot({
      Land2 = input$land2
      season2 = input$season2
      assist2 = input$assist2
      
      sæsonid <- ifelse(Land2 == "Holland",187502,186215)
      min21_1 <- input$minut21[1]
      min21_2 <- input$minut21[2]
      
      min22_1 <- input$minut22[1]
      min22_2 <- input$minut22[2]
      
      if (assist2 == TRUE) {
        
      df_mean_pass_1 <- df_s %>% 
        filter(sæsonid == seasonId, minute >= min21_1 & minute <= min21_2, sapply(type.secondary, function(x) "assist" %in% x)) %>% 
        group_by(team.name) %>%
        summarise(average_length_1 = mean(pass.length))
      
      df_mean_pass_2 <- df_s %>% 
        filter(sæsonid == seasonId, minute >= min22_1 & minute <= min22_2, sapply(type.secondary, function(x) "assist" %in% x)) %>% 
        group_by(team.name) %>%
        summarise(average_length_2 = mean(pass.length))
      
      } else {
        df_mean_pass_1 <- df_s %>% 
          filter(sæsonid == seasonId, minute >= min21_1 & minute <= min21_2) %>% 
          group_by(team.name) %>%
          summarise(average_length_1 = mean(pass.length))
        
        df_mean_pass_2 <- df_s %>% 
          filter(sæsonid == seasonId, minute >= min22_1 & minute <= min22_2) %>% 
          group_by(team.name) %>%
          summarise(average_length_2 = mean(pass.length))
      }

        df_hold_2 <- df_s %>% 
          ungroup %>% 
          filter(sæsonid == seasonId) %>% 
          select(team.name) %>% 
          distinct()
        
        merge_df <- merge(df_hold_2, df_mean_pass_1, by = "team.name", all.x = TRUE)
        merge_df <- merge(merge_df, df_mean_pass_2, by = "team.name", all.x = TRUE)
        merge_df[is.na(merge_df)] <- 0
      
        df_mean_pass_4 <- melt(merge_df, id.vars = "team.name")

      #ggplot over de længste afleveringer bund 5 og top 5-holdende 
      
      ggplot(df_mean_pass_4, aes(x = team.name)) +
        geom_col(aes(y = value, fill = variable), position = "dodge") +
        labs(title = paste0("Den gennemsnitlige længde for minut ", min21_1, "-", min21_2, " er ", round(mean(df_mean_pass_1$average_length_1),2),"m", "\n",
                            "og for minut ", min22_1, "-", min22_2, " er ", round(mean(df_mean_pass_2$average_length_2), 2),"m"),
             subtitle = ifelse(Land2 == "Holland","I den hollandske liga (Eredivisie) sæson 21/22", "I den polske liga (Ekstraklasa) sæson 21/22"),
             x = "Fodboldklub",
             y = "Gennemsnitlig afleveringslængde",
             caption = "Datakilde: WyScout")+
        scale_y_continuous(breaks = seq(min(0), max(ceiling(max(df_mean_pass_4$value) / 5) * 5), by = 5),
                           labels = scales::label_number(suffix = "m")) +
        coord_cartesian(ylim = c(0, ceiling(max(ceiling(max(df_mean_pass_4$value) / 5) * 5)))) + # Corrected coordinate limits
        scale_fill_manual(values = c("average_length_1" = "#d42a30", "average_length_2" = "steelblue4"), 
                          name = "",
                          labels = c(paste0("Minut ", min21_1,"-", min21_2), paste0("Minut ", min22_1, "-", min22_2))) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold"),
              text = element_text(family = "Verdana"))  
    })
    # Laver tabel til afleveringer på positioner
  output$pass_table <- renderDataTable({
    
    # Definerer valg fra UI
    Land3 = input$land2
    season3 = input$season2
    assist3 = input$assist2
    
    sæsonid<- ifelse(Land2 == "Holland",187502,186215)
    min31_1 <- input$minut21[1]
    min31_2 <- input$minut21[2]
    
    min32_1 <- input$minut22[1]
    min32_2 <- input$minut22[2]
    
    # Variable til at teste
    Land2 <- "Holland"
    season2 <- "21/22"
    assist2 <- FALSE
    seasonid <- 187502
    min21_1 <- 0
    min21_2 <- 90
    min22_1 <- 50
    min22_2 <- 90
    
    if (assist2 == TRUE) {
      
      # Virker ikke :()
      df_pct_pass <- df_s %>% 
        filter(seasonId == seasonid, minute >= min21_1 & minute <= min21_2, sapply(type.secondary, function(x) "assist" %in% x)) %>% 
        group_by(team.name, poss) %>%
        summarise(pass_percentage = n() / nrow(df_s) * 100) %>% 
        spread(poss, pass_percentage, fill = 0) %>% 
        arrange(team.name)
      
    } else {

    }
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

