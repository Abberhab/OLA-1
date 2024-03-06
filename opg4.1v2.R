library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(mongolite)
library(jsonlite)
library(ggsoccer)
library(extrafont)

setwd("D:\\DataAnalyseProjekter\\2Semester\\OLA1\\OPG4")

#####Indhentning af data#####
df <- readRDS("Shot_flat")
df_af <- readRDS("Alt data flatten")
df <- df %>% rowwise() %>% mutate(match=str_split(label,",")[[1]][1],
                                                      score=str_split(label,",")[[1]][2])
df <- df %>% rowwise() %>% mutate(home=str_split(match," - ")[[1]][1],
                                                      away=str_split(match," - ")[[1]][2])
df <- df %>% rowwise() %>% mutate(homescore=str_split(score,"-")[[1]][1],
                                                      awayscore=str_split(score,"-")[[1]][2])
df <- df %>% mutate(longinfo=paste0(match,"_",`matchId`))

# Max value for slider


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
    
  navbarPage(
    "Statistik over fodboldspillere",
      tabPanel("4.1 - xG sammenligning",
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mål",
                        "min antal mål:",
                        min = 1,
                        max = 21,
                        value = 15)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
),
      tabPanel("4.2 Aflevering og spillere",
               sidebarLayout(
                    sidebarPanel(
                      selectInput("land",
                                  label = " Vælg land",
                                  choices = c("Holland","Polen"),
                                  selected = "Holland"),
                      selectInput("hold",
                                  label = "Vælg hjemmehold",
                                  choices = NULL,
                                  selected = NULL),
                      selectInput("kamp",
                                  label = "Vælg kamp",
                                  choices = NULL,
                                  selected = NULL)),
                    # Show a plot of the generated distribution
                    mainPanel(
                      plotOutput("afleveringer")
                    )
                  )
         ),
tabPanel("4.2 - MongoDB",
         sidebarLayout(
           sidebarPanel(
             selectInput("land1",
                         label = " Vælg land",
                         choices = c("Holland","Polen"),
                         selected = "Holland"),
             selectInput("hold1",
                         label = "Vælg hjemmehold",
                         choices = NULL,
                         selected = NULL),
             selectInput("kamp1",
                         label = "Vælg kamp",
                         choices = NULL,
                         selected = NULL)),
           # Show a plot of the generated distribution
           mainPanel(
             plotOutput("afleveringer1")
           )
         )
),
tabPanel("4.3 - Skud i en kamp",
         sidebarLayout(
           sidebarPanel(
             selectInput("land2",
                         label = " Vælg land",
                         choices = c("Holland","Polen"),
                         selected = "Holland"),
             selectInput("hold2",
                         label = "Vælg hjemmehold",
                         choices = NULL,
                         selected = NULL),
             selectInput("kamp2",
                         label = "Vælg kamp",
                         choices = NULL,
                         selected = NULL),
           selectInput("hold3",
                       label = "Vælg hold til statisik",
                       choices = NULL,
                       selected = NULL),
         selectInput("Skud",
                     label = "Skal der være scoret",
                     choices = c("Ja", "Nej", "Blandet"),
                     selected = "Blandet")),
           # Show a plot of the generated distribution
           mainPanel(
             plotOutput("bane"),
             fluidRow(
               column(width = 8,
                      dataTableOutput("playertab")
           ),
         )
),
)
),
)
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
######Opgave 4.1#####
    output$distPlot <- renderPlot({
      #Gemmer variablerne fra valg
      sgoal <- input$mål
      #Filtere dataen ned
      df_goal <- df %>%
        select(player.name,shot.isGoal,shot.xg) %>% 
        group_by(player.name) %>% 
        summarise(
          Antal_mål = sum(shot.isGoal == TRUE),
          Sum_af_xG = sum(shot.xg)
        ) %>% 
        filter(Antal_mål >= sgoal)
      
      # Laver variable til ggplot
      spiller <- df_goal$player.name[which.max(df_goal$Antal_mål)]
      max_mål <- max(df_goal$Antal_mål)
      
        # laver ggplottet
      ggplot(df_goal, aes(x = reorder(player.name, -Antal_mål), y = Antal_mål)) +
        geom_col(fill = "#3b6995") +
        geom_point(aes(y = Sum_af_xG, color = "Sum af xG"), position = position_dodge(width = 0.9), size = 3)+
        labs(title = paste0("Spilleren ", spiller, " har scoret flest antal mål på ", max_mål,
                            ",\nhvilket er højere end hans forventede xG på ", round(df_goal[df_goal$player.name == spiller,]["Sum_af_xG"],2)),
             subtitle = "Den hollandske liga (Eredivisie) sæson 21/22",
             x = "Spiller",
             y = "Antal mål",
             caption = "Datakilde: WyScout",
             color = "") +
        scale_color_manual(values = c("Sum af xG" = "#cd8852")) +
        scale_y_continuous(breaks = seq(min(0), max(ceiling(max(df_goal[,2:3]) / 5) * 5), by = 5)) +
        coord_cartesian(ylim = c(0, ceiling(max(ceiling(max(df_goal[,2:3]) / 5) * 5))))+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold"),
              text = element_text(family = "Verdana"))

    })
######Opgave 4.2 - DF#####
    
    observe({
      #gør hjemmehold automatisk
      land <- ifelse(input$land == "Holland",635,692)
      hold <- df_af %>% 
        filter(competitionId == land) %>% 
        select(team.name) %>% 
        distinct() %>% 
        arrange(team.name)
        
      
      # Opdaterer input til at vælg hjemmehold
      updateSelectInput(session, "hold",
                        choices = hold,
                        selected = )
    })
    observe({
      #Gør kamp automatisk
      kamp <- df_af %>% 
        filter(home == input$hold) %>% 
        select(label,away) %>%
        distinct() %>% 
        arrange(away)
      
      # Opdaterer input til at vælge spiller
      updateSelectInput(session, "kamp",
                        choices = kamp[1],
                        selected = )
    })
    output$afleveringer <- renderPlot({
    
    # Laver data til plot
    afleveringer <- df_af %>% 
      filter(label == input$kamp) %>% 
      select(pass.accurate, team.name) %>% 
      group_by(team.name, pass.accurate) %>% 
      summarise(count = n()) %>% 
      pivot_wider(names_from = pass.accurate, values_from = count, values_fill = 0) %>%
      mutate(total = `TRUE` + `FALSE`)
    afleveringer <- melt(afleveringer, id.vars = "team.name")
    
    # Variable til ggplot
    flest_afl_hold <- afleveringer$team.name[which.max(afleveringer$value)]
    
    hold_data <- afleveringer[afleveringer$team.name == flest_afl_hold, ]
    successrate <- hold_data$value[hold_data$variable == "TRUE"] / hold_data$value[hold_data$variable == "total"] * 100
    
    # ggplot
    ggplot(data = afleveringer, aes(x = team.name, y = value, fill = variable)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("FALSE" = "#d42a30", "TRUE" = "#3fa62d", "total" = "#3b6995"),
                        labels = c("Mislykket aflevering", "Succesfuld aflevering", "Totale afleveringer"),
                        name = "") +
      labs(x = "Hold",
           y = "Antal afleveringer",
           title = paste0(flest_afl_hold, " har lavet flest afleveringer, med en successrate på ", round(successrate, 2), "%"),
           subtitle = paste0("Afleveringsfordeling på kampen ", input$kamp)) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            text = element_text(family = "Verdana"))
    })
######Opgave 4.2 - Mongo#####
    
    observe({
      #gør hjemmehold automatisk
      land <- ifelse(input$land1 == "Holland",635,692)
      con1 <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "matches")
      query <- sprintf('{"competitionId": %d}', land)
      df_opg2 <- con1$find(query = query, fields = '{}')
      df_opg2 <- df_opg2 %>% rowwise() %>% mutate(match=str_split(label,",")[[1]][1],
                                        score=str_split(label,",")[[1]][2])
      df_opg2 <- df_opg2 %>% rowwise() %>% mutate(home=str_split(match," - ")[[1]][1],
                                        away=str_split(match," - ")[[1]][2])
      df_opg2 <- df_opg2 %>% rowwise() %>% mutate(homescore=str_split(score,"-")[[1]][1],
                                        awayscore=str_split(score,"-")[[1]][2])
      df_opg2 <- df_opg2 %>% mutate(longinfo=paste0(match,"_",`_id`))
      
      hold <- df_opg2 %>% 
        select(home) %>% 
        distinct() %>% 
        arrange(home)

      
      # Opdaterer input til at vælg hjemmehold
      updateSelectInput(session, "hold1",
                        choices = hold,
                        selected = )
    })
    observe({
      land <- ifelse(input$land1 == "Holland",635,692)
      con1 <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "matches")
      query <- sprintf('{"competitionId": %d}', land)
      df_opg2 <- con1$find(query = query, fields = '{}')
      df_opg2 <- df_opg2 %>% rowwise() %>% mutate(match=str_split(label,",")[[1]][1],
                                                  score=str_split(label,",")[[1]][2])
      df_opg2 <- df_opg2 %>% rowwise() %>% mutate(home=str_split(match," - ")[[1]][1],
                                                  away=str_split(match," - ")[[1]][2])
      df_opg2 <- df_opg2 %>% rowwise() %>% mutate(homescore=str_split(score,"-")[[1]][1],
                                                  awayscore=str_split(score,"-")[[1]][2])
      df_opg2 <- df_opg2 %>% mutate(longinfo=paste0(match,"_",`_id`))

      #Gør kamp automatisk
      kamp <- df_opg2 %>% 
        filter(home == input$hold1) %>% 
        select(label,away) %>% 
        distinct() %>% 
        arrange(away)
      
      # Opdaterer input til at vælge spiller
      updateSelectInput(session, "kamp1",
                        choices = kamp[1],
                        selected = )
   })
    output$afleveringer1 <- renderPlot({
      kamp <- input$kamp1
      con1 <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "matches")
      query2 <- sprintf('{"label": "%s"}', kamp)
      df_opg2_m <- con1$find(query = query2, fields = '{}')
      df_opg2_m <- df_opg2_m %>% rowwise() %>% mutate(match=str_split(label,",")[[1]][1],
                                                  score=str_split(label,",")[[1]][2])
      df_opg2_m <- df_opg2_m %>% rowwise() %>% mutate(home=str_split(match," - ")[[1]][1],
                                                  away=str_split(match," - ")[[1]][2])
      df_opg2_m <- df_opg2_m %>% rowwise() %>% mutate(homescore=str_split(score,"-")[[1]][1],
                                                  awayscore=str_split(score,"-")[[1]][2])
      df_opg2_m <- df_opg2_m %>% mutate(longinfo=paste0(match,"_",`_id`))
      
      con <- mongo(url = "mongodb://cphola:Cph&Ola123@85.218.176.24:28574/", db = "WyScout", collection = "games")
      matchId_value <- df_opg2_m$`_id`[1]
      query1 <- sprintf('{"matchId": %d, "type.primary": "pass"}', matchId_value)
      df_opg2_g <- con$find(query = query1, fields = '{"matchId": 1, "team.name": 1, "pass.accurate": 1}')
      df_opg2_g <- fromJSON(toJSON(df_opg2_g), flatten = T)

      afleveringer1 <- df_opg2_g %>% 
        select(pass.accurate, team.name) %>% 
        group_by(team.name, pass.accurate) %>% 
        summarise(count = n()) %>% 
        pivot_wider(names_from = pass.accurate, values_from = count, values_fill = 0) %>%
        mutate(total = `TRUE` + `FALSE`)
      afleveringer1 <- melt(afleveringer1, id.vars = "team.name")
      
      # Variable til ggplot
      flest_afl_hold1 <- afleveringer1$team.name[which.max(afleveringer1$value)]
      
      hold_data1 <- afleveringer1[afleveringer1$team.name == flest_afl_hold, ]
      successrate1 <- hold_data$value[hold_data$variable == "TRUE"] / hold_data$value[hold_data$variable == "total"] * 100
      
      # ggplot
      
      ggplot(data = afleveringer1, aes(x = team.name, y = value, fill = variable)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("FALSE" = "#d42a30", "TRUE" = "#3fa62d", "total" = "#3b6995"),
                          labels = c("Mislykket aflevering", "Succesfuld aflevering", "Totale afleveringer"),
                          name = "") +
        labs(x = "Hold",
             y = "Antal afleveringer",
             title = paste0(flest_afl_hold1, " har lavet flest afleveringer, med en successrate på ", round(successrate1, 2), "%"),
             subtitle = paste0("Afleveringsfordeling på kampen ", input$kamp1)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"),
              text = element_text(family = "Verdana"))
      
    })
#####Opgvae 4.3#####
    observe({
      land <- ifelse(input$land2 == "Holland",635,692)
      hold <- df_af %>% 
        filter(competitionId == land) %>% 
        select(team.name) %>% 
        distinct()%>% 
        arrange(team.name)
      updateSelectInput(session, "hold2",
                        choices = hold,
                        selected = )
    })
    observe({
      #Gør kamp automatisk
      kamp <- df_af %>% 
        filter(home == input$hold2) %>% 
        select(label,away) %>%
        distinct() %>% 
        arrange(away)
      
      # Opdaterer input til at vælge spiller
      updateSelectInput(session, "kamp2",
                        choices = kamp[1],
                        selected = )
    })
    observe({
      #Gør kamp automatisk
      hold1 <- df_af %>% 
        filter(label == input$kamp2) %>% 
        select(team.name) %>%
        distinct() %>% 
        arrange(team.name)
      
      # Opdaterer input til at vælge spiller
      updateSelectInput(session, "hold3",
                        choices = c(hold1,"Begge"),
                        selected = )
    })
    
    #Laver datatabel til venstre
    
    output$playertab <- renderDataTable({
      hvalg <- input$hold3
      pskud <- input$Skud
      kvalg <- input$kamp2
      
      if (pskud == "Ja") {
        skud_valg <- TRUE
      } else if (pskud == "Nej") {
        skud_valg <- FALSE
      } else {
        skud_valg <- c(TRUE, FALSE)
      }
      
      if (hvalg=="Begge"){
        ny_df <- df %>%
          filter(label == kvalg, shot.isGoal %in% skud_valg) %>%
          select("Spillernavn"= player.name,"Holdnavn"=team.name,Spillerposition="player.position","Mål"=shot.isGoal,"Minut"=minute)
      } else {
        ny_df <- df %>%
          filter(label == kvalg, shot.isGoal %in% skud_valg,team.name==hvalg) %>%
          select("Spillernavn"= player.name,"Holdnavn"=team.name,Spillerposition="player.position","Mål"=shot.isGoal,"Minut"=minute)
      }

 
    })
    #laver fodboldbanen til Skud fanen
    output$bane <- renderPlot({
      
      hvalg <- input$hold3
      pskud <- input$Skud
      kvalg <- input$kamp2
      
     # hvalg <- "Begge"
     # pskud <- "qq"
     # kvalg <- "Nieciecza - Stal Mielec, 1-1"
      
      if (pskud == "Ja") {
        skud_valg <- TRUE
      } else if (pskud == "Nej") {
        skud_valg <- FALSE
      } else {
        skud_valg <- c(TRUE, FALSE)
      }
      
      if (hvalg=="Begge"){
      ny_df <- df %>%
        filter(label == kvalg, shot.isGoal %in% skud_valg) %>%
        select(location.x, location.y, shot.isGoal,"Hold"=team.name)
      } else {
        ny_df <- df %>%
          filter(label == kvalg, shot.isGoal %in% skud_valg,team.name==hvalg) %>%
          select(location.x, location.y, shot.isGoal,"Hold"=team.name)
      }
      
      skud_df <- df %>%
        filter(team.name==hvalg) %>%
        select(shot.isGoal)

      antal_mål <- sum(skud_df$shot.isGoal==TRUE)
      antal_skud <- nrow(skud_df)
      

      navne <- df %>%
        filter(label == kvalg, shot.isGoal %in% skud_valg) %>% 
        select("Hold"=team.name) %>%
        distinct()
      
      navne <- setNames(1:2,c(navne[1,],navne[2,]))
      

      ggplot(ny_df) +
        annotate_pitch(colour = "white",
                       dimensions = pitch_wyscout,
                       fill   = "steelblue4",
                       limits = FALSE) +
        geom_point(aes(x = location.x, y = location.y, color = shot.isGoal, shape = Hold),
                   size = 3) +
        scale_shape_manual(values = navne, 
                           name = "Team")+
        scale_color_manual(values = c("TRUE"="#35a653", "FALSE" = "#d42a30"), 
                           labels = c("TRUE" = "Mål","FALSE" ="Ikke mål"),
                           name = "") +
        theme_pitch() +
        labs(title = paste(hvalg, "har scoret", antal_mål, "mål, ud af", antal_skud, "skud forsøg"),
             subtitle = paste0("Scoringsrate på ",round(antal_mål/antal_skud*100,2),"%")) +
        theme(panel.background = element_rect(fill = "steelblue4"),
              text = element_text(family = "Verdana"),
              plot.title = element_text(face = "bold")) +
        guides(shape = guide_legend(order = 1),
               color = guide_legend(order = 2))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

