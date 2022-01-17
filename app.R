# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(plotly)
library(RColorBrewer)


# Load data
load('basetable.Rdata')

#Define app

ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("Gaming data overview", theme = shinytheme("yeti"),
             navbarMenu("Overview", icon = icon("globe-americas"),
                        tabPanel("Characteristics",
                                 sidebarLayout(
                                    sidebarPanel(
                                      titlePanel("Customer Characteristics"),
                                      
                                      #Select which factor to plot
                                      radioButtons("factor",
                                                   label = "Select factors:",
                                                   choices = c("Country" = "country",
                                                               "Language" = "language")),
                                      hr(),
                                      sliderInput(inputId = "number1",
                                                  label = "Select top countries:",
                                                  min = 1, max = 20,
                                                  value = c(1, 20),
                                                  width = "300px"),
                                      
                                      sliderInput(inputId = "number2",
                                                  label = "Select bottom countries:",
                                                  min = 1, max = 20,
                                                  value = c(1, 20),
                                                  width = "300px")
                                    ),
                                    mainPanel(plotly::plotlyOutput("map"),
                                              plotly::plotlyOutput("top"),
                                              plotly::plotlyOutput("bottom"))
                                  )
                        ),
                        tabPanel("Time",
                                 h3('First Activation Date'),
                                 plotly::plotlyOutput('first_activation'),
                                 h3("First betting money's date"),
                                 plotly::plotlyOutput('first_betting')
                        ),
                        tabPanel("Frequency",
                                 h3('Total Games'),
                                 plotly::plotlyOutput("gender")
                        )
             ),
             
             navbarMenu("Metrics", icon = icon("poll"),
                        tabPanel("Engagement",
                                 h3('Distribution of engagement'),
                                 plotOutput("metricsplot")
                        ),
                        tabPanel("Recency",
                                 h3('Time difference between last transactions and 31 Dec, 2005'),
                                 plotly::plotlyOutput("recency")
                        ),
                        tabPanel("Profit",
                                 h3('Profit'),
                                 plotOutput("profit")
                        )
             ),
             
             tabPanel("Games", icon = icon("dice"),
                      sidebarLayout(
                        sidebarPanel(titlePanel("Types of games"),
                                     
                          #Select which games to plot
                          selectInput("games", label = "Select game:", 
                                       choices = c("Sports book fixed-odd" = "a",
                                                   "Sports book live-action" = "b",
                                                   "Poker BossMedia" = "c",
                                                   "Casino BossMedia" = "d",
                                                   "Supertoto" = "e",
                                                   "Games VS" = "f",
                                                   "Games bwin" = "g",
                                                   "Casino Chartwell" = "h"),
                                                   selected = "Sports book fixed-odd"
                          )
                        ),
                        mainPanel(plotOutput("games")
                        )
                      )
             )
  )
  
)

#Define the sever logic
server <- function(input, output){
  
  #Create graphs with countries and languages 
   output$map <- plotly::renderPlotly({
     
     if(input$factor == "country"){
       data_country <- basetable %>% group_by(Country) %>%  
         summarise(count = n()) %>%
         arrange(desc(count))
       plot_geo(data_country, locationmode="country names") %>% 
         add_trace(locations = ~Country,
                   z = ~count,
                   color = ~count,
                   colors = 'YlGn') %>% 
         layout(title = "Which country do customers come from?") %>% 
         config(displayModeBar = F) 
  
     }
     else if(input$factor == "language"){
       data_lang <- basetable %>% group_by(Language) %>%  summarise(count = n())
       plot_ly(data_lang, x=~Language, y=~count, type = "bar", marker = list(color = "lightsteelblue")) %>% 
         layout(title = "Language used by gamers",
                xaxis = list(title = "", categoryorder = "total descending"),
                yaxis = list(title = "")) %>% 
         config(displayModeBar = F)
     }
   })
   
   #Create graph with top countries have highest number of gamers
   output$top <- plotly::renderPlotly({
     basetable %>% group_by(Country) %>%  
       summarise(count = n()) %>%
       arrange(desc(count)) %>%  
       slice(min(input$number1) : max(input$number1)) %>% 
       plot_ly(x = ~Country, y = ~count, type = "bar", marker = list(color = "darkseagreen")) %>% 
       layout(title = "Top countries have highest number of gamers",
              xaxis = list(categoryorder = "total descending"),
              yaxis = list(title = "")) %>% 
       config(displayModeBar = F)
   })
   
   #Create graph with top countries have lowest number of gamers
   output$bottom <- plotly::renderPlotly({
     basetable %>% group_by(Country) %>%  
       summarise(count = n()) %>%
       arrange(count) %>%  
       slice(min(input$number2) : max(input$number2)) %>% 
       plot_ly(x = ~Country, y = ~count, type = "bar", marker = list(color = "palegoldenrod")) %>% 
       layout(title = "Top countries have lowest number of gamers",
              xaxis = list(categoryorder = "total descending"),
              yaxis = list(title = "")) %>% 
       config(displayModeBar = F)
   })
  
   #Create density plot with first date of activation
   output$first_activation <- plotly::renderPlotly({
     activate <- basetable %>% group_by(FirstAct) %>%  summarise(value = n())
     plot_activation <- ggplot(activate, aes(FirstAct, value)) +
       geom_line(color = "aquamarine4", size = 1.1) + 
       theme_classic() +
       annotate(geom="text", x = ymd("2005-03-15"), y = 2150,
                label = "Number of activation reached 2,204 \nat Feb 22, 2005") +
       annotate(geom="point", x=ymd("2005-02-22"), y=2204, size=3, shape=21, fill="transparent") +
       labs (x = "Month", y = "") + 
        scale_x_date(date_breaks = "1 month", date_labels = "%B")
     ggplotly(plot_activation) %>%  config(displayModeBar = F)
   }) 
   
   #Create density plot with first date of betting money
   output$first_betting <- plotly::renderPlotly({
     betting <- basetable %>% group_by(FirstPay) %>%  summarise(value = n())
     plot_betting <- ggplot(betting, aes(FirstPay, value)) +
       geom_line(color = "olivedrab3", size = 1.1) + 
       theme_classic() +
       annotate(geom="text", x = ymd("2005-03-17"), y = 2270,
                label = "Number of participants betting money \nreached 2,298 at Feb 23, 2005") +
       annotate(geom="point", x=ymd("2005-02-23"), y=2298, size=3, shape=21, fill="transparent") +
       labs (x = "Month", y= "") + 
       scale_x_date(date_breaks = "1 month", date_labels = "%B") 
     ggplotly(plot_betting) %>%  config(displayModeBar = F)
   }) 
    
   #Create bar graph between gender and total games played per customer
    output$gender <- plotly::renderPlotly({
      plot_gender <- basetable %>%  drop_na(Gender) %>% 
        ggplot(aes(x = Total_Products, fill = Gender)) +
        geom_bar(position = 'stack') +
        theme_classic() +
        labs(x = "Different Number of Games Played", y = "") +
        scale_x_continuous(breaks = 1:8) +
        scale_fill_brewer(palette = 'YlGn')
        ggplotly(plot_gender) %>% config(displayModeBar = F)
    })
   
   #Create histogram of engagement
   output$metricsplot <- renderPlot({
     plot1 <- basetable %>%  drop_na(Engagement_SBFO) %>% 
       ggplot(aes(Engagement_SBFO)) +
       geom_histogram() +
       stat_bin(fill="darkseagreen3") +
       labs(x = "Engagement Sports book fixed-odd")
     plot2 <- basetable %>%  drop_na(Engagement_SBLA) %>% 
       ggplot(aes(Engagement_SBLA)) + 
       geom_histogram() +
       stat_bin(fill="darkseagreen3") +
       labs(x = "Engagement Sports book live-action")
     plot3 <- basetable %>%  drop_na(Engagement_Poker) %>% 
       ggplot(aes(Engagement_Poker)) + 
       geom_histogram() +
       stat_bin(fill="darkseagreen3") +
       labs(x = "Engagement Poker")
     plot4 <- basetable %>%  drop_na(Engagement_CasinoBM) %>% 
       ggplot(aes(Engagement_CasinoBM)) + 
       geom_histogram() +
       stat_bin(fill="darkseagreen3") +
       labs(x = "Engagement Casino Boss Media")
     plot5 <- basetable %>%  drop_na(Engagement_Supertoto) %>% 
       ggplot(aes(Engagement_Supertoto)) + 
       geom_histogram() +
       stat_bin(fill="darkseagreen3") +
       labs(x = "Engagement Supertoto")
     plot6 <- basetable %>%  drop_na(Engagement_GamesVS) %>% 
       ggplot(aes(Engagement_GamesVS)) + 
       geom_histogram() +
       stat_bin(fill="darkseagreen3") +
       labs(x = "Engagement GamesVS")
     plot7 <- basetable %>%  drop_na(Engagement_Gamesbwin) %>% 
       ggplot(aes(Engagement_Gamesbwin)) + 
       geom_histogram() +
       stat_bin(fill="darkseagreen3") +
       labs(x = "Engagement Gamesbwin")
     plot8 <- basetable %>%  drop_na(Engagement_CasinoCW) %>% 
       ggplot(aes(Engagement_CasinoCW)) + 
       geom_histogram() +
       stat_bin(fill="darkseagreen3") +
       labs(x = "Engagement Casino Chartwell")
     grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol=2)
   })
   
   #Create line graph of recency
   output$recency <- plotly::renderPlotly({
     recent <- basetable %>% group_by(Recency) %>%  summarise(value = n()) %>%  arrange(Recency)  %>% slice(1:245)
     p <- ggplot(recent, aes(Recency, value)) + 
       geom_line(color = "darkseagreen", size = 1.1) +
       theme_classic() +
       annotate(geom="text", x = 114, y = 4300,
                label = "Most customers played their last games \n3 months before the end of 2005") +
       annotate(geom="point", x=92, y=4418, size=3, shape=21, fill="transparent") +
       labs(x = "Recency (days)", y = "")
     ggplotly(p) %>%  config(displayModeBar = F)
   })
   
   #Create bar plot of profit
   output$profit <- renderPlot({
     df_profit <- data.frame(round(sum(basetable$`Sports book fixed-odd_profit`, na.rm = TRUE), 1),
                             round(sum(basetable$`Sports book live-action_profit`, na.rm = TRUE), 1),
                             round(sum(basetable$`Casino BossMedia_profit`, na.rm = TRUE),1),
                             round(sum(basetable$`Supertoto_profit`, na.rm = TRUE),1),
                             round(sum(basetable$`Games VS_profit`, na.rm = TRUE),1),
                             round(sum(basetable$`Games bwin_profit`, na.rm = TRUE),1),
                             round(sum(basetable$`Casino Chartwell_profit`, na.rm = TRUE),1))
     #Change column names
     colnames(df_profit) <- c('Sports book fixed-odd', 
                              'Sports book live-action',
                              'Casino BossMedia',
                              'Supertoto',
                              'Games VS',
                              'Games bwin',
                              'Casino Chartwell')
     #Pivot longer 
     df_profit1 <- pivot_longer(df_profit, cols = c('Sports book fixed-odd', 
                                                    'Sports book live-action',
                                                    'Casino BossMedia',
                                                    'Supertoto',
                                                    'Games VS',
                                                    'Games bwin',
                                                    'Casino Chartwell'),
                                names_to = 'type', values_to = 'profit')
     
     #Plot
     ggplot(df_profit1, aes(type, profit, fill = ifelse(profit != max(profit), "other", "HighestProfit"))) +
       geom_col() +
       scale_fill_manual(values=c("chartreuse4", "cornsilk1")) + # add the filling for the 2 groups
       theme_classic() + 
       theme(legend.title = element_blank()) +
       labs(x = "", y = "Profit") +
       geom_text(aes(label=profit),hjust=0.5, vjust=-0.5) + # add the labels, vertically and horizontally
       scale_y_continuous(labels = scales::comma) #change y scales to comma
   })
   
   
   #Create scatter plot of total stakes and total winnings
   output$games <- renderPlot({
     if(input$games == "a"){
       plota1 <- basetable  %>%  
         drop_na(`Sports book fixed-odd_TotalStake`, `Sports book fixed-odd_TotalWins`) %>% 
         ggplot(aes(`Sports book fixed-odd_TotalStake`, `Sports book fixed-odd_TotalWins`)) +
         geom_point(color = "darkolivegreen3", size = 2) +
         theme_classic() +
         labs(x = "Total Stake", y = "Total Wins", title = "Total Stake vs Total Wins") +
         scale_x_continuous(labels = scales::comma) +
         scale_y_continuous(labels = scales::comma)
        
       plota2 <-basetable %>% 
          drop_na(`Sports book fixed-odd_Frequency`) %>% 
          ggplot(aes(`Sports book fixed-odd_Frequency`)) +
          geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
          theme_classic() +
          labs(x = "Frequency", y = "Count", title = "Distribution of frequency")
       
       grid.arrange(plota1, plota2, ncol = 2)
     }
     else if(input$games == "b"){
        plotb1 <- basetable  %>%  
         drop_na(`Sports book live-action_TotalStake`, `Sports book live-action_TotalWins`) %>%
         ggplot(aes(`Sports book live-action_TotalStake`, `Sports book live-action_TotalWins`)) +
         geom_point(color = "darkolivegreen3", size = 2) +
         theme_classic() +
         labs(x = "Total Stake", y = "Total Wins", title = "Total Stake vs Total Wins") +
         scale_x_continuous(labels = scales::comma) +
         scale_y_continuous(labels = scales::comma)
        
        plotb2 <-basetable %>% 
           drop_na(`Sports book live-action_Frequency`) %>% 
           ggplot(aes(`Sports book live-action_Frequency`)) +
           geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
           theme_classic() +
           labs(x = "Frequency", y = "Count", title = "Distribution of frequency")
        
        grid.arrange(plotb1, plotb2, ncol = 2)
     }  
      else if(input$games == "c"){
        plotc1 <- basetable  %>%  
          drop_na(Total_Poker_Amount_Buy, Total_Poker_Amount_Sell) %>%
          ggplot(aes(Total_Poker_Amount_Buy, Total_Poker_Amount_Sell)) +
          geom_point(color = "darkolivegreen3", size = 2) +
          theme_classic() +
          labs(x = "Total Amount Buy", y = "Total Amount Sell", title = "Total Amount Buy vs Total Amount Sell")
        
        plotc2 <-basetable %>% 
          drop_na(PokerFrequencyBuy) %>% 
          ggplot(aes(PokerFrequencyBuy)) +
          geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
          theme_classic() +
          labs(x = "Frequency", y = "Count", title = "Distribution of frequency buy")
        
        plotc3 <-basetable %>% 
          drop_na(PokerFrequencySell) %>% 
          ggplot(aes(PokerFrequencySell)) +
          geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
          theme_classic() +
          labs(x = "Frequency", y = "Count", title = "Distribution of frequency sell")
        
        grid.arrange(plotc1, arrangeGrob(plotc2, plotc3), ncol = 2)
      }
     else if(input$games == "d"){
        plotd1 <- basetable  %>%  
         drop_na(`Casino BossMedia_TotalStake`, `Casino BossMedia_TotalWins`) %>%
         ggplot(aes(`Casino BossMedia_TotalStake`, `Casino BossMedia_TotalWins`)) +
         geom_point(color = "darkolivegreen3", size = 2) +
         theme_classic() +
         labs(x = "Total Stake", y = "Total Wins", title = "Total Stake vs Total Wins") +
         scale_x_continuous(labels = scales::comma) +
         scale_y_continuous(labels = scales::comma)
        
        plotd2 <- basetable %>% 
           drop_na(`Casino BossMedia_Frequency`) %>% 
           ggplot(aes(`Casino BossMedia_Frequency`)) +
           geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
           theme_classic() +
           labs(x = "Frequency", y = "Count", title = "Distribution of frequency")
        
        grid.arrange(plotd1, plotd2, ncol = 2)
     }
     else if(input$games == "e"){
        plote1 <- basetable  %>%  
         drop_na(`Supertoto_TotalStake`, `Supertoto_TotalWins`) %>%
         ggplot(aes(`Supertoto_TotalStake`, `Supertoto_TotalWins`)) +
         geom_point(color = "darkolivegreen3", size = 2) +
         theme_classic() +
         labs(x = "Total Stake", y = "Total Wins", title = "Total Stake vs Total Wins")
        
        plote2 <- basetable %>% 
           drop_na(`Supertoto_Frequency`) %>% 
           ggplot(aes(`Supertoto_Frequency`)) +
           geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
           theme_classic() +
           labs(x = "Frequency", y = "Count", title = "Distribution of frequency")
        
        grid.arrange(plote1, plote2, ncol = 2)
        
     }
     else if(input$games == "f"){
        plotf1 <- basetable  %>%  
         drop_na(`Games VS_TotalStake`, `Games VS_TotalWins`) %>%
         ggplot(aes(`Games VS_TotalStake`, `Games VS_TotalWins`)) +
         geom_point(color = "darkolivegreen3", size = 2) +
         theme_classic() +
         labs(x = "Total Stake", y = "Total Wins", title = "Total Stake vs Total Wins")
        
        plotf2 <- basetable %>% 
           drop_na(`Games VS_Frequency`) %>% 
           ggplot(aes(`Games VS_Frequency`)) +
           geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
           theme_classic() +
           labs(x = "Frequency", y = "Count", title = "Distribution of frequency")
        
        grid.arrange(plotf1, plotf2, ncol = 2)
     }
     else if(input$games == "g"){
        plotg1 <- basetable  %>%  
         drop_na(`Games bwin_TotalStake`, `Games bwin_TotalWins`) %>%
         ggplot(aes(`Games bwin_TotalStake`, `Games bwin_TotalWins`)) +
         geom_point(color = "darkolivegreen3", size = 2) +
         theme_classic() +
         labs(x = "Total Stake", y = "Total Wins", title = "Total Stake vs Total Wins")
        
        plotg2 <- basetable %>% 
           drop_na(`Games bwin_Frequency`) %>% 
           ggplot(aes(`Games bwin_Frequency`)) +
           geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
           theme_classic() +
           labs(x = "Frequency", y = "Count", title = "Distribution of frequency")
        
        grid.arrange(plotg1, plotg2, ncol = 2)
     }
     else {
        ploth1 <- basetable  %>%  
         drop_na(`Casino Chartwell_TotalStake`, `Casino Chartwell_TotalWins`) %>%
         ggplot(aes(`Casino Chartwell_TotalStake`, `Casino Chartwell_TotalWins`)) +
         geom_point(color = "darkolivegreen3", size = 2) +
         theme_classic() +
         labs(x = "Total Stake", y = "Total Wins", title = "Total Stake vs Total Wins") +
         scale_x_continuous(labels = scales::comma) +
         scale_y_continuous(labels = scales::comma)
        
        ploth2 <- basetable %>% 
           drop_na(`Casino Chartwell_Frequency`) %>% 
           ggplot(aes(`Casino Chartwell_Frequency`)) +
           geom_histogram(binwidth = 6, fill = "darkolivegreen3", color = "white") +
           theme_classic() +
           labs(x = "Frequency", y = "Count", title = "Distribution of frequency")
        
        grid.arrange(ploth1, ploth2, ncol = 2)
     }
   })
}
shinyApp(ui = ui, server = server)















