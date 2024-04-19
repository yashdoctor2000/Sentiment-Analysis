library(shiny)
library(shinydashboard)
library(png)
library(plotly)
library(dplyr)
library(rtweet)
library(ggplot2)
library(syuzhet)
library(shinycssloaders)
library(shinyWidgets)
library(memoise)
library(reshape2)
library(rsconnect)
library(highr)


# Define UI for application that draws a histogram
options(spinner.color="#48374f", spinner.color.background="#c8b6d6", spinner.size=0.3)
ui <- fluidPage(setBackgroundColor(
  color = "rgb(26, 44, 66)",
  gradient = c("linear"),
  direction = c("top"),
  shinydashboard = TRUE
),
shinyUI(
  dashboardPage( 
    title = "twitter_app",skin = "blue",
    dashboardHeader(title = "Sentiment Analysis"),
    dashboardSidebar(
      
      sidebarMenu(
        
        menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
        menuItem("Core Analysis",tabName = "Insight_Analysis",icon = icon("chart-bar")),
        menuItem("Top Trends",tabName = "data",icon = icon("poll")),
        menuItem("About",tabName = "tab_about",icon = icon("info"))
        
        
      )),
    dashboardBody(
      
      tags$head(tags$style(HTML(' .skin-blue .main-header .logo {
    background-color: rgb(55, 140, 139); color: rgb(255, 247, 253);
      font-size: 19px;font-family: Times New Roman;text-align: Center;
                    } 
                           /* navbar (rest of the header) */
                          .skin-blue .main-header .navbar {
                          background-color: rgb(55, 140, 139);
                          }
                          /* main sidebar-other tabs than selected one */
                            .skin-blue .main-sidebar {
                              background-color: rgb(68, 74, 84);
                            }
                            /* active selected tab in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: rgb(47, 120, 119);
                         #color:rgb(255,255,255);
                          }
                           '))),
      
      
      
      
      
      tabItems(
        
        tabItem(tabName= "dashboard",
                div(style="display:inline-block;color:#48b5b7;",
                    selectInput(inputId = "Dataset","Select Dataset: ",choice = c("Arizona"="arizona","Eclipse" = "eclipse","Technology" = "technology","Campions League"="champions"),selected = "arizona")),
                
                div(style = "display:inline-block;color:#48b5b7;padding-left:65px",selectInput(inputId = "Methods","Select Method:",choices= c("NRC"="nrc","BING"="bing","AFINN"="afinn","SYUZHET"="syuzhet"),selected = "nrc")),
                
                
                
                
                #value box---------------------------------------------
                fluidRow(tags$style(".small-box.bg-red { background-color: #4BB494 !important; color: #FFFFFF !important; }"),
                         
                         tags$style(".small-box.bg-yellow { background-color: #4AB5A4 !important; color: #FFFFFF !important; }"),
                         tags$style(".small-box.bg-green { background-color: #48B780 !important; color: #FFFFFF !important;}"),
                         
                         valueBoxOutput("hashtag"),
                         
                         
                         
                         valueBoxOutput("total_tweet"),
                         
                         
                         valueBoxOutput("total_share")
                         
                         
                ),
                #sentiment and source graph-----------------------
                fluidRow(
                  
                  box(title="Sentiment",status="info",style="border-radius: 30px;",
                      withSpinner(
                        plotlyOutput("senti"),type=4)),
                  
                  
                  
                  box(title="Trend",status="info",
                      withSpinner(
                        plotlyOutput("so"),type=4))
                  
                  
                )
        ),
        #insight analysis------------------------------------------
        tabItem(tabName = "Insight_Analysis",
                
                fluidRow(
                  
                  
                  tabBox( width = 12,height="606px",
                          tabPanel(title = "Graph 1",status="warning",
                                   withSpinner(
                                     plotlyOutput("p",width="900px",height="500px"),type= 4)),
                          
                          tabPanel(title = "Graph 2",status="warning",
                                   withSpinner(
                                     plotlyOutput("l_s",width="900px",height="500px"),type=4)),
                          
                          tabPanel(title = "Graph 3",status="warning",
                                   withSpinner(
                                     plotOutput("ls",width="900px",height="500px"),type=4)),
                          
                          
                          tabPanel(title = " Graph 4",status="warning",
                                   withSpinner(
                                     plotOutput("follower",width="900px",height="500px"),type=4)),
                          
                          
                          tabPanel(title = "Graph 5",status="warning",
                                   withSpinner(
                                     plotlyOutput("lang_retweet",width="900px",height="500px"),type=4))
                          
                          
                          
                  )
                  
                )
                
        ),
        
        
        #display data--------------------------------------------------------
        tabItem(tabName = "data",
                
                fluidRow(
                  tabBox( width = 12, title = "Summary",
                          
                          tabPanel(title = "Most Liked Tweet",
                                   withSpinner(dataTableOutput("data2"),type = 4)),
                          tabPanel(title = "Most Saved Tweet",
                                   withSpinner( dataTableOutput("shared"),type = 4))
                  )
                )
                
                
        ),
        #about-----------------------------------------------------------
        tabItem("tab_about", 
                fluidRow(tags$p("ABOUT :",style = "font-size: 150%;padding-left: 30px;padding-right: 100 px;padding-bottom: 10px; color: rgb(72, 181, 183);
                                font-family:OCR A Std, monospace;font-weight: bold;"
                ),  
                style = "padding-left: 30px;padding-right: 30px;padding-bottom: 10px;",
                tags$style(".box{border-radius: 15px}"),
                box(
                  status = "info",
                  width = 15,
                  style = "padding-left: 30px;padding-right: 30px;padding-bottom: 10px;",
                  
                  tags$p(
                    class = "text-center",
                    
                    
                    
                    tags$p(
                      "This app can be termed out as a knowledge leading web-tool spanning its 
                                               applications in open market insights.

                                                  The app provides us with real-time unique insights on twitter hashtags 
                                                  which count out to be the most tweeted on the space. It generates the 
                                                  real-time sentiment of the tweets contributing to the trending hashtags. 
                                                  Moreover, it also visualizes interactive graphics and displays us with 
                                                  various inferences of data and its features.
                                                It lays down a generalized yet unique path for research as well as commercial
                                                interests.
                                                   "
                      
                      
                    )    
                    
                    
                  )  
                ),imageOutput("t_image", height = "auto")
                
                
                
                
                
                
                
                )
                
                
                
        )
        
      )
      
    )
  )
)
)
#trend_world<-get_trends("mumbai")
#trend<-head(trend_world$trend,1)
#data<-search_tweets(trend,n=10)






# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive expression to load data based on input$Dataset
  data <- reactive({
    switch(input$Dataset,
           "arizona" = read.csv("data/Arizona.csv"),
           "eclipse" = read.csv("data/Eclipse2024.csv"),
           "technology" = read.csv("data/technology-final.csv"),
           "champions" = read.csv("data/ChampionsLeague_final.csv")
    )
  })
  
  # Define reactive expression for hashtag
  hashtag <- reactive({
    switch(input$Dataset,
           "arizona" = "#arizona",
           "eclipse" = "#Eclipse2024",
           "technology" = "#Tech",
           "champions" = "#ChampionsLeague"
    )
  })
  
  
  # Sentiment analysis output depending on selected method
  output$senti <- renderPlotly({
    df <- data()  # Get the reactive data
    if (input$Methods == "nrc") {
      sa_value <- get_nrc_sentiment(df$Tweet.Text)
      score <- colSums(sa_value[, ])
      score_df <- data.frame(score)
      sa_score <- cbind(sentiment = row.names(score_df), score_df, row.names = NULL)
      p <- ggplot(data = sa_score, aes(x = sentiment, y = score, fill = sentiment)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Sentiment") + ylab("Score") +
        ggtitle("Sentiment Analysis using NRC")
    } else if (input$Methods == "bing") {
      sa_value <- get_sentiment(df$Tweet.Text, method = "bing")
      score <- table(factor(sa_value, levels = c(-1, 0, 1), labels = c("Negative", "Neutral", "Positive")))
      p <- ggplot(data = as.data.frame(score), aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat = "identity") +
        xlab("Sentiment") + ylab("Frequency") +
        ggtitle("Sentiment Analysis using Bing")
    }else if (input$Methods == "afinn") {
      # Using the logic similar to your given method
      sa_value <- get_sentiment(df$Tweet.Text, method = "afinn")
      df["result"] <- sa_value
      positive <- nrow(df[df$result > 0, ])
      negative <- nrow(df[df$result < 0, ])
      neutral <- nrow(df[df$result == 0, ])
      sentiment_counts <- data.frame(Positive = positive, Negative = negative, Neutral = neutral)
      
      # Transforming the data frame for ggplot
      sentiment_df <- melt(sentiment_counts, variable.name = "Sentiment", value.name = "Total")
      
      # Plotting with ggplot
      p <- ggplot(data = sentiment_df, aes(x = Sentiment, y = Total, fill = Sentiment)) +
        geom_bar(stat = "identity") +
        xlab("Sentiment") + ylab("Total Tweets") +
        ggtitle("Sentiment Analysis using AFINN")
    } else if (input$Methods == "syuzhet") {
      # Extracting sentiment using the Syuzhet package's default function
      sa_value <- get_sentiment(df$Tweet.Text, method = "syuzhet")
      df["result"] <- sa_value
      
      # Categorizing sentiments into positive, negative, and neutral
      positive <- nrow(df[df$result > 0, ])
      negative <- nrow(df[df$result < 0, ])
      neutral <- nrow(df[df$result == 0, ])
      
      # Creating a data frame to store these values
      sentiment_counts <- data.frame(Positive = positive, Negative = negative, Neutral = neutral)
      
      # Transforming the data frame for ggplot
      sentiment_df <- melt(sentiment_counts, variable.name = "Sentiment", value.name = "Total")
      
      # Plotting the sentiment analysis results
      p <- ggplot(data = sentiment_df, aes(x = Sentiment, y = Total, fill = Sentiment)) +
        geom_bar(stat = "identity") +
        xlab("Sentiment") + ylab("Total Tweets") +
        ggtitle("Sentiment Analysis using Syuzhet")
      #ggplotly(p)
    }
    ggplotly(p)
  })
  
  output$so <- renderPlotly({
    df <- data()
    # Example source distribution visualization
    source_table <- setNames(data.frame(table(df$Tweet.Crea)), c("Source", "Count"))
    sorted_sources <- source_table[order(source_table$Count, decreasing = TRUE), ]
    top_sources <- head(sorted_sources, 5)
    plot_ly(data = top_sources, labels = ~Source, values = ~Count, type = 'pie', hole = 0.6)
  })
  
  output$data2 <- renderDataTable({
    df <- data()
    max(df$Likes)->Likes
    Most_likes<-subset(df,df$Likes==Likes)
    Most_likes$Tweet.Text->Tweet
    Most_likes$Retweets->Shared
    
    Total<-cbind(Tweet,Likes,Shared)
    as.data.frame(Total)
  })
  
  output$shared <- renderDataTable({
    df <- data()
    max(df$Bookmark.Count)->Saved
    Most_shared<-subset(df,df$Bookmark.Count==Saved)
    Most_shared$Tweet.Text->Tweet
    Most_shared$Likes->Likes
    Total1<-cbind(Tweet,Likes,Saved)
    as.data.frame(Total1)
  })
  
  output$hashtag <- renderValueBox({
    valueBox(value = hashtag(), subtitle = "Current Hashtag", icon = icon("hashtag"), color = "red")
  })
  
  output$total_tweet <- renderValueBox({
    df <- data()
    n <- nrow(df %>% distinct(Tweet.Text))
    valueBox(value = n, subtitle = "Total Tweets", icon = icon("comment-dots"), color = "yellow")
  })
  
  output$total_share <- renderValueBox({
    df <- data()
    total_shares <- sum(df$Bookmark.Count)
    valueBox(value = total_shares, subtitle = "Total Bookmarks", icon = icon("bookmark"), color = "green")
  })
  
  output$p <- renderPlotly({
    df <- data()
    p <- ggplot(df, aes(x = Impressions, y = Likes)) +
      geom_point() +
      geom_hline(yintercept = mean(na.omit(df$Likes)), color = "red") +
      geom_vline(xintercept = mean(na.omit(df$Impressions)), color = "green") +
      ggtitle("Impression vs Likes")
    ggplotly(p)
  })
  
  output$l_s <- renderPlotly({
    df <- data()
    l_s <- ggplot(df, aes(x = Impressions, y = Replies)) +
      geom_point() +
      geom_smooth(method = "loess") +
      ggtitle("Impression vs Replies")
    ggplotly(l_s)
  })
  
  output$ls <- renderPlot({
    df <- data()
    ls <- ggplot(df, aes(x = Likes, y = Quotes)) +
      geom_point() +
      geom_violin(fill = "pink") +
      ggtitle("Likes vs Quoted Tweets")
    ls
  })
  
  output$follower <- renderPlot({
    df <- data()
    follower <- ggplot(df, aes(x = Impressions, y = Bookmark.Count)) +
      geom_point() +
      geom_violin() +
      ggtitle("Impressions vs Bookmarks")
    follower
  })
  
  output$lang_retweet <- renderPlotly({
    df <- data()
    lang_retweet <- ggplot(df, aes(x = Likes, y = Retweets)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      ggtitle("Likes vs Retweet Count")
    ggplotly(lang_retweet)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)