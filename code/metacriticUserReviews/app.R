library(shiny)
library(shinydashboard)
library(shinyalert)

library(DT)

library(tidyverse)
library(reticulate)

library(tidytext)
library(stringi)

library(plotly)
library(ggwordcloud)
library(ggraph)
library(widyr)

# python function to pull metacritic user reviews using BeautifulSoup
reticulate::py_run_string("# !pip install requests
import requests
# !pip install bs4
from bs4 import BeautifulSoup

#import time
#import random as rand 
# !pip install pandas
import pandas as pd

# source: https://towardsdatascience.com/web-scraping-metacritic-reviews-using-beautifulsoup-63801bbe200e
def metacritic(title='the-last-of-us-part-ii',console='playstation-4',source='user',numpages=1):
  review_dict = {'name':[], 'date':[], 'rating':[], 'review':[]}#, 'thumbsUp':[], 'thumbsTotal':[]}
  for page in range(0,numpages): #Remember to update the number of pages 
      url = 'https://www.metacritic.com/game/' + console + '/' + title + '/' + source + '-reviews?page='+str(page)
      user_agent = {'User-agent': 'Mozilla/5.0'}
      response  = requests.get(url, headers = user_agent)
      #time.sleep(rand.randint(3,30)) 
      soup = BeautifulSoup(response.text, 'html.parser')
      for review in soup.find_all('div', class_='review_content'):
          if review.find('div', class_='name') == None:
                         break 
          try:
            review_dict['name'].append(review.find('div', class_='name').find('a').text)
          except AttributeError:
            review_dict['name'].append('none')
          try:
            review_dict['date'].append(review.find('div', class_='date').text)
          except AttributeError:
            review_dict['date'].append('none')
          try:
            review_dict['rating'].append(review.find('div', class_='review_grade').find_all('div')[0].text)
          except AttributeError:
            review_dict['rating'].append('none')
            # review_dict['thumbsUp'].append(review.find
            # ('span',class_='total_ups').text)
            # review_dict['thumbsTotal'].append(review.find('span',class_='total_thumbs').text)
          try:
            if review.find('span', class_='blurb blurb_expanded'):
                review_dict['review'].append(review.find('span', class_='blurb blurb_expanded').text)
            else:
                review_dict['review'].append(review.find('div', class_='review_body').find('span').text)
          except AttributeError:
            review_dict['review'].append('none')
  return(pd.DataFrame(review_dict))")

data("stop_words",package = "tidytext")

# bing <- tidytext::get_sentiments("bing")
# saveRDS(bing,"code/metacriticUserReviews/data/bing.rds")
# afinn <- tidytext::get_sentiments("afinn")
# saveRDS(afinn,"code/metacriticUserReviews/data/afinn.rds")
# loughran <- tidytext::get_sentiments("loughran")
# saveRDS(loughran,"code/metacriticUserReviews/data/loughran.rds")
# nrc <- tidytext::get_sentiments("nrc")
# saveRDS(nrc,"code/metacriticUserReviews/data/nrc.rds")
bing <- readRDS("data/nrc.rds")
afinn <- readRDS("data/afinn.rds")
loughran <- readRDS("data/loughran.rds")
nrc <- readRDS("data/nrc.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinybusy::add_busy_spinner(),
  # Application title
  titlePanel("Metacritic User Reviews"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectizeInput(inputId = "titleInput",
                                label = "Game Title",
                                choices = c("Choose one or type your own" = "",
                                            "Cyberpunk 2077",
                                            "No Man's Sky",
                                            "The Last of Us Part I",
                                            "The Last of Us Part II",
                                            "The Legend of Zelda: Breath of the Wild"),
                                multiple = FALSE,
                                options = list(create=TRUE)),
                 # textInput(inputId = "titleInput",label = "Game Title",value = "The Last of Us Part I"),
                 selectizeInput(inputId = "platformInput",
                                label = "Platform/Console",
                                choices = c("Choose one or more, or type your own" = "","PC","Playstation 5","Playstation 4","Xbox Series X","Xbox Series S","Xbox One","Switch"),
                                multiple = TRUE,
                                options = list(create=TRUE)),
                 numericInput(inputId = "numReviews",label = "Number of reviews (at most)",value = 100,min = 1,step = 1),
                 selectizeInput(inputId = "ignoreWords",
                                label = "Ignore these words",
                                choices = c("Choose one or more, or type your own" = "","game"),
                                multiple = TRUE,
                                options = list(create=TRUE)),
                 actionButton(inputId = "searchMetacriticButton",label = "Search Metacritic")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 9,
              tabsetPanel(id = "metacriticTabs",
                          tabPanel(title = "Basic Summary",
                                   fluidRow(column(width = 5,
                                                   htmlOutput("dataSummary")),
                                            column(width = 7,
                                                   plotOutput(outputId = "ratingBarchart"))),
                                   fluidRow(column(width = 6,
                                                   h4("Most Common Words"),
                                                   dataTableOutput("commonUnigrams")),
                                            column(width = 6,
                                                   h4("Most Common Word Pairs"),
                                                   dataTableOutput("commonBigrams")))),
                          tabPanel(title = "Ratings Over Time",
                                   h4("Click on a point to see all associated reviews."),
                                   fluidRow(width = 12,
                                            plotly::plotlyOutput(outputId = "ratingsOverTime")),
                                   fluidRow(width = 12,
                                            dataTableOutput(outputId = "ratingsOverTime_reviews"))),
                          tabPanel(title = "Single Word Sentiment",
                                   fluidRow(radioButtons(inputId = "sentimentMeasure",
                                                         label = "Sentiment Measure",
                                                         choiceNames = c("Bing","Loughran","NRC","AFINN"),
                                                         choiceValues = c("bing","loughran","nrc","afinn"),
                                                         inline = TRUE),
                                            numericInput(inputId = "sentimentMinFreq",
                                                         label = "Min. word frequency (larger shows fewer words)",
                                                         min = 1,step = 1,
                                                         value = 10)),
                                   fluidRow(h4("Positive/Negative Wordclouds"),
                                            plotOutput("sentimentWordcloud"))),
                          tabPanel(title = "Word Pairs",
                                   fluidRow(numericInput(inputId = "graphMinFreq",
                                                         label = "Min. pair frequency (larger shows fewer words)",
                                                         min = 1,step = 1,
                                                         value = 10)),
                                   fluidRow(h4("Word Pair Directed Graph"),
                                            plotOutput("bigramDirectedGraph")))
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # update the "ignore words" select input with the title of the game/console
  toListen <- reactive({list(input$titleInput,input$platformInput)})
  
  observeEvent(toListen(),{
    
    req(input$titleInput)
    req(input$platformInput)
    
    titleSplit <- input$titleInput %>%
      tolower() %>%
      stringr::str_remove_all("[[:punct:]]+") %>%
      stringr::str_split(" ") %>%
      .[[1]]
    
    titleSplit <- titleSplit[!(titleSplit %in% stop_words$word)]
    
    consoleSplit <- input$platformInput %>%
      map(~ {
        tolower(.)  %>%
          tolower() %>%
          stringr::str_remove_all("[[:punct:]]+") %>%
          stringr::str_split(" ") %>%
          .[[1]]
      }) %>%
      unlist() %>%
      unique()
    
    consoleSplit <- consoleSplit[!(consoleSplit %in% stop_words$word)]
    
    updateSelectizeInput(session = session,
                         inputId = "ignoreWords",
                         choices = c("Choose one or more, or type your own" = "","game",titleSplit,consoleSplit))
  })
  
  observeEvent(input$searchMetacriticButton,{
    
    # browser()
    
    title <- input$titleInput %>%
      tolower() %>%
      stringr::str_remove_all("[[:punct:]]+") %>%
      stringr::str_replace_all(" ","-")
    
    # browser()
    
    console <- input$platformInput %>%
      map_chr(~ {
        tolower(.)  %>%
          stringr::str_remove_all("[[:punct:]]+")%>%
          stringr::str_replace_all(" ","-")
      })
    
    reviews <- map2_dfr(console,input$platformInput,~ {
      ret <- py$metacritic(title = title,console = .x,numpages = as.integer(input$numReviews/100))
      ret <- ret %>%
        mutate(platform = .y)
      
      if(nrow(ret) > 0){
        ret <- ret  %>%
          mutate(name = as.character(name),
                 date = lubridate::mdy(date),
                 rating = as.numeric(rating),
                 review = as.character(review))
      }
      
    })
    
    # reviews <- py$metacritic(title = title,console = cons,numpages = as.integer(input$numReviews/100)) 
    
    # validate(nrow(reviews) > 0,"Couldn't find game reviews. Make sure to use full game and platform names and try again.")
    
    urlsAccessed <- map_chr(console,~ {
      
      paste0("https://www.metacritic.com/game/",.,'/',title,"/user-reviews")
      
    })
    
    if(nrow(reviews) < 1){
      shinyalert::shinyalert(title = "Unable to find game reviews.",
                             text = paste0("We tried to access: ",paste(urlsAccessed,collapse = "</br>"),"</br>Make sure that you type the full name of the game and console(s) and try again."),
                             type = "error",
                             closeOnClickOutside = TRUE)
    }
    req(nrow(reviews) > 0)
    
    
    ##### Code for Basic Summary tab
    
    output$dataSummary <- renderUI({
      
      reviewsByPlatform <- reviews %>%
        group_by(platform) %>%
        summarize(n = n()) %>%
        pmap_chr(~ {
          
          paste0(..1,": ",..2)
          
        }) %>%
        paste(collapse = ", ")
      
      aveRatingPerPlatform <- reviews %>%
        group_by(platform) %>%
        summarize(rating = round(mean(rating,na.rm=TRUE),2)) %>%
        pmap_chr(~ {
          
          paste0(..1,": ",..2)
          
        }) %>%
        paste(collapse = ", ")
      
      medRatingPerPlatform <- reviews %>%
        group_by(platform) %>%
        summarize(rating = median(rating,na.rm=TRUE)) %>%
        pmap_chr(~ {
          
          paste0(..1,": ",..2)
          
        }) %>%
        paste(collapse = ", ")
      
      urlsHTML <- map_chr(urlsAccessed,~ {
        
        paste0("<p><a href=",.," target='_blank'>",.,"</a></p>")
        
      })
      
      HTML(paste0("<b>Number of reviews:</b></br>",nrow(reviews)," (",reviewsByPlatform,")","</br>",
                  "<b>Average Rating:</b></br>",round(mean(reviews$rating,na.rm=TRUE),2)," (",aveRatingPerPlatform,")","</br>",
                  "<b>Median Rating:</b></br>",median(reviews$rating,na.rm=TRUE)," (",medRatingPerPlatform,")","</br></br>",
                  "<b>Earliest review date:</b></br>",min(reviews$date),"</br>",
                  "<b>Latest review date:</b></br>",max(reviews$date),"</br></br>",
                  "<b>URL(s) Accessed:</b></br>",paste(urlsHTML,collapse=" ")))
      
    })
    
    # reviews <- reviews %>%
    #   mutate(thumbsUp = as.numeric(thumbsUp),
    #          thumbsTotal = as.numeric(thumbsTotal),
    #          propThumbs = thumbsUp/thumbsTotal)
    
    output$ratingBarchart <- renderPlot({
      
      reviews %>%
        group_by(platform,rating) %>%
        tally() %>%
        ggplot() +
        geom_bar(aes(x = rating,y = n,fill = platform),
                 stat = "identity",position = position_dodge2(preserve = "single")) +
        theme_bw() +
        scale_x_continuous(breaks = 0:10,limits = c(0,10)) +
        labs(x = "Rating",y = "Number of Reviews",
             title = paste0("Metacritic Ratings for ",isolate(input$titleInput))) +
        scale_fill_manual(values = RColorBrewer::brewer.pal(n = max(3,length(console)),name = "Dark2"))
    })
    
    unigrams <- reviews %>%
      tidytext::unnest_tokens(word,review) %>%
      # mutate(word = stringi::stri_trans_general(word,"Latin-ASCII")) %>%
      anti_join(stop_words,by = "word") %>%
      filter(!(word %in% input$ignoreWords))
    
    output$commonUnigrams <- renderDataTable({
      
      unigrams %>%
        group_by(platform,word) %>%
        tally(sort = TRUE) %>%
        pivot_wider(id_cols = c(word),names_from = platform,values_from = n) %>%
        mutate(across(is.numeric,~ ifelse(is.na(.),0,.))) %>%
        DT::datatable()
      
    })
    
    bigrams <- reviews %>%
      tidytext::unnest_tokens(bigram,review,token = "ngrams",n=2) %>%
      # mutate(bigram = stringi::stri_trans_general(bigram,"Latin-ASCII")) %>%
      tidyr::separate(col = bigram,into = c("word1","word2"),sep = " ",remove = FALSE) %>%
      filter(!(word1 %in% stop_words$word | word2 %in% stop_words$word))
    
    output$commonBigrams <- renderDataTable({
      
      bigrams %>%
        filter(!(word1 %in% input$ignoreWords | word2 %in% input$ignoreWords)) %>%
        group_by(platform,bigram) %>%
        tally(sort = TRUE) %>%
        pivot_wider(id_cols = c(bigram),
                    names_from = platform,
                    values_from = n) %>%
        mutate(across(is.numeric,~ ifelse(is.na(.),0,.))) %>%
        DT::datatable()
      
    })
    
    ####### Code for Ratings Over Time tab
    
    aveRating <- 
      reviews %>%
      # mutate(platform = factor(platform)) %>%
      arrange(date) %>%
      group_by(date,platform) %>%
      summarize(aveRating = mean(rating),
                numRatings = n()) %>%
      group_by(platform) %>%
      mutate(runningAveRating = cummean(aveRating)) %>%
      ungroup()
    
    output$ratingsOverTime <- 
      plotly::renderPlotly({
        aveRating %>%
          ggplot(aes(x = date,colour = platform)) +
          geom_jitter(aes(y = aveRating,text = paste0("# Ratings: ",numRatings)),
                      alpha = .4,width = 0,height = .5) +
          geom_line(aes(y = runningAveRating),linewidth = 1.3) +
          theme_bw() +
          scale_y_continuous(breaks = 0:10,limits = c(0,10)) +
          labs(x = "Date",y = "Daily Average Rating",title = "Average Rating Over Time") +
          scale_colour_manual(values = RColorBrewer::brewer.pal(n = max(3,length(console)),name = "Dark2"))
      })
    
    
    output$ratingsOverTime_reviews <-
      renderDataTable({
        
        timeSeriesClick <- plotly::event_data(event = "plotly_click",session = session)
        
        # indexing starts at 0, we only want the user to click on points, which
        # are indexed lower than the curves
        req(timeSeriesClick$curveNumber < (length(input$platformInput)))
        
        # browser()
        
        selectedPlatform <- levels(factor(aveRating$platform))[timeSeriesClick$curveNumber + 1]
        
        selectedDate <- aveRating %>%
          filter(platform == selectedPlatform) %>%
          arrange(date) %>%
          slice(timeSeriesClick$pointNumber + 1) %>%
          pull(date)
        
        reviews %>%
          filter(date == selectedDate & platform == selectedPlatform)
        
      })
    
    ####### code for Sentiment tab
    
    lou_user_sentiment <- unigrams %>%
      left_join(bing %>%
                  rename(bing = sentiment),
                by = "word") %>%
      left_join(afinn %>%
                  rename(afinn = value),
                by = "word") %>%
      mutate(afinn = ifelse(afinn < 0,"negative","positive")) %>%
      left_join(loughran %>%
                  rename(loughran = sentiment),
                by = "word") %>%
      left_join(nrc %>%
                  rename(nrc = sentiment),
                by = "word") %>%
      pivot_longer(cols = c("bing","loughran","nrc","afinn"),
                   names_to = "measure",values_to = "value")
    
    output$sentimentWordcloud <- renderPlot({
      
      plt <- lou_user_sentiment %>%
        filter(measure == input$sentimentMeasure) %>%
        filter(value %in% c("positive","negative")) %>%
        group_by(measure,value,word,platform) %>%
        summarize(n = n()) %>%
        filter(n > input$sentimentMinFreq) %>%
        ggplot() +
        geom_text_wordcloud(aes(label = word,size = n)) +
        facet_grid(rows = vars(platform),
                   cols = vars(value),
                   labeller = label_both,
                   scales = "free",
                   space = "free") +
        theme_bw() +
        theme(strip.text = element_text(size = 10),
              panel.spacing = unit(2, "lines"))
      
      return(plt)
    })
    
    ###### code for Word Pairs tab
    
    output$bigramDirectedGraph <- renderPlot({
      
      set.seed(2020)
      
      bigrams <- bigrams %>%
        filter(!(word1 %in% input$ignoreWords | word2 %in% input$ignoreWords)) %>%
        group_by(word1,word2,platform) %>%
        tally(sort  = TRUE) %>%
        filter(n > input$graphMinFreq)
      
      validate(
        need(nrow(bigrams) > 0,message = "Unable to display graph. Decrease minimum frequency.")
      )
      req(nrow(bigrams) > 0)
      
      # browser()
      
      bigrams %>%
        igraph::graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_parallel(aes(edge_alpha = n,colour = platform), #show.legend = FALSE,
                           arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), 
                           end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void() +
        theme(strip.text = element_text(size = 10),
              panel.spacing = unit(2, "lines"),
              panel.background = element_rect(fill = NA, color = "black")) +
        guides(edge_alpha = "none") +
        theme(legend.position = "bottom") +
        scale_edge_colour_manual(values = RColorBrewer::brewer.pal(n = max(3,length(console)),name = "Dark2"))
      
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
