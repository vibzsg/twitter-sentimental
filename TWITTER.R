#GLOBAL.R
#Allocate memory
options(java.parameters = "-Xmx10g")

## clear console
cat("\014")

## clear global variables
rm(list=ls())

## list of packages required
list.of.packages = c("git2r","digest","devtools",
                     "RCurl","RJSONIO","stringr","syuzhet","httr",
                     "rjson","tm","NLP","RCurl","wordcloud","wordcloud2",
                     "tidytext","dplyr","zipcode","bit", "shiny", "shinythemes")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## for devtools
library(git2r);library(digest)
#require(devtools)
#install_github("hadley/devtools")
library(devtools)
install_github("geoffjentry/twitteR")

## data manipultion
library(dplyr);library(stringr)

# loading the libraries
## Linked to importing tweets
library(rjson);library(httr);library(twitteR);library(zipcode)

## Linked to generating a wordcloud
library(tm);library(NLP);library(RCurl);library(RJSONIO)
library(stringr);library(wordcloud);library(wordcloud2); 

#To create Shiny environment
library(shinythemes)

## Linked to sentiment analysis
library(syuzhet)

# Twitter authentication key
oauth = setup_twitter_oauth(api_key <- "OJxgo8mhQbO0gzf6Hj55CF2Vt",
                            api_secret <- "Ux55toLDdHQfF90f3SA8J7z0UDjaRNEVpo52cqwSOpUWNqX0xb",
                            access_token <- "165363081-0AYHrWTTdan62deuIIIkOwAbhugeGxiKsN53ngir",
                            access_token_secret <- "mHdaxfHS1kduf9Ht4tYuV7nZbLax43oA63FI2bq6KDHbV")
cat("\014")

cleanTweets = function(object.with.tweets){
  # list to dataframe
  df.tweets <- twListToDF(object.with.tweets)
  
  # Removes RT
  df.tweets$text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df.tweets$text)
  
  #Remove non-ASCII characters
  Encoding(df.tweets$text_clean) = "latin1"
  iconv(df.tweets$text_clean, "latin1", "ASCII", sub = "")
  
  # Removes @<twitter handle>
  df.tweets$text_clean = gsub("@\\w+", "", df.tweets$text_clean)
  # Removes punctuations
  df.tweets$text_clean = gsub("[[:punct:]]", "", df.tweets$text_clean)
  # Removes numbers
  df.tweets$text_clean = gsub("[[:digit:]]", "", df.tweets$text_clean)
  # Removes html links
  df.tweets$text_clean = gsub("http\\w+", "", df.tweets$text_clean)
  # Removes unnecessary spaces
  df.tweets$text_clean = gsub("[ \t]{2,}", "", df.tweets$text_clean)
  df.tweets$text_clean = gsub("^\\s+|\\s+$", "", df.tweets$text_clean)
  # Fix for error related to formatting 'utf8towcs'"
  df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
  return(df.tweets)
}

searchThis = function(search_string,geocode_string = "42.375,-71.1061111,1000mi",number.of.tweets = 100)
{
  searchTwitter(search_string,n = number.of.tweets, lang = "en")
}

userTL = function(user.name,number.of.tweets = 100)
{
  userTimeline(user.name,n = number.of.tweets)
}

# Generate Term Document Matrix using stopword list from tm pacakge
tdm.tmStopWord = function(clean.tweets.dataframe){
  # Creates a text corpus from the plain text document for every tweet
  text_corpus = Corpus(VectorSource(clean.tweets.dataframe$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  # creating a Term Document Matrix 
  tdm = TermDocumentMatrix(
    # the text corpus created from the text_clean object
    text_corpus,
    # defining the stopwords to be removed before creating a term document matrix
    control = list(
      removePunctuation = TRUE,
      stopwords("en"),
      removeNumbers = TRUE,
      tolower = TRUE)
  )
  
  return(tdm)
}

# Generate Term Document Matrix using TF-IDF
tdm.TFIDF = function(clean.tweets.dataframe){
  
  text_corpus = Corpus(VectorSource(clean.tweets.dataframe$text_clean))
  
  # Text_corpus is a collection of tweets where every tweet is a document
  tdm <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTfIdf))
  
  return(tdm)
}

# Generate Term Document Matrix without removing stopwords
tdm.tm = function(clean.tweets.dataframe){
  
  text_corpus = Corpus(VectorSource(clean.tweets.dataframe$text_clean))
  tdm = TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      tolower = TRUE))
  
  return(tdm)
}

getSentiments.TF_IDF.nrc = function(tdm.tfidf){
  
  m <- as.matrix(tdm.tfidf)
  
  word_tfidf <- sort(colSums(m), decreasing = TRUE)
  dm <- data.frame(word = names(word_tfidf), tfidf = word_tfidf)
  dm.subset <- dm[dm$tfidf>=quantile(dm$tfidf,0.25),]
  nrc.lex <- get_nrc_sentiment(as.character(dm.subset$word))
  
}

generateWordCloud.positive.tmStopWords = function(tdm.tm.stopword){
  
  
  # converting term document matrix to matrix
  m = as.matrix(tdm.tm.stopword)
  
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing = TRUE)
  
  # create a data frame with words and their frequencies
  dm = data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lexicons = get_nrc_sentiment(as.character(dm$word))
  tweets.positive = dm[nrc.lexicons$positive>0,]
  
}

generateWordCloud.negative.tmStopWords = function(tdm.tm.stopword){
  
  # converting term document matrix to matrix
  m = as.matrix(tdm.tm.stopword)
  
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing = TRUE)
  
  # create a data frame with words and their frequencies
  dm = data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lexicons = get_nrc_sentiment(as.character(dm$word))
  
  tweets.negative = dm[nrc.lexicons$negative>0,]
}

generateWordCloud.positive.TF_IDF = function(tdm.tfidf, tdm.tm.nostop){
  
  # converting term document matrix to matrix
  m <- as.matrix(tdm.tfidf)
  
  word_tfidf <- sort(colSums(m), decreasing = TRUE)
  # create a data frame with words and their frequencies
  dm <- data.frame(word = names(word_tfidf), tfidf = word_tfidf)
  #plot(dm$freq,type = "l")
  dm.subset <- dm[dm$tfidf>=quantile(dm$tfidf,0.25),]
  
  ## creating term frequency dataframe
  m.word.freq <- as.matrix(tdm.tm.nostop)
  word_freqs.word.freq <- sort(colSums(m), decreasing = TRUE)
  dm.word.freq <- data.frame(word = names(word_freqs.word.freq), freq = word_freqs.word.freq)
  
  ## subsetting the tdm 
  dm.word.freq.new <- dm.word.freq[dm.word.freq$word %in% dm.subset$word,]
  
  nrc.lexicons <- get_nrc_sentiment(as.character(dm.word.freq.new$word))
  tweets.positive <- dm.word.freq.new[nrc.lexicons$positive>0,]
}

generateWordCloud.negative.TF_IDF = function(tdm.tfidf, tdm.tm.nostop){
  # converting term document matrix to matrix
  m <- as.matrix(tdm.tfidf)
  
  word_tfidf <- sort(colSums(m), decreasing = TRUE)
  # create a data frame with words and their frequencies
  dm <- data.frame(word = names(word_tfidf), tfidf = word_tfidf)
  #plot(dm$freq,type = "l")
  dm.subset <- dm[dm$tfidf>=quantile(dm$tfidf,0.25),]
  
  ## creating term frequency dataframe
  m.word.freq <- as.matrix(tdm.tm.nostop)
  word_freqs.word.freq <- sort(colSums(m), decreasing = TRUE)
  dm.word.freq <- data.frame(word = names(word_freqs.word.freq), freq = word_freqs.word.freq)
  
  ## subsetting the tdm 
  dm.word.freq.new <- dm.word.freq[dm.word.freq$word %in% dm.subset$word,]
  
  nrc.lexicons <- get_nrc_sentiment(as.character(dm.word.freq.new$word))
  tweets.negative <- dm.word.freq.new[nrc.lexicons$negative>0,]
}


library(shiny)

# Functions for creating fluid page layouts in shiny Application.
# A fluid page layout consists of rows which in turn include columns
################################################################################
ui = fluidPage(
  
  #Setting a theme for the Shiny app| The theme that is used here is called 'darkly'
  #package used here is shinytheme
  theme = shinytheme("darkly"),
  # Defining the header Panel on the shiny application 
  #h3- argument is used to obtain a specific size for the header/ title.
  #windowTitle - The title that should be displayed by the browser window. 
  headerPanel(h3("Twitter Sentiment Analysis:8th Sem project"), windowTitle = "Project"),
  #Sidebar Layout - used to create a layout with a sidebar and main area in the Shiny Aplication.
  sidebarLayout(
    #Create a sidebar panel containing input controls that can in turn be passed to sidebarLayout.
    #img argument is used to load an image into the sodeba panel of the shiny Application
    sidebarPanel(
      # radioButtons -Create a set of radio buttons used to select an item from a list.          
      radioButtons("typeInput", "Extract tweets by: ",
                   list("Hashtag" = "hashtag", "Twitter Username"= "username")),
      #sliderInput -Constructs a slider widget to select a numeric value from a range.
      sliderInput("numberInput", "Select number of tweets",
                  min = 0, max = 3000, value = 100),
      #Creates a panel that is visible or not, depending on the value of the input.
      #Condition 1 - Only show this panel if input type is "Hashtag & Location"    
      conditionalPanel(
        condition = "input.typeInput == 'hashtag'",
        #Create an input control for entry of unstructured text values
        textInput("hashtagInput", "Enter search string","", placeholder = "input search string")),
      
      #Condition 2 - Only show this panel if Input type is "Twitter Username"
      conditionalPanel(
        condition = "input.typeInput == 'username'",
        textInput("usernameInput", "Username", placeholder = "input username")),
      #actionButton - Used to create a go button, that allows the shiny Application the execute the input
      actionButton("goButton", "Search", icon("twitter"),
                   style="color: #fff; background-color: #337ab7") ,width = 3),
    
    #Panel to display output
    # mainPanel - Create a main panel containing output elements that can in turn be passed to sidebarLayout.
    mainPanel(
      #Tabsets - used for dividing output into multiple independently viewable sections.
      #Dividing the main panel into multiple tabs
      tabsetPanel(
        #tabPanel - Create a tab panel that can be included within a tabsetPanel.
        #Argument plotOutput - used to create a plot as an output element based on the inputid that is passed to it
        tabPanel("Sentiment Plots TM", plotOutput("plot1")),
        tabPanel("Sentiments Plots TFIDF", plotOutput("plot3")),
        tabPanel("+/- Plots TM", plotOutput("plot2")),
        tabPanel("+/- Plots TFIDF", plotOutput("plot4")),
        #navbarMenu - Creates a page that contains a top level navigation bar that can be used to toggle a set of tabPanel elements.
        navbarMenu("Word Clouds TM",
                   #tabPanel - Create a tab panel that can be included within a tabsetPanel
                   tabPanel("Positive", wordcloud2Output("wordCloud1", width = "100%", height = "400px")),
                   #wordcloud2Output -used to render a wordcloud object| uses library - wordcloud2
                   tabPanel("Negative", wordcloud2Output("wordCloud2", width = "100%", height = "400px"))),
        navbarMenu("Word Clouds TFIDF",
                   tabPanel("Positive", wordcloud2Output("wordCloud3", width = "100%", height = "400px")),
                   tabPanel("Negative", wordcloud2Output("wordCloud4", width = "100%", height = "400px"))),
        #dataTableOutput -used to render a table as an output
        tabPanel("Tweets", dataTableOutput("tweetTable"))
        ,type = "pills"), width = 9)
  )
)



#SERVER.R
# Server is a function used to render the objects created in the User Interface function of the shiny Application
# It takes the input and output as an argument
server = function(input, output)
{
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data1 = eventReactive(input$goButton, {
    
    # Conditional data extraction based on user input, if the InputID selected by user = hashtag
    if (input$typeInput == "hashtag") 
    {
      #getLatLong.zip - to extract the zipcode from the user input and generate a user defined radius 
      
      
      #tweetOutput - extracts the hashtag provided as input in the shiny application along with the number of tweets and the geographical location
      tweetOutput = searchThis(search_string = input$hashtagInput,
                               number.of.tweets = input$numberInput)
      
    } 
    # Conditional data extraction based on user input, if the InputID selected by user = username
    else if (input$typeInput == "username") 
    {
      tweetOutput <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    #Cleans the tweet
    df.tweets = cleanTweets(tweetOutput)
    #Get sentiments
    nrc.lexicons = get_nrc_sentiment(df.tweets$text_clean)
  })
  
  # ## Rendering TM plots ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot1 and plot2
  output$plot1 = renderPlot({
    
    # ## creating Barplot for emotions ##
    barplot(
      #Sort  - (or order) a vector or factor (partially) into ascending or descending order
      #prop.table - Express Table Entries as Fraction of Marginal Table
      sort(colSums(prop.table(data1()[, 1:8]))), 
      horiz = TRUE, 
      cex.names = 0.8,
      col=rainbow(7),
      las = 1, 
      main = "Emotions in tweets", xlab="Percentage", xlim = c(0,.4))}, 
    width = 700, height = 500)
  
  output$plot2 = renderPlot({
    
    # ## Creating barplot for positive vs negative ##
    barplot(
      sort(colSums(prop.table(data1()[, 9:10]))), 
      horiz = TRUE, 
      cex.names = 0.75, 
      col=c("red","green"),
      las = 1, 
      main = "Ratio of positive to negative tweets",xlab="Percentage", xlim = c(0,1))},
    width = 700, height = 500)
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data2 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      # ## Generate geocode string ## #
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                               number.of.tweets = input$numberInput)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    searchtweet.clean = cleanTweets(tweetOutput)
    
    searchtweet.tdm.tfidf = tdm.TFIDF(searchtweet.clean)
    
    nrc.lex = getSentiments.TF_IDF.nrc(searchtweet.tdm.tfidf)
    
  })
  
  # ## Creating a Render plots for TFIDF ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot3 and plot4
  output$plot3 = renderPlot({
    barplot(
      sort(colSums(prop.table(data2()[, 1:8]))), 
      horiz = TRUE,
      col=rainbow(7),
      cex.names = 0.75, 
      las = 1, 
      main = "Emotions in tweets", xlab="Percentage",xlim = c(0,.4))}, width = 700, height = 500)
  
  output$plot4 = renderPlot({
    barplot(
      sort(colSums(prop.table(data2()[, 9:10]))), 
      horiz = TRUE, 
      col=c("red","green"),
      cex.names = 0.8,
      las = 1, 
      main = "Polarity in tweets", xlab="Percentage", xlim = c(0,1))}, width = 700, height = 500)
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data3 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                               number.of.tweets = input$numberInput)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL = function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets <- cleanTweets(tweetOutput)
    
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(df.tweets)
    
    tweets.positive = generateWordCloud.positive.tmStopWords(searchtweet.tdm.tm.stopword)
    
  })
  
  # ## Render Positive Wordcloud TM ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud1
  output$wordCloud1 = renderWordcloud2({wordcloud2(data = data3())})
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data4 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                               number.of.tweets = input$numberInput)
      
    }
    
    else if (input$typeInput == "username") 
    {
      userTL = function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      tweetOutput <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets = cleanTweets(tweetOutput)
    
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(df.tweets)
    
    tweets.negative = generateWordCloud.negative.tmStopWords(searchtweet.tdm.tm.stopword)
    
  })
  
  # ## Render negative wordcloud TM ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud2
  output$wordCloud2 = renderWordcloud2({wordcloud2(data = data4())})
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data5 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                               number.of.tweets = input$numberInput)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL = function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets = cleanTweets(tweetOutput)
    
    tdm.tfidf = tdm.TFIDF(df.tweets)
    
    tdm.tm.nostop = tdm.tm(df.tweets)
    
    tweets.positive = generateWordCloud.positive.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    
  })
  
  # ##Rendering positive wordcloud for TFIDF ## #
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud3
  output$wordCloud3 = renderWordcloud2({wordcloud2(data = data5())})
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data6 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                               number.of.tweets = input$numberInput)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL = function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets = cleanTweets(tweetOutput)
    
    tdm.tfidf = tdm.TFIDF(df.tweets)
    
    tdm.tm.nostop = tdm.tm(df.tweets)
    
    tweets.negative = generateWordCloud.negative.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    
  })
  
  # ##Render negative wordcloud TFIDF## #
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud4
  output$wordCloud4 = renderWordcloud2({wordcloud2(data = data6())})
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data7 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                               number.of.tweets = input$numberInput)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    #Converting the Tweets into data frame
    df.tweets = twListToDF(tweetOutput)
    
    #only displaying Text, Created, Screen Name, RT count, and Location
    
    # Remove all nongraphical characters
    text = str_replace_all(df.tweets$text,"[^[:graph:]]", " ")
    df.tweets = cbind(text, df.tweets[c(5,11,3,12,17)])
    
    #Changing column names
    colnames(df.tweets) = c("Tweets", "Date", "Username", "Fav Count", "RT Count", "Location")
    tweetOutput = df.tweets
  })
  
  # ##Render tweets## #
  # renderDataTable - Renders a reactive data table that is suitable for assigning to an output slot.
  # In this case the the object used is tweetTable
  output$tweetTable = renderDataTable({data7()}, options = list(lengthMenu = c(10, 30, 50), pageLength = 5))
  
}
library(shiny)


shinyApp(ui=ui, server=server)

