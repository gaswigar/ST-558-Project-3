#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(rlang)
library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(reshape2)
library(RWeka)
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(SnowballC) # for stemming
library(caret)
library(shiny)
library(colourpicker)

source("sentiment_functions.R")


#Readin in our data
analysis_set<-read_csv('C:/Users/gswigart/Documents/NCSU/ST 558/ST-558-Project-3/Data/Analaysis Files/GoodReads_4.csv')  %>%
    filter(!is.na(authoryearofbirth))%>%
    mutate(title=as_utf8_character(title),
           author_clean=as_utf8_character(author_clean),
           choices=paste0(title,' by ',author_clean)) %>%
    arrange(desc(downloads))

#Selecting the columns we want to model 
predictors<-c("authoryearofbirth","publication_year","num_tokens",
              "anger","anticipation","disgust","fear","joy","negative","positive","sadness",
              "surprise","trust","detect_fict","detect_juvenile","detect_stories",
              "detect_drama","detect_history","mean_word_length","perc_stop","lexical_diversity")

#Filtering ot miossing data
modelling_set<-analysis_set %>% select(predictors,'average_rating') %>%  na.omit()


ui <- dashboardPage(
    dashboardHeader(title = "Predicting Goodreads Reviews of Project Gutenberg Projects"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info", icon = icon("dashboard")),
            menuItem("Data Exploration", tabName = "expo", icon = icon("th")),
            menuItem("Clustering", tabName = "clust", icon = icon("th")),
            menuItem("Modelling Goodreads Review ", tabName = "model", icon = icon("th")),
            menuItem("Data", tabName = "data", icon = icon("th"))
        )
    ),
    
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "info",
                    #Adding introduction test . 
                    h2("Data Processing"),
                    
                    p('Project Gutenberg is a collection of over 60,000 free ebooks. It contains classics that you might have read in school such as :'),
                    em('Pride and Prejudice by Jane Austen,'),
                    p(''),
                    em('The Adventures of Sherlock Holmes by Arthur Conan Doyle,'),
                    p(' and ... '),
                    em('The 1994 CIA World Factbook.'),
                    p('This application provides basic sentiment analysis for these books. This includes word clouds, positive and negative sentiment, and emotional sentiment.The first step in this project was collecting the corpuses of the project gutenberg books. Luckily, some people have already done all of the hard work.  Martin Gerlach and Francesc Font-Clos have created a Github repository that can gather all of the current books available through project gutenberg.'),
                    tags$a(href="https://github.com/pgcorpus/gutenberg", "PGCorpus Github"),
                    
                    
                    p('I then signed up for a goodread API key here. Then I used the rgoodreads package to request the review information on each book. The  rgoodread package does not have a function to search by both author and title so I made some small changes to the functions to allow for this. I started off with roughly 50,000 texts and after filtering the data to cap number of tokens, have more than 5 reviews, and removing some missing data I have roughly 5000 records for analysi.  '),
                    tags$a(href="https://www.goodreads.com/api", "GoodReads API"),
                    p('Within this application you will also be able to fit 2 models, for which the user can select the model parameters, a boosted tree and a SVM. You can also download the tokens associated each book at the bottom of the next section. The last section allows you to view all of the data. '),
                    
                    
            ),
            
            # Second tab content
            tabItem(tabName = "expo",
                    h2("Exploring Data"),
                    
                    #Allowing user to analyze the summary statsitics of one varaible. 
                    selectInput('varstats', 'Calculate Summary Statistics For:', names(modelling_set)),
                    
                    #Allosing 
                    tableOutput("sum_stats"),

                    #Allowing author to choose a book for senetimetn analysis and a word cloud. 
                    selectizeInput(inputId='book',label='Choose a Book', choices = NULL),
                    p('Below is a histogram of the average bookreads rating. The average rating is approximately 3.8 and the distributinos appears to be normally distributed. '),
                    
                    plotOutput('hist'),
                    
                    p('There appear to be an association between count of ratinge and the average rating. I would assume that more popular books are more likely to receive higher rating and to have more reviews.'),
                    
                    plotOutput("rate_v_count", click = "plot_click"),
                    
                    verbatimTextOutput("info"),
                    
                    p('The books that are towards the bottom and top of the average rating distribution appear to have a lower number of ratings and number of downloads. Which makes sense because getting a 5 or 1 will be less likely with more review. '),
                    
                    plotOutput("down_v_rate"),
                    
                    p('There is no clear pattern between number of tokens and the number of goodreads ratings. There is fair amount of variation in the number of tokens and the number of ratings. '),
                    plotOutput("toke_v_count"),
                    
                    textOutput("selected_var"),
                    
                    p('Here are some graphical and numerical summaries for the book you selected'),
                    
                    downloadLink("downloadResults", "Download Data"),
                    downloadButton("save", "Save Plot"),
                    
                    #Ploting Word Cloud
                    wordcloud2Output("cloud"),
                    
                    #Graph of sentiment Anlsysis
                    plotOutput("sentiment"),
                    
                    #Adding Download Plot button 
                    
                    
                    
            ),
            
            # Third tab content
            tabItem(tabName = "clust",
                    
                    #K-Means Clustering Code taken form the notes
                    pageWithSidebar(
                        headerPanel('Book k-means clustering'),
                        sidebarPanel(
                            selectInput('xcol', 'X Variable', names(modelling_set)),
                            selectInput('ycol', 'Y Variable', names(modelling_set),
                                        selected=names(modelling_set)[[2]]),
                            numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
                            numericInput('iteration', '# of Iterations of Algorithm', value = 1, min = 1, max = 10)
                        ),
                        mainPanel(
                            p('Please select the number of cluster, number of iterations, and number of variables and play around with the data collected. Most of the variables dont appear to have a clear classfication with one other varaible.'),
                            plotOutput('plot1'),
                            p('Below is a dendogram of a clustering accross all of the predictor varaibles.'),
                            plotOutput('plot2')
                        )
                    )
            ),
            
            # Fourth tab content
            tabItem(tabName = "model",
                    h2("Modelling"),
                    sidebarLayout(
                    sidebarPanel(
                        
                        h3("Predict the Average Review for the Following Observation"),
                        #Creating sliders for new observation prediction 
                        sliderInput(inputId = "authoryearofbirth", label = "authoryearofbirth", min = -750,max = 2020,value = 2000),
                        sliderInput(inputId = "publication_year", label = "publication_year", min = 1950,max = 2020,value = 2000),
                        sliderInput(inputId = "anger", label = "anger", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "anticipation", label = "anticipation", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "disgust", label = "disgust", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "fear", label = "fear", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "joy", label = "joy", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "negative", label = "negative", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "positive", label = "postive", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "sadness", label = "sadness", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "surprise", label = "surprise", min = 0,max =.1,value = .01),
                        sliderInput(inputId = "trust", label = "trust", min = 0,max =.1,value = .01),
                        checkboxInput(inputId = "detect_fict", label = "Fiction",value = TRUE),
                        checkboxInput(inputId = "detect_juvenile",label = "Jevenile", value = TRUE),
                        checkboxInput(inputId = "detect_stories", label = "Stories",value = TRUE),
                        checkboxInput(inputId = "detect_drama", label = "Drama",value = TRUE),
                        checkboxInput(inputId = "detect_history", label = "History",value = TRUE),
                        sliderInput(inputId = "num_tokens", label = "num_tokens", min = 1,max = 1000000,value = 25000),
                        sliderInput(inputId = "mean_word_length", label = "mean_word_length", min = 5,max = 10,value = 6),
                        sliderInput(inputId = "perc_stop", label = "perc_stop", min = 0,max = 1,value = .5),
                        sliderInput(inputId = "lexical_diversity", label = "lexical_diversity", min = 0,max = 1,value = .5)
                        
                    ),
                    #Creating sliders for training model.
                    mainPanel(
                        h3("Build Boosted Tree"),
                        sliderInput(inputId = "ntree",
                                    label = "Select Number of Trees for Boosting:",
                                    min = 200,
                                    max = 300,
                                    value = 201),
                        
                        sliderInput(inputId = "interaction",
                                    label = "Select Interaction Depth for Boosting:",
                                    min = 4,
                                    max = 8,
                                    value = 4),
                        
                        h3("Build Radial SVM"),
                        sliderInput(inputId = "tune",
                                    label = "Select Tune Length for SVM :",
                                    min = 1,
                                    max = 10,
                                    value = 3),
                        
                        
                        
                        #Printing Model Results
                        h4('Boosted Tree Results'),
                        uiOutput('math'),
                        tableOutput("boost_results"),
        
                        
        
                        h4('SVM Results'),                        
                        tableOutput("svm_results"),
                        textOutput("Predictions"),
                    )
                    )
                    
            ),
            
            # Fifth tab content
            tabItem(tabName = "data",
                    h2("View Data"),
                   
                           box(
                               title = , width = NULL, status = "primary",
                               div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
                           )
                    
                    
            )
        )
    )
)
