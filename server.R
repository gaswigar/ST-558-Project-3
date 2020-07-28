#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

nrc<-read_csv('./Data/datasets_507452_937769_NRC.csv')

analysis_set<-read_csv('./Data/Analaysis Files/GoodReads_4.csv')  %>%
    filter(!is.na(authoryearofbirth))%>%
    mutate(title=as_utf8_character(title),
           author_clean=as_utf8_character(author_clean),
           choices=paste0(title,' by ',author_clean)) %>%
    arrange(desc(downloads))

predictors<-c("authoryearofbirth","publication_year","num_tokens",
              "anger","anticipation","disgust","fear","joy","negative","positive","sadness",
              "surprise","trust","detect_fict","detect_juvenile","detect_stories",
              "detect_drama","detect_history","mean_word_length","perc_stop","lexical_diversity")


modelling_set<-analysis_set %>% select(predictors,'average_rating') %>%  na.omit()

server <- function(input, output,session) {
    
    updateSelectizeInput(session, 'book', choices = c(analysis_set$choices), server = TRUE)
    
    
    #Creating Summary Statistics Table of the
    output$sum_stats <- renderTable({
        summary(analysis_set %>% select(input$varstats))
        
    })
    
    #Histogram of AVerage Rating
    output$hist <- renderPlot({
        ggplot(analysis_set,aes(x=average_rating))+
            geom_histogram(bins=50)+
            xlab('Average Rating')+
            ggtitle('Histogram of Average Rating')
        
    })
    
    #CLickable graph of Rate vs Count
    output$rate_v_count <- renderPlot({
        
        X<-log(analysis_set$ratings_count)
        Y<-analysis_set$average_rating
        par(mar = c(3, 3, 3, 3))
        plot(x=X,
             y=Y,
             main='Average Rating vs Rating Count',
             xlab = "Log Count of Ratings", 
             ylab = "Average Rating")
        abline(lm(Y ~ X, data = analysis_set), col = "blue")
    })
    
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    #Plot of Log downloads vs log ratings count
    output$down_v_rate <- renderPlot({
        
        ggplot(analysis_set,aes(x=log(downloads),y=log(ratings_count),color=as.factor(average_rating_quant)))+
            geom_point()+
            geom_smooth()+
            ggtitle('Number of Downloads vs Number of Ratings.')+
            xlab('Log Number of Downloads')+
            ylab('Log Number of Ratings') +
            labs(color ='Quantile of Rating')
    })
    
    
    
    output$toke_v_count <- renderPlot({
        
        ggplot(analysis_set,aes(x=log(num_tokens),y=log(ratings_count)))+
            geom_point()+
            geom_smooth()+
            xlab('Log Number of Tokens')+
            ylab('Log Count of Ratings') +
            ggtitle('umber of Tokens vs Number of Ratings')
        
    })
    
    
    #Reactively reading in the tokens for he selected text
    dataInput<- reactive({
        analysis_set_book <- analysis_set %>%
            filter(choices == input$book)
        
        tokens<-data.frame(word=read_lines(analysis_set_book$path[1]))
    })
    
    #Including the title name of the book further down in the report
    output$selected_var <- renderText({ 
        paste(input$book)
    })
    
    #Creating word cloud
    output$cloud <- renderWordcloud2({
        wordcloud2(frequentTerms(data.frame(dataInput())$word), size=0.4)
    })
    
    
    output$sentiment <- renderPlot({
        # Frequency of each sentiment
        ggplot(dataInput() %>%
                   inner_join(nrc, "word") %>%
                   count(word, sentiment, sort=TRUE), 
               
               aes(x=reorder(sentiment, -n, sum), y=n)) +
            geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
            labs(x="Sentiment", y="Frequency") +
            theme_bw()
    })
    
    resultsTable <- reactive({analysis_set_book <- analysis_set %>%
                                 filter(choices == input$book)
                             
                             tokens<-data.frame(word=read_lines(analysis_set_book$path[1]))%>%
                                 inner_join(nrc, "word") %>%
                                 count(word, sentiment, sort=TRUE)
                             })
    
    
    p <- reactive({ggplot(dataInput() %>%
                              inner_join(nrc, "word") %>%
                              count(word, sentiment, sort=TRUE), 
                          
                          aes(x=reorder(sentiment, -n, sum), y=n)) +
            geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
            labs(x="Sentiment", y="Frequency") +
            theme_bw()})
    
    #Download Handle for Graph
    output$save <- downloadHandler(
        file = "save.png" , # variable with filename
        content = function(file) {
            ggsave(p(), filename = file)
        })
    
    
    output$downloadResults <- downloadHandler(
        filename = function() {
            paste0("study_results_download_", Sys.Date(), ".csv")
        },
        content = function(file) {
            write_csv(resultsTable(), file)
        }
    )
    
    
    
    selectedData <- reactive({
        modelling_set[, c(input$xcol, input$ycol)]
    })
    
    
    clusters <- reactive({
        set.seed(10)
        kmeans(selectedData(), input$clusters, iter.max = input$iteration, algorithm = "MacQueen")
    })
    
    #Rending KNN Plot 
    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        par(mar = c(3, 3, 3, 3))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3,
             main = 'KNN Classification')
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    #Rendering clustering plot. 
    output$plot2 <- renderPlot({
        plot(hclust(dist(selectedData())),xlab = "")
        
    })
    
    #Adding Description of RBF
    output$math <- renderUI({
        withMathJax(
            helpText('We calculate a Support Vector Machine using a Radial Basis Function (RBF) kernel. The RBF kernel was selected because it can fit to non-linear decision boundaries. RBF kernels may not be suitable for a large number of features; however, we only have few available predictors in our dataset. The radial kernel is defined as the below where p is the number of predictors and is a positive constant. We perform a 3 times repeated 10 fold cross validation grid search to estimate the accuracy associated with various model parameters C and sigma. This prevents overfitting while still tuning the model. $$K(_K(xi,xi′ ) = exp(−\\gamma \\sum^{p}_{j}=x_{ij} − x_{i′j})2)$$')
        )
    })
    
    

    #Training Boost Input
    BoostInput<- reactive({
        set.seed(628)
        train.control <- trainControl(method = "repeatedcv",number = 10,repeats = 3)
        
        n.trees<-c(input$ntree)
        interaction.depth<-c(input$interaction)
        shrinkage<-c(.05)
        n.minobsinnode<-c(10)
        param_grid<-data.frame(crossing(n.trees,interaction.depth,shrinkage,n.minobsinnode))
        
        boost_fit = caret::train(average_rating ~ .,
                                 data=modelling_set,
                                 method="gbm",
                                 trControl=train.control,
                                 tuneGrid=param_grid,
                                 verbose=FALSE)
        
    })
    
    #Creating a progress bar
    output$boost_results <- renderTable({
        withProgress(message = 'Training Boosting Model ',
                     detail = 'This may take a while...', value = 0, {
                         boost_reg<-BoostInput()
                         boost_reg$results
                     })
    })
    
    #Training SVM 
    SVMInput<- reactive({
        set.seed(628)
        train.control <- trainControl(method = "repeatedcv",number = 10,repeats = 3)
        
        svm_full_reg <- caret::train(average_rating ~ .,
                                     data=modelling_set,  
                                     method = "svmRadial", 
                                     trControl = train.control, 
                                     preProcess = c("center","scale"), 
                                     tuneLength = input$tune)
    })
    
    #Creating a progress bar 
    output$svm_results <- renderTable({
        withProgress(message = 'Training Radial SVM Model ',
                     detail = 'This may take a while...', value = 0, {
                         svm_full_reg<-SVMInput() 
                         svm_full_reg$results
                     })
    })
    
    #Using user inputs to create a new data frame
    NewInput<- reactive({
        data.frame("authoryearofbirth"=input$authoryearofbirth,
                               "publication_year"=input$publication_year,
                               "num_tokens"=input$anger,
                               "anger"=input$anticipation,
                               "anticipation"=input$disgust,
                               "disgust"=input$fear ,
                               "fear"=input$joy,
                               "joy"=input$negative,
                               "negative"=input$positive,
                               "positive"=input$sadness,
                               "sadness"=input$surprise,
                               "surprise"=input$trust,
                               "trust"=as.numeric(input$detect_fict),
                               "detect_fict"=as.numeric(input$detect_juvenile),
                               "detect_juvenile"=as.numeric(input$detect_stories),
                               "detect_stories"=as.numeric(input$detect_drama),
                               "detect_drama"=as.numeric(input$detect_history),
                               "detect_history"=as.numeric(input$num_tokens),
                               "mean_word_length"=input$mean_word_length,
                               "perc_stop"=input$perc_stop,
                               "lexical_diversity"=input$lexical_diversity)
    })
    
    #Displaying Prediction Resultss
    output$Predictions <- renderText({ 
        
        model_boost<-BoostInput()
        model_svm<-SVMInput()
        new<-NewInput()
        paste('Boost Prediction:',round(predict(model_boost,new),2),'\\n',
        'SVM Prediction:',round(predict(model_svm,new),2))
    })
    

    
    #create output of observations    
    output$table <- DT::renderDataTable({
        DT::datatable(analysis_set,
                      options = list(dom = 't'))
        
    })

}