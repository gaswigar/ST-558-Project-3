# ST-558-Project-3

This repository contains a shiny application for sentiment analysis and prediction of average goodread review for project gutenberg texts. 
                    
Project Gutenberg is a collection of over 60,000 free ebooks. It contains classics that you might have read in school such as *Pride and Prejudice* by Jane Austen, *The Adventures of Sherlock Holmes* by Arthur Conan Doyle, and ... 
*The 1994 CIA World Factbook*. This application provides basic sentiment analysis for these books. This includes word clouds, positive and negative sentiment, and emotional sentiment.The first step in this project was collecting the corpuses of the project gutenberg books. Luckily, some people have already done all of the hard work.  [Martin Gerlach and Francesc Font-Clos](https://github.com/pgcorpus/gutenberg) have created a Github repository that can gather all of the current books available through project gutenberg.
 
 [I then signed up for a goodread API key here](https://www.goodreads.com/api). Then I used the rgoodreads package to request the review information on each book. The  rgoodread package does not have a function to search by both author and title so I made some small changes to the functions to allow for this. I started off with roughly 50,000 texts and after filtering the data to cap number of tokens, have more than 5 reviews, and removing some missing data I have roughly 3500 records for analysis which are included in the data folde . 
 

Within this application you will also be able to fit 2 models, for which the user can select the model parameters, a boosted tree and a Radial SVM. You can also download the tokens associated each book at the bottom of the next section. The last section allows you to view all of the data used in the report.
                    
                    
