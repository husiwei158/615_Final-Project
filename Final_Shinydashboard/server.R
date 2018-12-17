#################################################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(readr)
library(scales)
library(knitr)
library(leaflet)
library(geojsonio)
library(moments)
library(benford.analysis)
library(plotly)
library(DT)
options("scipen"=100, "digits"=4)

#################################################################
server <- function(input, output) {
  imdb <- read_csv("movies.csv")
  ##genre plot 
   output$plot.genre <- renderPlotly(
     if(length(input$genres) == 0){
       
       print("Please choose one genre you want to see")
     }
     else{
       imdb.genre <- imdb[imdb$genre == input$genres,]
       ggplot(data = imdb.genre) + 
         geom_density(aes(x = year, color = genre))+
         geom_density(aes(x = year),linetype = "dashed" ) + 
         labs(x= "Year",
              y= "Movie Genres Density", 
              title = "The distribution of Genres from 1986 ~ 2016") +  
         theme(axis.title.x = element_text(size = 12),
               axis.title.y = element_text(size = 12), 
               legend.title = element_text(size =18))
     }
   )
 
    ##genre text
   output$genre.text <- renderPrint({"From plot, we know how is the distribution of Years about different genres of movies in this density plot.
        We can know several things from plot:
        1. Before 2000, the drama ,musical,Romance and Thriller are more popular than after 2000. We can understand this through their decreasing line.
        2. The audiences were more interest in Scentific Movie from 2000 to 2010."
   })
   
   ## Released Date Check 
   
   output$plot.budget <- renderPlotly({
     imdb.budget <- imdb.budget[imdb.budget$year <= input$Year1,]
     ggplot(data = imdb.budget)+ 
       geom_jitter(aes(x = year, y = budget)) +
       #scale_y_continuous(labels = comma) + 
       labs(x = "Year", 
            y = "Movie Budget", 
            title = "The Changes of Movie Budget Every Year") +  
       theme(axis.title.x = element_text(size = 12),
             axis.title.y = element_text(size = 12), 
             plot.title = element_text(size = 15, face = "bold"))
   })
   
   ## Maximum and Mean Budget Check
   output$plot.mm <- renderPlotly({
     table.budget <- table.budget[table.budget$year <= input$Year1,]
     ggplot(data = table.budget)+ 
       geom_point(aes(x = year, y = Average,color = Average,size = Max))+
       scale_color_continuous(trans = "reverse") + 
       labs(x = "Year Interval", 
            y = "Average Movie Budget", 
            title = "Average and Maximum Movie Budget Every Five Years") + 
       scale_y_continuous(labels = comma)+
       theme(axis.title.x = element_text(size = 12),
             axis.title.y = element_text(size = 12), 
             plot.title = element_text(size = 15, face = "bold"))
   })
   
   ##Top budget movie earning
   output$plot.earning <- renderPlotly({
     imdb.top <- imdb.company[1:input$Top,]
     comp1 <- imdb.top %>% 
       gather(money,value, budget:gross)
    
      ggplot(data = comp1)+ 
       geom_bar(aes(x = year,
                    y = value,
                    fill = money), position = "dodge",stat = "identity") + 
       labs(x = "Year", 
            y = "Budget&Gross")+ 
       scale_y_continuous(labels = comma) +
       theme(axis.title.x = element_text(size = 12),
             axis.title.y = element_text(size = 12), 
             plot.title = element_text(size = 15, 
                                       face = "bold"), 
             axis.text.x = element_text(angle = 45))
   })
   
   ## Company earning from Top 100 movie
   output$plot.company <- renderPlotly({
     comp2 <- comp[comp$company == input$Company,]
     
     ggplot(data = comp2, aes(x = year))+ 
       geom_line(aes(y = budget,
                     color = "budget")) + 
       geom_line(aes(y = gross,
                     color = "gross"))+
       labs(x = "Year", 
            y = "Budget&Gross", 
            title = cat(input$Company,"earns from Movies"),
            colour ="Money")+ 
       scale_y_continuous(labels = comma) +
       theme(axis.title.x = element_text(size = 12),
             axis.title.y = element_text(size = 12), 
             plot.title = element_text(size = 15, 
                                       face = "bold"), 
             axis.text.x = element_text(angle = 45))
     
   })
   
   ##world Map about movie from 1986 to ?year.
   output$plot.map <- renderLeaflet({
     print(country)
   })
   
  ## rating with two type plot
   output$plot.bar <- renderPlotly(
     {
      print(bar.rating) 
     }
   )
   output$plot.density <- renderPlotly({
     print(density.rating)
   })
   ## rating VS score and votes
   output$plot.rscore <- renderPlotly({
     imdb.rs1 <- imdb.rs[imdb.rs$year == input$Year2,]
     ggplot(data = imdb.rs1) + 
       geom_bar(aes(x = rating,
                    y = average),
                fill = "pink",
                stat = "identity",
                show.legend = F)+
       labs(x = "Rating", 
            y = "Score per movie") +  
       theme(axis.title.x = element_text(size = 12),
             axis.title.y = element_text(size = 12), 
             plot.title = element_text(size = 15, face = "bold"))
     })
   
   output$plot.rvote <- renderPlotly({
     imdb.rv1 <- imdb.rv[imdb.rv$year == input$Year2,]
     ggplot(data = imdb.rv1) + 
       geom_bar(aes(x = rating,
                    y = average), 
                fill = "#0099FF",
                stat = "identity",
                show.legend = F)+
       labs(x = "Rating", 
            y = "Votes per movie") +  
       theme(axis.title.x = element_text(size = 12),
             axis.title.y = element_text(size = 12), 
             plot.title = element_text(size = 15, face = "bold"))
  } )
   
   ## Movie score distribution
   output$plot.distribution <- renderPlotly({
     imdbscore <- imdb.score[which(imdb.score$year %in% input$Year3),]
     p <- ggplot(imdbscore,aes(x = score)) + 
       geom_histogram(aes(y = ..density..),
                      fill = "red",
                      alpha = 0.5, 
                      bins = 100,
                      show.legend = FALSE)+ 
       geom_density(color = "blue")+ 
       labs(x = "Movie Score", 
            y = "Density of Movies",
            title = "Distribution of Movie Score")+ 
       theme(axis.title.x = element_text(size = 12),
             axis.title.y = element_text(size = 12), 
             plot.title = element_text(size = 15, face = "bold"))
       p +
       annotate("text",
                x=3.5,
                y=0.4,
                label = "Skewness = -0.634, kurtosis = 3.97") +
       annotate("text",
                x=3.5,
                y=0.3,
                label = "Skewness Distribution")
   })
   
   output$Movies <- renderDataTable({
     imdb.score[which(imdb.score$year %in% input$Year3),]
   })
   
   ##benfold law
   output$plot.votedis <- renderPlotly({
     print(votedis)
   })
   
   output$plot.benfold <- renderPlot({
     plot(bfd.voter) 
   })
   
   output$table.suspicious <- renderDataTable({
     suspect
   })
   
   output$data <- renderDataTable({
     data
   })
   output$sodata <- renderDataTable({
     sodata
   })
   
   output$bfd <- renderDataTable({
      bfd
   })
   output$analysis <- renderTable({
     bfd.voter$mantissa
   })
   
   }
