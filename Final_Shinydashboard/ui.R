#Set up library for all data

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


########################################################

dashboardPage( 
  dashboardHeader(title = "Final Report"),
  
  dashboardSidebar(
    ##Create the sidebar menu tabs ( Maybe 7 Aspects)
    sidebarMenu(
      menuItem("Genres Flavors", tabName = "genre",icon = icon("theater-masks")),
      menuItem("Movie Budget", tabName = "Budget", icon = icon("chart-line")),
      menuItem("Budget & Gross", tabName = "earning", icon = icon("dollar-sign")),
      menuItem("Country & Movies", tabName = "country", icon =icon("globe")),
      menuItem("Rating VS Score & Votes", tabName = "rating", icon = icon("smile")),
      menuItem("Score Distribution", tabName = "score", icon = icon("calculator")),
      menuItem("Benfold Analysis", tabName = "benfold", icon = icon("book"))
    )
    ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "genre",
        fluidRow(
          box(
            title = "Controls",
            width = 3,
            solidHeader = T,
            status = "primary",
            collapsible = T,
            collapsed = F,
            selectizeInput(
              "genres",
              label = "Index of genre",
              choice = unique(imdb$genre),
              multiple = T,
              options = list(maxItems = 10, placeholder = "choose a genre"),
              selected = c("Action","Drama","Sci-Fi","Thriller"))
          ),
          box(
            title = "Know from Genre",
            width = 8,
            solidHeader = T,
            status = "primary",
            collapsible = T,
            collapsed = T,
            h3("What I know from genres:"),
            textOutput("genre.text"))
            
          ),
          box(
            title = "Genre Distribution",
            width = 8,
            solidHeader = T,
            status = "warning",
            collapsible = F,
            plotlyOutput("plot.genre")
          )
        ),
      tabItem(
        tabName = "Budget",
        fluidRow(
          box(
            title = "Date Controls",
            width = 10,
            solidHeader = T,
            status = "primary",
            collapsible = T,
            collapsed = F,
            sliderInput(
              "Year1",
              "Choose a Date Interval",
              min = 1986, 
              max = 2017, 
              value = 2000
                        ))),
        fluidRow(  
          box(
            title = "How the Budget changes with varing year",
            width = 6,
            solidHeader = T,
            status = "warning",
            collapsible = F,
            plotlyOutput("plot.budget")),
          box(
            title = "Maximum and Mean Budget through year",
            width = 6,
            solidHeader = T,
            status = "warning",
            collapsible = F,
            plotlyOutput("plot.mm")))
          
          ),
      tabItem(
        tabName = "earning",
        fluidRow(
          box(
            title = "Top Number Control",
            width = 6,
            solidHeader = T,
            status = "primary",
            collapsible = T,
            collapsed = F,
            sliderInput(
              "Top",
              "Choose how many Top Movies you want to check",
              min = 100,
              max = 1000,
              value = 500)),
          box(
            title = "Company Control",
            width = 6,
            solidHeader = T,
            status = "primary",
            collapsible = T,
            collapsed = F,
            selectInput(
              "Company",
              "Choose one company you interested in",
              choices = unique(comp$company),
              selected = "Marvel Studios"))),
        fluidRow(
          box(
            title = "How much Top Budget Movie can earn?",
            width = 6,
            solidHeader = T,
            status = "warning",
            collapsible = F,
            plotlyOutput("plot.earning")),
          box(
            title = "How much movie company earn from their top movie?",
            width = 6,
            solidHeader = T,
            status = "warning",
            collapsible = F,
            plotlyOutput("plot.company"))
          )),
      tabItem(
        tabName = "country",
        fluidRow(
          box(
          title = "Map Description",
          width = 12,
          solidHeader = T,
          status = "primary",
          collapsible = T,
          collapsed = F,
          h3("Where is the center of Movie?"),
          h4("---- The USA"),
          print("I draw a geograph plot to see 
                how many movies was made in different 
                countries from 1989 to 2016. USA
                made the most movies in my dataset. 
                I believe the Holloywood contribute most of 
                the movie in USA. Each year USA pictured 
                hundreds of movies. They decided the popularity 
                of the movies. ")
          )),
        fluidRow(
          box(
            title = "World Map about Movie",
            width = 12,
            solidHeader = T,
            status = "warning",
            collapsible = F,
            leafletOutput("plot.map")
          )
        )
      ),
      tabItem(
        tabName = "rating",
        fluidRow(
          box(
            title = "Date Controls",
            width = 4,
            solidHeader = T,
            status = "primary",
            collapsible = T,
            collapsed = F,
            selectInput(
              "Year2",
              "Choose one Year",
              choice = unique(imdb.rating$year),
              selected = "1989"
            )
          ),
          box(
            title = "Idea about Rating",
            width = 8,
            solidHeader = T,
            status = "primary",
            collapsible = T,
            h3("People's attitude to rating movies"),
            h4(" ----- R are more favorable."),
            print("From plot, R rating movies were pictured most each year.From plot, it can check each year movie voters and score via three ratings.
The density plot displays a decreasing trend of density of PG. From 1986 to 2005, pg-13 shows a increasing trend and then it shows a decreasing trend from 2006 to 2016. R rating keeps same.")
          )
        ),
        fluidRow(
          box(
            title = "Rating Vs Score",
            width = 6,
            solidHeader = T,
            status = "warning",
            collapsible = T,
            plotlyOutput("plot.rscore")),
          box(
            title = "Rating Vs Vote",
            width = 6,
            solidHeader = T,
            status = "warning",
            collapsible = T,
            plotlyOutput("plot.rvote"))),
        fluidRow(
          box(
            tite = "Rating distribution throgh year",
            width = 12,
            solidHeader = T,
            status = "warning",
            collapsible = T,
            plotlyOutput("plot.density"),
            plotlyOutput("plot.bar")))),
      tabItem(
        tabName = "score",
        fluidPage(
          box(
            width = 3,
            status = "primary",
            solidHeader = F,
            selectizeInput(
              "Year3",
              label = "Year Control",
              choice = unique(imdb.score$year),
              multiple = T,
              options = list(maxItems = 32, placeholder = "Which year?"),
              selected = unique(imdb.score$year)
            ) 
          ),
          box(
            title = "Concern about Movie Score Distribution",
            width = 8,
            status ="primary",
            solidHeader = T,
            collapsible = T,
            h3("Movie Score is not belong to Normal Distribution！"),
            h4(" ------ the skewness distribution"),
            print("The distribution of score is skewness distribution. The skewness here is -0.63. This value implies that the distribution of the data is skewed to the left. And the Kurtosis is 3.97 , implying that the distribution of the data is leptokurtic.
                  The reason caused this skewness is that people are more willing to give a compareable higher score. In our normal idea, people think 5 credit is not good but not normal thing. "))),
        fluidRow(
          box(
          title = "Movie Score Distribution",
          width = 12,
          status = "warning",
          solidHeader = T,
          collapsible = T,
          plotlyOutput("plot.distribution")
       ) ),
       fluidRow(
         box(
           title = "Movie Information", 
           width = 12, 
           solidHeader = T, 
           status = "info",
           collapsible = T,
           dataTableOutput("Movies")
         ))
      ),
      tabItem(
        tabName = "benfold",
        fluidRow(
          box(
            title = "Introduction",
            width = 12,
            status = "info",
            collapsible = T,
            h3("Benfold analysis about Votes of Movies"),
            print("Benford's law is an observation about the frequency distribution of leading digits in many real-life sets of numerical data. The law states that in many naturally occurring collections of numbers, the leading significant digit is likely to be small.
                  The second-order test looks at relationships and patterns in data and is based on the digits of the differences between amounts that have been sorted from smallest to largest (ordered). 
                  The digit patterns of the differences are expected to closely approximate the digit frequencies of Benford's Law.
                  The summation test looks for excessively large numbers in the data.")
          )
        ),
        fluidRow(
          tabBox(
            title = "Explaination and analysis table",
            width = 12,
            height = "500px",
            tabPanel(
            h3("Votes First two digital distribution"),
            print("The plot of first 2 digitals show us a very simillar distribution like benfold distribution. "),
            plotlyOutput("plot.votedis")),
            tabPanel("Data",  dataTableOutput("data")),
            tabPanel("S.O Data", dataTableOutput("sodata")),
            tabPanel("Benfold", dataTableOutput("bfd")))
        ),
        fluidRow(
          tabBox(
            title = "Explaination and analysis table",
            width = 6,
            height = "500px",
            tabPanel("Interpretation",
            h4("Analysis about benfold analysis plots"),
            print("From two digits distribution, the 2 digits of votes follow benfold's distribution. Only a few two digits has slightly higher than benford distribution. This also shows in the suspects_ranked dataset.The biggest absolute difference is 32 which is small.In conclusion, There is no significant dispendency number.

                   The second-order test gives few, if any, false positives in that if the results are not as expected (close to Benford), then the data do indeed have some characteristic that is rare and unusual, abnormal, or irregular. 
                  From the plot, numbers which are multiple of 10 do not follow benfold's law.
                  The higest chi-square between observed 2 digits frequence and expected frequence is 4, which is small. This proves that the frequency of observed 2-digit number are close to what they expected.
                  
                  Used 'getSuspects' function, we can see suspicious observations from the original data. There exist 393 out of 6338 suspicious observations.") ),
            
            
            tabPanel("Analysis", tableOutput("analysis"), print("MAD: 0.001,             Distortion.factor: 1.32"))),
          
          box(
            title = "Digital Distribution & Benfold distribution",
            width = 6,
            status = "primary",
            solidHeader = T,
            plotOutput("plot.benfold")
          )
        ),
        fluidRow(
          box(
            title = "Table about Suspicious observations",
            width = 12,
            status = "info",
            collapsible = T,
            solidHeader = T,
            dataTableOutput("table.suspicious")
            
            )
        )
      )
          )
              
              )
          
        )
