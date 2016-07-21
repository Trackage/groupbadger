library(dygraphs)
library(dplyr)
#load("~/AMP2015/Data/tracks/EAnt_SES.RData")
#d <- ses_tracks[sort(sample(nrow(ses_tracks), 10000)), ]
#d <- d[order(d$ref, d$gmt), ]
#d <- d %>% distinct(ref, gmt, .keep_all = TRUE)
#uid <- as.character(unique(d$ref))
shinyUI(fluidPage(
  
  titlePanel("trips"),
  
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("choose_trip"), 
      actionButton("data_start", "Mark trip start"), 
      actionButton("data_end", "Mark trip end"), 
      tableOutput("outtable")
      #textOutput("headtail")

    ),
    mainPanel(
      dygraphOutput("dygraph1"),
      dygraphOutput("dygraph2")
      
      
    )
  )
))