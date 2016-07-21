library(dygraphs)
library(dplyr)
library(xts)
load("Data/tracks/EAnt_SES.RData")
d <- ses_tracks #`[sort(sample(nrow(ses_tracks), 10000)), ]
d <- d[order(d$ref, d$gmt), ]
d <- d %>% distinct(ref, gmt, .keep_all = TRUE)
tripids <- levels(d$ref)
adots <- c('LON', 'LAT', 'gmt')
#leadingEdge <- vector("list", length(unique(d$ref)))
#names(leadingEdge) <- unique(d$ref)
# ht <- data.frame(ref = character(0), start = character(0), end = character(0), 
#                  order = integer(0), stringsAsFactors = FALSE)
ht <- d %>% group_by_("ref") %>% summarize(start = format(min(gmt)), 
                                           end = format(max(gmt)), 
                                           START_fix = FALSE, END_fix = FALSE, 
                                           order = NA_integer_)



shinyServer(function(input, output) {
  
  update2 <- function(dw1,  start) {
    if (!is.null(dw1)) {
      ddd <- ht
      flog <- ddd$ref == currenttrip()
      ddd$endflog <- dw1
      if (start == 2) ddd$END_fix[flog] <- TRUE
      if (start == 1) ddd$START_fix[flog] <- TRUE
      ddd$order[flog] <- if (all(is.na(ddd$order))) 1 else  max(ddd$order[flog], na.rm = TRUE) + 1
      ht <<- ddd
    }
    ht %>%  filter(START_fix | END_fix) %>% arrange(desc(order)) %>% select(ref, start, end, START_fix, END_fix)
  }
  
  
  # Drop-down selection box for which data set
  output$choose_trip <- renderUI({
    selectInput("trip", "Choose Trip", as.list(tripids))
  })
  data1 <- reactive({
    a <- subset(d, ref == input$trip) %>% dplyr::select_(.dots = adots) 
    #m <- cbind(a[[1]], a[[2]])
    xts(a[[1]], a[[3]])
  })
  data2 <- reactive({
    a <- subset(d, ref == input$trip) %>% dplyr::select_(.dots = adots) 
    m <- cbind(a[[1]], a[[2]])
    xts(a[[2]], a[[3]])
  })
  dw <- function(x) {
    format(input$dygraph1_date_window[[x]])
  }
  currenttrip <- function() {
    input$trip
  }
  headtail <- reactive({
    input$data_end
    update2(dw(2), start = 2)
  })
  output$outtable <- renderTable(headtail())
 output$dygraph1 <- renderDygraph({
    dll <- data1()
    dygraph(dll, main = input$trip, group = "headtail") %>%
      dygraphs::dyRangeSelector()
  })
  
  output$dygraph2 <- renderDygraph({
    dll <- data2()
    dygraph(dll, main = input$trip, group = "headtail") %>%
      dygraphs::dyRangeSelector()
  })
})


# headtail <- reactive({
#   input$trip_end
#   data.frame(ref = input$trip, start = "1", 
#              end = if (is.null(input$dygraph_date_window)) "2" else input$dygraph_date_window[[2]], 
#   order = nrow(ht) + 1, stringsAsFactors = FALSE)
# })
# 
# reacteddata <- reactive({
#   if (!is.null(input$dygraph_date_window)){
#     startend =strftime(unlist(input$dygraph_date_window[1:2]))
#     } else {
#     startend = range(data1()[3])
#   }
#   startend
# })
#output$headtail <- renderText( as.character(reacteddata()))
#output$headtail <- renderText(headtail()$end)
