#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(dplyr)
library(rdrop2)
library(ggplot2)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
        
        theme = shinytheme("spacelab"),
        
        # Application title
        titlePanel("Text Prediction - Data Science Capstone Project"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        textInput(
                                "phrase",
                                "Phrase",
                                value = "against the boston red",
                                placeholder = "against the boston red"
                        ),
                        submitButton("Submit")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        h5("Please enter a phrase in the box to the left. The
                           app will then try to predict what word should come
                           next in the first tab, and will show a histogram with
                           the five most likely words in the second. NOTE: The 
                           app will load for about 30 seconds originally."),
                        h6("Loading data..."),
                        h3(textOutput("status")),
                        tabsetPanel(type = "tabs",
                                    tabPanel("Prediction",br(),textOutput("prediction")),
                                    tabPanel("Histogram",br(),plotOutput("hist"))
                        )
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        grams <- drop_read_csv("/ngrams/grams.csv",
                               stringsAsFactors=FALSE)[,2:4]
        fivegrams <- filter(grams,gram==5)[,c(1,2)]
        fourgrams <- filter(grams,gram==4)[,c(1,2)]
        threegrams <- filter(grams,gram==3)[,c(1,2)]
        twograms <- filter(grams,gram==2)[,c(1,2)]
        unigrams <- filter(grams,gram==1)[,c(1,2)]
        names(fivegrams)[1] <- "fivegram"
        names(fourgrams)[1] <- "fourgram"
        names(threegrams)[1] <- "threegram"
        names(twograms)[1] <- "twogram"
        names(unigrams)[1] <- "unigram"
        # fivegrams <- data.frame(fivegram=c("this is a test only", "is a test only this"),count = c(2341,23))
        # fourgrams <- data.frame(fourgram=c("this is a test", "is only a test"),count = c(324324,13))
        # threegrams <- data.frame(threegram=c("this is a", "is a test", "is only a"),count = c(2342,1,323))
        # twograms <- data.frame(twogram =c("this is","is a","is only","a test"),count=c(123123,234,12,23))
        # unigrams <- data.frame(unigram = c("this","is","a","test"),count=c(12341,123,112,2))
        temp <- reactive({
                rm(last4,last3,last2,last1)
                last4.tokens <- tokens(input$phrase)$text1
                if (length(last4.tokens) >= 4){
                last4 <- paste(last4.tokens[length(last4.tokens)-3],
                               last4.tokens[length(last4.tokens)-2],
                               last4.tokens[length(last4.tokens)-1],
                               last4.tokens[length(last4.tokens)])}
                if (length(last4.tokens) >= 3){
                last3 <- paste(last4.tokens[length(last4.tokens)-2],
                               last4.tokens[length(last4.tokens)-1],
                               last4.tokens[length(last4.tokens)])}
                if (length(last4.tokens) >= 2){
                last2 <- paste(last4.tokens[length(last4.tokens)-1],
                               last4.tokens[length(last4.tokens)])}
                last1 <- last4.tokens[length(last4.tokens)]
                MLT <- data.frame(feature=character(), score=numeric(), source=character())
                if(exists("last4")){
                denom4 <- as.integer(fourgrams[grepl(paste("^",last4,"$",sep=""),ignore.case = TRUE,fourgrams$fourgram),2])
                if(length(denom4) != 0){
                        num4 <- fivegrams[grepl(paste("^",last4," ",sep=""),ignore.case = TRUE,fivegrams$fivegram),]
                        if(dim(num4)[1]!=0)
                        {
                                num4$feature <- gsub(paste(last4, " ",sep=""),"",ignore.case = TRUE,num4$fivegram)
                                num4$score <- num4$count/denom4
                                num4$source <- "fivegram"
                                MLT <- num4[,c(3,4,5)]
                        }
                }}
                if(exists("last3")){
                denom3 <- as.integer(threegrams[grepl(paste("^",last3,"$",sep=""),ignore.case = TRUE,threegrams$threegram),2])
                if(length(denom3) != 0){
                        num3 <- fourgrams[grepl(paste("^",last3," ",sep=""),ignore.case = TRUE,fourgrams$fourgram),]
                        if(dim(num3)[1]!=0){
                                num3$feature <- gsub(paste(last3," ",sep=""),"",ignore.case = TRUE,num3$fourgram)
                                num3$score <- 0.4*num3$count/denom3
                                num3$source <- "fourgram"
                                MLT <- rbind(MLT,num3[,c(3,4,5)])}
                }}
                if(exists("last2")){
                denom2 <- as.integer(twograms[grepl(paste("^",last2,"$",sep=""),ignore.case = TRUE,twograms$twogram),2])
                if(length(denom2) != 0){
                        num2 <- threegrams[grepl(paste("^",last2," ",sep = ""),ignore.case = TRUE,threegrams$threegram),]
                        if(dim(num2)[1]!=0){
                                num2$feature <- gsub(paste(last2," ",sep = ""),"",ignore.case = TRUE,num2$threegram)
                                num2$score <- 0.4^2*(num2$count/denom2)
                                num2$source <- "threegram"
                                MLT <- rbind(MLT,num2[,c(3,4,5)])}
                }}
                denom1 <- as.integer(unigrams[grepl(paste("^",last1,"$",sep=""),ignore.case = TRUE,unigrams$unigram),2])
                if(length(denom1) != 0){
                        num1 <- twograms[grepl(paste("^",last1," ",sep=""),ignore.case = TRUE,twograms$twogram),]
                        if(dim(num1)[1]!=0){
                                num1$feature <- gsub(paste(last1," ",sep=""),"",ignore.case = TRUE,num1$twogram)
                                num1$score <- 0.4^3*(num1$count/denom1)
                                num1$source <- "twogram"
                                MLT <- rbind(MLT,num1[,c(3,4,5)])}
                }
                def <- data.frame(feature = unigrams$unigram,score = as.numeric(0.4^4*(unigrams$count)/sum(unigrams$count)),source = "unigram", stringsAsFactors = FALSE)
                MLT <- rbind(MLT,def)
                MLT <- aggregate(score ~ feature, data = MLT, FUN = sum)
                output$status <- renderText({"Done."})
                return(arrange(MLT,desc(score)))
        })
        
        output$prediction <- renderText({temp()[1,1]})
        output$hist <- renderPlot({
                ggplot(temp()[1:5,],aes(x=reorder(feature,score),y=score)) +
                        geom_bar(stat="identity",color="black",fill="red") +
                        coord_flip() +
                        geom_text(aes(label=round(score,3)),hjust=1.5,color="black",size=3.5) +
                        labs(x = "Score", y = "Word", title = "Most Likely Next Words") +
                        theme_minimal()
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

