#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(DT)
library(stringr)
library(stringi)
library(bslib)
source("helpers.R")

l <- readRDS("models/stupid_backoff_model.Rds")

lookup <- l[["lookup"]]
ngram_lut <- l[["ngram"]]

app_theme <- bs_theme(primary = "#5281CE", 
                      heading_font = font_collection(font_google("Arvo"), 
                      font_google("Catamaran"), font_google("Ubuntu")), 
                      font_scale = 1.5, `enable-gradients` = TRUE, `enable-shadows` = TRUE, 
                      spacer = "2rem", bootswatch = "superhero")

rm(l)

# Define UI for application that predicts a word from a sentence
ui <- fluidPage(
    bootstrapLib(app_theme), 
    
    # Application title
    titlePanel("WordPredictR"),

    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "id_sentence",
                      label = NULL,
                      placeholder = "Start typing your sentence..."), 
            br(), 
            strong("Introduction"), 
            p("Welcome to the word predicting shiny app. 
               Simply start typing your sentence, press space and see the top 5 predicted words. 
               You can click on the desired word from the table and it will be autofilled 
               in the sentence and the predictions will refresh.
               Clicking on other parts of the table other than the words will not autofill the word.
               For each prediction you can also see its probability.")
        ),

        mainPanel(
           dataTableOutput("word_pred")
        )
    )
)

server <- function(input, output) {
    
    observeEvent(input$word_pred_cell_clicked$value, {
        
        x <- input$word_pred_cell_clicked
        
        if (is.null(x$value) || x$col != 1) return()
        
        y <- input$id_sentence
        append <- paste0(y, x$value, " ")
        
        updateTextInput(session = getDefaultReactiveDomain(), inputId = "id_sentence", value = append)
        
    })
    
    output$word_pred <- renderDataTable({
        
        req(str_detect(input$id_sentence, "\\s$"), cancelOutput = TRUE)
        
        pred <- predict_ngram(input$id_sentence, ngram_l = ngram_lut, lookup = lookup, n_words_predict = 5)
        
        pred[, `:=`(order = NULL)]
        
        setnames(pred, old = c("word_5", "prob"), new = c("word", "probability"))
        
        datatable(pred, options = list(dom = "t")) %>% 
            formatPercentage("probability", 2)

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
