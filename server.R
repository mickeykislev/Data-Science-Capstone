# Load packages
library(shiny)
library(stringr)
library(utils)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
      # Generate the outputs
    output$value1 <-  renderTable(data.frame("option1" = Pred_text({ input$caption }, 
                                                                 working_data2grams_top_mid, 
                                                                 working_data3grams_top_mid, 
                                                                 working_data4grams_top_mid)[1],
                                             "option2" = Pred_text({ input$caption }, 
                                                                 working_data2grams_top_mid, 
                                                                 working_data3grams_top_mid, 
                                                                 working_data4grams_top_mid)[2],
                                             "option3" = Pred_text({ input$caption }, 
                                                                 working_data2grams_top_mid, 
                                                                 working_data3grams_top_mid, 
                                                                 working_data4grams_top_mid)[3]))
    output$value2 <-  renderTable(data.frame("option1" = Pred_text({ input$caption }, 
                                                                 blogs2grams_top_mid, 
                                                                 blogs3grams_top_mid, 
                                                                 blogs4grams_top_mid)[1],
                                             "option2" = Pred_text({ input$caption }, 
                                                                 blogs2grams_top_mid, 
                                                                 blogs3grams_top_mid, 
                                                                 blogs4grams_top_mid)[2],
                                             "option3" = Pred_text({ input$caption }, 
                                                                 blogs2grams_top_mid, 
                                                                 blogs3grams_top_mid, 
                                                                 blogs4grams_top_mid)[3]))
    output$value3 <-  renderTable(data.frame("option1" = Pred_text({ input$caption }, 
                                                                 twitter2grams_top_mid, 
                                                                 twitter3grams_top_mid, 
                                                                 twitter4grams_top_mid)[1],
                                             "option2" = Pred_text({ input$caption }, 
                                                                 twitter2grams_top_mid, 
                                                                 twitter3grams_top_mid, 
                                                                 twitter4grams_top_mid)[2],
                                             "option3" = Pred_text({ input$caption }, 
                                                                 twitter2grams_top_mid, 
                                                                 twitter3grams_top_mid, 
                                                                 twitter4grams_top_mid)[3]))
    output$value4 <-  renderTable(data.frame("option1" = Pred_text({ input$caption }, 
                                                                 news2grams_top_mid, 
                                                                 news3grams_top_mid, 
                                                                 news4grams_top_mid)[1],
                                             "option2" = Pred_text({ input$caption }, 
                                                                 news2grams_top_mid, 
                                                                 news3grams_top_mid, 
                                                                 news4grams_top_mid)[2],
                                             "option3" = Pred_text({ input$caption }, 
                                                                 news2grams_top_mid, 
                                                                 news3grams_top_mid, 
                                                                 news4grams_top_mid)[3]))
})
