# ui.R
library(shiny)

shinyUI(pageWithSidebar(
        
        headerPanel("YoNeWo -- your next word"),
        sidebarPanel(
                textInput(inputId = "text", label = "Enter Text:", width = '400px'),
                submitButton('Submit'),
                h4("Next word"),
                verbatimTextOutput("outputValueWord"),
                h4("All predicted words"),
                verbatimTextOutput("outputValueList")
                
        ),
        mainPanel("")
                
        
        )
)


