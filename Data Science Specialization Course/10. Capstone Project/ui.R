library(shiny)
shinyUI(fluidPage(
    titlePanel(h1("Five-gram word prediction model")),
    sidebarLayout(
        sidebarPanel(h3("Instructions"), p("1. Please wait for the model to load for the first time."), p("2. Once the first 10 suggested words appear, you may begin by either typing some text or selecting one of the suggested starting words."), p("3. The model will update the choice of word predictions based on your input."), width = 4),
        mainPanel(
            textInput("usertext", NULL, value = "", width = NULL, placeholder = "Please type your text here."), 
            actionButton("pred1", ""),
            actionButton("pred2", ""),
            actionButton("pred3", ""),
            actionButton("pred4", ""),
            actionButton("pred5", ""),
            actionButton("pred6", ""),
            actionButton("pred7", ""),
            actionButton("pred8", ""),
            actionButton("pred9", ""),
            actionButton("pred10", "")
        )
    )
))