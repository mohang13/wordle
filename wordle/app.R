library(shiny)
library(shinydashboard)
library(tidyverse)

"functions.R" %>% source()


choices <- c("Correct position", "Incorrect position", "Not found")


ui <- dashboardPage(
    header = dashboardHeader(title = "Wordle Solver"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Version 0.2", tabName = "v1"),
            menuItem("Version 0.1", tabName = "v0")
        )
        
    ),
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "v1",
                h1("Version 0.2"),
                fluidRow(
                    box(width = 12, title = "Letters", 
                        splitLayout(
                            textInput("inputA3", "I"),
                            textInput("inputB3", "II"),
                            textInput("inputC3", "III"),
                            textInput("inputD3", "IV"),
                            textInput("inputE3", "V")
                        )
                    )
                ),
                fluidRow(
                    tags$head(
                        tags$style("
                                  
        #inputA4 ~ .selectize-control .option:nth-child(1){
          background-color: rgb(106, 170, 100);

        }
        #inputA4 ~ .selectize-control .option:nth-child(2) {
          background-color: rgb(234, 178, 7);
        }
        #inputA4 ~ .selectize-control .option:nth-child(3) {
          background-color: rgb(120, 124, 126);
        }
        #inputB4 ~ .selectize-control .option:nth-child(1){
          background-color: rgb(106, 170, 100);

        }
        #inputB4 ~ .selectize-control .option:nth-child(2) {
          background-color: rgb(234, 178, 7);
        }
        #inputB4 ~ .selectize-control .option:nth-child(3) {
          background-color: rgb(120, 124, 126);
        }
        #inputC4 ~ .selectize-control .option:nth-child(1){
          background-color: rgb(106, 170, 100);

        }
        #inputC4 ~ .selectize-control .option:nth-child(2) {
          background-color: rgb(234, 178, 7);
        }
        #inputC4 ~ .selectize-control .option:nth-child(3) {
          background-color: rgb(120, 124, 126);
        }
        #inputD4 ~ .selectize-control .option:nth-child(1){
          background-color: rgb(106, 170, 100);

        }
        #inputD4 ~ .selectize-control .option:nth-child(2) {
          background-color: rgb(234, 178, 7);
        }
        #inputD4 ~ .selectize-control .option:nth-child(3) {
          background-color: rgb(120, 124, 126);
        }
        #inputE4 ~ .selectize-control .option:nth-child(1){
          background-color: rgb(106, 170, 100);

        }
        #inputE4 ~ .selectize-control .option:nth-child(2) {
          background-color: rgb(234, 178, 7);
        }
        #inputE4 ~ .selectize-control .option:nth-child(3) {
          background-color: rgb(120, 124, 126);
        }
        "
                        )
                    ),
                    box(width = 12, title = "Position", 
                        splitLayout(
                            selectizeInput("inputA4", "I", choices, "Correct position", multiple = F),
                            selectInput("inputB4", "II", choices, "Correct position", multiple = F),
                            selectInput("inputC4", "III", choices, "Correct position", multiple = F),
                            selectInput("inputD4", "IV", choices, "Correct position", multiple = F),
                            selectInput("inputE4", "V", choices, "Correct position", multiple = F)
                        ),
                        br(),
                        actionButton("word3", "Next word list"),
                    )
                ),
        uiOutput("filetxt_rui1")
            ),
            tabItem(tabName = "v0",
                h2("Input your letters in the correct place (Green)"),
                
                fluidRow(
                    box(width = 16, title = "Letters", 
                        splitLayout(
                            textInput("inputA", "I"),
                            textInput("inputB", "II"),
                            textInput("inputC", "III"),
                            textInput("inputD", "IV"),
                            textInput("inputE", "V")
                        )
                    )
                ),
                h2("Input your letters in the incorrect place (Orange)"),
                
                fluidRow(
                    box(width = 16, title = "Letters", 
                        splitLayout(
                            textInput("inputA1", "I"),
                            textInput("inputB1", "II"),
                            textInput("inputC1", "III"),
                            textInput("inputD1", "IV"),
                            textInput("inputE1", "V")
                        )
                    )
                ),
                h2("Input your unwanted letters (Gray)"),
                
                fluidRow(
                    box(width = 4, title = "Letters", 
                        splitLayout(
                            textInput("inputA2", "")
                        )
                    )
                ),
                actionButton("word2", "Next word list"),
                uiOutput("w33"),
                h2("Generated word list"),
                
                fluidRow(
                    box(width = 16, title = "List of words", 
                        #pre(id = "console")
                        uiOutput("filetxt_rui")
                    )
                )
                
            )
            
        )
    )
    
)

server <- function(input, output, session) {
    
    dir.create(session$token)
    
    wordle_extension %>% 
        write_rds(paste0(session$token,"/w2.rds"))
    
    wordle_extension %>% 
        write_rds(paste0(session$token,"/w3.rds"))
    
    
    observeEvent(input$word2,{
        
        tibble(value = 1:5,
               name = c((input$inputA) %>% tolower(), (input$inputB) %>% tolower(), (input$inputC) %>% tolower(), (input$inputD) %>% tolower(), (input$inputE) %>% tolower())) %>% 
            filter(name != "") %>% 
            pull(value, name) -> cw
        if(length(cw) == 0){
            cw <- NULL
        }
        
        tibble(value = 1:5,
               name = c((input$inputA1) %>% tolower(), (input$inputB1) %>% tolower(), (input$inputC1) %>% tolower(), (input$inputD1) %>% tolower(), (input$inputE1) %>% tolower())) %>% 
            filter(name != "") %>% 
            pull(value, name) -> mw
        
        if(length(mw) == 0){
            mw <- NULL
        }
        
        (input$inputA2) %>% tolower() -> nt 
        
        paste0(session$token,"/w2.rds") %>% 
            read_rds() -> wordle_extension
        
        wordle_crack(wordle_extension,cw, mw, nt)  -> w2
        
        write_rds(w2, paste0(session$token,"/w2.rds"))
        output$output_text <- renderText({
            wordle_scores %>% 
                filter(word %in% w2$word) %>% 
                arrange(desc(uni_len), desc(score)) %>% 
                select(word) %>% 
                pull()
        })
        output$filetxt_rui <- renderUI({
            verbatimTextOutput("output_text")
            
        })
        # output$w33 <- renderUI({
        #     actionButton("word3", "List of third words")
        #     
        # })
    })
    
    observeEvent(input$word3,{
        tibble(no = 1:5,
               name = c((input$inputA3) %>% tolower(), (input$inputB3) %>% tolower(), (input$inputC3) %>% tolower(), (input$inputD3) %>% tolower(), (input$inputE3) %>% tolower()),
               type = c((input$inputA4) %>% tolower(), (input$inputB4) %>% tolower(), (input$inputC4) %>% tolower(), (input$inputD4) %>% tolower(), (input$inputE4) %>% tolower())) ->> df1
        
        df1 %>%
            filter(type == "correct position") %>% 
            pull(no, name) -> cw1
        if(length(cw1) == 0){
            cw1 <- NULL
        }
        
        df1 %>%
            filter(type == "incorrect position") %>% 
            pull(no, name) -> mw1
        
        if(length(mw1) == 0){
            mw1 <- NULL
        }
        
        df1 %>% 
            filter(type == "not found") %>% 
            pull(name) %>% 
            paste(collapse = "") %>% 
            tolower -> nt1
        
        paste0(session$token,"/w3.rds") %>% 
            read_rds() -> wordle_extension
        
        wordle_crack(wordle_extension,cw1, mw1, nt1)  -> w3
        
        write_rds(w3, paste0(session$token,"/w3.rds"))
        output$output_text1 <- renderText({
            wordle_scores %>% 
                filter(word %in% w3$word) %>% 
                arrange(desc(uni_len), desc(score)) %>% 
                select(word) %>% 
                pull()
        })
        output$filetxt_rui1 <- renderUI({
            verbatimTextOutput("output_text1")
            
        })
        
    })
    
    
}

shinyApp(ui = ui, server = server)