library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Text Annotation App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a text file", accept = ".txt"),
      textInput("search_string", "Search String"),
      actionButton("search_paragraphs", "Search Paragraphs"),
      textInput("annotation", "Add Annotation"),
      selectInput("paragraph_number", "Paragraph Number", choices = NULL),
      actionButton("add_annotation", "Add Annotation"),
      downloadButton("download_csv", "Download CSV")
    ),
    mainPanel(
      DTOutput("paragraphs_table")
    )
  )
)

# Server function
server <- function(input, output, session) {
  # Initialize data frame to store paragraphs
  paragraphs_df <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    inFile <- input$file
    paragraphs <- readLines(inFile$datapath)
    df <- data.frame(start_line = integer(0),
                     end_line = integer(0),
                     text = character(0),
                     annotations = character(0),
                     page_number = integer(0),
                     stringsAsFactors = FALSE)
    start_line <- 1
    end_line <- 1
    for (i in seq_along(paragraphs)) {
      if (nzchar(paragraphs[i])) {
        end_line <- i
      } else {
        paragraph_text <- paste(paragraphs[start_line:end_line], collapse = " ")
        df <- rbind(df, 
                    data.frame(start_line = start_line,
                               end_line = end_line,
                               text = paragraph_text,
                               annotations = "",
                               page_number = NA,
                               stringsAsFactors = FALSE))
        start_line <- i + 1
        end_line <- i + 1
      }
    }
    if (nzchar(paste(paragraphs[start_line:length(paragraphs)], collapse = ""))) {
      paragraph_text <- paste(paragraphs[start_line:length(paragraphs)], collapse = " ")
      df <- rbind(df, 
                  data.frame(start_line = start_line,
                             end_line = length(paragraphs),
                             text = paragraph_text,
                             annotations = "",
                             page_number = NA,
                             stringsAsFactors = FALSE))
    }
    paragraphs_df(df)
    updateSelectInput(session, "paragraph_number", choices = NULL)
  })
  
  search_paragraphs_df <- function(search_string) {
    matching_paragraphs <- paragraphs_df()[grep(tolower(search_string), tolower(paragraphs_df()$text)), , drop = FALSE]
    return(matching_paragraphs)
  }
  
  reactive_search_results <- reactive({
    if (!is.null(paragraphs_df())) {
      if (input$search_paragraphs > 0) {
        search_paragraphs_df(input$search_string)
      } else {
        paragraphs_df()
      }
    } else {
      data.frame(start_line = integer(), end_line = integer(), text = character(), annotations = character(), page_number = integer(), stringsAsFactors = FALSE)
    }
  })
  
  output$paragraphs_table <- renderDT({
    dt_data <- reactive_search_results()
    
    # Add annotation input dynamically
    if (!is.null(input$add_annotation)) {
      print("Add Annotation Button Clicked")
      print(paste("Input Paragraph Number:", input$paragraph_number))
      if (!is.null(input$paragraph_number) && input$paragraph_number != "") {
        # Update the corresponding row in the datatable with the annotation input
        paragraph_index <- which(dt_data$start_line == input$paragraph_number)
        dt_data$annotations[paragraph_index] <- input$annotation
      }
    }
    
    print(dt_data)  # Print the dataframe to check if annotation inputs are added
    
    datatable(dt_data, rownames = FALSE, escape = FALSE, selection = "none")
  })
  
  
  observe({
    if (!is.null(paragraphs_df())) {
      updateSelectInput(session, "paragraph_number", choices = reactive_search_results()$start_line)
    }
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("annotations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(paragraphs_df(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
