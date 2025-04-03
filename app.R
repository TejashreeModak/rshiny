library(shiny)

# Function to compute the reverse complement of a DNA sequence
reverse_complement <- function(seq) {
  seq <- toupper(seq) # Convert to uppercase
  complement <- chartr("ATGC", "TACG", seq) # Complementary bases
  rev_comp <- paste(rev(strsplit(complement, NULL)[[1]]), collapse = "") # Reverse
  return(rev_comp)
}

# UI
ui <- fluidPage(
  titlePanel("DNA Reverse Complement Tool"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("sequence", "Enter DNA Sequence:", ""),
      actionButton("compute", "Get Reverse Complement"),
      br(), br(),
      strong("Example Input:"),
      helpText("ATGCATGC")
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("original"),
      verbatimTextOutput("reverse_complement"),
      br(),
      textOutput("warning")  # Add a warning message for invalid input
    )
  )
)

# Server
server <- function(input, output) {
  result <- reactiveVal()
  warning_message <- reactiveVal()  # Store warning message
  
  observeEvent(input$compute, {
    seq_input <- gsub("[^ATGCatgc]", "", input$sequence) # Remove invalid characters
    invalid_chars <- nchar(input$sequence) - nchar(seq_input)
    if (invalid_chars > 0) {
      warning_message("Warning: Invalid characters detected and removed. Only A, T, G, and C are allowed.")
    } else {
      warning_message("")  # Clear warning if no invalid characters are found
    }
    if (nchar(seq_input) == 0) {
      result("Invalid input. Please enter a valid DNA sequence (A, T, G, C).")
    } else if (nchar(seq_input) > 1000) {
      result("Error: Sequence length exceeds the maximum limit of 1000 bases.")
    } else {
      result(reverse_complement(seq_input))
    }
  })
  
  output$original <- renderText({
    paste("Original Sequence: ", input$sequence)
  })
  
  output$reverse_complement <- renderText({
    paste("Reverse Complement: ", result())
  })
  
  output$warning <- renderText({
    warning_message()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
