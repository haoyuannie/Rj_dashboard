
# input$file1 will be NULL initially. After the user selects
# and uploads a file, head of that data file by default,
# or all rows if selected, will be shown.

req(input$csv)

# when reading semicolon separated files,
# having a comma separator causes `read.csv` to error
tryCatch(
  {
    df <- read.csv(input$csv$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    df$date <- as.Date(df$date)
  },
  error = function(e) {
    # return a safeError if a parsing error occurs
    stop(safeError(e))
  }
)
