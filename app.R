library(wordcloud)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("Simple Image Recognition App using TensorFlow and Shiny"),
                tags$hr(),
                fluidRow(
                  column(width=4,
                         fileInput('file1', '',accept = c('.jpg','.jpeg')),
                         imageOutput('outputImage')
                  ),
                  column(width=8, verbatimTextOutput("answer"),
                         plotOutput("plot")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  outputtext <- reactive({
    ###This is to compose image recognition template###
    inFile <- input$file1 
    template <- paste0("python"," ", "classify_image.py") 
    runpy <- paste0(template," --image_file ",inFile$datapath)
    if (is.null(inFile))
    {system(paste0(template," --image_file /tmp/imagenet/cropped_panda.jpg"))} else { #Initially the app classifies cropped_panda.jpg, if you download the model data to a different directory, you should change /tmp/imagenet to the location you use. 
      system(runpy,intern=TRUE) #Uploaded image will be used for classification
    }
    
  })
  
  output$answer <- renderPrint({outputtext()})
  
  output$plot <- renderPlot({
    ###This is to create wordcloud based on image recognition results###
    df <- data.frame(gsub(" *\\(.*?\\) *", "", outputtext()),gsub("[^0-9.]", "", outputtext())) #Make a dataframe using detected objects and scores
    names(df) <- c("Object","Score") #Set column names
    df$Object <- as.character(df$Object) #Convert df$Object to character
    df$Score <- as.numeric(as.character(df$Score)) #Convert df$Score to numeric
    s <- strsplit(as.character(df$Object), ',') #Split rows by comma to separate rows
    df <- data.frame(Object=unlist(s), Score=rep(df$Score, sapply(s, FUN=length))) #Allocate scores to split words
    df2 <- df[complete.cases(df),]
    # By separating long categories into shorter terms, we can avoid "could not be fit on page. It will not be plotted" warning as much as possible
    wordcloud(df2$Object, df2$Score, scale=c(4,2),
              colors=brewer.pal(6, "RdBu"),random.order=F) #Make wordcloud
  })
  
  output$outputImage <- renderImage({
    ###This is to plot uploaded image###
    if (is.null(input$file1)){
      outfile <- "/tmp/imagenet/cropped_panda.jpg"
      contentType <- "image/jpg"
      #Panda image is the default
    }else{
      outfile <- input$file1$datapath
      contentType <- input$file1$type
      #Uploaded file otherwise
    }
    
    list(src = outfile,
         contentType=contentType,
         width=300)
  }, deleteFile = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

