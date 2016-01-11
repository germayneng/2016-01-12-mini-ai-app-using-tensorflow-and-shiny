library(wordcloud)
shinyServer(function(input, output) {
    PYTHONPATH <- "path/to/your/python"  #should look like /Users/yourname/anaconda/bin if you use anaconda python distribution in OS X
    CLASSIFYIMAGEPATH <- "path/to/your/classify_image.py" #should look like ~/anaconda/lib/python2.7/site-packages/tensorflow/models/image/imagenet
    
    outputtext <- reactive({
      ###This is to compose image recognition template###
      inFile <- input$file1 #This creates input button that enables image upload
      template <- paste0(PYTHONPATH,"/python ",CLASSIFYIMAGEPATH,"/classify_image.py") #Template to run image recognition using Python
      if (is.null(inFile))
        {res <- system(paste0(template," --image_file /tmp/imagenet/cropped_panda.jpg"),intern=T)} else { #Initially the app classifies cropped_panda.jpg, if you download the model data to a different directory, you should change /tmp/imagenet to the location you use. 
      res <- system(paste0(template," --image_file ",inFile$datapath),intern=T) #Uploaded image will be used for classification
        }
      })
    
    output$plot <- renderPlot({
      ###This is to create wordcloud based on image recognition results###
      df <- data.frame(gsub(" *\\(.*?\\) *", "", outputtext()),gsub("[^0-9.]", "", outputtext())) #Make a dataframe using detected objects and scores
      names(df) <- c("Object","Score") #Set column names
      df$Object <- as.character(df$Object) #Convert df$Object to character
      df$Score <- as.numeric(as.character(df$Score)) #Convert df$Score to numeric
      s <- strsplit(as.character(df$Object), ',') #Split rows by comma to separate rows
      df <- data.frame(Object=unlist(s), Score=rep(df$Score, sapply(s, FUN=length))) #Allocate scores to split words
      # By separating long categories into shorter terms, we can avoid "could not be fit on page. It will not be plotted" warning as much as possible
      wordcloud(df$Object, df$Score, scale=c(4,2),
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
})