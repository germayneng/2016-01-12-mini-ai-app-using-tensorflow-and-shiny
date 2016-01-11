shinyUI(
  fluidPage(titlePanel("Simple Image Recognition App using TensorFlow and Shiny"),
            tags$hr(),
            fluidRow(
              column(width=4,
                     fileInput('file1', '',accept = c('.jpg','.jpeg')),
                     imageOutput('outputImage')
                     ),
              column(width=8,
                     plotOutput("plot")
                     )
              )
            )
  )