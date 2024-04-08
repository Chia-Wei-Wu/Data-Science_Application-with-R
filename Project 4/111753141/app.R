#import package
library(shiny)
library(FactoMineR)
library(factoextra)
library(datasets)
library(corrplot)
library(ggbiplot)
library(stringr)

#Defined UI
ui <- fluidPage(
  tags$head(tags$style(HTML(".A1{font-family:'Times New Roman',Times,serif;font-size:20px;line-height:1.5;}"))),
  tags$head(tags$style(HTML(".A2{font-family:'Times New Roman',Times,serif;font-size:30px;line-height:1.5;}"))),
  tags$head(tags$style(HTML(".A3{font-family:'Times New Roman',Times,serif;font-size:20px;line-height:1.5;font-weight:bold;}"))),
  tags$head(tags$script("$(document).on('keypress', '#tableData', function(e){var key = e.which || e.keyCode;if (key === 13) {$(this).hide();}});")),
  
  titlePanel("NCCUCS 111753141 Chia_Wei,Wu"),
  
  tabsetPanel(
    # Page 0: Introduction
      tabPanel("Introduction",
               br(),
               br(),
               HTML("<div class='A1'><p>Welcome to this page! Let me introduce how to use this page.</p></div>"),
               br(),
               br(),
               HTML("<div class='A1'>
                            <p>This page provides various functionalities to analyze the Iris dataset.</p>
                            <p>In the 'DataSet' tab, you can explore Summary Statistics and correlation of the dataset. You can also upload your own database and observe your raw data.</p>
                            <p>In the 'PCA' tab, you can perform Principal Component Analysis (PCA) on selected features of the dataset.</p>
                            <p>In the 'CA' tab, you can perform Correspondence Analysis (CA) with tuning the number of data.</p>
                            <p>In the 'PCA_Information' tab, you can figure out the Summary Statistics, Rotation Table, Center Table, Sdev Table, and Scale Table by PCA.</p>
                            <p>Choose the features you want to analyze using the dropdown menus, and select the analysis method.</p>
                            <p>Feel free to interact with the different tabs and explore the capabilities of this page.</p>
                    </div>")
       ),
    
    # Page 1: IrisDB
      tabPanel("DataSet",
               br(),
               HTML("<div class='A1')<p>From this page, you can observe dataset.But this web is for IRIS, so you only upload dataset like IRIS.</P></div>"),
               br(),
               br(),
               HTML("<div class='A3')
                            <p>About IRIS dataset.
                            <a href='https://www.kaggle.com/datasets/uciml/iris' target='_blank'>(Read More)</a></p>
                    </div>"),
               HTML("<div class='A1')
                            <p>The Iris dataset was used in R.A. Fisher's classic 1936 paper, The Use of Multiple Measurements in Taxonomic Problems, and can also be found on the UCI Machine Learning Repository.
                            It includes three iris species with 50 samples each as well as some properties about each flower. One flower species is linearly separable from the other two, but the other two are not linearly separable from each other.</p>
                    </div>"),
               br(),
               br(),
               fluidPage(
                    sidebarLayout(
                            sidebarPanel(
                                  selectInput("dataset", "Select Dataset", choices = c("iris", "Upload your database"), selected = "iris"),
                                  br(),
                                  fileInput("file", "Choose CSV file"),
                                  actionButton("showData", "Show Raw Data")),
                            mainPanel(
                                  HTML("<div class='A1')<p>Summary Statistics.</P></div>"),
                                  verbatimTextOutput("summaryTable"),
                                  br(),
                                  HTML("<div class='A1')<p>Correlation.</P></div>"),
                                  plotOutput("confusionMatrix"),
                                  br(),
                                  verbatimTextOutput("rawData"))))
       ),
    
    # Page 2: PCA 
      tabPanel("PCA",
               br(),
               HTML("<div class='A1'>
                            <p>In this page, you can select the features for the x-axis and y-axis.</p>
                            <p>If you enter 'Run', it will plot the PCA result, and if you enter 'Reset', it will clear the PCA result.</p>
                            <p>Note that if you choose the same PCA index for both the x-axis and y-axis, it is not well-defined by positive definition.</p>
                    </div>"),
               br(),
               br(),
               fluidPage(
                    sidebarLayout(
                            sidebarPanel(
                                  selectInput(inputId = "x_feature", label = "X-Axis Feature:", choices = c("PCA1","PCA2","PCA3","PCA4"), selected = "PCA1"),
                                  selectInput(inputId = "y_feature", label = "Y-Axis Feature:", choices = c("PCA1","PCA2","PCA3","PCA4"), selected = "PCA2"),
                                  actionButton("run_pca", "Run"),
                                  actionButton("reset", "Reset")),
                            mainPanel(
                                  HTML("<div class='A1')<p>Data Visualization by PCA.</P></div>"),
                                  plotOutput("factoPlot1"),
                                  br())))),
    
    # page3: CA
      tabPanel("CA",
               br(),
               HTML("<div class='A1'>
                            <p>In this page, you can select the number of data to observe the CA result.</p>
                            <p>Note that if you choose the number of data smaller than four, it is not well-defined.</p>
                            <p>Please set again.</p>
                    </div>"),
               br(),
               br(),
               fluidPage(
                    sidebarLayout(
                            sidebarPanel(
                                  numericInput(inputId = "data_carow", label = "Number of Data Rows:", value = 150, min = 1, max = 150)),
                    mainPanel(
                             plotOutput("factoPlot2"))
                    ))),
    
    #page4: 
    tabPanel("PCA_Information", 
             br(),
             HTML("<div class='A1'>
                            <p>In this page, you can select the number of data to observe the Statistics by method named PCA.</p>
                            <p>Note that if you choose the number of data smaller than six, it is not well-defined.</p>
                            <p>Please set again.</p>
                    </div>"),
             br(),
             br(),
             fluidPage(
                   sidebarLayout(
                           sidebarPanel(
                                 numericInput(inputId = "data_pcairow", label = "Number of Data Rows", value = 150, min = 1, max = 150)),
                           mainPanel(
                                 HTML("<div class='A1')<p>Summary Statistics.</P></div>"),
                                 verbatimTextOutput("pcaSummary"),
                                 br(),
                                 HTML("<div class='A1')<p>Rotation Table.</P></div>"),
                                 verbatimTextOutput("pcaRotationTable"),
                                 br(),
                                 HTML("<div class='A1')<p>Center Table.</P></div>"),
                                 verbatimTextOutput("pcaCenterTable"),
                                 br(),
                                 HTML("<div class='A1')<p>Sdev Table.</P></div>"),
                                 verbatimTextOutput("pcaSdevTable"),
                                 br(),
                                 HTML("<div class='A1')<p>Scale Table.</P></div>"),
                                 verbatimTextOutput("pcaScaleTable")))))
  )
)

server <- function(input, output){
  # Input data
  data <- reactive({
    if(input$dataset == "iris"){
      iris
    }else{
        if(is.null(input$file)){
            return(NULL)
        }else{
            read.csv(input$file$datapath)
    }}})
  
  pca_data <- reactiveVal(NULL)
  
  observeEvent(input$run_pca,{
      req(data())
      pca <- prcomp(data()[, 1:4], scale. = TRUE)
      pca_data(pca)
  })
  
  observeEvent(input$reset,{pca_data(NULL)})
  
  # Output page 1
  output$summaryTable <- renderPrint({
    if(!is.null(data())){
      summary(data())
    }else{
      "NO dataset selected."
    }})
  
  output$confusionMatrix <- renderPlot({
    if (!is.null(data())) {
      corr_matrix <- cor(data()[, 1:4])
      corrplot(corr_matrix, method = "color")
    }
  })
  
  observeEvent(input$showData,{
    showModal(modalDialog(title = "Raw Data",renderPrint({data()}),
    footer = modalButton("Close")
    ))})
  
  # Output page 2
  output$factoPlot1 <- renderPlot({
    if (!is.null(pca_data())){
    x_axis <- switch(input$x_feature, "PCA1" = 1, "PCA2" = 2, "PCA3" = 3, "PCA4" = 4)
    y_axis <- switch(input$y_feature, "PCA1" = 1, "PCA2" = 2, "PCA3" = 3, "PCA4" = 4)
    validate(need(x_axis!=y_axis, "The leading minor of order 2 is not positive definite, since this chose is not well-defined."))
    g <- ggbiplot(pca_data(),choices = c(x_axis,y_axis),obs.scale = 1, var.scale = 1, groups = data()[, 5], ellipse = TRUE, circle = TRUE)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    g <- g + labs(x = input$x_feature, input$y_feature)
    g <- g + scale_x_continuous(name = input$x_feature)
    g <- g + scale_y_continuous(name = input$y_feature)
    print(g)}
  })
  
  #output page3
  output$factoPlot2 <- renderPlot({
    validate(need(input$data_carow >= 4, "Number of Data Rows must be greater than or equal to 4."))
    result <- CA(data()[1:input$data_carow, 1:4])
    fviz_ca_row(result, col.row = data()[1:input$data_carow, "Species"])
  })
  
  # output page4
  output$pcaSummary <- renderPrint({
    validate(need(input$data_pcairow >= 6, "Number of Data Rows must be greater than or equal to 6."))
    result <- prcomp(data()[1:input$data_pcairow, 1:4], scale = TRUE)
    summary(result)
  })
  
  output$pcaRotationTable <- renderPrint({
    validate(need(input$data_pcairow >= 6, "Number of Data Rows must be greater than or equal to 6."))
    result <- prcomp(data()[1:input$data_pcairow, 1:4], scale = TRUE)
    print(result$rotation)
  })
  
  output$pcaCenterTable <- renderPrint({
    validate(need(input$data_pcairow >= 6, "Number of Data Rows must be greater than or equal to 6."))
    result <- prcomp(data()[1:input$data_pcairow, 1:4], scale = TRUE)
    print(result$center)
  })
  
  output$pcaSdevTable <- renderPrint({
    validate(need(input$data_pcairow >= 6, "Number of Data Rows must be greater than or equal to 6."))
    result <- prcomp(data()[1:input$data_pcairow, 1:4], scale = TRUE)
    print(result$sdev)
  })
  
  output$pcaScaleTable <- renderPrint({
    validate(need(input$data_pcairow >= 6, "Number of Data Rows must be greater than or equal to 6."))
    result <- prcomp(data()[1:input$data_pcairow, 1:4], scale = TRUE)
    print(result$scale)
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)