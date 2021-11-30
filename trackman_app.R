library(shiny)
library(ggplot2)
library(shinythemes)
library(data.table)
library(reshape)
source("iowa_baseball_functions.R")
ui<-fluidPage(
  theme = shinytheme("slate"), 
  navbarPage(title = "SELECT" ,
            tabPanel("File Input", 
                     h1("Data visualization of Trackman Data"),
                     h2("Enter one or more CSV below"),
                      fileInput(inputId = "csvs",label="",multiple = TRUE),
                     h3("Created by Harris Kingsley"),
                     h3("Contributor: Sumair Shah")
                    
                     ),
            
            tabPanel("Hitters", 
                sidebarLayout(
                   sidebarPanel(
                     uiOutput("batter"), 
                     selectInput(inputId = "Batter_stat",label =  "Heatmap Stat", choices = c("ExitSpeed","Iso","Slug","Frequency","Miss")),
                     selectInput(inputId = "Pitcher_throws",label =  "Pitcher Throws", choices = c("Both","Left","Right")),
                     checkboxInput(inputId = "Batter_smooth",label =  "Smooth Heatmap", value=FALSE)
                     
                   ),
                   mainPanel( fluidRow( column(8, plotOutput(outputId = "Batter_spraychart"),style = "background-color: grey;"  ) ))
                ),   
                splitLayout(
                          plotOutput(outputId = "Batter_heatmap"),
                          plotOutput(outputId = "Launch_angle"),
                          tableOutput(outputId = 'Batter_summary')
                )     
                     
                     
            ),
             
            tabPanel("Pitchers",  
                sidebarLayout(
                  sidebarPanel( 
                     uiOutput("pitcher") ,
                     selectInput(inputId = "Pitcher_stat",label =  "Heatmap Stat", choices = c("ExitSpeed","Iso","Slug","Frequency","Miss")),
                     selectInput(inputId = "Batter_side",label =  "Batter Side", choices = c("Both","Left","Right")),
                     checkboxInput(inputId = "Pitcher_smooth",label =  "Smooth Heatmap", value=FALSE)
                    ),
                  mainPanel(fluidRow(column(12,plotOutput(outputId = "Pitcher_count"),style = "background-color: grey;" )))
                    ),
                     splitLayout(  
                      plotOutput(outputId = "Pitcher_heatmap"),
                      tableOutput(outputId = 'Pitcher_summary')
                  )     
             )        
  )   
  )

  
  
  
    





server<-function(input,output){
  
  #increases max file size input from 5MB to 30MB
  options(shiny.maxRequestSize=30*1024^2) 
  
  #takes the user selected data that is read in and stores it in a reactive variable 
  reactive_data<- reactive({
     inFile <-      rbindlist(lapply(input$csvs$datapath, read.csv), use.names=TRUE, fill=TRUE)
    if (is.null(inFile)){
      return(NULL) }
    return(data.frame(inFile))
    
  })
  
  
  ####outputs and inputs that are dependent on the data that is read in 
  output$batter<-  renderUI({selectInput(inputId = "BatterId", label = "Batter Name",choices = reactive_data()['Batter'] )})
  output$pitcher<-  renderUI({selectInput(inputId = "PitcherId", label = "Pitcher Name",choices = reactive_data()['Pitcher'] )})
  
  #Batter Stats 
  output$Batter_summary<-renderTable({
    baseball_data<-reactive_data()
    return(batter_sum(baseball_data,input$BatterId,input$Pitcher_throws))    

  },rownames = TRUE)  
  
  
  

  #Batter Heatmap 
  output$Batter_heatmap<-renderPlot({
    baseball_data<-reactive_data()
    return(player_heatmap(baseball_data,input$BatterId,pitcher = FALSE,measure = input$Batter_stat , smooth = input$Batter_smooth,PitcherThrows = input$Pitcher_throws   ))
  })  
  #Batter SprayChart 
  output$Batter_spraychart<-renderPlot({
    baseball_data<-reactive_data()
    return(spray_chart(baseball_data,input$BatterId , PitcherThrows = input$Pitcher_throws   ))
  })  
  #batter LA Plot
  output$Launch_angle<-renderPlot({
    baseball_data<-reactive_data()
    return(LA_plot(baseball_data,input$BatterId , PitcherThrows = input$Pitcher_throws   ))
  })  
  
  #pitcher Heatmap 
  output$Pitcher_heatmap<-renderPlot({
    baseball_data<-reactive_data()
    return(player_heatmap(baseball_data,input$PitcherId,pitcher = TRUE,measure = input$Pitcher_stat , smooth = input$Pitcher_smooth,BatterSide  = input$Batter_side ))
    })
  #pitcher count plots
  output$Pitcher_count<-renderPlot({
    baseball_data<-reactive_data()
    return(pitcher_counts(baseball_data,input$PitcherId,BatterSide  = input$Batter_side ))
  })
  
  #Pitcher Stats 
  output$Pitcher_summary<-renderTable({
    baseball_data<-reactive_data()
    return(pitcher_sum(baseball_data,input$PitcherId,input$Batter_side))    
    
  },rownames = TRUE)  
  


  
}
shinyApp(ui = ui, server = server)