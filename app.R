#
# This is a Shiny web application. 
#
#This app uploads a cvs and displays % green cover time series
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##This should detect and install missing packages before loading them - hopefully!

# list.of.packages <- c("shiny","ggplot2","shinyFiles","rgdal","raster")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
# 
library(shiny)
library(shinyFiles)
library(rgdal)
library(raster)
library(ggplot2)

source("helpers.R")


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Percent Green Cover"),

    # Sidebar with data upload widget and date range slider 
    sidebarLayout(
        #data uplead widget
        sidebarPanel(
        #select folder    
            strong("Select a folder on your computer with images you want to process."),
            
            shinyDirButton("dir", "Chose a directory", "Upload"),
            
            tags$hr(),
        #convert image   
            strong("If the images are RGB convert them to VARI or GLI."),
            
            selectInput("select1", "", 
                               c("Convert RGB to VARI" = "cnV",
                                 "Convert RGB to GLI" = "cnG")),
            actionButton("RunConvt","Run Convert"),
            
            tags$hr(),
         #anylise image   
            strong("Select an anylisis you want to run."),
            
            selectInput("select2", "",
                        c("% green from VARI" = "pctgV",
                          "% green from GLI" = "pctgG",
                          "% green from NDVI" = "pctgN")),
            
            "Enter threshold value. Recommend .001 for VARI, .07 for GLI and .6 for NDVI.",
            
            numericInput("thr", "Threshold:", 0.001, min = 0, max = 100, step = 0.001),
            
            actionButton("RunAnl","Run Analysis"),
            
            tags$hr(),
         #select file to graph   
            strong("Select a set of values in a csv file to graph."),
            
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
        
            dateRangeInput("daterange", "Date range:",
                               start=Sys.Date()-10,
                               end = Sys.Date()
                               )


            
        ),
   
        mainPanel(
            "Directory",
            verbatimTextOutput("dir", placeholder = TRUE),
            "Files",
            verbatimTextOutput('files',placeholder = TRUE),
            "Graph",

           plotOutput("distPlot"),
           
           "First few lines of data table",

           # Show table of data header
           dataTableOutput("contents")),

    # 
     # verbatimTextOutput('rawInputValue'),
     # verbatimTextOutput('filepaths')
))

# Define server logic
server <- function(input, output, session) {
    volumes = getVolumes()
    shinyDirChoose(
        input,
        'dir',
        roots = volumes,
        filetypes = c('JPG', 'tif', 'tiff')
    )
    
    # init a reactive data frame for plotting
    df1 <- data.frame()
    vals <- reactiveValues(
        df1 = NULL,
        data = df1
    )  
    
    # #global <- reactiveValues(datapath = getwd())
    # 
    # dir <- reactive({ parseDirPath(volumes,input$dir)
    #     #renderText({dir.path})
    #     })
    
 #list directory path and file names   
    observe({
        req(input$dir)
        
            dir.path <- isolate({parseDirPath(volumes,input$dir)})
            output$dir<-renderText(dir.path)
            files.path <- isolate({list.files(path = dir.path ,pattern=('.JPG|.tiff|.tif'), full.names = TRUE, recursive=FALSE)})
            files.names<- isolate({list.files(path = dir.path ,pattern=('.JPG|.tiff|.tif'), full.names = FALSE, recursive=FALSE)})
            output$files<-renderText(files.names)
        
    })
    
#run file conversion    
    observeEvent(
        eventExpr = input[["RunConvt"]],
        handlerExpr = {
            path = parseDirPath(volumes,input$dir)
            print("Running Convert") 
            files<-list.files(path = path ,pattern=('.JPG|.tiff'), full.names = TRUE, recursive=FALSE)
            names<-sub("\\..*", "", basename(files))
            dir.create(path+"/converted")
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Converting", value = 0)
            # Close the progress when this reactive exits (even if there's an error)
            on.exit(progress$close())
            
            
            if (input$select1 == "cnV") {
                    for (i in seq_along(files)){
                        progress$inc(1/seq_along(files), detail = paste("image", i))
                        my_raster<-brick(files[i])
                        VARI_1<-VARI(my_raster,1,2,3)
                        writeRaster(VARI_1,paste0(path+"/converted/VARI_",names[i],".tiff"),overwrite=TRUE)
                        }
                }else if (input$select1 == "cnG") {
                    for (i in seq_along(files)){
                        progress$inc(1/seq_along(files), detail = paste("image", i))
                        my_raster<-brick(files[i])
                        GLI_1<-GLI(my_raster,1,2,3)
                        writeRaster(GLI_1,paste0(path+"/converted/GLI_",names[i],".tiff"),overwrite=TRUE)
                        }
                    } 
            })       

#run % green analysis                 
    observeEvent(
        eventExpr = input[["RunAnl"]],
        handlerExpr = {
            print("Running Analysis") 
            path=parseDirPath(volumes,input$dir)
 path=getwd()  
 
            if (input$select2 == "pctgV") {
                path=parseDirPath(volumes,input$dir)
                files<-list.files(path = path,pattern='VARI.*tif', full.names = TRUE, recursive=TRUE)
                names<- sub("\\..*", "", basename(files))
                dates<-as.Date(sub("(?<=2019|2020).*","",substring(names,11),perl=TRUE),format="%d%b%Y")

                #this runs the raster calculation on all files
                pctg<-pctgreen(files,dates,input$thr)
                
                pctg$date<-as.Date(pctg$date)

                #update daterange
                updateDateRangeInput(session, "daterange",
                                     start = as.Date(min(pctg$date,na.rm=TRUE)),
                                     end = as.Date(max(pctg$date,na.rm=TRUE))
                )
                

                vals$df1<-pctg

                write.csv(pctg, file = path+"/VARIpctGreen.csv")

            }else if (input$select2 == "pctgG") {
                path=parseDirPath(volumes,input$dir)
                files<-list.files(path = path,pattern='GLI.*tif', full.names = TRUE, recursive=TRUE)
                names<- sub("\\..*", "", basename(files))
                dates<-as.Date(sub("(?<=2019|2020).*","",substring(names,10),perl=TRUE),format="%d%b%Y")
                #this runs the raster calculation on all files
                pctg<-pctgreen(files,dates,input$thr)
                
                pctg$date<-as.Date(pctg$date)
                
                #update daterange
                updateDateRangeInput(session, "daterange",
                                     start = as.Date(min(pctg$date,na.rm=TRUE)),
                                     end = as.Date(max(pctg$date,na.rm=TRUE))
                )
                
                vals$df1<-pctg
       
                write.csv(pctg, file = path+"/GLIpctGreen.csv")
                
            } else if (input$select2 == "pctgN") {
                path=parseDirPath(volumes,input$dir)
                files<-list.files(path = path,pattern='NDVI.*tif', full.names = TRUE, recursive=TRUE)
                names<- sub("\\..*", "", basename(files))
                dates<-as.Date(sub("(?<=2019|2020).*","",names,perl=TRUE),format="%d%b%Y")
                #this runs the raster calculation on all files
                pctg<-pctgreenNDVI(files,dates,input$thr)
                
                pctg$date<-as.Date(pctg$date)
                
                #update daterange
                updateDateRangeInput(session, "daterange",
                                     start = as.Date(min(pctg$date,na.rm=TRUE)),
                                     end = as.Date(max(pctg$date,na.rm=TRUE))
                )
                
                vals$df1<-pctg

                write.csv(pctg, file = path+"/NDVIpctGreen.csv")
            }
        }
        
    )            



#get uploaded data file
    observe({ 
        
        infile <- input$file1
        
        if (is.null(infile))
            return(NULL)
        dft<-read.csv(infile$datapath, header = TRUE)
        dft$date<-as.Date(dft$date)
        vals$df1<-dft
        #print(head(vals$df1))
        #when loaded update daterange

        updateDateRangeInput(session, "daterange",
                             start = as.Date(min(vals$df1$date,na.rm=TRUE)),
                             end = as.Date(max(vals$df1$date,na.rm=TRUE))
        )
        #print(vals$df1$date)
        
    })
    


#output plot

    output$distPlot <- renderPlot({

        req(length(vals$df1)>0)

        df1<-vals$df1

        df1$date<-as.Date(df1$date)
        #allow user to change daterange in graph
        df1 <- df1[df1$date >= input$daterange[1] &
                          df1$date <= input$daterange[2],]

        ggplot(df1,aes(x=date,y=pct))+geom_point()
        
        #+geom_smooth(method=lm)


    })
#show data table


    output$contents <- renderDataTable({
        
        req(length(vals$df1)>0)

        vals$df1

    })

}

# Run the application 
shinyApp(ui = ui, server = server)




