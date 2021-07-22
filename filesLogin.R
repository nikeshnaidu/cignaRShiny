library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(zip)
library(shinythemes)
library(shinyWidgets)
library(aws.s3)
#library(RPostgres)
library(DBI)
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAVEESBKEWFWCJS6AH",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "ap-south-1" #"us-east-1"
)
s3BucketName <- "cignabuck" # Bucket Name


pw<- {
  "nikesh"
}
con <- dbConnect(RMySQL::MySQL()
                 , host="cigna.cjbibw2yu90b.ap-south-1.rds.amazonaws.com"
                 , port=3306
                 , user="admin"
                 , password="16113194"
                 ,dbname = "cignadb"
)
# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username",value = "nikesh@gmail.com", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: skyblue; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     # br(),
                     # br(),
                     # tags$code("Username: mnikesh@pkglobal.com  Password: mnikeshS6AH"),
                     # br(),
                     # tags$code("Username: myuser1  Password: mypass1")
                   ))
)
header <- dashboardHeader( title = "Cigna")

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")




server <- function(input, output, session) {
  login = FALSE
  USER <- reactiveValues(login = login)
  observe({
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          mailqry<-paste0("select count(mail) from subs_db where mail=","'",Username ,"'",";")
          mailcheck<-dbGetQuery(con,mailqry)
          
          if(mailcheck == 1) {
            pwdqry = paste0("select pswd from subs_db where mail = ","'",Username ,"'",";")
            pwdcheck <- dbGetQuery(con,pwdqry)
            if(pwdcheck == Password) {
              USER$login <- TRUE
            } else{
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } 
          else
          {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Second Page", tabName = "second", icon = icon("th"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        
        # First tab
        tabItem(tabName ="dashboard", class = "active",
                fluidRow(
                  #box(width = 12, dataTableOutput('results'))
                  setBackgroundImage(
                    src = "Background.png"
                  ),
                  #theme = shinytheme(), #"superhero" #"sandstone"
                  shinythemes::themeSelector(),
                  imageOutput("cigImg",height = "50px",width = "50px"),#shinythemes::themeSelector(),
                  h4(textOutput("currentTime"),align = "right"),
                  titlePanel(
                    withTags({i(strong(h2("Hello User", align = "center",style = "color:skyblue")))})),
                  h4("Select and Download your files", align = "center"),
                  sidebarLayout(
                    sidebarPanel(
                      uiOutput("links_list"),
                      downloadButton("downloadzip", label = "Download")
                    ),
                    mainPanel() 
                  )
                )),
        
        # Second tab
        tabItem(tabName = "second",
                fluidRow(
                  box(width = 12, dataTableOutput('results2'))
                )
        ))
      
    }
    else {
      loginpage
    }
  })
  output$results2 <-  DT::renderDataTable({
    datatable(mtcars, options = list(autoWidth = TRUE,
                                     searching = FALSE))
  })
  
  get.file <-function(){
    folder<-isolate({
      fldqry<-paste0("select files from subs_db where mail = ","'",input$userName,"'",";")
      fold <- dbGetQuery(con,fldqry)
      fold
    })
    lis<-unlist(strsplit(paste(folder,sep="[ ]"), ","))
    return(lis)
  }
  #})
  obsList <- list()
  output$links_list <- renderUI({ 
    lapply(as.list(1:length(get.file())), function(i)      
    {
      #Store the name of the file in btName
      btName <- get.file()[i]
      # creates an observer only if it doesn't already exists
      if (is.null(obsList[[btName]])) {
        obsList[[btName]] <<- btName 
      }
      fluidRow(checkboxInput(btName, get.file()[i]))
    })
  })
  
  output$downloadzip<-downloadHandler(
    filename = function(){
      paste0("Extract.zip")
    },
    content = function(file){
      withProgress(message = "Writing Files to Disk. Please wait...", {
        fs<-c()           
        for (i in 1:length(obsList)){
          if(input[[obsList[[i]]]])  #checks if condition is TRUE
            #fs<-c()
          {
            path <-obsList[[i]]
            fs <- c(fs, path)
            files <-paste0("s3://",s3BucketName,"/",obsList[i],sep="")  #attaches s3://cignabuck/ with user selected files in obsList
            writeBin(rawToChar(get_object(files,s3BucketName)),path) #write as binary files and change to raw from char and get_object gives the object
            #upload_archive(vault="cignaVault",contents=get_object(files,"cignabuck"))
            #delete_object(files,"cignabuck")
          }
        }
        #create the zip file
        zip(zipfile=file,files=fs)
      })
    },
    contentType = "application/zip"
  )
  
  
}
shinyApp(ui, server)
