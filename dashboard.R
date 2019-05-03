library(shiny)
library(shinydashboard)

client <- read.delim("data.csv", sep = ",")
client<-client[-1]
varChoices <- c("Select All",as.list(names(client)))
#Transformation en factor
client$Churn <- as.factor(client$Churn)
client$gender <- as.factor(client$gender)
client$SeniorCitizen <- as.factor(client$SeniorCitizen)
client$Partner <- as.factor(client$Partner)
client$Dependents <- as.factor(client$Dependents)
client$PhoneService <- as.factor(client$PhoneService)
client$MultipleLines <- as.factor(client$MultipleLines)
client$InternetService <- as.factor(client$InternetService)
client$OnlineSecurity <- as.factor(client$OnlineSecurity)
client$OnlineBackup <- as.factor(client$OnlineBackup)
client$DeviceProtection <- as.factor(client$DeviceProtection)
client$TechSupport <- as.factor(client$PhoneService)
client$StreamingTV <- as.factor(client$StreamingTV)
client$StreamingMovies <- as.factor(client$StreamingMovies)
client$Contract <- as.factor(client$Contract)
client$PaperlessBilling <- as.factor(client$PaperlessBilling)
client$PaymentMethod <- as.factor(client$PaymentMethod)



ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(sidebarMenu(menuItem("Introduction", tabName = "intro"),
                               
                               menuItem("Data", tabName = "data",
                                        menuSubItem("Dataset", tabName="dataset"),
                                        menuSubItem("Summary", tabName="summary")),
                               menuItem("Link between several variables",tabName="link",
                                        selectInput(inputId = "selected_var",label = "Variable",choices = varChoices, multiple = FALSE)))),
  dashboardBody(tags$head(tags$style(HTML('/* main sidebar */.skin-blue .main-sidebar {background-color: white;}
                              /* navbar (rest of the header) */.skin-blue .main-header .navbar {background-color: #FE8000;}
                              /* logo */.skin-blue .main-header .logo {background-color: #FE8000;}
                              /* logo when hovered */.skin-blue .main-header .logo:hover {background-color: #FE8000;}
                              /* toggle button when hovered  */.skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #333333;}'))),
                tabItems(
                  tabItem(tabName="intro", h3("Hello")),
                  tabItem(tabName="dataset", dataTableOutput("table")),
                  tabItem(tabName="summary",verbatimTextOutput("summary")),
                  tabItem(tabName="link", plotOutput("plot"))
                ))
)

server <- function(input, output,session) {
  output$plot <- renderPlot({ plot(client[,input$selected_var])
  })
  
  # Generate the dataset
  output$table <- renderDataTable({client})
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({summary(client)})
  
  
  observe({
    if("Select All" %in% input$selected_var)
      selected_choices=varChoices[-1] # choose all the choices _except_ "Select All"
    else
      selected_choices=input$selected_var # update the select input with choice selected by user
    updateSelectInput(session, "selected_var", selected = selected_choices)
  })
  
}

shinyApp(ui, server)