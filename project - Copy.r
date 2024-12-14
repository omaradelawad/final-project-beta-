library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT) 
library(dplyr) 
library(arules)


ui <- fluidPage(theme = shinytheme("superhero"),
                
                tags$head(
                  tags$style(HTML("
                #min , #high , #customers , #cities , #top3 {
                      background-color : orange ; 
                      color : white ;  
                      display : block ;  
                      width : 100% ; 
                  
                   } 
               
                .dataTable thead th {
                  background-color: #4CAF50;  
                  color: white;             
                
                }
    .dataTable tbody tr:nth-child(even) {
      background-color: #f2f2f2; 
    }
    
    .dataTable { 
        display : block ; 
        width : 100%
    }
    .dataTable tbody tr:nth-child(odd) {
      background-color: #ffffff;
    }
    
      
    "))),
                
                navbarPage(
                  "data science project",
                  tabPanel("the dahboard",
                           sidebarPanel(
                             tags$h3("Enter your CSV file:"),
                             fileInput("file1", "Choose CSV File", accept = ".csv"), 
                           
                             
                             tags$h3("Number of clusters") ,
                             sliderInput(
                               "slider1",
                               "Set value range",
                               min = 2,
                               max = 4,
                               value = 4
                             )  ,  
                             tags$h3("Min_Support") ,
                             sliderInput(
                               "slider2",
                               "Set value range",
                               min = 0.001,
                               max = 1,
                               value =  1 , 
                             ) ,
                             tags$h3("Min_Confidence") ,
                             sliderInput(
                               "slider3",
                               "Set value range",
                               min = 0.001,
                               max = 1,
                               value = 1 , 
                             )
                           ), 
                           mainPanel(
                             h1("Dashboard 📈"),
                             
                             box(
                               title = "Cash vs Credit", 
                               plotOutput("pie_chart"), 
                             
                             ) , 
                             box(
                               title = "age and totla spending" ,
                               plotOutput("plot2")
                             ) ,
                             box(
                               title = "cities and total spending" ,
                               plotOutput("plot3")
                             ) , 
                             
                             box(
                               title = "distribution of total spending." ,
                               plotOutput("plot4")
                             )
                             
                           ) 
                  ),
                  tabPanel("k means" ,  
                           mainPanel(
                             box( 
                               title = "k means" ,
    
                               box(
                                 tags$h5("the table") , 
                                 tableOutput("table1")
                               ),
                               
                             )
                           )
                           ),
                  tabPanel("Navbar 3", 
                           
                           box(
                             title = "association rules",
                             DTOutput("rules_table")
                           )
                           
                           ) , 
                  tabPanel("stats", 
                           tags$h3("useful stats 📊") ,
                           box( 
                             title = "ages stats",
                             tags$h4("min age") , 
                             verbatimTextOutput("min"), 
                             
                             tags$h4("Highest age") , 
                             verbatimTextOutput("high"),
                             class = "box-1" ,
                           ), 
                           
                           box( 
                             title = "customers" , 
                             tags$h4("number of customers") , 
                             verbatimTextOutput("customers"), 
                             
                             tags$h4("top 3 names") , 
                             verbatimTextOutput("top3"),
                           ) , 
                           
                           box( 
                             title = "cities and locations" ,
                             tags$h4("cities") , 
                             verbatimTextOutput("cities")
                           )
                           )
                )
) 


server <- function(input, output) {
  
  data <- reactive({ 
    req(input$file1)  
    read.csv(input$file1$datapath , header = TRUE)   
    
  })
  
  
 
  output$pie_chart <- renderPlot({
    paymentsmethods <- table(data()$paymentType)  
    
    labels <- c("Cash", "Credit")  
    ratio <- round(paymentsmethods / sum(paymentsmethods) * 100, 1)  
    
   
    pie(
      paymentsmethods,
      labels = paste(labels, ratio, "%"),
      main = "Cash vs Credit",
      col = c("navy", "orange")
    )
  }) 
  
  output$plot2 <- renderPlot({
    # the sum of the total and ages 
    ageByTotal2 <- tapply(data()$total , data()$age , sum)
    
    # ترتيب الإجماليات من الأعلى إلى الأدنى
    sortedTotal <- sort(ageByTotal2 , decreasing = FALSE)
    
    # رسم بياني
    dotchart( sortedTotal  , main = "Age vs Total Spending", 
             xlab = "Total Spending", ylab = "Age", 
             col = "navy")
  }) 
  
  output$plot3 <- renderPlot({
    totalCitySpending <- tapply(data()$total, data()$city, sum)
    
    sortedCities <- sort(totalCitySpending, decreasing = TRUE)
    
    barplot(sortedCities,
            xlab = "Cities",
            ylab = "Total Spending",
            main = "Total Spending per City",
            las = 2,  
            col = rainbow(length(sortedCities)))
  })
  
  output$plot4 <- renderPlot({
    
    boxplot(x = data()$total,  
            main = "Distribution of Total Spending",  
            
            xlab = "Total Spending",  
            
            col = "navy") 
    
    
  }) 
  
  
 output$table1 <- renderTable({
   
   library(stats)
   library(readxl)
   library(ggplot2)
   library(tidyr)
   library(dplyr)
   
   # Load the data from the Excel file
   
    req(data())
   print("Missing values before cleaning:")
   print(sapply(data(), function(x) sum(is.na(x))))
   
   # Remove rows with any missing values (NA) in 'age' or 'total'
   data_clean <- data() %>%
     select(age, total) %>%
     filter(!is.na(age) & !is.na(total))
   
   # Remove duplicate rows from the cleaned data to prevent any many-to-many relationships
   data_clean <- data_clean %>% distinct()
   
   # Verify the cleaned data (show the first few rows)

   
   # Check for any missing values in the cleaned data
   print("Missing values after cleaning:")
   print(sapply(data_clean, function(x) sum(is.na(x))))
   
   # Set the number of clusters and a random seed for reproducibility
   set.seed(200)
   n_clusters <- input$slider1
   
   # Apply K-means clustering to 'age' and 'total' columns
   kmeans_result <- kmeans(data_clean, centers = n_clusters)
   
   # Add the cluster assignment to the cleaned data
   data_clean$Cluster <- kmeans_result$cluster
   
   # Merge the cluster assignments back to the original data
   data_with_cluster <- left_join(data(), data_clean, by = c("age", "total"))
   
   # Check if there are any rows in the original data that were not matched with the cleaned data
   unmatched_rows <- data() %>% anti_join(data_clean, by = c("age", "total"))
   print("Rows in data not matched with data_clean:")
   print(unmatched_rows)
   
   # Display the final data with the cluster assignments
   print("Final Data with Clusters:")
   print(head(data_with_cluster))
   
   # Check for missing values in the final data after the merge
   print("Missing values in the final data:")
   print(sapply(data_with_cluster, function(x) sum(is.na(x))))
   
  
   
   #create a table with customer names, age, total spending, and their assigned cluster number.
   customer_table <- head( data_with_cluster  , 50)  %>%
     select(customer, age, total, Cluster)
   
   # Print the table displaying customer information
   print("Customer Table with Name, Age, Total Spending, and Cluster:")
   print(customer_table)
 }) 

 output$rules_table <- renderDT({
   item_list <- strsplit(as.character(data()$items), ",")
   

   transactions <- as(item_list, "transactions")
   
  
   rules <- apriori(transactions, parameter = list(supp = as.numeric( input$slider2 ) ,  conf = as.numeric( input$slider3 ) ))
   
   if (length(rules) == 0) {
     return(tagList(
       h3("No association rules found with the given parameters."),
       tags$p("Try adjusting the Min Support or Min Confidence values.")
     ))
   }
   

   

   rules_df <- as(rules, "data.frame")
   
  
   datatable(rules_df, options = list(pageLength = 5, scrollX = TRUE))
 })
 

 
  output$min <- renderText({
    min(data()$age)
  }) 
  output$high <- renderText({
    max(data()$age)
  })
  
  output$customers <- renderText({
    sum(  table(data()$customer) )
  }) 
  
  output$cities <- renderText({
    unique(data()$city)
  }) 
  
  output$top3 <- renderText({
    names( head( sort(table(data()$customer), decreasing = TRUE) , 3 )      ) 
  })
}


# Create Shiny app
shinyApp(ui = ui, server = server)
