# Data Analyst Job Recommendation Tool
# RShiny Application

# This script file includes the user-interface and server definition of the app
## Defining three parts of the dashboard

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lazyeval) # so we can use interpret


ui <- dashboardPage(
  # Dashboard Page Setup ----------------------------------------------------
  title = "Job_Tool",
  skin = "yellow",
  
  dashboardHeader(title = h4("Job Recommendation Tool")),
  
  # Dashboard Sidebar -------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "input_dashboard", icon = icon("dashboard")),
      menuItem("Summaries", tabName = "visual_dashboard", icon = icon("dashboard")),
      menuItem("Job Recommendations", tabName = "output_dashboard", icon = icon("project-diagram")),
      menuItem("Help", tabName = "help_dashboard", icon = icon("question"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      
      # First Tab Content
      tabItem(tabName = "input_dashboard",
              fluidRow(
                
                box(
                  title = "Input Data",width = 12,
                  fileInput("raw_data", "Please Upload a CSV File",
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",".csv")                                  
                  )
                )
              )
      ),
      
      
      
      # Second Tab Content
      tabItem(tabName = "visual_dashboard",
              fluidRow(
                
                box(
                  
                  title = "Job Categoty",width = 12,
                  selectInput("job_type_user", "Select Job Title:",
                              c("Business Analyst" = "BA",
                                "Data Analyst" = "DA",
                                "Data Engineer" = "DE",
                                "Data Scientist" = "DS"),multiple = TRUE),
                  
                  selectInput("groupby_user", "Select Level:",
                              c("Job Title" = "Category",
                                "State" = "State",
                                "Industry" = "Industry"
                              )),
                  
                  selectInput("summarize_user", "Select Feature:",
                              c("Number of Jobs" = "n_jobs",
                                "Average Minimum Salary" = "min_salary",
                                "Average Maximum Salary" = "max_salary",
                                "Average Rating" = "Rating"
                              )),
                  
                  actionButton("update_output", "Update")),
                
                box(
                  title = "Data Visualization",width = 12,
                  plotOutput("plot")
                )
                
                
                
              )
              
              
              
              
      ),
      
      
      
      # Third Tab Content
      tabItem(tabName = "output_dashboard",
              fluidRow(
                
                box(
                  title = "User Inputs for Job Recommendation",
                  width = 12,
                  sliderInput("work_exp", "Work Experience in Months:", 1, 100, 50),
                  
                  selectInput("job_role", "Job Role(s)",
                              c("Business Analyst",
                                "Data Analyst",
                                "Data Engineer",
                                "Data Scientist"), multiple = TRUE),
                  
                  selectInput("skill_set",  "Skill Set", 
                              choices = list("SQL" = "SQL", "R" = "R", "Python" = "Python", "MS Excel" = "MS Excel"),
                              multiple = TRUE
                  ),
                  
                  selectInput("location_set","State Location", 
                              choices = list("CA" = "CA", "TX" = "TX", "NY" = "NY", "CH" = "CH"),
                              multiple = TRUE
                  ),
                  
                  selectInput("industry_set", "Prefered Industry Domain", 
                              choices = list("NA"),
                              multiple = TRUE
                  ),
                  
                  actionButton("generate_output", "Run Search")
                  
                  
                )
              ),
              fluidRow(
                
                
                box(
                  title = "Recommended Job:",width=12,
                  column(12,
                         dataTableOutput('table'))
                  #tableOutput('table')
                )
              )
      )
      
    )
  )
  
  
)

## Server Part
options(shiny.maxRequestSize = 100*1024^2)
server <- function(input, output,session){
  
  observeEvent(input$raw_data, {
    infile <<- input$raw_data
    RawData <<- read_csv(infile$datapath)
    
    unique(RawData$Category)
    
    data <- RawData
    
    #split Salary to min_salary and max_salary
    data <- data[!(data$Salary.Estimate == "-1"), ]
    data$min_salary <- str_split_fixed(data$Salary.Estimate, "-", 2)[,1]
    data$max_salary <- str_split_fixed(data$Salary.Estimate, "-", 2)[,2]
    data$min_salary <- parse_number(data$min_salary )
    data$max_salary <- parse_number(data$max_salary )
    
    ##change easy.apply == -1 to False
    data$Easy.Apply <- ifelse(data$Easy.Apply< 0, 0, data$Easy.Apply)
    data$Easy.Apply <- ifelse(data$Easy.Apply == 'TRUE', 1, data$Easy.Apply)
    
    #unique(data$Easy.Apply)
    n_easy <- data %>% group_by(Easy.Apply)%>% dplyr::summarise (n=n()) 
    perc_easy_apply = n_easy[[2]][2]/sum(n_easy[[2]])
    
    ## convert type Rating to numeric
    data$Rating <- as.numeric(data$Rating)
    data$Rating <- ifelse(data$Rating< 0, 0, data$Rating)
    
    ## clean up company name
    data$Company.Name= str_split_fixed(data$Company.Name, "\n", 2)[,1]
    
    ## take out State from location
    data$State <- str_split_fixed(data$Location, ", ", 2)[,2]
    data <- data[!(nchar(as.character(data$State)) > 2),]
    data <- data[!(data$State == ""), ]
    
    ## clean up empty values from industry        
    data <- data[!(data$Industry == "-1"), ]
    
    Raw_Data_Cleaned <<- data
    
    updateSelectInput(session, "industry_set",
                      label =  "Prefered Industry Domain", 
                      choices = unique(Raw_Data_Cleaned$Industry)
    )
    
    updateSelectInput(session, "location_set",
                      label =  "Prefered Industry Domain", 
                      choices = unique(Raw_Data_Cleaned$State)
    )
    
    
    
  })
  
  
  
  
  observeEvent(input$update_output, {
    
    ## Storing selection option by user
    job_type_user <<- input$job_type_user
    groupby_user  <<- input$groupby_user
    summarize_user <<- input$summarize_user
    
    #RawData_Filter <<- RawData[RawData$Category %in% input$job_type_user,]
    
    RawData_Filter <<- Raw_Data_Cleaned[Raw_Data_Cleaned$Category %in% input$job_type_user,]
    
    
    
    # data <- RawData_Filter
    # 
    # #split Salary to min_salary and max_salary
    # data <- data[!(data$Salary.Estimate == "-1"), ]
    # data$min_salary <- str_split_fixed(data$Salary.Estimate, "-", 2)[,1]
    # data$max_salary <- str_split_fixed(data$Salary.Estimate, "-", 2)[,2]
    # data$min_salary <- parse_number(data$min_salary )
    # data$max_salary <- parse_number(data$max_salary )
    # 
    # ##change easy.apply == -1 to False
    # data$Easy.Apply <- ifelse(data$Easy.Apply< 0, 0, data$Easy.Apply)
    # data$Easy.Apply <- ifelse(data$Easy.Apply == 'TRUE', 1, data$Easy.Apply)
    # 
    # #unique(data$Easy.Apply)
    # n_easy <- data %>% group_by(Easy.Apply)%>% dplyr::summarise (n=n()) 
    # perc_easy_apply = n_easy[[2]][2]/sum(n_easy[[2]])
    # 
    # ## convert type Rating to numeric
    # data$Rating <- as.numeric(data$Rating)
    # data$Rating <- ifelse(data$Rating< 0, 0, data$Rating)
    # 
    # ## clean up company name
    # data$Company.Name= str_split_fixed(data$Company.Name, "\n", 2)
    # 
    # ## take out State from location
    # data$State <- str_split_fixed(data$Location, ", ", 2)[,2]
    # data <- data[!(nchar(as.character(data$State)) > 2),]
    # data <- data[!(data$State == ""), ]
    # 
    # ## clean up empty values from industry        
    # data <- data[!(data$Industry == "-1"), ]
    # 
    # RawData_Filter <<-data
    
    if(groupby_user == "Industry"){
      
      if(summarize_user == "n_jobs"){
        
        Group_Summary <<- RawData_Filter %>% group_by_(Group_var = groupby_user) %>% 
          summarise_(Summary_var =interp(~n(), x = as.name(summarize_user)))%>%
          arrange_(desc(summarize_user), by_group = TRUE)
        Group_Summary <<- Group_Summary[1:5,]
        
      } else{
        
        Group_Summary <<- RawData_Filter %>% group_by_(Group_var = groupby_user) %>% 
          summarise_(Summary_var =interp(~mean(x), x = as.name(summarize_user))) %>%
          arrange_(desc(summarize_user), by_group = TRUE)
        Group_Summary <<- Group_Summary[1:5,]
        
      }
      
      
    }else{
      
      if(summarize_user == "n_jobs"){
        
        Group_Summary <<- RawData_Filter %>% group_by_(Group_var = groupby_user) %>% 
          summarise_(Summary_var =interp(~n(), x = as.name(summarize_user)))
        
      } else{
        
        Group_Summary <<- RawData_Filter %>% group_by_(Group_var = groupby_user) %>% 
          summarise_(Summary_var =interp(~mean(x), x = as.name(summarize_user)))
        
      }
      
    }
    
    output$plot <- renderPlot({
      ggplot(data = Group_Summary, aes(x =  Group_var  , y= Summary_var )) + 
        geom_bar(stat = "identity", color = "red")
    })
    
    
  })
  
  ## Output Button For Recommendation
  observeEvent(input$generate_output, {
    work_exp_selected <<- input$work_exp
    job_selected  <<- input$job_role
    location_selected <<- input$location_set
    industry_selected <<- input$industry_set
    skill_set_selected <<- input$skill_set
    
    #### Shana Code Part
    
    #browser()
    
    data = Raw_Data_Cleaned %>%
      mutate(category_new = case_when(
        Category == 'BA' ~ "Business Analyst" ,
        Category == 'DA'  ~ "Data Analyst"  ,
        Category == 'DE'  ~ "Data Engineer" ,
        Category == 'DS' ~ "Data Scientist"
      )
      )
    
    
    # #split Salary to min_salary and max_salary
    # data$min_salary = str_split_fixed(data$Salary.Estimate, "-", 2)[,1]
    # data$max_salary = str_split_fixed(data$Salary.Estimate, "-", 2)[,2]
    # data$min_salary <- parse_number(data$min_salary )
    # data$max_salary <- parse_number(data$max_salary )
    # 
    # ##change easy.apply == -1 to False 
    # #data$Easy.Apply <- ifelse(data$Easy.Apply< 0, False, data$Easy.Apply)
    # # unique(data$Easy.Apply)
    # n_easy <- data %>% group_by(Easy.Apply)%>% dplyr::summarise (n=n()) 
    # perc_easy_apply = n_easy[[2]][2]/sum(n_easy[[2]])
    # 
    # ## convert type Rating to numeric
    # data$Rating <- as.numeric(data$Rating)
    # 
    # ## clean up company name
    # data$Company.Name= str_split_fixed(data$Company.Name, "\n", 2)
    # 
    # ## take out State from location
    # data$State <- str_split_fixed(data$Location, ", ", 2)[,2]
    data$city <- str_split_fixed(data$Location, ", ", 2)[,1]
    #data$city
    # data <- data[!(nchar(as.character(data$State)) > 2),]
    # data <- data[!(data$State == ""), ]
    # 
    # View(data)
    #unique(data$State)
    
    data$Rating_new = paste(as.character(floor(data$Rating)), "-", as.character(floor(data$Rating)+1))
    
    
    
    colnames(data)
    
    # input data
    location =  location_selected    #c("CA", "NY") #variable 1
    position =  job_selected            #c('Data Scientist', 'Data Analyst') #variable 2
    skill_set = tolower(skill_set_selected)                       #c('python', 'r', 'sql', 'tableau')   #要記得轉換大小寫
    industry = industry_selected                                                    #c('Internet', 'Consulting')
    
    # data[which(data$State %in% location)]
    data_new = subset(data, data$State %in% location)
    # data_new
    nrow(data_new)
    data_new = subset(data_new, data_new$category_new %in% position)
    nrow(data_new)
    data_new = subset(data_new, data_new$Industry %in% industry)
    
    nrow(data_new)
    
    data_new$all_skill_needed = c(paste(data_new$python, 
                                        data_new$r,       
                                        data_new$sql,                                                
                                        data_new$c,                                                                                                   
                                        data_new$java,                                                  data_new$javascript,                                         
                                        data_new$julia,                                                 data_new$swift,                                                
                                        data_new$bash,                                                  data_new$matlab, data_new$gradient,                                                                                        
                                        data_new$matplotlib,                                           
                                        data_new$seaborn,                                               data_new$plotly ,                                            
                                        data_new$ggplot,                                               data_new$shiny,   data_new$leaflet,                                      
                                        data_new$gradient.boosting.machines,                            data_new$bayesian.approaches,                                  
                                        data_new$convolutional.neural.networks,                       #data_new$excel,   
                                        data_new$generative.adversarial.networks ,                  
                                        data_new$generative.networks,                                  
                                        data_new$word.embeddings.vectors,                                                         
                                        data_new$microsoft.azure,                                      
                                        data_new$mysql  ,                                              data_new$postgressql,                                          
                                        data_new$sqlite   ,                                             data_new$oracle.database,                                     
                                        data_new$mongodb ,                                            data_new$ibm.db2,                                              
                                        data_new$microsoft.sql.server ,                                 data_new$microsoft.access,                                     
                                        data_new$microsoft.power.bi ,                                                                
                                        data_new$salesforce,                                            data_new$qlik,                                                 
                                        data_new$tibco.spotfire  ,                                      data_new$sap.analytics.cloud,                                  
                                        data_new$feature.engineering ,                                
                                        data_new$automated.model.architecture.searches,                data_new$github,                                        
                                        data_new$tableau   , sep=","))
    
    # View(data_new)
    
    
    data_new$all_skill_needed = strsplit(data_new$all_skill_needed, ",")
    
    
    #define Jaccard Similarity function
    jaccard <- function(a, b) {
      intersection = length(intersect(a, b))
      union = length(a) + length(b) - intersection
      return (intersection/union)
    }
    
    # jaccard(skill_set, 'python')
    
    
    similarity = NULL
    
    for (i in 1:length(data_new$all_skill_needed)){
      print(i)
      similarity[i] =  jaccard(skill_set, unlist(data_new$all_skill_needed[i], recursive = TRUE, use.names = TRUE))
    }
    
    #browser()
    
    similarity
    sim_df = data.frame(similarity)
    sim_df
    
    data_final = cbind(data_new, sim_df) 
    
    nrow(data_final)
    
    data_final = data_final[order(data_final$similarity, decreasing = TRUE), ]
    
    head(data_final)
    
    recommend = head(data_final, 5)
    
    
    
    
    recommend = subset(recommend, select= c( 'Company.Name', 'Job.Title', 'Salary.Estimate', 'State', 'city', 'Size', 'Industry', 'Category'))
    # data_final = data.frame(recommend)
    # data_final
    #View(recommend)
    
    Final_Display_Data <<- as.data.frame(recommend)
    #### Shana Code Part END
    
    ## Display the table to UI
    #output$table <- renderTable(Final_Display_Data)
    output$table <- renderDataTable(Final_Display_Data)
    
  })
  
  
  
}
