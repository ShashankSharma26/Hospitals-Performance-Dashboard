##Installing and loading packages


#install.packages("readxl")
#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("plyr")
#install.packages("dplyr")
library(rsconnect)
library(readxl)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(shiny)

#reading the excel file

data <- read_excel("PCOR File.xlsx")

#extracting the month, year and date form the date
data$Monthofdiagnosis = format(data$Dateofdiagnosis,"%B")
data$Yearofdiagnosis =  format(data$Dateofdiagnosis,"%Y")
data$Dayofdiagnosis =  format(data$Dateofdiagnosis,"%d")

##calculating number of completed and incompleted assessmeents
sum_data <- aggregate(Indicator_X ~ Yearofdiagnosis +Hospital, data = data, sum)
names(sum_data)[names(sum_data) == 'Indicator_X'] <- 'Completed'
sum_data$Incompleted <- plyr::count(data,c("Hospital","Yearofdiagnosis"))[,3] - sum_data$Completed

#resahping using melting
new_data <- melt(sum_data,id=c( "Yearofdiagnosis","Hospital"))

##Calculating the completion ratio
new_data_A <- new_data[new_data$Hospital == 'A',]
new_data_A <- group_by(new_data_A, Yearofdiagnosis) %>% mutate(percent = value/sum(value))
new_data_A$percent <- new_data_A$percent*100

##User interface for the shiny app
ui <- fluidPage(
  tags$style("body {background-color: #F0F0F0; }"), ##background colour
  headerPanel(h2(strong("PSA Assessment And Documentation Completion report (2016-2018)"), align = "center", style = "color: brown; font-family: 'georgia';")), ##Main heading
  tags$style(HTML("
                  .tabbable > .nav > li > a                  {background-color: #C0C0C0;  color:black}   
                  .tabbable > .nav > li[class=active]    > a {background-color: #A0A0A0; color:white}
                  ")),
  tabsetPanel(
    tabPanel(
      h6(strong("Hospital A"),style = "color:blue;font-family: 'georgia';"), value = 1, ##for hospital A
      
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.tabs == 1",
            radioButtons("a_show",                                #year option radio button
                         h4(strong("Display the data from:"),style = "color:brown;font-family: 'georgia';" ),
                         choices = c("All years","Particular year"),
                         selected = c("All years"))
            
            
            
          ),
          conditionalPanel(
            condition = "(input.a_show == 'Particular year') & (input.tabs == 1)",
            selectInput("the_year",                                                 ##year selection input
                        h6(strong("Select year"),style = "color:brown;font-family: 'georgia';"),
                        choices = c("2016","2017","2018")
            )
            
            
            
          ),
          conditionalPanel(
            condition = "(input.tabs == 1)",
            radioButtons("the_plot",                                               #radion button fro type of plot
                         h4(strong("Report based on:"),style = "color:brown;font-family: 'georgia';"),
                         choices = c("Number of PSA Assessments","Percentage of Completed PSA Assessments")
            )
            
            
            
          ),h5(strong("Key Observations"),style ="color:brown;font-family: 'georgia';" ),
          
          tags$ul(
            tags$li(strong("Hospital A exhibted its best PSA assessment and documentation completion ratio in year 2018.")), 
            tags$li(strong("The inconsistency in PSA assessment and documentation completion ratio within a year has increased with time at Hospital A."))
            
          ),
          width = 4
          
          
        ),
        
        mainPanel(h3("Hospital A Report", align = "center", style = "color:blue;font-family: 'georgia';"),  ##main panel for plots
                  conditionalPanel(
                    condition = "(input.tabs == 1) & (input.the_plot =='Number of PSA Assessments' ) & (input.a_show == 'All years')",
                    plotOutput("allplot_all_years")
                    
                  ),
                  conditionalPanel(
                    condition = "(input.tabs == 1) & (input.the_plot =='Number of PSA Assessments' ) & (input.a_show == 'Particular year')",
                    plotOutput("allplot_select_years")
                  ),
                  conditionalPanel(
                    condition = "(input.tabs == 1) & (input.the_plot =='Percentage of Completed PSA Assessments' ) & (input.a_show == 'All years')",
                    plotOutput("percent_all_years")
                  ),
                  conditionalPanel(
                    condition = "(input.tabs == 1) & (input.the_plot =='Percentage of Completed PSA Assessments' ) & (input.a_show == 'Particular year')",
                    plotOutput("percent_select_years")
                  )
        )
        
      )
      
      
    ),
    
    tabPanel(
      h6(strong("All Hospitals"),style = "color:blue;font-family: 'georgia';"), value = 2, ##for all hospitals
      
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.tabs == 2",
            radioButtons("all_show",                                #year option radio button
                         h5(strong("Display the data from:"),style = "color:brown;font-family: 'georgia';" ),
                         choices = c("All years","Particular year"),
                         selected = c("All years"))
            
            
            
          ),
          conditionalPanel(
            condition = "(input.all_show == 'Particular year') & (input.tabs == 2)",
            selectInput("all_the_year",
                        h6(strong("Select year"),style = "color:brown;font-family: 'georgia';"),
                        choices = c("2016","2017","2018")
            )
            
            
            
          ),h5(strong("Key Observations"),style ="color:brown;font-family: 'georgia';" ),
          
          tags$ul(
            tags$li(strong("Hospital B and P have the best PSA assessment and documentation completion ratio amongst all other hopitals throughout the 3 years period ")), 
            tags$li(strong("The PSA assessment and documentation completion ratio of Hospital E has improved with time but still it has has the worst completion ratio amongst all other hospitals.")),
            tags$li(strong("Hospital D and E exhibit most consistent PSA assessment and documentation completion ratio amongst all other Hospitals. "))
            
          ),
          width = 3
          
        ),
        mainPanel(h3("Combined Report For All Hospitals", align = "center", style = "color:blue;font-family: 'georgia';"),
                  conditionalPanel(
                    condition = "(input.tabs == 2) & (input.all_show == 'All years')",
                    plotOutput("all_years")
                  ),
                  
                  conditionalPanel(
                    condition = "(input.tabs == 2) & (input.all_show == 'Particular year')",
                    plotOutput("a_years_allmon")
                  )
        )
        
      )
      
      
    ),
    tabPanel(
      h6(strong("Selected Hospitals"),style = "color:blue;font-family: 'georgia';"), value = 3, 
      
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.tabs == 3",
            checkboxGroupInput("selected_show",                            ##to select hospitals
                               h5(strong("Display the data for:"),style = "color:brown;font-family: 'georgia';" ),
                               choices = c("A","B","C","D","E","F","G","H","I","L","M","N","O","P"),
                               selected = c("A","B")),
            
            radioButtons("selected_years",                                #year selection radio button
                         h5(strong("Display the data from:"),style = "color:brown;font-family: 'georgia';" ),
                         choices = c("All years","Particular year"),
                         selected = c("All years"))
            
            
          ),
          conditionalPanel(
            condition = "(input.selected_years == 'Particular year') & (input.tabs == 3)",
            selectInput("selected_the_year",
                        h6(strong("Select year"),style = "color:brown;font-family: 'georgia';"),
                        choices = c("2016","2017","2018")
                        
            )
            
            
            
          ),
          conditionalPanel(
            condition = "(input.tabs == 3)",
            radioButtons("the_multi_hosp_plot",
                         h6(strong("Report based on:"),style = "color:brown;font-family: 'georgia';"),
                         choices = c("Number of PSA Assessments","Percentage of Completed PSA Assessments")
            )
            
            
            
          ),
          width = 3
          
          
          
          
          
          
        ),
        mainPanel(h3("Report For Selected Hospital", align = "center", style = "color:blue;font-family: 'georgia';"),
                  conditionalPanel(
                    condition = "(input.tabs == 3) & (input.selected_years == 'All years') & (input.the_multi_hosp_plot == 'Number of PSA Assessments')",
                    plotOutput("selec_hopital_all_years")
                  ),
                  conditionalPanel(
                    condition = "(input.tabs == 3) & (input.selected_years == 'Particular year') & (input.the_multi_hosp_plot == 'Number of PSA Assessments')",
                    plotOutput("selec_hopital_one_years")
                  ),
                  conditionalPanel(
                    condition = "(input.tabs == 3) & (input.selected_years == 'All years') & (input.the_multi_hosp_plot == 'Percentage of Completed PSA Assessments')",
                    plotOutput("selec_hopital_perc")
                  ),
                  conditionalPanel(
                    condition = "(input.tabs == 3) & (input.selected_years == 'Particular year') & (input.the_multi_hosp_plot == 'Percentage of Completed PSA Assessments')",
                    plotOutput("selec_hopital_last")
                  )
        )
        
      )
      
    ),
    id = 'tabs'
    
    
    
  )
  
  
  )


server <- function(input, output) {
  output$allplot_all_years  <- reactivePlot(function()
  {
    ggplot(new_data_A,aes(x=Yearofdiagnosis,y=value,fill=factor(variable)))+coord_flip() +geom_bar(stat="identity",position="dodge", width = 0.6)+scale_fill_manual("PSA assessment and documentation status \n", values = c("lightgreen","darkred"), labels = c(" Complete", " Incomplete"))+xlab("Year")+ylab("Count") +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=13))
    
    
  }, height = 600, width = 1000)
  
  output$allplot_select_years  <- reactivePlot(function()
  {
    year <- input$the_year
    part_year_A <- data[(data$Yearofdiagnosis == year) & (data$Hospital == 'A'), ]
    
    A_month <- aggregate(Indicator_X ~ Monthofdiagnosis +Hospital, data = part_year_A, sum)
    names(A_month)[names(A_month) == 'Indicator_X'] <- 'Completed'
    A_month$Incompleted <- plyr::count(part_year_A,c("Hospital","Monthofdiagnosis"))[,3] - A_month$Completed
    
    
    A_new_data <- melt(A_month,id=c( "Monthofdiagnosis","Hospital"))
    A_new_data$Monthofdiagnosis = factor(A_new_data$Monthofdiagnosis, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
    
    
    ggplot(A_new_data,aes(x=Monthofdiagnosis,y=value,fill=factor(variable)))+coord_flip() +geom_bar(stat="identity",position="dodge", width = 0.6)+scale_fill_manual("PSA assessment and documentation status \n", values =  c("lightgreen","darkred"), labels = c(" Complete", " Incomplete"))+xlab("Month")+ylab("Count") +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=12)) +  scale_x_discrete(limits=c("December","November","October","September" ,"August","July","June","May","April","March" ,"February" ,"January" )) 
    
    
    
  }, height = 600, width = 1000)
  
  output$all_years  <- reactivePlot(function()
  {
    ggplot(new_data, aes(x = variable, y = value,fill=factor(variable))) + geom_bar(stat="identity",position="dodge", width = 0.6) + facet_grid( Yearofdiagnosis ~ Hospital) + scale_fill_manual("PSA assessment and documentation status \n", values =  c("lightgreen","darkred"), labels = c(" Complete", " Incomplete")) + labs(y ='Count', x = 'PSA assessment and documentation status',title = 'PSA Assessment Completion status for all hospitals from 2016-2018') +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=13) , axis.text.x = element_text(angle = 90, hjust = 1))
    
    
  }, height = 800, width = 1200)
  
  output$a_years_allmon  <- reactivePlot(function()
  {
    select_year <- input$all_the_year
    
    part_year <- data[(data$Yearofdiagnosis == select_year), ]
    
    all_month <- aggregate(Indicator_X ~ Monthofdiagnosis +Hospital, data = part_year, sum)
    names(all_month)[names(all_month) == 'Indicator_X'] <- 'Completed'
    all_month$Incompleted <- plyr::count(part_year,c("Hospital","Monthofdiagnosis"))[,3] - all_month$Completed
    
    
    all_new_data <- melt(all_month,id=c( "Monthofdiagnosis","Hospital"))
    all_new_data$Monthofdiagnosis = factor(all_new_data$Monthofdiagnosis, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
    
    ggplot(all_new_data, aes(x = variable, y = value,fill=factor(variable))) + geom_bar(stat="identity",position="dodge", width = 0.6) + facet_grid( Monthofdiagnosis ~ Hospital) + scale_fill_manual("PSA assessment and documentation status \n", values =  c("lightgreen","darkred"), labels = c(" Complete", " Incomplete")) + labs(y ='Count', x = 'PSA assessment and documentation status',title = 'PSA Assessment Completion status for all hospitals for Selected Year') +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold") , axis.text.x = element_text(angle = 90, hjust = 1),legend.text=element_text(size=13))
    
    
  }, height = 1500, width = 1300)
  
  output$percent_all_years  <- reactivePlot(function()
  {
    ggplot(new_data_A[new_data_A$variable == 'Completed',], aes(x=Yearofdiagnosis, y = percent, group = 1)) + geom_line(linetype = "dashed",color="red") + geom_point()  + labs(y ='Percentage of completed assessments', x = 'Year',title = 'PSA Assessment Completion status for Selected hospitals for Selected Year')
    
  }, height = 500, width = 700)
  
  
  output$percent_select_years  <- reactivePlot(function()
  {
    year <- input$the_year
    part_year_A <- data[(data$Yearofdiagnosis == year) & (data$Hospital == 'A'), ]
    
    A_month <- aggregate(Indicator_X ~ Monthofdiagnosis +Hospital, data = part_year_A, sum)
    names(A_month)[names(A_month) == 'Indicator_X'] <- 'Completed'
    
    A_month$Incompleted <- plyr::count(part_year_A,c("Hospital","Monthofdiagnosis"))[,3] - A_month$Completed
    A_new_data <- melt(A_month,id=c( "Monthofdiagnosis","Hospital"))
    
    A_new_data <- group_by(A_new_data, Monthofdiagnosis) %>% mutate(percent = value/sum(value))
    A_new_data$percent <- A_new_data$percent*100
    A_new_data$Monthofdiagnosis = factor(A_new_data$Monthofdiagnosis, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
    
    ggplot(A_new_data[A_new_data$variable == 'Completed',], aes(x=Monthofdiagnosis, y = percent, group = 2)) + geom_line(linetype = "dashed",color="red") + geom_point() + scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December"))  + labs(y ='Percentage of completed assessments', x = 'Month of Year',title = 'PSA Assessment Completion status for A hospital for Selected Year')
    
    
  }, height = 500, width = 700)
  
  output$selec_hopital_all_years  <- reactivePlot(function()
  {
    the_hospital <- c(input$selected_show)
    
    sum_data <- aggregate(Indicator_X ~ Yearofdiagnosis +Hospital, data = data, sum)
    names(sum_data)[names(sum_data) == 'Indicator_X'] <- 'Completed'
    sum_data$Incompleted <- plyr::count(data,c("Hospital","Yearofdiagnosis"))[,3] - sum_data$Completed
    
    
    new_data <- melt(sum_data,id=c( "Yearofdiagnosis","Hospital"))
    the_data <- new_data[new_data$Hospital %in% the_hospital,]
    
    
    ggplot(the_data, aes(x = variable, y = value,fill=factor(variable))) + geom_bar(stat="identity",position="dodge", width = 0.6) + facet_grid( Yearofdiagnosis ~ Hospital) + scale_fill_manual("PSA assessment and documentation status \n", values =  c("lightgreen","darkred"), labels = c(" Complete", " Incomplete")) + labs(y ='Count', x = 'PSA assessment and documentation status',title = "PSA Assessment Completion status for Selected hospitals for Selected Year") +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold") , axis.text.x = element_text(angle = 90, hjust = 1),legend.text=element_text(size=13))
    
    
  }, height = 1000, width = 1000)
  
  
  output$selec_hopital_perc  <- reactivePlot(function()
  {
    se_hospitals <- c(input$selected_show)
    new_data_B <- new_data[new_data$Hospital %in% se_hospitals,]
    new_data_B <- group_by(new_data_B, .dots =c("Yearofdiagnosis","Hospital")) %>% mutate(percent = value/sum(value))
    new_data_B$percent <- new_data_B$percent*100
    
    ggplot(new_data_B[new_data_B$variable == 'Completed',], aes(x=Yearofdiagnosis, y = percent, colour = Hospital, group = Hospital)) + geom_line() + geom_point()  + labs(y ='Percentage of completed assessments', x = 'Year',title = 'PSA Assessment Completion status for Selected hospitals in Years 2016-2018')
    
    
  }, height = 700, width = 700)
  
  output$selec_hopital_one_years  <- reactivePlot(function()
  {
    a_hospital <- c(input$selected_show)
    sel_year <- input$selected_the_year
    
    a_data <- data[(data$Yearofdiagnosis == sel_year),  ]
    
    sub_data <- aggregate(Indicator_X ~ Monthofdiagnosis +Hospital, data = a_data, sum)
    names(sub_data)[names(sub_data) == 'Indicator_X'] <- 'Completed'
    sub_data$Incompleted <- plyr::count(a_data,c("Hospital","Monthofdiagnosis"))[,3] - sub_data$Completed
    
    sub_data <- melt(sub_data,id=c( "Monthofdiagnosis","Hospital"))
    
    sub_data <- sub_data[sub_data$Hospital %in% a_hospital,]
    sub_data$Monthofdiagnosis = factor(sub_data$Monthofdiagnosis, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
    
    
    ggplot(sub_data, aes(x = variable, y = value,fill=factor(variable))) + geom_bar(stat="identity",position="dodge", width = 0.6) + facet_grid( Monthofdiagnosis ~ Hospital) + scale_fill_manual("PSA assessment and documentation status \n", values =  c("lightgreen","darkred"), labels = c(" Complete", " Incomplete")) + labs(y ='Count', x = 'PSA assessment and documentation status',title = 'PSA Assessment Completion status for Selected hospitals for Selected Year') +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold") , axis.text.x = element_text(angle = 90, hjust = 1),legend.text=element_text(size=13))
    
    
  }, height = 1500, width = 800)
  
  output$selec_hopital_last <- reactivePlot(function()
  {
    e_hospitals <- c(input$selected_show)
    selected_year <- input$selected_the_year
    
    D_data <- data[(data$Yearofdiagnosis == selected_year),  ]
    
    
    D_data <- aggregate(Indicator_X ~ Monthofdiagnosis +Hospital, data = data, sum)
    names(D_data)[names(D_data) == 'Indicator_X'] <- 'Completed'
    
    D_data$Incompleted <- plyr::count(data,c("Hospital","Monthofdiagnosis"))[,3] - D_data$Completed
    
    D_data <- melt(D_data,id=c( "Monthofdiagnosis","Hospital"))
    
    D_data <- D_data[D_data$Hospital %in% e_hospitals,]
    
    D_data <- group_by(D_data, .dots =c("Monthofdiagnosis","Hospital")) %>% mutate(percent = value/sum(value))
    D_data$percent <- D_data$percent*100
    
    D_data$Monthofdiagnosis = factor(D_data$Monthofdiagnosis, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
    
    ggplot(D_data[D_data$variable == 'Completed',], aes(x=Monthofdiagnosis, y = percent, colour = Hospital, group = Hospital)) + geom_line() + geom_point()  + labs(y ='Percentage of completed assessments', x = 'Month of Year',title = 'PSA Assessment Completion status for Selected hospitals for Selected Year')
    
    
  }, height = 700, width = 700)
  
}


shinyApp(ui, server)